/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package huancun.inclusive

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink._
import huancun._
import huancun.MetaData._
import huancun.prefetch._

class MSHR()(implicit p: Parameters) extends BaseMSHR[DirResult, DirWrite, TagWrite] {
  val io = IO(new BaseMSHRIO[DirResult, DirWrite, TagWrite] {
    override val tasks = new MSHRTasks[DirWrite, TagWrite] {
      override val dir_write: DecoupledIO[DirWrite] = DecoupledIO(new DirWrite())
      override val tag_write: DecoupledIO[TagWrite] = DecoupledIO(new TagWrite())
    }
    override val dirResult = Flipped(ValidIO(new DirResult()))
  })

  val req = Reg(new MSHRRequest)
  val req_valid = RegInit(false.B)
  val meta_reg = Reg(new DirResult)
  val meta = Wire(new DirResult)
  val meta_valid = RegInit(false.B)

  // Get directory result
  assert(
    RegNext(!io.dirResult.valid || req_valid && !meta_valid, true.B),
    "Directory result was sent to mismatch MSHR(mshrId:%d, resultId:%d)",
    io.id,
    OHToUInt(io.dirResult.bits.idOH)
  )
  assert(!req_valid || !req.fromCmoHelper, "TODO: Inclusive cache support CMO operation")
  when(io.dirResult.valid) {
    meta_valid := true.B
    meta_reg := io.dirResult.bits
  }
  meta := Mux(io.dirResult.valid, io.dirResult.bits, meta_reg)
  dontTouch(meta)

  // Final meta to be written
  val new_meta = WireInit(meta)
  val req_acquire = req.opcode === AcquireBlock || req.opcode === AcquirePerm
  val req_needT = needT(req.opcode, req.param)
  val gotT = RegInit(false.B) // L3 might return T even though L2 wants B
  val meta_no_client = !meta.clients.orR
  val req_promoteT = req_acquire && Mux(meta.hit, meta_no_client && meta.state === TIP, gotT)
  val req_realBtoT = meta.hit && (meta.clients & getClientBitOH(req.source)).orR
  val prefetch_miss = hintMiss(meta.state, req.param)
  val probes_toN = RegInit(0.U(clientBits.W))
  val probes_done = RegInit(0.U(clientBits.W))
  val probe_exclude =
    Mux(
      req.fromA && meta.hit && skipProbeN(req.opcode),
      getClientBitOH(req.source),
      0.U
    ) // Client acquiring the block does not need to be probed
  val probe_next_state = Mux(
    isT(meta.state) && req.param === toT,
    meta.state,
    Mux(meta.state =/= INVALID && req.param =/= toN, BRANCH, INVALID)
  )
  when(req.fromC) {
    // Release / ReleaseData
    new_meta.dirty := meta.dirty || req.opcode(0)
    new_meta.state := Mux(req.param === TtoB || req.param === TtoN, TIP, meta.state)
    new_meta.clients := meta.clients & ~Mux(isToN(req.param), getClientBitOH(req.source), 0.U)
    new_meta.hit := true.B
  }.elsewhen(req.fromB) {
    new_meta.dirty := req.param === toT && meta.dirty
    new_meta.state := probe_next_state
    new_meta.clients := Mux(req.param === toN, 0.U, meta.clients)
    // TODO: if a TIP/TRUNK is probed to be BRANCH, do we need to probe clients to INVALID?
    new_meta.hit := false.B
  }.otherwise {
    // Acquire / Intent / Put / Get / Atomics
    new_meta.dirty := meta.hit && meta.dirty || !req.opcode(2) // Put / Atomics
    new_meta.state := Mux(
      req_needT,
      Mux(
        req_acquire || req.opcode === Hint && meta.state === TRUNK,
        TRUNK, // Acquire (NtoT/BtoT) / Intent (PrefetchWrite) on a TRUNK
        TIP
      ), // Intent (PrefetchWrite) on un-TRUNK / Put / Atomics
      Mux(
        !meta.hit, // The rest are Acquire (NtoB) / Intent (PrefetchRead) / Get
        // If tag miss, new state depends on what L3 grants
        Mux(gotT, Mux(req_acquire, TRUNK, TIP), BRANCH),
        MuxLookup(
          meta.state,
          BRANCH)(
          Seq(
            INVALID -> BRANCH,
            BRANCH -> BRANCH,
            TRUNK -> Mux(req.opcode === Hint, TRUNK, TIP),
            TIP -> Mux(meta_no_client && req_acquire, TRUNK, TIP)
          )
        )
      )
    )
    new_meta.clients := Mux(meta.hit, meta.clients & ~probes_toN, 0.U) | Mux(
      req_acquire,
      getClientBitOH(req.source),
      0.U
    )
    new_meta.hit := true.B
  }
  val new_dir = Wire(new DirectoryEntry)
  new_dir.dirty := new_meta.dirty
  new_dir.state := new_meta.state
  new_dir.clients := new_meta.clients
  new_dir.prefetch.foreach(_ := prefetch_miss && req.opcode === Hint || meta.prefetch.get)

  val sink = Reg(UInt(edgeOut.bundle.sinkBits.W))

  val bad_grant = Reg(Bool())
  when(bad_grant) {
    new_meta.dirty := false.B
    new_meta.state := Mux(meta.hit, BRANCH, INVALID)
    new_meta.clients := Mux(meta.hit, meta.clients & ~probes_toN, 0.U)
    new_meta.hit := meta.hit
  }

  assert(RegNext(!meta_valid || !req.fromC || meta.hit, true.B)) // Release should always hit

  val change_meta = meta_valid && meta_reg.state =/= INVALID &&
    (io.nestedwb.set === req.set && io.nestedwb.tag === meta.tag)

  when(change_meta) {
    when(io.nestedwb.b_clr_dirty) { meta_reg.dirty := false.B }
    when(io.nestedwb.c_set_dirty) { meta_reg.dirty := true.B }
    when(io.nestedwb.b_toB) { meta_reg.state := BRANCH }
    when(io.nestedwb.b_toN) { meta_reg.hit := false.B }
  }

  // Set tasks to be scheduled and resps to wait for
  val s_acquire = RegInit(true.B) // source_a
  val s_rprobe = RegInit(true.B) // source_b
  val s_pprobe = RegInit(true.B)
  val s_release = RegInit(true.B) // source_c
  val s_probeack = RegInit(true.B)
  val s_execute = RegInit(true.B) // source_d
  val s_grantack = RegInit(true.B) // source_e
  val s_writebacktag = RegInit(true.B) // tag_write
  val s_writebackdir = RegInit(true.B) // dir_write
  val s_transferput = RegInit(true.B) // writeput to source_a
  val s_writerelease = RegInit(true.B) // sink_c
  val s_triggerprefetch =
    prefetchOpt.map(_ => RegInit(true.B)) // trigger a prefetch training to prefetcher
  val s_prefetchack = prefetchOpt.map(_ => RegInit(true.B)) // resp to prefetcher

  val w_rprobeackfirst = RegInit(true.B)
  val w_rprobeacklast = RegInit(true.B)
  val w_pprobeackfirst = RegInit(true.B)
  val w_pprobeacklast = RegInit(true.B)
  val w_pprobeack = RegInit(true.B)
  val w_grantfirst = RegInit(true.B)
  val w_grantlast = RegInit(true.B)
  val w_grant = RegInit(true.B)
  val w_releaseack = RegInit(true.B)
  val w_grantack = RegInit(true.B)

  when(io.dirResult.valid) {
    // Default value
    s_acquire := true.B
    s_rprobe := true.B
    s_pprobe := true.B
    s_release := true.B
    s_probeack := true.B
    s_execute := true.B
    s_grantack := true.B
    s_writebacktag := true.B
    s_writebackdir := true.B
    s_transferput := true.B
    s_writerelease := true.B
    s_triggerprefetch.foreach(_ := true.B)
    s_prefetchack.foreach(_ := true.B)

    w_rprobeackfirst := true.B
    w_rprobeacklast := true.B
    w_pprobeackfirst := true.B
    w_pprobeacklast := true.B
    w_pprobeack := true.B
    w_grantfirst := true.B
    w_grantlast := true.B
    w_grant := true.B
    w_releaseack := true.B
    w_grantack := true.B

    gotT := false.B
    probes_toN := 0.U
    probes_done := 0.U
    bad_grant := false.B

    assert(!io.dirResult.bits.hit || !io.dirResult.bits.error)

    when(req.fromC) {
      // Release
      s_execute := false.B
      when(
        !meta.dirty && req.opcode(0) || // from clean to dirty
          (req.param === TtoB || req.param === TtoN) && meta.state === TRUNK || // from TRUNK to TIP
          isToN(req.param)
      ) { // change clients
        s_writebackdir := false.B
      }

      when(req.opcode(0)) { // has data
        s_writerelease := false.B
        // when (req.opcode === ReleaseData) {
        //  assert(req.dirty === true.B) // for inclusive data, we assume releaseData as dirty
        // }
      }

    }.elsewhen(req.fromB) {
      // Probe
      s_probeack := false.B
      when(meta.hit) {
        when(isT(meta.state) && req.param =/= toT || meta.state === BRANCH && req.param === toN) { // state demotion
          s_writebackdir := false.B
          when(!meta_no_client) {
            s_pprobe := false.B
            w_pprobeackfirst := false.B
            w_pprobeacklast := false.B
            w_pprobeack := false.B
          }
        }
      }
    }.elsewhen(req.opcode(2,1) === 0.U) { // Put
      // need pprobe
      when(meta.hit && meta.state === TRUNK) {
        s_pprobe := false.B
        w_pprobeackfirst := false.B
        w_pprobeacklast := false.B
        w_pprobeack := false.B
        s_writebackdir := false.B
      }
      // Put and Atomics need to write
      when(meta.hit && meta.state === TIP) {
        s_writebackdir := false.B
      }
      // need to transfer exactly the request to sourceA when Put miss
      when(!meta.hit || meta.state === BRANCH) { // Put[Full/Partial]Data
        s_transferput := false.B
      }
    }.otherwise {
      // A channel requests
      // TODO: consider parameterized write-through policy for put/atomics
      // Since prefetch uses inner interface, Hint does not need a HintAck
      s_execute := req.opcode === Hint
      // need replacement
      when(!meta.hit && meta.state =/= INVALID) {
        s_release := false.B
        w_releaseack := false.B
        // need rprobe for release
        when(!meta_no_client) {
          s_rprobe := false.B
          w_rprobeackfirst := false.B
          w_rprobeacklast := false.B
        }
      }
      // need Acquire downwards
      when(!meta.hit || meta.state === BRANCH && req_needT) {
        s_acquire := false.B
        s_grantack := false.B
        s_writebackdir := false.B
        w_grantfirst := false.B
        w_grantlast := false.B
        w_grant := false.B
      }
      // need pprobe
      when(
        meta.hit && (req_needT || meta.state === TRUNK) && req.opcode =/= Hint &&
          (
            meta.clients & (~Mux(skipProbeN(req.opcode), getClientBitOH(req.source), 0.U)).asUInt
          ).orR
      ) {
        s_pprobe := false.B
        w_pprobeackfirst := false.B
        w_pprobeacklast := false.B
        w_pprobeack := false.B
        s_writebackdir := false.B
      }
      // need grantack
      when(req_acquire) {
        w_grantack := false.B
        s_writebackdir := false.B
      }
      // need write tag
      when(!meta.hit) {
        s_writebacktag := false.B
      }
      // trigger a prefetch when req is from DCache, and miss / prefetched hit in L2
      prefetchOpt.map(_ => {
        when(req.opcode =/= Hint && getClientBitOH(req.source).orR && (!meta.hit || meta.prefetch.get)) {
          s_triggerprefetch.map(_ := false.B)
        }
        when(req.opcode === Hint) {
          s_prefetchack.map(_ := false.B)
        }
      })
    }
  }

  // 5. Send out tasks and mark the s_* state regs
  /* Consider a partial order as follows:
   *
   *        s_rprobe(B)
   *                 \
   *                  \
   *           s_release(C)      s_pprobe(B)
   *                    \         /       \
   *                     \       /         \
   *                    s_acquire(A)       s_probeack(C)
   *                   /     |     \
   *                  /      |      \
   *      s_grantack(E) s_execute(D) s_writeput
   *                   or s_prefetchack
   *
   * The edges between s_* state regs from top to bottom indicate the scheduling priority.
   * For example, s_release > s_acquire and s_pprobe > s_acquire mean that before
   * sending an Acquire, make sure Release and PProbe have been sent out. Some edges
   * in this diagram need the prerequisite task to be issued while the others need
   * the prerequisite to be issued and also acknowledged (e.g. s_acquire > s_execute).
   *
   * Assume that in data array, sinkA > sinkC > sourceC > sinkD > sourceDw > sourceDr
   */
  val no_wait = w_rprobeacklast && w_pprobeacklast && w_grantlast && w_releaseack && w_grantack
  io.tasks.source_a.valid := (!s_acquire || !s_transferput) && s_release && s_pprobe
  io.tasks.source_b.valid := !s_rprobe || !s_pprobe
  io.tasks.source_c.valid := !s_release && w_rprobeackfirst || !s_probeack && w_pprobeackfirst
  io.tasks.source_d.valid := !s_execute && w_grant && w_pprobeack
  io.tasks.source_e.valid := !s_grantack && w_grantfirst
  io.tasks.dir_write.valid := !s_writebackdir && no_wait || !s_release && w_rprobeackfirst // TODO: Is the latter clause necessary?
  io.tasks.tag_write.valid := !s_writebacktag && no_wait
  // io.tasks.sink_a.valid := !s_transferput && w_grant && w_pprobeack
  io.tasks.sink_a.valid := false.B
  io.tasks.sink_c.valid := !s_writerelease // && w_grant && w_pprobeack
  io.tasks.prefetch_train.foreach(_.valid := !s_triggerprefetch.get)
  io.tasks.prefetch_resp.foreach(_.valid := !s_prefetchack.get && w_grantfirst)

  val oa = io.tasks.source_a.bits
  val ob = io.tasks.source_b.bits
  val oc = io.tasks.source_c.bits
  val od = io.tasks.source_d.bits
  val oe = io.tasks.source_e.bits
  val ia = io.tasks.sink_a.bits
  val ic = io.tasks.sink_c.bits

  oa.tag := req.tag
  oa.set := req.set
  oa.off := req.off
  oa.mask := req.mask
  oa.opcode := Mux(!s_transferput, req.opcode, Mux(meta.hit, TLMessages.AcquirePerm, TLMessages.AcquireBlock))
  oa.param := Mux(!s_transferput, req.param, Mux(req_needT, Mux(meta.hit, BtoT, NtoT), NtoB))
  oa.source := io.id
  oa.needData := !(req.opcode === AcquirePerm) || req.size =/= offsetBits.U
  oa.bufIdx := req.bufIdx
  oa.putData := req.opcode(2,1) === 0.U
  oa.size := req.size

  ob.tag := Mux(!s_rprobe, meta.tag, req.tag)
  ob.set := req.set
  ob.param := Mux(!s_rprobe, toN, Mux(req.fromB, req.param, Mux(req_needT, toN, toB)))
  ob.clients := meta.clients & ~probe_exclude // TODO: Provides all clients needing probe

  oc.opcode := Mux(req.fromB, Cat(ProbeAck(2,1), meta.dirty.asUInt), if (alwaysReleaseData) ReleaseData else Cat(Release(2, 1), meta.dirty.asUInt))
  oc.tag := meta.tag
  oc.set := req.set
  oc.param := Mux(
    req.fromB,
    MuxLookup(
      Cat(meta.state, probe_next_state),
      NtoN)(
      Seq( // TODO: optimize this
        Cat(TRUNK, TRUNK) -> TtoT,
        Cat(TIP, TIP) -> TtoT,
        Cat(TRUNK, BRANCH) -> TtoB,
        Cat(TIP, BRANCH) -> TtoB,
        Cat(TRUNK, INVALID) -> TtoN,
        Cat(TIP, INVALID) -> TtoN,
        Cat(BRANCH, BRANCH) -> BtoB,
        Cat(BRANCH, INVALID) -> BtoN,
        Cat(INVALID, INVALID) -> NtoN
      )
    ),
    Mux(meta.state === BRANCH, BtoN, TtoN)
  )
  oc.source := io.id
  oc.way := meta.way
  oc.dirty := meta.dirty

  od.sinkId := io.id
  od.sourceId := req.source
  od.set := req.set
  od.tag := req.tag
  od.channel := Cat(req.fromC.asUInt, 0.U(1.W), req.fromA.asUInt)
  def odOpGen(r: MSHRRequest) = {
//    val grantOp = Mux(r.param === BtoT && req_realBtoT, Grant, GrantData)
    val grantOp = GrantData
    val opSeq = Seq(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, grantOp, Grant)
    val opToA = VecInit(opSeq)(r.opcode)
    Mux(r.fromA, opToA, ReleaseAck)
  }
  od.opcode := odOpGen(req)
  od.param :=
    Mux(
      !req_acquire,
      req.param,
      MuxLookup(req.param, req.param)(Seq(NtoB -> Mux(req_promoteT, toT, toB), BtoT -> toT, NtoT -> toT))
    )

  od.size := req.size
  od.way := meta.way
  od.off := req.off
  od.denied := bad_grant
  od.dirty := false.B // TODO
  od.useBypass := false.B
  od.bufIdx := req.bufIdx
  od.bypassPut := false.B

  oe.sink := sink

  ia.sourceId := req.source
  ia.set := req.set
  ia.tag := req.tag
  ia.size := req.size
  ia.off := req.off

  ic.sourceId := DontCare //req.source
  ic.source := io.id
  ic.set := req.set
  ic.tag := req.tag
  ic.size := req.size
  ic.off := req.off
  ic.way := meta.way
  ic.bufIdx := req.bufIdx
  ic.opcode := req.opcode
  ic.param := req.param
  ic.save := true.B // inclusive always save
  ic.drop := false.B
  ic.release := false.B
  ic.dirty := false.B // ignored

  io.tasks.dir_write.bits.set := req.set
  io.tasks.dir_write.bits.way := meta.way
  io.tasks.dir_write.bits.data := new_dir

  io.tasks.tag_write.bits.set := req.set
  io.tasks.tag_write.bits.way := meta.way
  io.tasks.tag_write.bits.tag := req.tag

  io.tasks.prefetch_train.foreach { train =>
    train.bits.tag := req.tag
    train.bits.set := req.set
    train.bits.needT := req_needT
    train.bits.source := io.id
  }

  io.tasks.prefetch_resp.foreach { resp =>
    resp.bits.tag := req.tag
    resp.bits.set := req.set
  }

  dontTouch(io.tasks)
  when(io.tasks.source_a.fire) {
    s_acquire := true.B
    s_transferput := true.B
  }
  when(io.tasks.source_b.fire) {
    s_rprobe := true.B
    s_pprobe := true.B
  }
  when(io.tasks.source_c.fire) {
    s_release := true.B
    s_probeack := true.B
  }
  when(io.tasks.source_d.fire) {
    s_execute := true.B
  }
  when(io.tasks.source_e.fire) {
    s_grantack := true.B
  }
  when(no_wait && !s_writebackdir && io.tasks.dir_write.ready) {
    s_writebackdir := true.B
  }
  when(io.tasks.tag_write.fire) {
    s_writebacktag := true.B
  }
  when(io.tasks.sink_c.fire) {
    s_writerelease := true.B
  }
  if (prefetchOpt.nonEmpty) {
    when(io.tasks.prefetch_train.get.fire) {
      s_triggerprefetch.get := true.B
    }
    when(io.tasks.prefetch_resp.get.fire) {
      s_prefetchack.get := true.B
    }
  }

  // Monitor resps and mark the w_* state regs
  val probeack_bit = getClientBitOH(io.resps.sink_c.bits.source)
  val probeack_last = (probes_done | probeack_bit) === (meta.clients & ~probe_exclude)
  when(io.resps.sink_c.valid) {
    // ProbeAck in resp to rprobe/pprobe
    val resp = io.resps.sink_c.bits
    probes_done := probes_done | probeack_bit
    probes_toN := probes_toN | Mux(isToN(resp.param), probeack_bit, 0.U)
    w_rprobeackfirst := w_rprobeackfirst || probeack_last // ProbeAck from the last client
    w_rprobeacklast := w_rprobeacklast || probeack_last && resp.last // the last beat of the last ProbeAck
    w_pprobeackfirst := w_pprobeackfirst || probeack_last
    w_pprobeacklast := w_pprobeacklast || probeack_last && resp.last
    w_pprobeack := w_pprobeack || (resp.last || req.off === 0.U) && probeack_last

    // TODO: is the following logic correct?
    // When ProbeAck for pprobe writes back dirty data, set dirty bit
    when((req.fromB && req.param === toT || req.fromA && meta.hit) && resp.hasData) {
      new_meta.dirty := true.B
    }
    // When ProbeAck for rprobe writes back dirty data, set dirty bit immediately for release
    when(meta.state =/= INVALID && resp.hasData) {
      meta_reg.dirty := true.B
    }
  }
  when(io.resps.sink_d.valid) {
    when(io.resps.sink_d.bits.opcode === Grant || io.resps.sink_d.bits.opcode === GrantData) {
      w_grantfirst := true.B
      w_grantlast := io.resps.sink_d.bits.last
      w_grant := req.off === 0.U || io.resps.sink_d.bits.last
      bad_grant := io.resps.sink_d.bits.denied
      gotT := io.resps.sink_d.bits.param === toT
      sink := io.resps.sink_d.bits.sink
    }
    when(io.resps.sink_d.bits.opcode === ReleaseAck) {
      w_releaseack := true.B
    }
  }
  when(io.resps.sink_e.valid) {
    w_grantack := true.B
  }

  // Release MSHR
  val no_schedule = s_execute && s_probeack && meta_valid && s_writebacktag && s_writebackdir && s_writerelease &&
    s_triggerprefetch.getOrElse(true.B) &&
    s_prefetchack.getOrElse(true.B) &&
    s_grantack && s_transferput
  when(no_wait && no_schedule) { // TODO: remove s_writebackdir to improve perf
    req_valid := false.B
    meta_valid := false.B
  }
  io.status.bits.will_free := no_wait && no_schedule

  // Alloc MSHR (alloc has higher priority than release)
  assert(RegNext(!req_valid || !io.alloc.valid, true.B)) // TODO: support fully-pipelined
  when(io.alloc.valid) {
    req_valid := true.B
    req := io.alloc.bits
  }
  // Status
  io.status.valid := req_valid
  io.status.bits.set := req.set
  io.status.bits.tag := req.tag
  io.status.bits.way := meta.way
  io.status.bits.way_reg := meta_reg.way
  io.status.bits.will_grant_data := false.B //req.fromA && od.opcode(0)
  io.status.bits.will_save_data := true.B
  io.status.bits.is_prefetch := req.isPrefetch.getOrElse(false.B)
  io.status.bits.blockB := !meta_valid ||
    ((!w_releaseack || !w_rprobeacklast || !w_pprobeacklast) && !w_grantfirst)
  // B nest A
  io.status.bits.nestB := meta_valid &&
    w_releaseack && w_rprobeacklast && w_pprobeacklast && !w_grantfirst

  io.status.bits.blockC := !meta_valid
  // C nest B | C nest A
  io.status.bits.nestC := meta_valid && (!w_rprobeackfirst || !w_pprobeackfirst || !w_grantfirst)

  io.ecc := DontCare
  io.ecc.valid := false.B
}
