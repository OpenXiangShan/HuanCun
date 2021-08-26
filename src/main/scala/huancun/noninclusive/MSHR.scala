package huancun.noninclusive

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import huancun._
import huancun.utils.{ParallelMax}
import huancun.MetaData._

class MSHR()(implicit p: Parameters) extends BaseMSHR[DirResult, SelfDirWrite, SelfTagWrite] with DontCareInnerLogic {
  val io = IO(new BaseMSHRIO[DirResult, SelfDirWrite, SelfTagWrite] {
    override val tasks = new MSHRTasks[SelfDirWrite, SelfTagWrite] {
      override val dir_write: DecoupledIO[SelfDirWrite] = DecoupledIO(new SelfDirWrite())
      override val tag_write: DecoupledIO[SelfTagWrite] = DecoupledIO(new SelfTagWrite())
      val client_dir_write = Vec(clientBits, DecoupledIO(new ClientDirWrite()))
      val client_tag_write = Vec(clientBits, DecoupledIO(new ClientTagWrite()))
    }
    override val dirResult = Flipped(ValidIO(new DirResult()))
  })

  val req = Reg(new MSHRRequest)
  val req_valid = RegInit(false.B)
  val iam = Reg(UInt(log2Up(clientBits).W)) // Which client does this req come from?
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
  when (io.dirResult.valid) {
    meta_valid := true.B
    meta_reg := io.dirResult.bits
  }
  meta := Mux(io.dirResult.valid, io.dirResult.bits, meta_reg)
  val self_meta = meta.self
  val clients_meta = meta.clients
  dontTouch(self_meta)
  dontTouch(clients_meta)

  // Final meta to be written
  val new_self_meta = WireInit(self_meta)
  val new_clients_meta = WireInit(clients_meta)
  val req_acquire = req.opcode === AcquireBlock || req.opcode === AcquirePerm
  val req_needT = needT(req.opcode, req.param)
  val gotT = RegInit(false.B)
  val probe_dirty = RegInit(false.B) // probe a block that is dirty
  val probes_done = RegInit(0.U(clientBits.W))
  val client_shrink_perm = isToN(req.param) && clients_meta(iam).state =/= INVALID || isToB(req.param) && isT(clients_meta(iam).state)
  val clients_hit = VecInit(clients_meta.map(_.hit)).asUInt.orR

  val highest_perm = ParallelMax(
                Seq(Mux(self_meta.hit, self_meta.state, INVALID)) ++
                clients_meta.map { case m => Mux(m.hit, m.state, INVALID) }
             ) // the highest perm of this whole level, including self and clients
  val req_promoteT = req_acquire && isT(highest_perm)

  // self cache does not have the acquired block, but some other client owns the block
  val transmit_from_other_client = !self_meta.hit && VecInit(clients_meta.zipWithIndex.map {
    case (meta, i) =>
      i.U =/= iam && meta.hit
  }).asUInt.orR

  // When replacing a block in data array, it is not always necessary to send Release,
  // but only when state perm > clientStates' perm
  val replace_clients_perm = ParallelMax(self_meta.clientStates)
  val replace_need_release = self_meta.state > replace_clients_perm
  val replace_param = Mux(self_meta.state === BRANCH, BtoN, Mux(replace_clients_perm === INVALID, TtoN, TtoB))

  def probe_next_state(state: UInt, param: UInt): UInt = Mux(
    isT(state) && param === toT,
    state,
    Mux(state =/= INVALID && param =/= toN, BRANCH, INVALID)
  )
  def probe_shrink_perm(state: UInt, perm: UInt): Bool = state =/= INVALID && perm === toN || isT(state) && perm === toB

  when (req.fromC) {
    // Release / ReleaseData
    new_self_meta.dirty := self_meta.hit && self_meta.dirty || req.opcode(0) && isParamFromT(req.param)
    new_self_meta.state := MuxLookup(
                              req.param,
                              self_meta.state,
                              Seq(
                                TtoT -> TRUNK,
                                TtoB -> TIP,
                                TtoN -> TIP,
                                // BtoB -> self_meta.state,
                                BtoN -> Mux(self_meta.hit && self_meta.state === TIP, TIP, BRANCH)//,
                                // NtoN -> self_meta.state
                              )
                           )
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := Mux(iam === i.U,
                    Mux(isToN(req.param), INVALID, Mux(isToB(req.param), BRANCH, self_meta.clientStates(i))),
                    self_meta.clientStates(i))
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) => 
        m.state := Mux(iam === i.U,
                      Mux(isToN(req.param), INVALID, Mux(isToB(req.param), BRANCH, clients_meta(i).state)),
                      clients_meta(i).state)
    }
  }.elsewhen (req.fromB) {
    // Probe
    new_self_meta.dirty := req.param === toT && self_meta.dirty || probe_dirty
    new_self_meta.state := probe_next_state(self_meta.state, req.param)
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := probe_next_state(self_meta.clientStates(i), req.param)
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) => 
        m.state := probe_next_state(clients_meta(i).state, req.param)
    }
  }.otherwise {
    // Acquire / Intent / Put / Get / Atomics
    new_self_meta.dirty := self_meta.hit && self_meta.dirty || probe_dirty || !req.opcode(2) // Put / Atomics
    // AcqurieB / Intent / Put / Atomics allocate a block in self dir, 
    // while AcquireT / Get do not.
    // TODO: consider Hint
    new_self_meta.state := Mux(
      req_needT || gotT,
      Mux(
        req_acquire/* || req.opcode === Hint && self_meta.hit && meta.state === TRUNK*/,
        TRUNK,
        TIP
      ),
      Mux(
        !self_meta.hit,
        BRANCH,
        MuxLookup(
          self_meta.state,
          BRANCH,
          Seq(
            INVALID -> BRANCH,
            BRANCH -> BRANCH,
            TRUNK -> TIP, // Mux(req.opcode === Hint, TRUNK, TIP),
            TIP -> TIP
          )
        )
      )
    )
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        when (iam === i.U) {
          state := Mux(req_acquire, Mux(req_needT || gotT, TIP, BRANCH), self_meta.clientStates(i))
        }.otherwise {
          state := Mux(
                      req_acquire,
                      Mux(
                        self_meta.clientStates(i) =/= INVALID && req.param =/= NtoB,
                        INVALID,
                        Mux(
                          self_meta.clientStates(i) === TIP && req.param === NtoB,
                          BRANCH,
                          self_meta.clientStates(i)
                        )
                      ),
                      self_meta.clientStates(i)
                   )
        }
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) =>
        when (iam === i.U) {
          m.state := Mux(req_acquire, Mux(req_needT || gotT, TIP, BRANCH), clients_meta(i).state)
        }.otherwise {
          m.state := Mux(
                        req_acquire,
                        Mux(
                          clients_meta(i).state =/= INVALID && req.param =/= NtoB,
                          INVALID,
                          Mux(
                            clients_meta(i).state === TIP && req.param === NtoB,
                            BRANCH,
                            clients_meta(i).state
                          )
                        ),
                        clients_meta(i).state
                     )
        }
    }
  }

  val new_clients_dir = Wire(Vec(clientBits, new ClientDirEntry))
  val new_self_dir = Wire(new SelfDirEntry)
  new_self_dir.dirty := new_self_meta.dirty
  new_self_dir.state := new_self_meta.state
  new_self_dir.clientStates := new_self_meta.clientStates
  new_self_dir.prefetch.foreach(_ := false.B) // TODO
  new_clients_dir.zip(new_clients_meta).foreach { case (dir, meta) => dir.state := meta.state }

  val sink = Reg(UInt(edgeOut.bundle.sinkBits.W))

  val bad_grant = Reg(Bool())
  // TODO: consider bad grant
  when (bad_grant) {
    new_self_dir.dirty := false.B
    new_self_dir.state := self_meta.state
    new_self_dir.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := Mux(self_meta.hit && i.U =/= iam, new_self_meta.clientStates(i), self_meta.clientStates(i))
    }
  }

  assert(RegNext(!meta_valid || !req.fromC || self_meta.hit || clients_meta(iam).hit)) // Release should always hit

  // TODO: nested writeback to meta_reg

  // Set tasks to be scheduled and resps to wait for
  val s_acquire = RegInit(true.B) // source_a
  val s_probe = RegInit(true.B) // source_b
  val s_release = RegInit(true.B) // source_c
  val s_probeack = RegInit(true.B) // source_c
  val s_execute = RegInit(true.B) // source_d
  val s_grantack = RegInit(true.B) // source_e
  val s_wbselfdir = RegInit(true.B) // write self dir
  val s_wbselftag = RegInit(true.B) // write self tag
  val s_wbclientsdir = RegInit(VecInit(Seq.fill(clientBits)(true.B))) // write clients' dir
  val s_wbclientstag = RegInit(VecInit(Seq.fill(clientBits)(true.B))) // write clients' tag
  val s_writeput = RegInit(true.B) // sink_a
  val s_writerelease = RegInit(true.B) // sink_c
  val s_triggerprefetch = prefetchOpt.map(_ => RegInit(true.B))
  val s_prefetchack = prefetchOpt.map(_ => RegInit(true.B))

  val w_probeackfirst = RegInit(true.B) // first beat of the last probeack
  val w_probeacklast = RegInit(true.B) // last beat of the last probeack
  val w_probeack = RegInit(true.B)
  val w_grantfirst = RegInit(true.B)
  val w_grantlast = RegInit(true.B)
  val w_grant = RegInit(true.B)
  val w_releaseack = RegInit(true.B)
  val w_grantack = RegInit(true.B)

  when (io.dirResult.valid) {
    // Default value
    s_acquire := true.B
    s_probe := true.B
    s_release := true.B
    s_probeack := true.B
    s_execute := true.B
    s_grantack := true.B
    s_wbselfdir := true.B
    s_wbselftag := true.B
    s_wbclientsdir.foreach(s => s := true.B)
    s_wbclientstag.foreach(s => s := true.B)
    s_writeput := true.B
    s_writerelease := true.B
    s_triggerprefetch.foreach(_ := true.B)
    s_prefetchack.foreach(_ := true.B)

    w_probeackfirst := true.B
    w_probeacklast := true.B
    w_probeack := true.B
    w_grantfirst := true.B
    w_grantlast := true.B
    w_grant := true.B
    w_releaseack := true.B
    w_grantack := true.B

    gotT := false.B
    probe_dirty := false.B
    probes_done := 0.U
    bad_grant := false.B

    when (req.fromC) {
      // Release
      s_execute := false.B
      // When the req shrinks the perm in clients indeed, write client dir.
      when (client_shrink_perm) {
        s_wbclientsdir(iam) := false.B
      }
      // When miss in self dir, allocate a new block in self dir.
      when (!self_meta.hit) {
        s_wbselftag := false.B
      }
      // When miss in self dir or , write self dir.
      when (!self_meta.hit || req.opcode(0) || client_shrink_perm) {
        s_wbselfdir := false.B
      }
      // When miss in self dir or Release dirty block, write data array.
      when (req.opcode(0) || !self_meta.hit) {
        s_writerelease := false.B
      }
      when (!self_meta.hit && self_meta.state =/= INVALID && replace_need_release) {
        s_release := false.B
        w_releaseack := false.B
      }
    }.elsewhen (req.fromB) {
      // Probe
      s_probeack := false.B
      // When probe hits in self dir and needs to shrink perm, write self dir.
      when (self_meta.hit && probe_shrink_perm(self_meta.state, req.param)) {
        s_wbselfdir := false.B
        // When probe hits in some client dir and needs to shrink perm, write corresponding client dir.
        clients_meta.zipWithIndex.foreach {
          case (meta, i) =>
            when (meta.hit && probe_shrink_perm(meta.state, req.param)) {
              s_wbclientsdir(i) := false.B
              s_probe := false.B
              w_probeackfirst := false.B
              w_probeacklast := false.B
              w_probeack :=  false.B
            }
        }
      }
    }.otherwise {
      // A channel requests
      // TODO: consider parameterized write-through policy for put/atomics
      // TODO: consider prefetch
      s_execute := req.opcode === Hint
      // need replacement when:
      // (1) some other client owns the block, probe this block and allocate a block in self cache (transmit_from_other_client),
      // (2) other clients and self dir both miss, allocate a block only when this req acquires a BRANCH (!req_needT).
      when (!self_meta.hit && self_meta.state =/= INVALID && replace_need_release && (transmit_from_other_client || req_acquire/*!req_needT*/)) {
        s_release := false.B
        w_releaseack := false.B
      }
      // need Acquire downwards
      when (!self_meta.hit || self_meta.state === BRANCH && req_needT) {
        s_acquire := false.B
        w_grantfirst := false.B
        w_grantlast := false.B
        w_grant := false.B
        s_grantack := false.B
        s_wbselfdir := false.B
      }
      // need probe
      clients_meta.zipWithIndex.foreach {
        case (meta, i) =>
          when (i.U =/= iam && meta.hit && (req_needT && meta.state =/= INVALID || req_acquire && isT(meta.state))) {
            s_probe := false.B
            w_probeackfirst := false.B
            w_probeacklast := false.B
            w_probeack := false.B
            s_wbclientsdir(i) := false.B
          }
      }
      // need grantack
      when (req_acquire) {
        w_grantack := false.B
        s_wbselfdir := false.B
        s_wbclientsdir(iam) := false.B
        when (!clients_meta(iam).hit) {
          s_wbclientstag(iam) := false.B
        }
      }
      // Put and Atomics need to write
      when (!req.opcode(2) && !self_meta.dirty) {
        s_wbselfdir := false.B
      }
      // need write self tag
      when (!self_meta.hit) {
        s_wbselftag := false.B
      }
      // need write putbuffer in Sink A into data array
      when (req.opcode(2, 1) === 0.U) {
        s_writeput := false.B
      }
      // TODO: trigger a prefetch when req is from DCache, and miss / prefetched hit in L2
    }
  }

  val no_wait = w_probeacklast && w_grantlast && w_releaseack && w_grantack
  io.tasks.source_a.valid := !s_acquire && s_release && s_probe
  io.tasks.source_b.valid := !s_probe
  io.tasks.source_c.valid := !s_release /*&& w_probeackfirst*/ || !s_probeack && w_probeackfirst
  io.tasks.source_d.valid := !s_execute && w_grant && w_probeack
  io.tasks.source_e.valid := !s_grantack && w_grantfirst
  io.tasks.dir_write.valid := !s_wbselfdir && no_wait
  io.tasks.tag_write.valid := !s_wbselftag && no_wait
  io.tasks.client_dir_write.zip(s_wbclientsdir).foreach { case (t, s) => t.valid := !s && no_wait }
  io.tasks.client_tag_write.zip(s_wbclientstag).foreach { case (t, s) => t.valid := !s && no_wait }
  io.tasks.sink_a.valid := !s_writeput && w_grant && w_probeack
  io.tasks.sink_c.valid := !s_writerelease
  io.tasks.prefetch_train.foreach(_.valid := false.B)
  io.tasks.prefetch_resp.foreach(_.valid := false.B)

  val oa = io.tasks.source_a.bits
  val ob = io.tasks.source_b.bits
  val oc = io.tasks.source_c.bits
  val od = io.tasks.source_d.bits
  val oe = io.tasks.source_e.bits
  val ia = io.tasks.sink_a.bits
  val ic = io.tasks.sink_c.bits

  oa.tag := req.tag
  oa.set := req.set
  oa.opcode := Mux(clients_hit || self_meta.hit, AcquirePerm, AcquireBlock)
  oa.param := Mux(req_needT, Mux(clients_hit || self_meta.hit, BtoT, NtoT), NtoB)
  oa.source := io.id
  oa.needData := !(req.opcode === AcquirePerm) || req.size =/= offsetBits.U

  ob.tag := req.tag
  ob.set := req.set
  ob.param := Mux(req.fromB, req.param, Mux(req_needT, toN, toB))
  // Which clients should be probed?
  val probe_clients = VecInit(clients_meta.map {
    case m => Mux(ob.param === toN, m.hit, ob.param === toB && isT(m.state))
  }).asUInt & ~Mux(req.fromA && skipProbeN(req.opcode), UIntToOH(iam), 0.U)
  ob.clients := probe_clients

  oc.opcode := Mux(req.fromB, Cat(ProbeAck(2, 1), (probe_dirty || self_meta.dirty).asUInt), Cat(Release(2, 1), self_meta.dirty.asUInt))
  oc.tag := Mux(req.fromB, req.tag, self_meta.tag)
  oc.set := req.set
  oc.param := Mux(
    req.fromB,
    MuxLookup( // TODO: optimize this
      Cat(highest_perm, probe_next_state(highest_perm, req.param)),
      NtoN,
      Seq(
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
    replace_param 
  )
  oc.source := io.id
  oc.way := self_meta.way
  oc.dirty := Mux(req.fromB, probe_dirty, self_meta.dirty)

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
      MuxLookup(req.param, req.param, Seq(NtoB -> Mux(req_promoteT, toT, toB), BtoT -> toT, NtoT -> toT))
    )
  od.size := req.size
  od.way := self_meta.way
  od.off := req.off
  od.denied := bad_grant

  oe.sink := sink

  ia.sourceId := req.source
  ia.set := req.set
  ia.tag := req.tag
  ia.size := req.size
  ia.off := req.off

  ic.sourceId := req.source
  ic.set := req.set
  ic.tag := req.tag
  ic.size := req.size
  ic.off := req.off
  ic.way := self_meta.way
  ic.bufIdx := req.bufIdx

  io.tasks.dir_write.bits.set := req.set
  io.tasks.dir_write.bits.way := self_meta.way
  io.tasks.dir_write.bits.data := new_self_dir

  io.tasks.tag_write.bits.set := req.set
  io.tasks.tag_write.bits.way := self_meta.way
  io.tasks.tag_write.bits.tag := req.tag

  io.tasks.client_dir_write.zip(new_clients_meta).foreach {
    case (w, meta) =>
      w.bits.apply(Cat(req.tag, req.set), meta.way, meta.state)
  }

  io.tasks.client_tag_write.zip(new_clients_meta).foreach {
    case (w, meta) =>
      w.bits.apply(Cat(req.tag, req.set), meta.way)
  }

  io.tasks.prefetch_train.foreach{ train =>
    train.bits.tag := req.tag
    train.bits.set := req.set
    train.bits.needT := req_needT
  }

  io.tasks.prefetch_resp.foreach { resp =>
    resp.bits.tag := req.tag
    resp.bits.set := req.set
  }

  dontTouch(io.tasks)
  when (io.tasks.source_a.fire()) {
    s_acquire := true.B
  }
  when (io.tasks.source_b.fire()) {
    s_probe := true.B
  }
  when (io.tasks.source_c.fire()) {
    s_release := true.B
    s_probeack := true.B
  }
  when (io.tasks.source_d.fire()) {
    s_execute := true.B
  }
  when (io.tasks.source_e.fire()) {
    s_grantack := true.B
  }
  when (io.tasks.dir_write.fire()) {
    s_wbselfdir := true.B
  }
  when (io.tasks.tag_write.fire()) {
    s_wbselftag := true.B
  }
  io.tasks.client_dir_write.zip(s_wbclientsdir).foreach {
    case (t, s) => when (t.fire()) { s := true.B }
  }
  io.tasks.client_tag_write.zip(s_wbclientstag).foreach {
    case (t, s) => when (t.fire()) { s := true.B }
  }
  when (io.tasks.sink_a.fire()) {
    s_writeput := true.B
  }
  when (io.tasks.sink_c.fire()) {
    s_writerelease := true.B
  }
  if (prefetchOpt.nonEmpty) {
    when (io.tasks.prefetch_train.get.fire()) {
      s_triggerprefetch.get := true.B
    }
    when (io.tasks.prefetch_resp.get.fire()) {
      s_prefetchack.get := true.B
    }
  }

  val probeack_bit = getClientBitOH(io.resps.sink_c.bits.source)
  val probeack_last = (probes_done | probeack_bit) === probe_clients // This is the last client sending probeack
  when (io.resps.sink_c.valid) {
    val resp = io.resps.sink_c.bits
    probes_done := probes_done | probeack_bit
    w_probeackfirst := w_probeackfirst || probeack_last
    w_probeacklast := w_probeacklast || probeack_last && resp.last
    w_probeack := w_probeack || probeack_last && (resp.last || req.off === 0.U)

    probe_dirty := probe_dirty || resp.hasData && !w_probeackfirst
  }
  when (io.resps.sink_d.valid) {
    sink := io.resps.sink_d.bits.sink
    when (io.resps.sink_d.bits.opcode === Grant || io.resps.sink_d.bits.opcode === GrantData) {
      w_grantfirst := true.B
      w_grantlast := w_grantlast || io.resps.sink_d.bits.last
      w_grant := req.off === 0.U || io.resps.sink_d.bits.last
      bad_grant := io.resps.sink_d.bits.denied
      gotT := io.resps.sink_d.bits.param === toT
    }
    when (io.resps.sink_d.bits.opcode === ReleaseAck) {
      w_releaseack := true.B
    }
  }
  when (io.resps.sink_e.valid) {
    w_grantack := true.B
  }

  // Release MSHR
  val no_schedule = s_probeack && s_execute && s_grantack && s_wbselfdir && s_wbselftag && 
    s_wbclientsdir.asUInt.andR && s_wbclientstag.asUInt.andR && s_writerelease &&
    meta_valid && 
    s_triggerprefetch.getOrElse(true.B) &&
    s_prefetchack.getOrElse(true.B) // TODO: s_writeput?
  when (no_wait && no_schedule) {
    meta_valid := false.B
    req_valid := false.B
  }

  // Alloc MSHR (alloc has higher priority than release)
  assert(RegNext(!req_valid || !io.alloc.valid, true.B)) // TODO: support fully-pipelined
  when(io.alloc.valid) {
    req_valid := true.B
    req := io.alloc.bits
    iam := OHToUInt(getClientBitOH(io.alloc.bits.source))
  }
  
  // Status
  io.status.valid := req_valid
  io.status.bits.set := req.set
  io.status.bits.tag := req.tag
  io.status.bits.reload := false.B // TODO
  io.status.bits.way := self_meta.way
  // TODO:
  io.status.bits.blockB := true.B
  io.status.bits.nestB := false.B
  io.status.bits.blockC := true.B
  io.status.bits.nestC := false.B
}
