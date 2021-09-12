package huancun.noninclusive

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.TLHints._
import huancun._
import huancun.utils.{ParallelMax}
import huancun.MetaData._

class C_Status(implicit p: Parameters) extends HuanCunBundle {
  // When C nest A, A needs to know the status of C and tells C to release through to next level
  val set = Input(UInt(setBits.W))
  val tag = Input(UInt(tagBits.W))
  val way = Input(UInt(wayBits.W))
  val nestedReleaseData = Input(Bool())
  val releaseThrough = Output(Bool())
}

class B_Status(implicit p: Parameters) extends HuanCunBundle {
  val set = Input(UInt(setBits.W))
  val tag = Input(UInt(tagBits.W))
  val way = Input(UInt(wayBits.W))
  val nestedProbeAckData = Input(Bool())
  val probeAckDataThrough = Output(Bool())
}

class MSHR()(implicit p: Parameters) extends BaseMSHR[DirResult, SelfDirWrite, SelfTagWrite] {
  val io = IO(new BaseMSHRIO[DirResult, SelfDirWrite, SelfTagWrite] {
    override val tasks = new MSHRTasks[SelfDirWrite, SelfTagWrite] {
      override val dir_write: DecoupledIO[SelfDirWrite] = DecoupledIO(new SelfDirWrite())
      override val tag_write: DecoupledIO[SelfTagWrite] = DecoupledIO(new SelfTagWrite())
      val client_dir_write = Vec(clientBits, DecoupledIO(new ClientDirWrite()))
      val client_tag_write = Vec(clientBits, DecoupledIO(new ClientTagWrite()))
    }
    override val dirResult = Flipped(ValidIO(new DirResult()))
  })

  val io_c_status = IO(new C_Status)
  val io_b_status = IO(new B_Status())
  val io_releaseThrough = IO(Input(Bool()))
  val io_probeAckDataThrough = IO(Input(Bool()))
  val io_is_nestedReleaseData = IO(Output(Bool()))
  val io_is_nestedProbeAckData = IO(Output(Bool()))

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
  when(io.dirResult.valid) {
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
  val client_shrink_perm =
    isToN(req.param) && clients_meta(iam).state =/= INVALID || isToB(req.param) && isT(clients_meta(iam).state)
  val clients_hit = VecInit(clients_meta.map(_.hit)).asUInt.orR
  val other_clients_hit = VecInit(clients_meta.zipWithIndex.map {
    case (meta, i) => i.U =/= iam && meta.hit
  }).asUInt.orR
  val clients_have_T = VecInit(clients_meta.map {
    case meta => meta.hit && isT(meta.state)
  }).asUInt.orR

  val highest_perm = ParallelMax(
    Seq(Mux(self_meta.hit, self_meta.state, INVALID)) ++
      clients_meta.map(m => Mux(m.hit, m.state, INVALID))
  ) // the highest perm of this whole level, including self and clients

  // val req_promoteT = req_acquire && isT(highest_perm)
  val req_promoteT = req_acquire && gotT

  // self cache does not have the acquired block, but some other client owns the block
  val transmit_from_other_client = !self_meta.hit && VecInit(clients_meta.zipWithIndex.map {
    case (meta, i) =>
      i.U =/= iam && meta.hit
  }).asUInt.orR

  // When replacing a block in data array, it is not always necessary to send Release,
  // but only when state perm > clientStates' perm or replacing a dirty block
  val replace_clients_perm = ParallelMax(self_meta.clientStates)
  val replace_need_release = self_meta.state > replace_clients_perm || self_meta.dirty && isT(self_meta.state)
  //  val replace_param = Mux(self_meta.state === BRANCH, BtoN, Mux(replace_clients_perm === INVALID, TtoN, TtoB))
  val replace_param = MuxLookup(
    Cat(self_meta.state, replace_clients_perm),
    TtoB,
    Seq(
      Cat(BRANCH, INVALID) -> BtoN,
      Cat(BRANCH, BRANCH) -> BtoB,
      Cat(BRANCH, TRUNK) -> TtoT,
      Cat(BRANCH, TIP) -> TtoT,
      Cat(TIP, INVALID) -> TtoN,
      Cat(TIP, BRANCH) -> TtoB,
      Cat(TRUNK, TIP) -> TtoT
    )
  )

  val prefetch_miss_need_acquire = Mux(req.param === PREFETCH_READ, highest_perm === INVALID, !isT(highest_perm))
  val prefetch_miss_need_probe_vec = VecInit(clients_meta.zipWithIndex.map {
    case (meta, i) =>
      i.U =/= iam &&
        (req.param === PREFETCH_WRITE && isT(meta.state) && meta.hit && (!self_meta.hit || !isT(self_meta.state)) ||
          req.param === PREFETCH_READ && meta.hit && !self_meta.hit && !clients_meta(iam).hit)
  })
  val prefetch_miss_need_probe = prefetch_miss_need_probe_vec.asUInt.orR
  val prefetch_miss = prefetch_miss_need_acquire || prefetch_miss_need_probe
  val prefetch_need_data = !self_meta.hit
  val prefetch_write_self_next_state = Mux(
    !clients_meta(iam).hit || !isT(clients_meta(iam).state),
    TIP,
    self_meta.state
  )

  def probe_next_state(state: UInt, param: UInt): UInt = Mux(
    isT(state) && param === toT,
    state,
    Mux(state =/= INVALID && param =/= toN, BRANCH, INVALID)
  )

  def probe_shrink_perm(state: UInt, perm: UInt): Bool = state =/= INVALID && perm === toN || isT(state) && perm === toB

  def onCReq(): Unit = {
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
        BtoN -> Mux(self_meta.hit && self_meta.state === TIP, TIP, BRANCH) //,
        // NtoN -> self_meta.state
      )
    )
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := Mux(
          iam === i.U,
          Mux(isToN(req.param), INVALID, Mux(isToB(req.param), BRANCH, self_meta.clientStates(i))),
          self_meta.clientStates(i)
        )
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) =>
        m.state := Mux(
          iam === i.U,
          Mux(isToN(req.param), INVALID, Mux(isToB(req.param), BRANCH, clients_meta(i).state)),
          clients_meta(i).state
        )
    }
  }

  def onBReq(): Unit = {
    // Probe
    //    new_self_meta.dirty := Mux(self_meta.hit, req.param === toT && self_meta.dirty || probe_dirty, self_meta.dirty)
    new_self_meta.dirty := !self_meta.hit && self_meta.dirty
    new_self_meta.state := Mux(self_meta.hit, probe_next_state(self_meta.state, req.param), self_meta.state)
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := Mux(self_meta.hit, probe_next_state(self_meta.clientStates(i), req.param), self_meta.clientStates(i))
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) =>
        m.state := Mux(m.hit, probe_next_state(clients_meta(i).state, req.param), clients_meta(i).state)
    }
  }

  def onAReq(): Unit = {
    // Acquire / Intent / Put / Get / Atomics
    new_self_meta.dirty := self_meta.hit && self_meta.dirty || probe_dirty || !req.opcode(2) // Put / Atomics
    // AcqurieB / Intent / Put / Atomics allocate a block in self dir,
    // while AcquireT / Get do not.
    // TODO: consider Hint
    new_self_meta.state := Mux(
      req_needT || gotT,
      Mux(
        req_acquire,
        Mux((clients_hit || self_meta.hit) && self_meta.state === INVALID, INVALID, TRUNK),
        Mux(req.opcode === Hint, prefetch_write_self_next_state, TIP)
      ),
      Mux(
        !self_meta.hit,
        Mux(clients_meta(iam).hit && req.opcode === Hint, self_meta.state, BRANCH),
        MuxLookup(
          self_meta.state,
          BRANCH,
          Seq(
            INVALID -> BRANCH,
            BRANCH -> BRANCH,
            TRUNK -> Mux(req.opcode === Hint, TRUNK, TIP),
            TIP -> TIP
          )
        )
      )
    )
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        when(iam === i.U) {
          state := Mux(req_acquire, Mux(req_needT || gotT, TIP, BRANCH), self_meta.clientStates(i))
        }.otherwise {
          state := Mux(
            req_acquire,
            Mux(
              self_meta.hit && self_meta.clientStates(i) =/= INVALID && req.param =/= NtoB,
              INVALID,
              Mux(
                // self_meta.clientStates(i) === TIP && req.param === NtoB,
                clients_meta(i).hit && req.param === NtoB,
                BRANCH,
                INVALID
              )
            ),
            // self_meta.clientStates(i)
            Mux(clients_meta(i).hit, clients_meta(i).state, INVALID)
          )
        }
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) =>
        when(iam === i.U) {
          m.state := Mux(req_acquire, Mux(req_needT || gotT, TIP, BRANCH), clients_meta(i).state)
        }.otherwise {
          m.state := Mux(
            req_acquire,
            Mux(
              clients_meta(i).hit && clients_meta(i).state =/= INVALID && req.param =/= NtoB,
              INVALID,
              Mux(
                // clients_meta(i).state === TIP && req.param === NtoB,
                clients_meta(i).hit && req.param === NtoB,
                BRANCH,
                INVALID
              )
            ),
            // clients_meta(i).state
            Mux(clients_meta(i).hit, clients_meta(i).state, INVALID)
          )
        }
    }
  }

  when(req.fromC) {
    onCReq()
  }.elsewhen(req.fromB) {
    onBReq()
  }.otherwise {
    onAReq()
  }

  val new_clients_dir = Wire(Vec(clientBits, new ClientDirEntry))
  val new_self_dir = Wire(new SelfDirEntry)
  new_self_dir.dirty := new_self_meta.dirty
  new_self_dir.state := new_self_meta.state
  new_self_dir.clientStates := new_self_meta.clientStates
  new_self_dir.prefetch.foreach(_ := self_meta.prefetch.get || prefetch_miss && req.opcode === Hint)
  new_clients_dir.zip(new_clients_meta).foreach { case (dir, meta) => dir.state := meta.state }

  val sink = Reg(UInt(edgeOut.bundle.sinkBits.W))

  val bad_grant = Reg(Bool())
  // TODO: consider bad grant
  when(bad_grant) {
    new_self_dir.dirty := false.B
    new_self_dir.state := self_meta.state
    new_self_dir.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := Mux(self_meta.hit && i.U =/= iam, new_self_meta.clientStates(i), self_meta.clientStates(i))
    }
  }

  assert(RegNext(!meta_valid || !req.fromC || self_meta.hit || clients_meta(iam).hit)) // Release should always hit

  // nested writeback to meta_reg
  val change_self_meta = meta_valid && self_meta.state =/= INVALID &&
    io.nestedwb.set === req.set && io.nestedwb.tag === self_meta.tag
  val change_clients_meta = clients_meta.zipWithIndex.map {
    case (meta, i) =>
      meta_valid && meta.state =/= INVALID &&
        io.nestedwb.set === req.set &&
        meta.parseTag(Cat(io.nestedwb.tag, io.nestedwb.set)) === meta.tag
  }
  when(change_self_meta) {
    when(io.nestedwb.b_clr_dirty) {
      meta_reg.self.dirty := false.B
    }
    when(io.nestedwb.c_set_dirty) {
      meta_reg.self.dirty := true.B
    }
    when(io.nestedwb.b_toB) {
      meta_reg.self.state := BRANCH
      meta_reg.self.clientStates.foreach { s => s := Mux(isT(s), BRANCH, s) }
    }
    when(io.nestedwb.b_toN) {
      meta_reg.self.state := INVALID
      meta_reg.self.hit := false.B
      meta_reg.self.clientStates.foreach(_ := INVALID)
    }
  }
  meta_reg.clients.zipWithIndex.foreach {
    case (reg, i) =>
      when(change_clients_meta(i)) {
        when((io.nestedwb.clients.get) (i).isToB) {
          // reg.state := BRANCH
        }
        when((io.nestedwb.clients.get) (i).isToN) {
          // reg.state := INVALID
          reg.hit := false.B
        }
      }
  }

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
  val s_writeprobe = RegInit(true.B)
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

  // 1 cycle ahead its' corresponding register defs
  // these signals are used to decide mshr actions when dirResult.valid on c_schedule
  val will_release_through = WireInit(false.B)
  val will_drop_release = WireInit(false.B)
  val will_save_release = WireInit(true.B)

  val releaseThrough = RegInit(false.B)
  val releaseDrop = RegInit(false.B)
  val releaseSave = !releaseThrough && !releaseDrop

  // these signals are used to decide mshr actions when dirResult.valid on b_schedule
  val will_probeack_through = WireInit(false.B)
  val will_drop_probeack = WireInit(false.B)
  val will_save_probeack = WireInit(true.B)

  val probeAckDataThrough = RegInit(false.B)
  val probeAckDataDrop = RegInit(false.B)
  val probeAckDataSave = !probeAckDataThrough && !probeAckDataDrop

  def reset_all_flags(): Unit = {
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
    s_writeprobe := true.B
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

  }

  def c_schedule(): Unit = {
    // Release
    s_execute := false.B
    // When the req shrinks the perm in clients indeed, write client dir.
    when(client_shrink_perm) {
      s_wbclientsdir(iam) := false.B
    }
    when(will_save_release) {
      // When miss in self dir and has data, allocate a new block in self dir.
      when(!self_meta.hit && req.opcode(0)) {
        s_wbselftag := false.B
      }
      when(self_meta.hit || req.opcode(0)) {
        s_wbselfdir := false.B
      }
      when(
        !self_meta.hit && req.opcode(0) &&
          self_meta.state =/= INVALID && replace_need_release
      ) {
        s_release := false.B
        w_releaseack := false.B
      }
    }
    // When ReleaseData,
    // schedule sinkC task to write bankedstore or outer cache(release through)
    when(req.opcode(0)) {
      s_writerelease := false.B // including drop and release-through
    }
    /*
      for c mshr:
      req.opcode(0) <=> s_writerelease
      set_release_through <=> send outer release
      so 'w_releaseack' should be set
    */
    when(will_release_through && req.opcode(0)) {
      w_releaseack := false.B
    }
  }

  def b_schedule(): Unit = {
    // Probe
    // TODO: probe a non-existing block is possible?
    // assert(self_meta.hit || clients_meta.map(_.hit).reduce(_ || _), "Trying to probe a non-existing block")
    s_probeack := false.B
    when(will_save_probeack) {
      when(self_meta.hit) {
        // TODO: consider Report?
        assert(probe_shrink_perm(self_meta.state, req.param), "Probe should always shrink perm")
        s_wbselfdir := false.B
      }
    }
    clients_meta.zipWithIndex.foreach {
      case (meta, i) =>
        when(meta.hit) {
          assert(probe_shrink_perm(meta.state, req.param), "Probe should always shrink perm")
          s_probe := false.B
          s_wbclientsdir(i) := false.B
          w_probeackfirst := false.B
          w_probeacklast := false.B
          w_probeack := false.B
        }
    }
  }


  val preferCache = req.preferCache
  def a_schedule(): Unit = {
    // A channel requests
    // TODO: consider parameterized write-through policy for put/atomics
    s_execute := req.opcode === Hint
    // need replacement when prefer cache and:
    // (1) some other client owns the block, probe this block and allocate a block in self cache (transmit_from_other_client),
    // (2) other clients and self dir both miss, allocate a block only when this req acquires a BRANCH (!req_needT).
    when(
      !self_meta.hit && self_meta.state =/= INVALID &&
        replace_need_release && preferCache &&
        (transmit_from_other_client ||
          req.opcode === AcquireBlock ||
          req.opcode === Get ||
          req.opcode === Hint)
    ) {
      s_release := false.B
      w_releaseack := false.B
    }

    // need Acquire downwards
    val acquirePermMiss = req.opcode === AcquirePerm && !self_meta.hit
    when(Mux(req_needT, !isT(highest_perm), highest_perm === INVALID)) {
      s_acquire := false.B
      w_grantfirst := false.B
      w_grantlast := false.B
      w_grant := false.B
      s_grantack := false.B
      // for acquirePermMiss and (miss && !preferCache):
      // no data block will be saved, so self dir won't change
      when(!acquirePermMiss && (self_meta.hit || preferCache)) {
        s_wbselfdir := false.B
      }
    }
    // need probe
    clients_meta.zipWithIndex.foreach {
      case (meta, i) =>
        when(
          i.U =/= iam && meta.hit && (
            req_acquire && (
              req_needT && meta.state =/= INVALID || isT(meta.state)
              ) || req.opcode === Hint && prefetch_miss_need_probe
            )
        ) {
          s_probe := false.B
          w_probeackfirst := false.B
          w_probeacklast := false.B
          w_probeack := false.B
          s_wbclientsdir(i) := false.B
        }
        assert(
          !(req.opcode === AcquirePerm && (i.U =/= iam && meta.hit && isT(meta.state))),
          "AcquirePerm cannot occur when other client has Tip"
        )
    }
    // need grantack
    when(req_acquire) {
      w_grantack := false.B
      when(!acquirePermMiss && (self_meta.hit || preferCache)) {
        s_wbselfdir := false.B
      }
      s_wbclientsdir(iam) := false.B
      when(!clients_meta(iam).hit) {
        s_wbclientstag(iam) := false.B
      }
    }
    // Put and Atomics need to write
    when(!req.opcode(2) && !self_meta.dirty) {
      s_wbselfdir := false.B
    }
    // need to write self tag
    val prefetchMiss = req.opcode === Hint && Mux(req.param === PREFETCH_WRITE,
      !isT(clients_meta(iam).state),
      !clients_meta(iam).hit
    )
    when(
      !self_meta.hit && preferCache &&
        (req.opcode === Get || req.opcode === AcquireBlock || prefetchMiss)
    ) {
      s_wbselftag := false.B
    }
    // need to write putbuffer in Sink A into data array
    when(req.opcode(2, 1) === 0.U) {
      s_writeput := false.B
    }
    prefetchOpt.map(_ => {
      when(req.opcode =/= Hint && req.needHint && (!self_meta.hit || self_meta.prefetch.get)) {
        s_triggerprefetch.map(_ := false.B)
      }
      when(req.opcode === Hint) {
        s_prefetchack.map(_ := false.B)
      }
    })
  }

  when(io.dirResult.valid) {

    reset_all_flags()

    when(req.fromC) {
      c_schedule()
    }.elsewhen(req.fromB) {
      b_schedule()
    }.otherwise {
      a_schedule()
    }
  }

  when(io_releaseThrough && io.dirResult.valid) {
    assert(req_valid)
    will_release_through := req.fromC && !other_clients_hit
    will_drop_release := req.fromC && other_clients_hit
    will_save_release := !(will_release_through || will_drop_release)
    releaseThrough := will_release_through
    releaseDrop := will_drop_release
  }

  when(req.fromB && io.dirResult.valid) {
    will_probeack_through := clients_have_T && (io_probeAckDataThrough || !self_meta.hit || req.param === toN)
    will_drop_probeack := !clients_have_T
    will_save_probeack := !(will_probeack_through || !will_drop_probeack)
    probeAckDataThrough := will_probeack_through
    probeAckDataDrop := will_drop_probeack
  }

  val no_wait = w_probeacklast && w_grantlast && w_releaseack && w_grantack
  io.tasks.source_a.valid := !s_acquire && s_release && s_probe
  io.tasks.source_b.valid := !s_probe
  io.tasks.source_c.valid := !s_release && w_probeack && s_writeprobe || !s_probeack && s_writerelease // && w_probeackfirst
  io.tasks.source_d.valid := !s_execute && w_grant && s_writeprobe && w_probeacklast // TODO: is there dependency between s_writeprobe and w_probeack?
  io.tasks.source_e.valid := !s_grantack && w_grantfirst
  io.tasks.dir_write.valid := !s_wbselfdir && no_wait
  io.tasks.tag_write.valid := !s_wbselftag && no_wait
  io.tasks.client_dir_write.zip(s_wbclientsdir).foreach { case (t, s) => t.valid := !s && no_wait }
  io.tasks.client_tag_write.zip(s_wbclientstag).foreach { case (t, s) => t.valid := !s && no_wait }
  io.tasks.sink_a.valid := !s_writeput && w_grant && s_writeprobe && w_probeacklast // TODO: is there dependency between s_writeprobe and w_probeack?
  io.tasks.sink_c.valid := (!s_writerelease && (!releaseSave || s_release)) || (!s_writeprobe)
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
  oa.opcode := Mux(clients_hit || self_meta.hit, AcquirePerm, AcquireBlock)
  oa.param := Mux(req_needT, Mux(clients_hit || self_meta.hit, BtoT, NtoT), NtoB)
  oa.source := io.id
  oa.needData := !(req.opcode === AcquirePerm) || req.size =/= offsetBits.U

  ob.tag := req.tag
  ob.set := req.set
  ob.param := Mux(
    req.fromB,
    req.param,
    Mux(
      req.opcode === Hint,
      toT, // If prefetch needs to find the block in other clients, send Probe toT to get data
      Mux(req_needT, toN, toB)
    )
  )
  // Which clients should be probed?
  val probe_clients = RegEnable(
    Mux(
      req.opcode === Hint,
      prefetch_miss_need_probe_vec,
      VecInit(clients_meta.map {
        case m => Mux(ob.param === toN, m.hit, m.hit && ob.param === toB && isT(m.state))
      })
    ).asUInt & ~Mux(req.fromA && skipProbeN(req.opcode), UIntToOH(iam), 0.U),
    io.dirResult.valid
  )
  ob.clients := probe_clients

  oc.opcode := Mux(
    req.fromB,
    Cat(ProbeAck(2, 1), (probe_dirty || self_meta.hit && self_meta.dirty).asUInt),
    Cat(Release(2, 1), self_meta.dirty.asUInt)
  )
  oc.tag := Mux(req.fromB, req.tag, self_meta.tag)
  oc.set := req.set

  val probeack_param = MuxLookup( // TODO: optimize this
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
  )
  oc.param := Mux(
    req.fromB,
    probeack_param,
    replace_param
  )
  oc.source := io.id
  oc.way := self_meta.way

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

  ic.sourceId := DontCare
  ic.source := io.id
  ic.set := req.set
  ic.tag := req.tag
  ic.size := req.size
  ic.off := req.off
  ic.way := self_meta.way
  ic.bufIdx := Mux(
    s_writeprobe,
    req.bufIdx,
    RegEnable(io.resps.sink_c.bits.bufIdx, io.resps.sink_c.valid && io.resps.sink_c.bits.hasData)
  )
  ic.opcode := Mux(s_writeprobe, req.opcode, ProbeAckData)
  // ic.param will only be used when ic.release is true
  ic.param := Mux(
    !s_writeprobe,
    probeack_param,
    Mux(self_meta.hit, replace_param, req.param)
  )
  ic.save := Mux(s_writeprobe, releaseSave, probeAckDataSave)
  ic.drop := Mux(s_writeprobe, releaseDrop, probeAckDataDrop)
  ic.release := Mux(s_writeprobe, releaseThrough, probeAckDataThrough)

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

  io.tasks.prefetch_train.foreach { train =>
    train.bits.tag := req.tag
    train.bits.set := req.set
    train.bits.needT := req_needT
    train.bits.source := req.source
  }

  io.tasks.prefetch_resp.foreach { resp =>
    resp.bits.tag := req.tag
    resp.bits.set := req.set
  }

  dontTouch(io.tasks)
  when(io.tasks.source_a.fire()) {
    s_acquire := true.B
  }
  when(io.tasks.source_b.fire()) {
    s_probe := true.B
  }
  when(io.tasks.source_c.fire()) {
    s_release := true.B
    s_probeack := true.B
  }
  when(io.tasks.source_d.fire()) {
    s_execute := true.B
  }
  when(io.tasks.source_e.fire()) {
    s_grantack := true.B
  }
  when(io.tasks.dir_write.fire()) {
    s_wbselfdir := true.B
  }
  when(io.tasks.tag_write.fire()) {
    s_wbselftag := true.B
  }
  io.tasks.client_dir_write.zip(s_wbclientsdir).foreach {
    case (t, s) => when(t.fire()) {
      s := true.B
    }
  }
  io.tasks.client_tag_write.zip(s_wbclientstag).foreach {
    case (t, s) => when(t.fire()) {
      s := true.B
    }
  }
  when(io.tasks.sink_a.fire()) {
    s_writeput := true.B
  }
  when(io.tasks.sink_c.fire()) {
    when(!s_writeprobe) {
      s_writeprobe := true.B
    }.otherwise {
      s_writerelease := true.B
    }
  }
  if (prefetchOpt.nonEmpty) {
    when(io.tasks.prefetch_train.get.fire()) {
      s_triggerprefetch.get := true.B
    }
    when(io.tasks.prefetch_resp.get.fire()) {
      s_prefetchack.get := true.B
    }
  }

  val probeack_bit = getClientBitOH(io.resps.sink_c.bits.source)
  val probeack_last = (probes_done | probeack_bit) === probe_clients // This is the last client sending probeack
  when(req_valid && io.resps.sink_c.valid && io.resps.sink_c.bits.hasData) {
    when(probeack_last && io.resps.sink_c.bits.last) {
      // TODO: this is slow, optimize this
      s_writeprobe := false.B
    }
  }
  when(io.resps.sink_c.valid) {
    val resp = io.resps.sink_c.bits
    probes_done := probes_done | probeack_bit
    w_probeackfirst := w_probeackfirst || probeack_last
    w_probeacklast := w_probeacklast || probeack_last && resp.last
    w_probeack := w_probeack || probeack_last && (resp.last || req.off === 0.U)

    probe_dirty := probe_dirty || resp.hasData && !w_probeackfirst
  }
  when(io.resps.sink_d.valid) {
    when(io.resps.sink_d.bits.opcode === Grant || io.resps.sink_d.bits.opcode === GrantData) {
      sink := io.resps.sink_d.bits.sink
      w_grantfirst := true.B
      w_grantlast := w_grantlast || io.resps.sink_d.bits.last
      w_grant := req.off === 0.U || io.resps.sink_d.bits.last
      bad_grant := io.resps.sink_d.bits.denied
      gotT := io.resps.sink_d.bits.param === toT
    }
    when(io.resps.sink_d.bits.opcode === ReleaseAck) {
      w_releaseack := true.B
    }
  }
  when(io.resps.sink_e.valid) {
    w_grantack := true.B
  }

  // Release MSHR
  val no_schedule = s_probeack && s_execute && s_grantack && s_wbselfdir && s_wbselftag &&
    s_wbclientsdir.asUInt.andR && s_wbclientstag.asUInt.andR && s_writerelease && s_writeprobe &&
    meta_valid &&
    s_triggerprefetch.getOrElse(true.B) &&
    s_prefetchack.getOrElse(true.B) // TODO: s_writeput?
  when(no_wait && no_schedule) {
    meta_valid := false.B
    req_valid := false.B
    releaseThrough := false.B
    releaseDrop := false.B
    probeAckDataThrough := false.B
    probeAckDataDrop := false.B
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
  io.status.bits.will_grant_data := req.fromA && od.opcode(0)
  io.status.bits.will_save_data := req.fromA && (preferCache || self_meta.hit)
  io.status.bits.blockB := true.B
  // B nest A
  io.status.bits.nestB := meta_valid && w_releaseack && w_probeacklast && !w_grantfirst
  io.status.bits.blockC := true.B
  // C nest B | C nest A
  io.status.bits.nestC := meta_valid && w_releaseack && (!w_probeackfirst || !w_grantfirst)

  // C nest A (C -> A)
  io_is_nestedReleaseData := req.fromC && !other_clients_hit /*&& isToN(req.param) */ && req_valid
  // B nest A (B -> A)
  io_is_nestedProbeAckData := req.fromB && clients_hit && req_valid

  // C nest A (A -> C)
  io_c_status.releaseThrough := req_valid &&
    io_c_status.set === req.set && io_c_status.tag =/= req.tag &&
    io_c_status.way === self_meta.way && io_c_status.nestedReleaseData && req.fromA
  // B nest A (A -> B)
  io_b_status.probeAckDataThrough := req_valid &&
    io_b_status.set === req.set && io_c_status.tag =/= req.tag &&
    io_b_status.way === self_meta.way &&
    io_b_status.nestedProbeAckData && req.fromA
}
