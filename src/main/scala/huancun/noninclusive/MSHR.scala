package huancun.noninclusive

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.TLHints._
import huancun._
import huancun.utils._
import huancun.MetaData._
import utility.{MemReqSource, ParallelMax}

class C_Status(implicit p: Parameters) extends HuanCunBundle {
  // When C nest A, A needs to know the status of C and tells C to release through to next level
  val set = Input(UInt(setBits.W))
  val tag = Input(UInt(tagBits.W))
  val way = Input(UInt(wayBits.W))
  val nestedReleaseData = Input(Bool())
  val releaseThrough = Output(Bool())
  val metaValid = Input(Bool())
  val reqDirty = Input(Bool())
}

class B_Status(implicit p: Parameters) extends HuanCunBundle {
  val set = Input(UInt(setBits.W))
  val tag = Input(UInt(tagBits.W))
  val way = Input(UInt(wayBits.W))
  val nestedProbeAckData = Input(Bool())
  val probeHelperFinish = Input(Bool())
  val probeAckDataThrough = Output(Bool())
}

class MSHR()(implicit p: Parameters) extends BaseMSHR[DirResult, SelfDirWrite, SelfTagWrite] with HasClientInfo {
  class MSHRTasksWithClient(implicit p: Parameters)
    extends MSHRTasks[SelfDirWrite, SelfTagWrite] {
      override val dir_write: DecoupledIO[SelfDirWrite] = DecoupledIO(new SelfDirWrite())
      override val tag_write: DecoupledIO[SelfTagWrite] = DecoupledIO(new SelfTagWrite())
      val client_dir_write = DecoupledIO(new ClientDirWrite())
      val client_tag_write = DecoupledIO(new ClientTagWrite())
    }
  val io = IO(new BaseMSHRIO[DirResult, SelfDirWrite, SelfTagWrite] {
    override val tasks = new MSHRTasksWithClient
    override val dirResult = Flipped(ValidIO(new DirResult()))
  })

  val io_c_status = IO(new C_Status)
  val io_b_status = IO(new B_Status())
  val io_releaseThrough = IO(Input(Bool()))
  val io_probeAckDataThrough = IO(Input(Bool()))
  val io_is_nestedReleaseData = IO(Output(Bool()))
  val io_is_nestedProbeAckData = IO(Output(Bool()))
  val io_probeHelperFinish = IO(Output(Bool()))
  val io_metaValid = IO(Output(Bool()))
  val io_reqDirty = IO(Output(Bool()))

  val req = Reg(new MSHRRequest)
  val req_valid = RegInit(false.B)
  val iam = Reg(UInt((log2Up(clientBits)+1).W)) // Which client does this req come from?
  val meta_reg = Reg(new DirResult)
  val clients_meta_reg = WireInit(0.U.asTypeOf(io.dirResult.bits.clients.states))
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
  val clients_meta = meta.clients.states
  clients_meta_reg := meta_reg.clients.states
  dontTouch(self_meta)
  dontTouch(clients_meta)

  // Final meta to be written
  val new_self_meta = WireInit(self_meta)
  val new_clients_meta = WireInit(clients_meta)
  val req_acquire = req.opcode === AcquireBlock || req.opcode === AcquirePerm
  val req_put = req.opcode(2,1) === 0.U
  val req_needT = needT(req.opcode, req.param)
  val promoteT_safe = RegInit(true.B)
  val gotT = RegInit(false.B)
  val gotDirty = RegInit(false.B)
  val a_do_release = RegInit(false.B)
  val a_do_probe = RegInit(false.B)
  val meta_no_clients = Cat(self_meta.clientStates.map(_ === INVALID)).andR
  val req_promoteT = req_acquire && Mux(
    self_meta.hit,
    meta_no_clients && self_meta.state === TIP,
    gotT && promoteT_safe
  )
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

  val will_be_free = Wire(Bool())

  val req_client_meta = clients_meta(iam)
  val client_hit_flag = Hold(req_client_meta.hit, l2Only = true)
  val cache_alias = !req.isPrefetch.getOrElse(false.B) && client_hit_flag && req_acquire &&
    req_client_meta.alias.getOrElse(0.U) =/= req.alias.getOrElse(0.U)
  val highest_perm = ParallelMax(
    Seq(Mux(self_meta.hit && !io_probeAckDataThrough, self_meta.state, INVALID)) ++
      clients_meta.map(m => Mux(m.hit, m.state, INVALID))
  ) // the highest perm of this whole level, including self and clients
  val highest_perm_reg = Hold(highest_perm, l2Only = false)
  val highest_perm_except_me = ParallelMax(
    Seq(Mux(self_meta.hit, self_meta.state, INVALID)) ++
      clients_meta.zipWithIndex.map {
        case (m, i) =>
          Mux(req_acquire && iam === i.U && !cache_alias,
            INVALID,
            Mux(m.hit, m.state, INVALID)
          )
      }
  )


  val need_block_downwards = RegInit(false.B)
  val inv_self_dir = RegInit(false.B)
  val client_probeack_param_vec_reg = RegInit(VecInit(Seq.fill(clientBits)(0.U(3.W))))
  val client_probeack_param_vec = WireInit(VecInit(Seq.fill(clientBits)(0.U(3.W))))

  // When replacing a block in data array, it is not always necessary to send Release,
  // but only when state perm > clientStates' perm or replacing a dirty block
  val replace_clients_perm = ParallelMax(self_meta.clientStates)
  val replace_need_release = self_meta.state > replace_clients_perm || self_meta.dirty && (self_meta.state === BRANCH || self_meta.state === TIP)
  val replace_param = MuxLookup(
    Cat(self_meta.state, replace_clients_perm),
    TtoB)(
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
  val prefetch_miss_need_probe = Hold(prefetch_miss_need_probe_vec.asUInt.orR, l2Only = true)
  val prefetch_miss = req.opcode === Hint && (prefetch_miss_need_acquire || prefetch_miss_need_probe)
  val prefetch_need_data = prefetch_miss && !self_meta.hit

  // self cache does not have the acquired block, but some other client owns the block
  val transmit_from_other_client = !self_meta.hit && Hold(VecInit(clients_meta.zipWithIndex.map {
    case (meta, i) =>
      (req.opcode === Get || req_put || i.U =/= iam) && meta.hit
  }).asUInt.orR, l2Only = false) && (!req.isPrefetch.getOrElse(false.B) || prefetch_need_data)

  val a_need_data = req.fromA && (req.opcode === Get || req_put || req.opcode === AcquireBlock || req.opcode === Hint)
  val acquireperm_alias = req.fromA && req.opcode === AcquirePerm && cache_alias

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
  val probeAckDataSave = RegInit(false.B)

  val someClientHasProbeAckData = RegInit(false.B)

  // has been nested C
  val nested_c_hit_reg = RegInit(false.B)
  val nested_c_hit = WireInit(nested_c_hit_reg)

  // Which clients should be probed?
  // a req:
  // 1. cache alias
  // 2. transmit from other clients
  val a_probe_clients = VecInit(clients_meta.zipWithIndex.map {
    case (m, i) =>
      Mux(i.U === iam && req_acquire,
        cache_alias,
        m.hit && (
          req_needT && m.state =/= INVALID || isT(m.state) ||
            !self_meta.hit   // transmit from other client since we are non-inclusive
          )
      )
  })

  def probe_next_state(state: UInt, param: UInt): UInt = Mux(
    isT(state) && param === toT,
    state,
    Mux(state =/= INVALID && param =/= toN, BRANCH, INVALID)
  )

  def shrink_next_state(param: UInt): UInt = {
    assert(param =/= TtoT, "ProbeAck toT is not allowed in current design")
    Mux(param === TtoB || param === BtoB, BRANCH, INVALID)
  }

  def probe_shrink_perm(state: UInt, perm: UInt): Bool = {
    perm === toN || isT(state) && perm === toB
  }
  def probe_is_report(state: UInt, perm: UInt): Bool = {
    // TODO: support TtoT
    state === BRANCH && perm === toB
  }

  def onXReq(): Unit = {
    new_self_meta.dirty := false.B
    new_self_meta.state := Mux(req.param === 1.U,
      Mux(self_meta.hit,
        Mux(isT(self_meta.state), TIP, BRANCH),
        self_meta.state),
      INVALID
    )
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := Mux(self_meta.hit, Mux(self_meta.clientStates(i) =/= INVALID && req.param === 1.U, BRANCH, INVALID), self_meta.clientStates(i))
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) =>
        m.state := Mux(m.hit, Mux(clients_meta(i).state =/= INVALID && req.param === 1.U, BRANCH, INVALID), clients_meta(i).state)
    }
  }

  def onCReq(): Unit = {
    // Release / ReleaseData
    new_self_meta.dirty := self_meta.hit && self_meta.dirty || req.dirty && isParamFromT(req.param)
    new_self_meta.state := MuxLookup(
      req.param,
      self_meta.state)(
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
          clients_meta_reg(i).state
        )
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) =>
        m.state := Mux(
          iam === i.U,
          Mux(isToN(req.param), INVALID, Mux(isToB(req.param), BRANCH, clients_meta_reg(i).state)),
          clients_meta_reg(i).state
        )
    }
  }

  def onBReq(): Unit = {
    /*
      if this request is from probe helper, the req param must be toN,
      but self state should not change to INVALID
      --------------------------
      self state   |  after probe
      --------------------------
        TIP       ->    TIP
        TRUNK     ->    TIP
        BRANCH    ->    BRANCH
      --------------------------
    */
    new_self_meta.dirty := req.fromProbeHelper && !probeAckDataThrough && (self_meta.hit && self_meta.dirty || probe_dirty)
    new_self_meta.state := Mux(self_meta.hit,
      Mux(req.fromProbeHelper && !probeAckDataThrough,
        Mux(isT(self_meta.state), TIP, BRANCH),
        probe_next_state(self_meta.state, req.param)
      ),
      self_meta.state
    )
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := Mux(self_meta.hit, probe_next_state(self_meta.clientStates(i), req.param), self_meta.clientStates(i))
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) =>
        m.state := Mux(m.hit, shrink_next_state(client_probeack_param_vec(i)), clients_meta_reg(i).state)
    }
  }

  def onAReq(): Unit = {
    // reqs: Acquire / Intent / Put / Get / Atomics
    // new_self_meta.dirty := self_meta.hit && self_meta.dirty || probe_dirty || !req.opcode(2)
    new_self_meta.dirty := Mux(
      req_acquire,
      Mux(req_needT,
        false.B,
        Mux(self_meta.hit,
          Mux(req_promoteT, false.B, self_meta.dirty || probe_dirty),
          gotDirty || probe_dirty
        )
      ),
      Mux(req_put,
        true.B,  // Put
        gotDirty || probe_dirty || (self_meta.hit && self_meta.dirty), // Hint & Get
      )
    )
    new_self_meta.state := Mux(
      req_needT,
      Mux(req_acquire,
        TRUNK,
        // for prefetch, if client hit, we should not change self_meta
        // however, we won't update self_meta in that case
        // so this is the case for Put
        TIP
      ),
      Mux(!self_meta.hit,
        Mux(
          transmit_from_other_client || cache_alias, // For cache alias, !promoteT is granteed
          Mux(gotT, Mux(req_acquire && promoteT_safe, TRUNK, TIP), highest_perm_reg),
          Mux(gotT,
            Mux(req_acquire && promoteT_safe, TRUNK, TIP),
            // for prefetch, if client already hit, self meta won't update,
            // so we don't care new_self_meta.state.
            // if client miss, we will be BRANCH
            BRANCH
          ),
        ),
        MuxLookup(self_meta.state, INVALID)(Seq(
          INVALID -> BRANCH,
          BRANCH -> BRANCH,
          // if prefetch read && hit && self is Trunk
          // self meta won't update, we don't care new_meta
          TRUNK -> TIP,
          TIP -> Mux(
            meta_no_clients && req_acquire, // promoteT
            TRUNK, TIP
          )
        ))
      )
    )
    when(inv_self_dir){
      new_self_meta.state := INVALID
    }
    new_self_meta.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        val perm_after_probe = shrink_next_state(client_probeack_param_vec(i))
        when(iam === i.U) {
          state := Mux(req_acquire,
            Mux(req_needT || req_promoteT, TIP, BRANCH),
            Mux(
              req.opcode === Get,
              Mux(clients_meta_reg(i).hit, BRANCH, INVALID),
              Mux(clients_meta_reg(i).hit, clients_meta_reg(i).state, INVALID)
            )
          )
        }.otherwise {
          val cond = if(cacheParams.name == "L2") {
            Mux(a_probe_clients(i), perm_after_probe, clients_meta_reg(i).state)
          } else {
            perm_after_probe
          }
          state := Mux(
            req_acquire || req_put,
            Mux(
              req.param =/= NtoB || req_promoteT || req_put,
              INVALID,
              Mux(clients_meta_reg(i).hit && a_do_probe, cond, INVALID)
            ),
            Mux(
              req.opcode === Get,
              Mux(clients_meta_reg(i).hit && a_do_probe, perm_after_probe, INVALID), // Get
              Mux(prefetch_miss_need_probe, Mux(req.param === PREFETCH_READ, perm_after_probe, INVALID), clients_meta_reg(i).state)
            )
          )
        }
    }
    new_clients_meta.zipWithIndex.foreach {
      case (m, i) =>
        val perm_after_probe = shrink_next_state(client_probeack_param_vec(i))
        when(iam === i.U) {
          m.state := Mux(
            req_acquire,
            Mux(req_needT || req_promoteT, TIP, BRANCH),
            clients_meta_reg(i).state
          )
          m.alias.foreach(_ := Mux(req_acquire, req.alias.get, clients_meta_reg(i).alias.get))
        }.otherwise {
          val cond = if(cacheParams.name == "L2") {
            Mux(a_probe_clients(i), perm_after_probe, clients_meta_reg(i).state)
          } else {
            perm_after_probe
          }
          m.state := Mux(
            req_acquire || req_put,
            Mux(
              req.param =/= NtoB || req_promoteT || req_put,
              Mux(clients_meta_reg(i).hit && a_do_probe, INVALID, clients_meta_reg(i).state),
              // NtoB
              Mux(clients_meta_reg(i).hit && a_do_probe, cond, clients_meta_reg(i).state)
            ),
            Mux(prefetch_miss_need_probe, Mux(req.param === PREFETCH_READ, perm_after_probe, INVALID), clients_meta_reg(i).state)
          )
          m.alias.foreach(_ := clients_meta_reg(i).alias.get)
        }

        when (req.opcode === Get) {
          m.state := Mux(clients_meta_reg(i).hit && a_do_probe, perm_after_probe, clients_meta_reg(i).state)
          m.alias.foreach(_ := clients_meta_reg(i).alias.get)
        }
    }
  }

  when(req.fromC) {
    when(req.fromCmoHelper) {
      onXReq()
    }.otherwise {
      onCReq()
    }
  }.elsewhen(req.fromB) {
    onBReq()
  }.otherwise {
    onAReq()
  }

  val new_clients_dir = Wire(Vec(clientBits, new ClientDirEntry))
  val new_self_dir = Wire(new SelfDirEntry)
  new_self_dir.dirty := new_self_meta.dirty || nested_c_hit
  new_self_dir.state := new_self_meta.state
  new_self_dir.clientStates := new_self_meta.clientStates
  new_self_dir.prefetch.foreach(_ := self_meta.prefetch.get || prefetch_miss)
  new_clients_dir.zip(new_clients_meta).foreach {
    case (dir, meta) =>
      dir.state := meta.state
      dir.alias.foreach(_ := meta.alias.get)
  }

  val sink = Reg(UInt(edgeOut.bundle.sinkBits.W))

  val bad_grant = RegInit(false.B)
  // TODO: consider bad grant
  when(bad_grant) {
    new_self_dir.dirty := false.B
    new_self_dir.state := self_meta.state
    new_self_dir.clientStates.zipWithIndex.foreach {
      case (state, i) =>
        state := Mux(self_meta.hit && i.U =/= iam, new_self_meta.clientStates(i), self_meta.clientStates(i))
    }
  }

  val debug_addr = Cat(req.tag, req.set, 0.U(offsetBits.W))
  assert(RegNext(!meta_valid || !req.fromC || req.fromCmoHelper || clients_meta(iam).hit),
    s"${cacheParams.name} Release should always hit: mshrId:[%d] addr: [%x]",
    io.id, debug_addr
  ) // Release should always hit

  // nested writeback to meta_reg
  val change_self_meta = meta_valid && self_meta.state =/= INVALID &&
    io.nestedwb.set === req.set && io.nestedwb.tag === self_meta.tag
  val nested_client_match = (
    meta.clients.parseTag(Cat(io.nestedwb.tag, io.nestedwb.set)) === meta.clients.tag &&
      io.nestedwb.set(clientSetBits - 1, 0) === req.set(clientSetBits - 1, 0)
  )
  val change_clients_meta = clients_meta.zipWithIndex.map {
    case (meta, i) =>
      meta_valid && meta.state =/= INVALID && nested_client_match
  }
  when(change_self_meta) {
    when(io.nestedwb.b_clr_dirty) {
      meta_reg.self.dirty := false.B
    }
    when(io.nestedwb.b_set_dirty) {
      meta_reg.self.dirty := true.B
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

  when (meta_valid && !self_meta.hit && req.fromA &&
    io.nestedwb.set === req.set && io.nestedwb.c_set_hit
  ) {
    nested_c_hit := true.B
    nested_c_hit_reg := true.B
  }

  meta_reg.clients.states.zipWithIndex.foreach {
    case (reg, i) =>
      when(change_clients_meta(i)) {
        when(io.nestedwb.clients.get (i).isToB) {
          // reg.state := BRANCH
        }
        when(io.nestedwb.clients.get (i).isToN) {
          reg.state := INVALID
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
  val s_wbclientsdir = RegInit(true.B) // write clients' dir
  val s_wbclientstag = RegInit(true.B) // write clients' tag
  val s_transferput = RegInit(true.B) // writeput to source_a
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
  val w_putwritten = RegInit(true.B)
  val w_sinkcack = RegInit(true.B)

  val acquire_flag = RegInit(false.B)
  val nestC_save_flag = RegInit(false.B)
  val nestC_saveDirty_flag = RegInit(false.B)
  val nestC_save = WireInit(false.B)
  val nestC_saveDirty = WireInit(false.B)

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
    s_wbclientsdir := true.B
    s_wbclientstag := true.B
    s_transferput := true.B
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
    w_putwritten := true.B
    w_sinkcack := true.B

    promoteT_safe := true.B
    gotT := false.B
    probe_dirty := false.B
    probes_done := 0.U
    bad_grant := false.B
    need_block_downwards := false.B
    inv_self_dir := false.B
    nested_c_hit_reg := false.B
    gotDirty := false.B
    acquire_flag := false.B
    a_do_release := false.B
    a_do_probe := false.B
    nestC_save_flag := false.B
    nestC_saveDirty_flag := false.B
  }
  when(!s_acquire) { acquire_flag := acquire_flag | true.B }

  def x_schedule(): Unit = { // TODO
    // Do probe to maintain coherence
    clients_meta.zipWithIndex.foreach {
      case (meta, i) =>
        when(meta.hit) {
          s_probe := false.B
          s_wbclientsdir := false.B
          w_probeackfirst := false.B
          w_probeacklast := false.B
          w_probeack := false.B
        }
    }
    when(self_meta.hit) {
      s_wbselfdir := false.B
    }
    // clean or flush need release when self_meta.hit
    //                need probeAckDataThrough when !self_meta.hit and clients_hit
    when(req.param =/= 0.U && self_meta.hit) {
      s_release := false.B
      w_releaseack := false.B
    }
  }

  def c_schedule(): Unit = {
    // Release
    s_execute := false.B
    // When the req shrinks the perm in clients indeed, write client dir.
    when(client_shrink_perm) {
      s_wbclientsdir := false.B
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
      w_sinkcack := false.B
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

    // if save/drop, read data from sourceC (s_probeack)
    // if through, read data from sinkC (s_writeprobe)
    when(!will_probeack_through && !req.fromProbeHelper) {
      s_probeack := false.B
    }
    when(self_meta.hit) {
      when(probe_shrink_perm(self_meta.state, req.param)){
        s_wbselfdir := false.B
      }.otherwise({
        assert(probe_is_report(self_meta.state, req.param),
          "Probe not supported: [self: %d] [req: %d]\n",
          self_meta.state, req.param
        )
      })
    }
    // no need to probe client if self-meta is B/T and param is toB
    when(!(self_meta.hit && self_meta.state(0) && req.param === toB)) {
      for (meta <- clients_meta) {
        when(meta.hit) {
          assert(
            probe_shrink_perm(meta.state, req.param) || probe_is_report(meta.state, req.param),
            "Invalid Probe Request: [client: %d] [req: %d]\n", meta.state, req.param
          )
        }
      }
      when(Cat(clients_meta.map(_.hit)).orR) {
        s_probe := false.B
        s_wbclientsdir := false.B
        w_probeackfirst := false.B
        w_probeacklast := false.B
        w_probeack := false.B
      }
    }
  }

  val bypassPut = req_put && !self_meta.hit && !Cat(clients_meta.map(_.hit)).orR
  val bypassPut_latch = Keep(bypassPut)
  val bypassPut_all = Mux(io.dirResult.valid, bypassPut, bypassPut_latch)
  // Cache alias will always preferCache to avoid trifle
  val preferCache = (req.preferCache && !bypassPut_all) || cache_alias
  val bypassGet = req.opcode === Get && !preferCache

  def set_probe(): Unit = {
    a_do_probe := true.B
    s_probe := false.B
    w_probeackfirst := false.B
    w_probeacklast := false.B
    w_probeack := false.B
    when(!self_meta.hit){
      s_wbselfdir := false.B
      s_wbselftag := false.B
    }
  }
  val acquirePermMiss = req.opcode === AcquirePerm && !self_meta.hit

  def a_schedule(): Unit = {
    // A channel requests
    // TODO: consider parameterized write-through policy for put/atomics
    s_execute := req.opcode === Hint
    when(!self_meta.hit && self_meta.state =/= INVALID && replace_need_release &&
      (
        (preferCache && (req.opcode === AcquireBlock || req.opcode === Get)) ||
          (
            prefetch_need_data ||
            transmit_from_other_client || // inner probe -> replace -> release
            cache_alias                   // inner probe -> replace -> release
          )
        )
    ){
      s_release := false.B
      w_releaseack := false.B
      a_do_release := true.B
    }

    // need Acquire downwards
    when (
      Mux(
        req_acquire,
        Mux(req_needT, !isT(highest_perm_except_me), highest_perm_except_me === INVALID),
        Mux(req_needT, !isT(highest_perm), highest_perm === INVALID)
      )
    ) {
      s_acquire := false.B // for bypassPut case, s_acquire actually means s_put
      w_grantfirst := false.B
      w_grantlast := false.B
      w_grant := false.B  // for bypassPut case, w_grant actually means w_accessack
      when (!bypassGet && !bypassPut) {
        s_grantack := false.B
      }
      // for acquirePermMiss and (miss && !preferCache):
      // no data block will be saved, so self dir won't change
      when(!acquirePermMiss && ((self_meta.hit && !(req.opcode === Get)) || preferCache)) {
        s_wbselfdir := false.B
      }
    }
    // need probe
    clients_meta.zipWithIndex.foreach {
      case (meta, i) =>
        when (req.opcode =/= Get && !req_put) {
          // Acquire / Hint
          when(i.U =/= iam) {
            when(
              meta.hit && (
                req_acquire && (req_needT || !self_meta.hit || isT(meta.state)) ||
                req.opcode === Hint && prefetch_miss_need_probe
                )
            ) {
              set_probe()
              s_wbclientsdir := false.B
            }
          }.otherwise {
            when(cache_alias) {
              set_probe()
              s_wbclientsdir := false.B
            }
          }
        }.otherwise {
          // Get / Put
          when (meta.hit && (isT(meta.state) || !self_meta.hit || req_put)) {
            set_probe()
            when(self_meta.hit) { // For get/put, self meta hit and need probe, then wbselfdir is necessary
              s_wbselfdir := false.B
            }
            s_wbclientsdir := false.B
          }
        }
    }
    // need grantack
    when(req_acquire) {
      w_grantack := false.B
      when(!acquirePermMiss && (self_meta.hit || preferCache)) {
        s_wbselfdir := false.B
      }
      s_wbclientsdir := false.B
      when(!clients_meta(iam).hit) {
        s_wbclientstag := false.B
      }
    }
    // need to write self tag
    when(
      !self_meta.hit && preferCache &&
        (req.opcode === Get || req.opcode === AcquireBlock || prefetch_need_data)
    ) {
      s_wbselftag := false.B
    }
    // need to transfer exactly the request to sourceA when Put miss
    when(bypassPut) {
      s_transferput := false.B
    }
    when(req_put && !bypassPut) {
      w_putwritten := false.B
    }
    // Put needs to write (self = T, clean ==> dirty)
    when(req_put && self_meta.hit && !self_meta.dirty) {
      s_wbselfdir := false.B
    }
    prefetchOpt.map(_ => {
      when(req.opcode =/= Hint && req.needHint.getOrElse(false.B) && (!self_meta.hit || self_meta.prefetch.get)) {
        s_triggerprefetch.map(_ := false.B)
      }
      when(req.opcode === Hint && req.isBop.getOrElse(false.B)) {
        s_prefetchack.map(_ := false.B)
      }
    })
    assert(req.opcode =/= Hint || req.preferCache, "Hint should always preferCache!")
  }

  val self_ecc_err = meta_reg.self.hit && meta_reg.self.error
  val client_ecc_err = Cat(meta_reg.clients.states.map(_.hit)).orR && meta_reg.clients.error
  io.ecc.valid := meta_valid && (self_ecc_err || client_ecc_err)
  io.ecc.bits.addr := Cat(req.tag, req.set, req.off)
  io.ecc.bits.errCode := Mux(self_ecc_err, EccInfo.ERR_SELF_DIR, EccInfo.ERR_CLIENT_DIR)

  when(io.dirResult.valid) {

    reset_all_flags()

    when(meta.self.error || meta.clients.error){
      when(req.fromC){
        s_execute := false.B
      }
      when(req.fromB){
        s_probeack := false.B
      }
      when(req.fromA) {
        s_execute := false.B
        when(req_acquire){
          w_grantack := false.B
        }
      }
    }.otherwise({
      when(req.fromC) {
        when(req.fromCmoHelper) {
          x_schedule()
        }.otherwise {
          c_schedule()
        }
      }.elsewhen(req.fromB) {
        b_schedule()
      }.otherwise {
        when (!(req.opcode === Hint && clients_hit)) {
          a_schedule()
        }
      }
    })
  }

  when(io_releaseThrough && io.dirResult.valid && req.fromC) {
    assert(req_valid)
    // TtoN or BtoN should release through
    will_release_through := !other_clients_hit && isShrink(req.param)
    // report or TtoB will be dropped
    will_drop_release := !will_release_through
    // if enter this part, we must NOT save release
    // since there is a refill want to replace us
    will_save_release := false.B
    releaseThrough := will_release_through
    releaseDrop := will_drop_release
  }

  when(io.dirResult.valid && req.fromC) {
    assert(
      PopCount(
        Seq(will_release_through, will_drop_release, will_save_release)
      ) === 1.U,
      "release ploicy conflict! [through drop save]: [%b %b %b]",
      will_release_through,
      will_drop_release,
      will_save_release
    )
  }

  when(io.dirResult.valid) {
    when(req.fromB) {
      will_probeack_through := clients_have_T || (req.needProbeAckData.getOrElse(false.B) && highest_perm =/= INVALID && !self_meta.hit)
      will_drop_probeack := !will_probeack_through
      will_save_probeack := will_probeack_through && !io_probeAckDataThrough && self_meta.hit && req.param === toB
      when(req.fromProbeHelper) {
        probeAckDataThrough := will_probeack_through && (!self_meta.hit || io_probeAckDataThrough)
        probeAckDataDrop := will_drop_probeack
        probeAckDataSave := will_probeack_through && !io_probeAckDataThrough && self_meta.hit
      }.otherwise {
        probeAckDataThrough := will_probeack_through
        probeAckDataDrop := will_drop_probeack
        probeAckDataSave := will_save_probeack
      }
    }.otherwise {
      probeAckDataSave := !probeAckDataThrough && !probeAckDataDrop
    }

    when(req.fromCmoHelper) {
      probeAckDataThrough := req.param =/= 0.U && (clients_have_T && !self_meta.hit) // Clean & Flush
      probeAckDataDrop := req.param === 0.U // Invalidate
      probeAckDataSave := !probeAckDataDrop && !probeAckDataThrough
    }
  }

  val no_wait = w_probeacklast && w_grantlast && w_releaseack && w_grantack && w_putwritten && w_sinkcack

  val clients_meta_busy = Cat(clients_meta.map(s => !s.hit && s.state =/= INVALID)).orR
  val client_dir_conflict = RegEnable(
    req.fromA && req_acquire && clients_meta_busy,
    io.dirResult.valid
  )
  val probe_helper_finish = RegInit(false.B)
  val client_set_match = req.set(clientSetBits - 1, 0) === io_b_status.set(clientSetBits - 1, 0)
  when(req_valid && io_b_status.probeHelperFinish && client_set_match){
    probe_helper_finish := true.B
  }
  when(req_valid && req.fromA && req_acquire && client_dir_conflict && probe_helper_finish){
    assert(RegNext(!clients_meta_busy, true.B),
      s"Error ${cacheParams.name}: meta still conflict when probe helper finish! mshrId: %d",
      io.id
    )
  }

  val can_start = Mux(client_dir_conflict, probe_helper_finish, true.B)
  io.tasks.source_a.valid := io.enable && (!s_acquire || !s_transferput) && s_release && s_probe && w_probeacklast && can_start
  io.tasks.source_b.valid := io.enable && !s_probe && s_release
  io.tasks.source_c.valid := io.enable && (!s_release || !s_probeack && s_writerelease && w_sinkcack && w_probeack)
  io.tasks.source_d.valid := io.enable && !s_execute && can_start && w_grant && s_writeprobe && w_sinkcack && w_probeacklast && s_transferput // TODO: is there dependency between s_writeprobe and w_probeack?
  io.tasks.source_e.valid := !s_grantack && w_grantfirst
  io.tasks.dir_write.valid := io.enable && !s_wbselfdir && no_wait && can_start
  io.tasks.tag_write.valid := io.enable && !s_wbselftag && no_wait && can_start
  io.tasks.client_dir_write.valid := io.enable && !s_wbclientsdir && no_wait && can_start
  io.tasks.client_tag_write.valid := io.enable && !s_wbclientstag && no_wait && can_start
  // io.tasks.sink_a.valid := !s_writeput && w_grant && s_writeprobe && w_probeacklast
  io.tasks.sink_a.valid := false.B
  io.tasks.sink_c.valid := io.enable && ((!s_writerelease && (!releaseSave || s_release)) || (!s_writeprobe))
  io.tasks.prefetch_train.foreach(_.valid := !s_triggerprefetch.get)
  io.tasks.prefetch_resp.foreach(_.valid := !s_prefetchack.get && w_grantfirst)

  val oa = io.tasks.source_a.bits
  val ob = io.tasks.source_b.bits
  val oc = io.tasks.source_c.bits
  val od = io.tasks.source_d.bits
  val oe = io.tasks.source_e.bits
  val ia = io.tasks.sink_a.bits
  val ic = io.tasks.sink_c.bits

  // if we think a client is T,
  // but client acquire NtoB/NtoT/BtoT
  // this means T block is missing from client,
  // then we must grant data
  val client_dir_error = req_acquire && clients_meta(iam).hit &&
    (growFrom(req.param) < clients_meta(iam).state)
  val client_hit_acquire_prem = if(cacheParams.level == 2) {
    // L2 is special:
    // we always grant data to L1,
    // so we can only acquire perm when we hit (have data)
    false.B
  } else {
    // for Hint reqs, we can only acquire perm when we have data
    clients_hit && !need_block_downwards && !client_dir_error && req.opcode =/= Hint
  }
  oa.tag := req.tag
  oa.set := req.set
  oa.off := req.off
  oa.mask := req.mask
  // full overwrite, we can always acquire perm, no need to acquire block
  val acquire_perm_NtoT = req.opcode === AcquirePerm && req.param === NtoT

  val acquire_opcode = if (cacheParams.name == "L2") {
    Mux(req.opcode === AcquirePerm && req.param === BtoT, AcquirePerm, Mux(req.opcode === Hint, AcquireBlock, req.opcode))
  } else {
    Mux(req_put, AcquireBlock, Mux(req.opcode === Hint, AcquireBlock, req.opcode))
    // for put & !bypassPut, cache hierachy should have B/T. AcquirePerm is enough
  }

  val acquire_param = if (cacheParams.name == "L2") {
    Mux(req.opcode === Hint,
      Mux(req_needT, Mux(highest_perm_reg === BRANCH, BtoT, NtoT), NtoB),
      req.param
    )
  } else {
    Mux(req.opcode === Hint,
      Mux(req_needT, Mux(highest_perm_reg === BRANCH, BtoT, NtoT), NtoB),
      Mux(req.opcode === AcquireBlock && req.param === BtoT, NtoT, Mux(req_put, NtoT, req.param))
    )
  }

  oa.opcode := Mux(!s_transferput || bypassGet, req.opcode,
    /*
    Mux(self_meta.hit, AcquireBlock,
      Mux(client_hit_acquire_prem || acquire_perm_NtoT,
        AcquirePerm,
        AcquireBlock
      )
    )
    */
    Mux(req.opcode === Get, AcquireBlock, acquire_opcode)
  )
  oa.param := Mux(!s_transferput, req.param,
    /*
    Mux(req_needT,
      Mux(self_meta.hit,
        BtoT,
        Mux(client_hit_acquire_prem,
          // if we send BtoT, outer cache may not grant data even we are acquiring block,
          // so we should only acquire BtoT when we are sure to not requiring data
          BtoT,
          NtoT
        )
      ),
      NtoB
    )
    */
    Mux(req.opcode === Get, NtoB, acquire_param)
  )
  oa.source := io.id
  oa.needData := !(req.opcode === AcquirePerm) || req.size =/= offsetBits.U // TODO: this is deprecated?
  oa.putData := bypassPut_latch
  oa.bufIdx := req.bufIdx
  oa.size := req.size
  oa.reqSource := Mux(req.opcode === Hint,
    if (cacheParams.level == 2) req.reqSource else MemReqSource.Prefetch2L3Unknown.id.U,
    req.reqSource
  )

  ob.tag := req.tag
  ob.set := req.set
  ob.alias.foreach(a => a.zip(clients_meta.map(_.alias.get)).foreach{
    case (sink, source) => sink := source
  })
  assert(ob.set.getWidth == req.set.getWidth)
  ob.param := Mux(
    req.fromB,
    req.param,
    Mux(
      req.opcode === Hint,
      Mux(req.param === PREFETCH_READ, toB, toN),
      Mux(req.fromCmoHelper,
        Mux(req.param === 1.U, toB, toN),
        Mux(
          req_needT || cache_alias,
          toN,
          toB
        )
      )
    )
  )
  val b_probe_clients = VecInit(clients_meta.map(_.hit))
  val x_probe_clients = VecInit(clients_meta.map {
    case m => Mux(req.param === 1.U, m.hit && isT(m.state), m.hit)
  })
  val probe_clients = RegEnable(
    Mux(
      req.fromA && req.opcode === Hint,
      prefetch_miss_need_probe_vec, // Hint
      Mux(
        req.fromA,
        a_probe_clients, // Acquire / Get
        Mux(req.fromCmoHelper,
          x_probe_clients,
          b_probe_clients // Probe
        )
      )
    ).asUInt,
    io.dirResult.valid
  )
  ob.clients := probe_clients
  ob.needData.foreach(_ :=
    (a_need_data && !self_meta.hit) || (req.fromB && req.needProbeAckData.getOrElse(false.B))
  )

  oc.opcode := Mux(
    req.fromB,
    Cat(
      Mux(req.fromProbeHelper, Release(2, 1), ProbeAck(2, 1)),
      (probe_dirty && !probeAckDataDrop || self_meta.hit && self_meta.dirty ||
        req.needProbeAckData.getOrElse(false.B)) && highest_perm =/= INVALID
    ),
    if (alwaysReleaseData) ReleaseData else Cat(Release(2, 1), self_meta.dirty.asUInt)
  )
  oc.tag := Mux(req.fromB, req.tag, self_meta.tag)
  oc.set := req.set

  val probeack_param = MuxLookup( // TODO: optimize this
    Cat(highest_perm, probe_next_state(highest_perm, req.param)),
    NtoN)(
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
  oc.way := meta_reg.self.way
  oc.dirty := Mux(req.fromB, probe_dirty || self_meta.hit && self_meta.dirty, self_meta.dirty)

  od.sinkId := io.id
  od.useBypass := (!self_meta.hit || self_meta.state === BRANCH && req_needT) &&
    (!probe_dirty && !nestC_save || acquire_flag && oa.opcode =/= AcquirePerm) &&
    !(meta_reg.self.error || meta_reg.clients.error) && !req_put
  od.sourceId := req.source
  od.set := req.set
  od.tag := req.tag
  od.channel := Cat(req.fromC.asUInt, 0.U(1.W), req.fromA.asUInt)

  def odOpGen(r: MSHRRequest) = {
    // for L3: AcquireBlock BtoT -> Grant
    // for L2: our L1D requrire us always grant data
    // val grantOp = if(cacheParams.level == 2) GrantData else Mux(req.param === BtoT, Grant, GrantData)
    // val grantOp = Mux(r.param === BtoT, Grant, GrantData)
    // val grantOp = GrantData
    val grantOp = if (cacheParams.level == 2) {
      Mux(req.param === BtoT && req.opcode === AcquirePerm, Grant, GrantData)
    } else {
      Mux(req.param === BtoT && req.opcode === AcquirePerm, Grant, GrantData)
    }
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
  od.way := meta_reg.self.way
  od.off := req.off
  od.denied := bad_grant || (meta_reg.self.error || meta_reg.clients.error)
  od.dirty := Mux(
    req_acquire,
    Mux(
      self_meta.hit,
      Mux(req.param === NtoB && !req_promoteT, false.B, self_meta.dirty || probe_dirty || gotDirty || nestC_saveDirty),
      gotDirty || probe_dirty || nestC_saveDirty
    ),
    false.B
  )
  od.isHit := self_meta.hit
  od.bufIdx := req.bufIdx
  od.bypassPut := bypassPut_latch

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
  ic.opcode := Mux(s_writeprobe,
    req.opcode,
    Mux(req.fromProbeHelper || req.fromCmoHelper,
      ReleaseData, // convert ProbeAckData -> ReleaseData
      ProbeAckData
    )
  )
  // ic.param will only be used when ic.release is true
  // if need release through
  //      req.param must be [TtoN, BtoN]
  //      Report/TtoB will be dropped
  ic.param := Mux(
    !s_writeprobe,
    probeack_param, // TODO: check this
    req.param
  )
  ic.save := Mux(s_writeprobe, releaseSave, probeAckDataSave)
  ic.drop := Mux(s_writeprobe, releaseDrop, probeAckDataDrop)
  ic.release := Mux(s_writeprobe, releaseThrough, probeAckDataThrough)
  ic.dirty := Mux(s_writeprobe,
    req.dirty || self_meta.hit && self_meta.dirty,
    probe_dirty || self_meta.hit && self_meta.dirty
  )

  io.tasks.dir_write.bits.set := req.set
  io.tasks.dir_write.bits.way := meta_reg.self.way
  io.tasks.dir_write.bits.data := new_self_dir

  io.tasks.tag_write.bits.set := req.set
  io.tasks.tag_write.bits.way := meta_reg.self.way
  io.tasks.tag_write.bits.tag := req.tag

  val req_line_addr = Cat(req.tag, req.set)
  io.tasks.client_dir_write.bits.apply(
    req_line_addr,
    meta_reg.clients.way,
    new_clients_dir
  )
  io.tasks.client_tag_write.bits.apply(
    req_line_addr,
    meta_reg.clients.way
  )

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
  when(io.tasks.source_a.fire) {
    s_acquire := true.B
    s_transferput := true.B
  }
  when(io.tasks.source_b.fire) {
    s_probe := true.B
    assert(io.tasks.source_b.bits.clients =/= 0.U, "Invalid probe task\n")
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
  when(io.tasks.dir_write.fire) {
    s_wbselfdir := true.B
  }
  when(io.tasks.tag_write.fire) {
    s_wbselftag := true.B
  }
  when(io.tasks.client_dir_write.fire){
    s_wbclientsdir := true.B
  }
  when(io.tasks.client_tag_write.fire){
    s_wbclientstag := true.B
  }
  when(io.tasks.sink_c.fire) {
    when(!s_writeprobe) {
      s_writeprobe := true.B
    }.otherwise {
      s_writerelease := true.B
    }
  }
  if (prefetchOpt.nonEmpty) {
    when(io.tasks.prefetch_train.get.fire) {
      s_triggerprefetch.get := true.B
    }
    when(io.tasks.prefetch_resp.get.fire) {
      s_prefetchack.get := true.B
    }
  }

  val sink_c_resp_valid = io.resps.sink_c.valid && !w_probeacklast
  val probeack_bit = getClientBitOH(io.resps.sink_c.bits.source)
  // ! This is the last client sending probeack
  val probeack_last = if(clientBits == 1) sink_c_resp_valid else (probes_done | probeack_bit) === probe_clients

  // Update client_probeack_param_vec according to the param of ProbeAck
  client_probeack_param_vec := client_probeack_param_vec_reg

  val sinkc_resp_last = sink_c_resp_valid && io.resps.sink_c.bits.last
  // update for each client
  when(req_valid && sinkc_resp_last) {
    val client = OHToUInt(getClientBitOH(io.resps.sink_c.bits.source))
    client_probeack_param_vec_reg(client) := io.resps.sink_c.bits.param
    client_probeack_param_vec(client) := io.resps.sink_c.bits.param
  }
  when(req_valid && sinkc_resp_last && io.resps.sink_c.bits.hasData) {
    someClientHasProbeAckData := true.B
  }

  when(req_valid && sinkc_resp_last && probeack_last) {
    when(someClientHasProbeAckData || io.resps.sink_c.bits.hasData){
      // TODO: this is slow, optimize this
      s_writeprobe := false.B
      w_sinkcack := false.B
      when(req.fromB && req.fromProbeHelper && probeAckDataThrough || req.fromCmoHelper && probeAckDataThrough){
        w_releaseack := false.B // inner ProbeAck -> outer Release
      }
    }.otherwise{
      when(req.fromB && ((probeAckDataThrough && io.resps.sink_c.bits.param =/= NtoN) || !req.fromProbeHelper)){
        // client didn't response data
        // but we still need to send probeack
        // let sourceC do this
        s_probeack := false.B
        when(req.fromProbeHelper){
          w_releaseack := false.B
        }
      }
    }
  }
  when(req_valid && sink_c_resp_valid) {
    val resp = io.resps.sink_c.bits
    probes_done := probes_done | probeack_bit
    w_probeackfirst := w_probeackfirst || probeack_last
    w_probeacklast := w_probeacklast || probeack_last && resp.last
    w_probeack := w_probeack || probeack_last && (resp.last || req.off === 0.U)
    probe_dirty := probe_dirty || resp.hasData || nested_c_hit
    when (
      !acquire_flag && req.fromA &&
        probeack_last && resp.last && !resp.hasData && !nested_c_hit && !self_meta.hit && !nestC_save
    ) {
      when(acquireperm_alias && !req_client_meta.hit) {
        printf("BUG_REPRODUCE: nested acquire perm alias\n")
      }
      promoteT_safe := false.B
      s_acquire := false.B
      w_grantfirst := false.B
      w_grantlast := false.B
      w_grant := false.B
      when (!bypassGet && !bypassPut_latch) {
        s_grantack := false.B
      }
      need_block_downwards := true.B
    }

    // we assume clients will ack data for us at first,
    // if they only ack perm, we should change our schedule
    when (
      req.fromA && probeack_last && resp.last && !resp.hasData && !nested_c_hit && !self_meta.hit
    ) {
      when(!(preferCache || self_meta.hit)) {
        // if we don't save grant data, we should not write tag
        s_wbselftag := true.B
        // we may have released the original block at first,
        // so we need to invalidate that block
        inv_self_dir := true.B
      }
    }
  }

  when(req_valid && io.resps.sink_d.valid) {
    when(io.resps.sink_d.bits.opcode === Grant || io.resps.sink_d.bits.opcode === GrantData || io.resps.sink_d.bits.opcode === AccessAckData || io.resps.sink_d.bits.opcode === AccessAck) {
      sink := io.resps.sink_d.bits.sink
      w_grantfirst := true.B
      w_grantlast := w_grantlast || io.resps.sink_d.bits.last
      w_grant := (req.off === 0.U && req.size === log2Ceil(blockBytes).U) || io.resps.sink_d.bits.last  // if req.off != 0, w_grant indicates all beats are acked
      bad_grant := io.resps.sink_d.bits.denied
      gotT := io.resps.sink_d.bits.param === toT
      gotDirty := io.resps.sink_d.bits.dirty
      when (!req_put) {
        req.bufIdx := io.resps.sink_d.bits.bufIdx
      }
    }
    when(io.resps.sink_d.bits.opcode === ReleaseAck) {
      w_releaseack := true.B
    }
  }

  when(io.resps.sink_e.valid) {
    w_grantack := true.B
  }

  when(io.resps.source_d.valid) {
    w_putwritten := true.B
  }

  when(io.resps.sink_c_ack.valid) {
    w_sinkcack := true.B
  }

  // Release MSHR
  val no_schedule = s_probeack && s_execute && s_grantack &&
    RegNext(s_wbselfdir && s_wbselftag && s_wbclientsdir && s_wbclientstag && meta_valid, true.B) &&
    s_writerelease && s_writeprobe &&
    s_triggerprefetch.getOrElse(true.B) &&
    s_prefetchack.getOrElse(true.B) &&
    s_transferput
  will_be_free := no_wait && no_schedule
  when(will_be_free) {
    meta_valid := false.B
    req_valid := false.B
    releaseThrough := false.B
    releaseDrop := false.B
    probeAckDataThrough := false.B
    probeAckDataDrop := false.B
    probeAckDataSave := false.B
    probe_helper_finish := false.B
    someClientHasProbeAckData := false.B
  }

  def Hold[T <: Data](x: T, l2Only: Boolean): T = {
    if(l2Only && cacheParams.name != "L2") {
      x
    } else {
      val reg = RegInit(0.U.asTypeOf(x))
      when(io.dirResult.valid){
        reg := x
      }.elsewhen(will_be_free){
        reg := 0.U
      }
      (x.asUInt | reg.asUInt).asTypeOf(x)
    }
  }

  def Keep[T <: Data](x: T): T = {
    val reg = RegInit(0.U.asTypeOf(x))
    when(io.dirResult.valid){
      reg := x
    }.elsewhen(will_be_free){
      reg := 0.U
    }
    reg.asUInt.asTypeOf(x)
  }

  io.status.bits.will_free := will_be_free

  // Alloc MSHR (alloc has higher priority than release)
  assert(RegNext(!req_valid || !io.alloc.valid, true.B)) // TODO: support fully-pipelined
  when(io.alloc.valid) {
    req_valid := true.B
    req := io.alloc.bits
    req.alias.foreach(_ := io.alloc.bits.alias.get)
    val clientBitOH = getClientBitOH(io.alloc.bits.source)
    // The highest bit of iam indicates that req comes from TL-UL node
    iam := Mux(clientBitOH === 0.U, Cat(1.U(1.W), 0.U(log2Up(clientBits).W)), OHToUInt(clientBitOH))
  }

  // Latched control signals for timing
  val cache_alias_latch = RegNext(cache_alias, false.B)
  val preferCache_latch = RegNext(preferCache, false.B)
  val transmit_from_other_client_latch = RegNext(transmit_from_other_client, false.B)

  // Status
  io.status.valid := req_valid
  io.status.bits.channel := req.channel
  io.status.bits.set := req.set
  io.status.bits.tag := req.tag
  io.status.bits.is_miss := !self_meta.hit
  io.status.bits.way := self_meta.way
  io.status.bits.way_reg := meta_reg.self.way  // used to ease timing issue
  io.status.bits.will_grant_data := req.fromA && od.opcode(0) && io.tasks.source_d.bits.useBypass
  io.status.bits.will_save_data := req.fromA && (preferCache_latch || self_meta.hit) && !acquirePermMiss
  io.status.bits.is_prefetch := req.isPrefetch.getOrElse(false.B)
  io.status.bits.blockB := true.B
  io.status.bits.reqSource := req.reqSource

  // B nest A
  // if we are waitting for probeack,
  // we should not let B req in (avoid multi-probe to client)
  io.status.bits.nestB := meta_valid &&
    (w_releaseack && w_probeacklast) && s_writeprobe && w_sinkcack &&
    (!w_grantfirst || (client_dir_conflict && !probe_helper_finish))
  io.status.bits.blockC := true.B
  // C nest B | C nest A
  io.status.bits.nestC := meta_valid &&
    w_releaseack &&
    (
      !w_probeackfirst || !w_grantfirst || (client_dir_conflict && !probe_helper_finish)
    )
  // C nest A (C -> A)
  io_is_nestedReleaseData := req.fromC && req_valid
  // B nest A (B -> A)
  io_is_nestedProbeAckData := req.fromB /*&& clients_hit*/ && req_valid
  io_probeHelperFinish := req.fromB && req.fromProbeHelper && no_schedule && no_wait
  io_metaValid := meta_valid || io.dirResult.valid
  io_reqDirty := req.dirty && req_valid

  // C nest A/B (A/B -> C)
  val nest_c_set_match = io_c_status.set === req.set
  val nest_c_tag_match = io_c_status.tag === req.tag
  val nest_c_way_match = io_c_status.way === self_meta.way

  val a_c_through = req.fromA && (
      nest_c_tag_match && !self_meta.hit && !nest_c_way_match ||
      !nest_c_tag_match && nest_c_way_match && (cache_alias_latch || (preferCache_latch && !acquirePermMiss) || Hold(self_meta.hit, false) || transmit_from_other_client_latch) ||
      nest_c_tag_match && nest_c_way_match && !self_meta.hit && a_do_release
    )

  val b_c_through = req.fromB && (nest_c_tag_match && !self_meta.hit || nest_c_way_match && self_meta.hit =/= nest_c_tag_match)

  io_c_status.releaseThrough := req_valid &&
    io_c_status.nestedReleaseData &&
    nest_c_set_match &&
    (a_c_through || b_c_through)

  // B nest A (A -> B)
  io_b_status.probeAckDataThrough := req_valid &&
    io_b_status.set === req.set && io_b_status.tag =/= req.tag &&
    io_b_status.way === self_meta.way &&
    io_b_status.nestedProbeAckData &&
    req.fromA && (preferCache_latch || self_meta.hit) && !acquirePermMiss

  val read_miss = !self_meta.hit || self_meta.state === INVALID

  val nestedReleaseDataSave = req_valid &&
    io_c_status.nestedReleaseData &&
    nest_c_set_match && nest_c_tag_match &&
    (nest_c_way_match && io_c_status.metaValid) &&
    ~a_c_through

  val nestedReleaseDirtyDataSave = nestedReleaseDataSave && io_c_status.reqDirty

  when(nestedReleaseDataSave) {
    nestC_save_flag := true.B
  }

  when(nestedReleaseDirtyDataSave) {
    nestC_saveDirty_flag := true.B
  }

  nestC_save := nestC_save_flag || nestedReleaseDataSave
  nestC_saveDirty := nestC_saveDirty_flag || nestedReleaseDirtyDataSave
}
