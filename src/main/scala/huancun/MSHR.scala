package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import TLMessages._
import TLPermissions._
import MetaData._

class MSHRTasks(implicit p: Parameters) extends HuanCunBundle {
  // inner
  val sink_a = DecoupledIO(new SinkAReq) // put
  val source_b = DecoupledIO(new SourceBReq) // probe
  val sink_c = DecoupledIO(new SinkCReq) // inner release
  val source_d = DecoupledIO(new SourceDReq) // grant & atomics
  // outer
  val source_a = DecoupledIO(new SourceAReq) // acquire
  val source_c = DecoupledIO(new SourceCReq) // outer release & probe ack
  val source_e = DecoupledIO(new SourceEReq) // grant ack
  // direcotry & tag write
  val dir_write = DecoupledIO(new DirWrite)
  val tag_write = DecoupledIO(new TagWrite)
}

class MSHRResps(implicit p: Parameters) extends HuanCunBundle {
  val sink_c = ValidIO(new SinkCResp)
  val sink_d = ValidIO(new SinkDResp)
  val sink_e = ValidIO(new SinkEResp)
}

class MSHR()(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val id = Input(UInt())
    val alloc = Flipped(ValidIO(new MSHRRequest))
    val status = ValidIO(new MSHRStatus)
    val tasks = new MSHRTasks
    val dirResult = Flipped(ValidIO(new DirResult))
    val resps = Flipped(new MSHRResps)
  })

  val req = Reg(new MSHRRequest)
  val req_valid = RegInit(false.B)
  val meta = Reg(new DirResult)
  val meta_valid = RegInit(false.B)

  // 1. Alloc MSHR
  assert(RegNext(!req_valid || !io.alloc.valid)) // TODO: support fully-pipelined
  when (io.alloc.valid) {
    req_valid := true.B
    req := io.alloc.bits
  }

  // 2. Get directory result
  assert(RegNext(!io.dirResult.valid || req_valid && !meta_valid))
  when (io.dirResult.valid) {
    meta_valid := true.B
    meta := io.dirResult.bits
  }

  // 3. Final meta to be written
  val new_meta = WireInit(meta)
  val req_acquire = req.opcode === AcquireBlock || req.opcode === AcquirePerm
  val gotT = RegInit(false.B) // L3 might return T even though L2 wants B // TODO
  val meta_no_client = !meta.clients.orR
  val probes_toN = RegInit(0.U(clientBits.W)) // TODO
  when (req.fromC) {
    // Release / ReleaseData
    new_meta.dirty := meta.dirty || req.opcode(0)
    new_meta.state := Mux(req.param === TtoB || req.param === TtoN, TIP, meta.state)
    new_meta.clients := meta.clients & ~Mux(isToN(req.param), getClientBitOH(req.source), 0.U)
    new_meta.hit := true.B
  }.elsewhen (req.fromB) {
    val nextState = Mux(isT(meta.state) && req.param === toT, meta.state,
      Mux(meta.state =/= INVALID && req.param =/= toN, BRANCH, INVALID))
    new_meta.dirty := req.param === toT && meta.dirty
    new_meta.state := nextState
    new_meta.clients := Mux(req.param === toN, 0.U, meta.clients)
    // TODO: if a TIP/TRUNK is probed to be BRANCH, do we need to probe clients to INVALID?
    new_meta.hit := false.B
  }.otherwise {
    // Acquire / Intent / Put / Get / Atomics
    new_meta.dirty := meta.hit && meta.dirty || !req.opcode(2) // Put / Atomics
    new_meta.state := Mux(needT(req.opcode, req.param),
                        Mux(req_acquire,
                          TRUNK, // Acquire (NtoT/BtoT)
                          TIP), // Intent (PrefetchWrite) / Put / Atomics
                        Mux(!meta.hit, // The rest are Acquire (NtoB) / Intent (PrefetchRead) / Get
                          // If tag miss, new state depends on what L3 grants
                          Mux(gotT, Mux(req_acquire, TRUNK, TIP), BRANCH),
                          // If tag hit, 
                          MuxLookup(meta.state, BRANCH, Seq(
                            INVALID -> BRANCH,
                            BRANCH  -> BRANCH,
                            TRUNK   -> TIP,
                            TIP     -> Mux(meta_no_client && req_acquire, TRUNK, TIP)
                          ))))
    new_meta.clients := Mux(meta.hit, meta.clients & ~probes_toN, 0.U) | Mux(req_acquire, getClientBitOH(req.source), 0.U)
    new_meta.hit := true.B
  }
  assert(RegNext(!meta_valid || !req.fromC || meta.hit)) // Release should always hit


  // 4. Set tasks to be scheduled and resps to wait for
  val s_acquire        = RegInit(true.B)
  val s_rprobe         = RegInit(true.B)
  val s_pprobe         = RegInit(true.B)
  val s_release        = RegInit(true.B)
  val s_probeack       = RegInit(true.B)
  val s_grant          = RegInit(true.B)
  val s_grantack       = RegInit(true.B)
  val w_rprobeackfirst = RegInit(true.B)
  val w_rprobeacklast  = RegInit(true.B)
  val w_pprobeackfirst = RegInit(true.B)
  val w_pprobeacklast  = RegInit(true.B)
  val w_grantfirst     = RegInit(true.B)
  val w_grantlast      = RegInit(true.B)
  val w_releaseack     = RegInit(true.B)
  val w_grantack       = RegInit(true.B)

  when (io.alloc.valid) {
    s_acquire        := true.B
    s_rprobe         := true.B
    s_pprobe         := true.B
    s_release        := true.B
    s_probeack       := true.B
    s_grant          := true.B
    s_grantack       := true.B
    w_rprobeackfirst := true.B
    w_rprobeacklast  := true.B
    w_pprobeackfirst := true.B
    w_pprobeacklast  := true.B
    w_grantfirst     := true.B
    w_grantlast      := true.B
    w_releaseack     := true.B
    w_grantack       := true.B
  }


  // Status
  io.status.valid := req_valid
  io.status.bits.set := req.set
  io.status.bits.tag := req.tag


}
