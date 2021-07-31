package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntToOH1
import freechips.rocketchip.rocket.DecodeLogic
import freechips.rocketchip.tilelink.TLMessages.{AcquireBlock, AcquirePerm}

class SourceD(implicit p: Parameters) extends HuanCunModule {
  /*
      Message         Operation       Channel          Data
      -------------|---------------|------------|--------------
      AccessAck       Put                A           Y/N TODO: put may be done in SinkA?
      AccessAckData   Get/Atomic         A            Y
      HintAck         Hint               A            N
      Grant           Acquire            A            N
      GrantData       Acquire            A            Y
      ReleaseAck      Release            C            N
   */
  val io = IO(new Bundle() {
    val d = DecoupledIO(new TLBundleD(edgeIn.bundle))
    val task = Flipped(DecoupledIO(new SourceDReq))
    val bs_raddr = DecoupledIO(new DSAddress)
    val bs_rdata = Input(new DSData)
    val bs_waddr = DecoupledIO(new DSAddress)
    val bs_wdata = Output(new DSData)
    // data hazards
    val sourceD_r_hazard = ValidIO(new SourceDHazard)
  })

  io.bs_waddr.valid := false.B
  io.bs_waddr.bits := DontCare
  io.bs_wdata := DontCare

  val d = io.d
  val s1_valid = Wire(Bool())
  val s2_valid, s2_ready = Wire(Bool())
  val s3_valid, s3_ready = Wire(Bool())

  // stage1
  val busy = RegInit(false.B)
  val s1_block_r = RegInit(false.B)
  val s1_req_reg = RegEnable(io.task.bits, io.task.fire())
  val s1_req = Mux(busy, s1_req_reg, io.task.bits)
  val s1_needData = s1_req.fromA && (
    s1_req.opcode === TLMessages.GrantData ||
      s1_req.opcode === TLMessages.AccessAckData
  )
  val s1_counter = RegInit(0.U(beatBits.W)) // how many beats have been sent
  val s1_total_beats = Mux(s1_needData, totalBeats(s1_req.size), 0.U(beatBits.W))
  val s1_beat = startBeat(s1_req.off) | s1_counter
  val s1_valid_r = (busy || io.task.valid) && s1_needData && !s1_block_r
  val s1_last = s1_beat === s1_total_beats

  io.bs_raddr.valid := s1_valid_r
  io.bs_raddr.bits.way := s1_req.way
  io.bs_raddr.bits.set := s1_req.set
  io.bs_raddr.bits.beat := s1_beat // TODO: support unaligned address
  io.bs_raddr.bits.write := false.B
  io.bs_raddr.bits.noop := false.B

  when(io.task.fire()) {
    busy := true.B
  }
  when(io.bs_raddr.fire()) {
    s1_block_r := true.B
  }
  when(s1_valid && s2_ready) {
    s1_counter := s1_counter + 1.U
    s1_block_r := false.B
    when(s1_last) {
      s1_counter := 0.U
      busy := false.B
    }
  }
  io.task.ready := !busy
  s1_valid := (busy || io.task.valid) && (!s1_valid_r || io.bs_raddr.ready)

  // stage2
  val s2_latch = s1_valid && s2_ready
  val s2_req = RegEnable(s1_req, s2_latch)
  val s2_needData = RegEnable(s1_needData, s2_latch)
  val s2_full = RegInit(false.B)
  val s2_acquire = s2_req.opcode === AcquireBlock || s2_req.opcode === AcquirePerm

  when(s2_valid && s3_ready) { s2_full := false.B }
  when(s2_latch) { s2_full := true.B }

  s2_valid := s2_full
  s2_ready := !s2_full || s3_ready

  // stage3
  val s3_latch = s2_valid && s3_ready
  val s3_full = RegInit(false.B)
  val s3_needData = RegInit(false.B)
  val s3_req = RegEnable(s2_req, s3_latch)
  val s3_acquire = RegEnable(s2_acquire, s3_latch)

  val queue = Module(new Queue(new DSData, 3, flow = true))
  queue.io.enq.valid := RegNext(RegNext(io.bs_raddr.fire(), false.B), false.B)
  queue.io.enq.bits := io.bs_rdata
  assert(!queue.io.enq.valid || queue.io.enq.ready)
  queue.io.deq.ready := d.ready && s3_needData && s3_valid

  when(d.ready) {
    s3_full := false.B
    s3_needData := false.B
  }
  when(s3_latch) {
    s3_full := true.B
    s3_needData := s2_needData
  }

  val s3_rdata = queue.io.deq.bits.data
  d.valid := s3_valid
  d.bits.opcode := s3_req.opcode
  d.bits.param := Mux(s3_req.fromA && s3_acquire, s3_req.param, 0.U)
  d.bits.sink := s3_req.sinkId
  d.bits.size := s3_req.size
  d.bits.source := s3_req.sourceId
  d.bits.denied := false.B
  d.bits.data := s3_rdata
  d.bits.corrupt := false.B

  s3_ready := d.ready
  s3_valid := s3_full

  io.sourceD_r_hazard.valid := !busy && s1_needData
  io.sourceD_r_hazard.bits.set := s1_req.set
  io.sourceD_r_hazard.bits.way := s1_req.way
}
