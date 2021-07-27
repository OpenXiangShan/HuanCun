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
  })

  io.bs_waddr.valid := false.B
  io.bs_waddr.bits := DontCare
  io.bs_wdata := DontCare

  val d = io.d
  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  val s3_ready = Wire(Bool())

  // stage0
  // TODO: check this
  val s0_needData = io.task.bits.fromA && (
    io.task.bits.opcode === TLMessages.GrantData ||
      io.task.bits.opcode === TLMessages.AccessAckData
  )

  // stage1
  val s1_req = RegEnable(io.task.bits, io.task.fire())
  val s1_valid = RegInit(false.B)
  val s1_counter = Reg(UInt(beatBits.W)) // how many beats have been sent
  val s1_beats = Reg(UInt(beatBits.W)) // total beats need to be sent
  val s1_r_done = Wire(Bool())
  val s1_needData = Reg(Bool())
  val s1_can_go = Wire(Bool())

  s1_can_go := s1_valid && s2_ready && ((io.bs_raddr.ready && s1_needData) || !s1_needData)

  io.bs_raddr.valid := s1_valid && s1_needData
  io.bs_raddr.bits.way := s1_req.way
  io.bs_raddr.bits.set := s1_req.set
  io.bs_raddr.bits.beat := (s1_req.off >> log2Up(beatBytes)).asUInt() | s1_counter
  io.bs_raddr.bits.write := false.B
  io.bs_raddr.bits.noop := false.B  // TODO: assign noop signal

  s1_r_done := s1_counter === s1_beats

  when(io.bs_raddr.fire()) {
    when(s1_r_done) {
      when(s2_ready) { s1_valid := false.B }
      s1_needData := false.B
    }.otherwise({
      s1_counter := s1_counter + 1.U
    })
  }

  // S0 -> S1
  when(io.task.fire()) {
    s1_valid := true.B
    s1_counter := 0.U
    s1_beats := Mux(s0_needData, UIntToOH1(io.task.bits.size, log2Up(blockBytes)) >> log2Up(beatBytes), 0.U)
    s1_needData := s0_needData
  }.otherwise {
    when(!s1_needData) {
      s1_valid := false.B
    }
  }

  s1_ready := !s1_valid || (s1_can_go && s1_r_done)
  io.task.ready := s1_ready

  // stage2
  val s2_req = RegEnable(s1_req, s1_can_go)
  val s2_valid = RegInit(false.B)
  val s2_can_go = Wire(Bool())

  s2_ready := !s2_valid || s3_ready
  s2_can_go := s2_valid && s3_ready
  when(s1_can_go) {
    s2_valid := true.B
  }.elsewhen(s2_can_go) {
    s2_valid := false.B
  }

  // stage3
  val s3_req = RegEnable(s2_req, s2_can_go)
  val s3_valid = RegInit(false.B)
  val s3_valid_d = RegInit(false.B)

  val queue = Module(new Queue(new DSData, 3, flow = true))
  queue.io.enq.valid := RegNext(RegNext(io.bs_raddr.fire(), false.B), false.B)
  queue.io.enq.bits := io.bs_rdata
  assert(!queue.io.enq.valid || queue.io.enq.ready)

  queue.io.deq.ready := d.ready

  val s3_rdata = queue.io.deq.bits.data

  when(s2_can_go) {
    s3_valid := true.B
  }.elsewhen(s3_valid && s3_ready) {
    s3_valid := false.B
  }

  val s3_acquire = s3_req.opcode === AcquireBlock || s3_req.opcode === AcquirePerm

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
}
