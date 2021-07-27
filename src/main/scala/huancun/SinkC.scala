package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkC(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    val way = Input(UInt(wayBits.W))
    val set = Input(UInt(setBits.W))
    val alloc = DecoupledIO(new MSHRRequest)
    val resp = ValidIO(new SinkCResp)
    val task = Flipped(DecoupledIO(new SinkCReq))
    val bs_waddr = DecoupledIO(new DSAddress)
    val bs_wdata = Output(new DSData)
  })
  /*
      Release/ReleaseData
      ProbeAck/ProbeAckData
   */
  val releaseBuf = Reg(Vec(bufBlocks, Vec(blockBytes / beatBytes, UInt((beatBytes * 8).W))))
  val bufValids = RegInit(VecInit(Seq.fill(bufBlocks)(false.B)))
  val bufFull = Cat(bufValids).andR()
  val insertIdx = PriorityEncoder(bufValids.map(b => !b))

  val c = io.c
  val isRelease = c.bits.opcode === TLMessages.Release
  val isReleaseData = c.bits.opcode === TLMessages.ReleaseData
  val isProbeAck = c.bits.opcode === TLMessages.ProbeAck
  val isProbeAckData = c.bits.opcode === TLMessages.ProbeAckData
  val isResp = isProbeAck || isProbeAckData
  val isReq = isRelease || isReleaseData
  val (first, last, done, count) = edgeIn.count(c)
  val hasData = edgeIn.hasData(c.bits)

  // bankedstore w counter
  val w_counter = RegInit(0.U(beatBits.W))
  val w_done = (w_counter === ((blockBytes / beatBytes) - 1).U) && io.bs_waddr.ready

  val task_r = RegEnable(io.task.bits, io.task.fire())
  val busy_r = RegInit(false.B)
  val do_release = io.task.valid || busy_r

  io.task.ready := !busy_r // TODO: flow here
  when(w_done) {
    busy_r := false.B
  }.elsewhen(io.task.fire()) {
    busy_r := true.B
  }

  val noSpace = hasData && bufFull

  val can_recv_req = Mux(first, io.alloc.ready && !noSpace, !noSpace)
  val can_recv_resp = Mux(do_release, false.B, !hasData || io.bs_waddr.ready)

  c.ready := Mux(isResp, can_recv_resp, can_recv_req)

  val (tag, set, off) = parseAddress(c.bits.address)

  io.alloc.valid := c.valid && can_recv_req && isReq && first
  io.alloc.bits.channel := "b100".U
  io.alloc.bits.opcode := c.bits.opcode
  io.alloc.bits.param := c.bits.param
  io.alloc.bits.size := c.bits.size
  io.alloc.bits.source := c.bits.source
  io.alloc.bits.tag := tag
  io.alloc.bits.set := set
  io.alloc.bits.off := off
  io.alloc.bits.bufIdx := insertIdx

  val insertIdxReg = RegEnable(insertIdx, c.fire() && isReleaseData && first)
  when(c.fire() && isReleaseData) {
    when(first) {
      releaseBuf(insertIdx)(count) := c.bits.data
    }.otherwise({
      releaseBuf(insertIdxReg)(count) := c.bits.data
    })
    when(last) {
      bufValids(insertIdxReg) := true.B
    }
  }
  when(w_done && busy_r) { // release data write done
    bufValids(task_r.bufIdx) := false.B
  }

  when(io.bs_waddr.fire()) { w_counter := w_counter + 1.U }
  when(w_done) { w_counter := 0.U }

  val bs_w_task = Mux(busy_r, task_r, io.task.bits)
  val req_w_valid = io.task.fire() || busy_r
  val resp_w_valid = c.valid && isProbeAckData && can_recv_req
  io.bs_waddr.valid := req_w_valid || resp_w_valid
  io.bs_waddr.bits.way := Mux(req_w_valid, bs_w_task.way, io.way)
  io.bs_waddr.bits.set := Mux(req_w_valid, bs_w_task.set, io.set) // TODO: do we need io.set?
  io.bs_waddr.bits.beat := w_counter
  io.bs_waddr.bits.write := true.B
  io.bs_waddr.bits.noop := false.B  // TODO: assign noop signal
  io.bs_wdata.data := Mux(req_w_valid, releaseBuf(bs_w_task.bufIdx)(w_counter), c.bits.data)

  io.resp.valid := c.valid && isResp && can_recv_resp
  io.resp.bits.hasData := hasData
  io.resp.bits.param := c.bits.param
  io.resp.bits.source := c.bits.source
  io.resp.bits.last := last
  io.resp.bits.set := set
}
