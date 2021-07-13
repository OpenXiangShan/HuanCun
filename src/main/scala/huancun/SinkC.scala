package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkC(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = Flipped(DecoupledIO(new TLBundleC(edge.bundle)))
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
  val releaseBuf = Mem(bufBlocks, Vec(blockBytes / beatBytes, UInt(beatBits.W)))
  val bufValids = RegInit(VecInit(Seq.fill(bufBlocks)(false.B)))
  val bufFull = Cat(bufValids).andR()
  val insertIdx = PriorityEncoder(bufValids.map(b => !b))

  val c = io.c
  val isRelease = c.bits.opcode === TLMessages.Release
  val isReleaseData = c.bits.opcode === TLMessages.ReleaseData
  val (first, last, done, count) = edge.count(c)
  val hasData = edge.hasData(c.bits)

  assert(!c.fire() || c.fire() && (isRelease || isReleaseData), "Unknown sink c opcode: %d", c.bits.opcode)

  val (tag, set, off) = parseAddress(c.bits.address)

  val noSpace = hasData && bufFull

  c.ready := Mux(first, io.alloc.ready && !noSpace, !noSpace)

  io.alloc.valid := c.fire() && (isRelease || isReleaseData) && first
  io.alloc.bits.channel := "b100".U
  io.alloc.bits.opcode := c.bits.opcode
  io.alloc.bits.param := c.bits.param
  io.alloc.bits.size := c.bits.size
  io.alloc.bits.source := c.bits.source
  io.alloc.bits.tag := tag
  io.alloc.bits.set := set
  io.alloc.bits.off := off
  io.alloc.bits.bufIdx := insertIdx

  when(c.fire() && isReleaseData) {
    releaseBuf(insertIdx)(count) := c.bits.data
    bufValids(insertIdx) := true.B
  }

  val task_r = RegEnable(io.task.bits, io.task.fire())
  val busy_r = RegInit(false.B)
  val w_counter = RegInit(0.U(beatBits.W))
  val w_done = (w_counter === ((blockBytes / beatBytes) - 1).U) && io.bs_waddr.ready

  io.task.ready := !busy_r // TODO: flow here
  when(io.task.fire()) {
    busy_r := true.B
  }.elsewhen(w_done) {
    busy_r := false.B
    w_counter := 0.U
    bufValids(task_r.bufIdx) := false.B
  }

  when(io.bs_waddr.fire()) { w_counter := w_counter + 1.U }

  val bs_w_task = Mux(busy_r, task_r, io.task.bits)
  io.bs_waddr.valid := io.task.fire() || busy_r
  io.bs_waddr.bits.way := bs_w_task.way
  io.bs_waddr.bits.set := bs_w_task.set
  io.bs_waddr.bits.beat := w_counter
  io.bs_waddr.bits.write := true.B
  io.bs_wdata.data := releaseBuf(bs_w_task.bufIdx)(w_counter)

  // now we don't support probeAck
  io.resp.valid := false.B
  io.resp.bits := DontCare
}
