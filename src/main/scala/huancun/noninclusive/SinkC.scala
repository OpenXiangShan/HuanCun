package huancun.noninclusive

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink.{TLBundleC, TLMessages}
import huancun._

class SinkC(implicit p: Parameters) extends BaseSinkC {

  val beats = blockBytes / beatBytes
  val buffer = Reg(Vec(bufBlocks, Vec(beats, UInt((beatBytes * 8).W))))
  val beatVals = RegInit(VecInit(Seq.fill(bufBlocks) {
    VecInit(Seq.fill(beats) { false.B })
  }))
  val bufVals = VecInit(beatVals.map(_.asUInt().orR())).asUInt()
  val full = bufVals.andR()
  val c = io.c
  val isRelease = c.bits.opcode === TLMessages.Release
  val isReleaseData = c.bits.opcode === TLMessages.ReleaseData
  val isProbeAck = c.bits.opcode === TLMessages.ProbeAck
  val isProbeAckData = c.bits.opcode === TLMessages.ProbeAckData
  val isResp = isProbeAck || isProbeAckData
  val isReq = isRelease || isReleaseData
  val (first, last, done, count) = edgeIn.count(c)
  val hasData = edgeIn.hasData(c.bits)
  val noSpace = full && hasData
  val insertIdx = PriorityEncoder(~bufVals)
  val insertIdxReg = RegEnable(insertIdx, c.fire() && first)
  val (tag, set, off) = parseAddress(c.bits.address)

  c.ready := Mux(first, !noSpace && !(isReq && !io.alloc.ready), true.B)

  // alloc.ready depends on alloc.valid
  io.alloc.valid := c.valid && isReq && first && !noSpace
  io.alloc.bits.channel := "b100".U
  io.alloc.bits.opcode := c.bits.opcode
  io.alloc.bits.param := c.bits.param
  io.alloc.bits.size := c.bits.size
  io.alloc.bits.source := c.bits.source
  io.alloc.bits.tag := tag
  io.alloc.bits.set := set
  io.alloc.bits.off := off
  io.alloc.bits.bufIdx := insertIdx
  io.alloc.bits.needHint.foreach(_ := false.B)
  io.alloc.bits.alias.foreach(_ := 0.U)
  io.alloc.bits.preferCache := true.B
  io.alloc.bits.dirty := c.bits.echo.lift(DirtyKey).getOrElse(true.B)
  io.alloc.bits.fromProbeHelper := false.B
  assert(!io.alloc.fire() || c.fire() && first, "alloc fire, but c channel not fire!")

  io.resp.valid := c.fire() && isResp
  io.resp.bits.hasData := hasData
  io.resp.bits.param := c.bits.param
  io.resp.bits.source := c.bits.source
  io.resp.bits.last := last
  io.resp.bits.set := set
  io.resp.bits.bufIdx := Mux(first, insertIdx, insertIdxReg)

  // buffer write
  when(c.fire() && hasData) {
    when(first) {
      buffer(insertIdx)(count) := c.bits.data
      beatVals(insertIdx)(count) := true.B
    }.otherwise({
      buffer(insertIdxReg)(count) := c.bits.data
      beatVals(insertIdxReg)(count) := true.B
    })
  }

  val task_r = RegEnable(io.task.bits, io.task.fire())
  val busy = RegInit(false.B)
  val task_v = io.task.fire() || busy
  val task = Mux(busy, task_r, io.task.bits)
  val w_counter = RegInit(0.U(beatBits.W))
  val task_w_safe = !(io.sourceD_r_hazard.valid &&
    io.sourceD_r_hazard.bits.safe(io.task.bits.set, io.task.bits.way))

  io.task.ready := !busy && task_w_safe
  when(io.task.fire()) { busy := true.B }

  io.bs_waddr.valid := task_v && task.save
  io.bs_waddr.bits.way := task.way
  io.bs_waddr.bits.set := task.set
  io.bs_waddr.bits.beat := w_counter
  io.bs_waddr.bits.write := true.B
  io.bs_waddr.bits.noop := !beatVals(task.bufIdx)(w_counter)
  io.bs_wdata.data := buffer(task.bufIdx)(w_counter)

  io.release.valid := busy && task_r.release
  io.release.bits.address := Cat(task_r.tag, task_r.set, task_r.off)
  io.release.bits.data := buffer(task_r.bufIdx)(w_counter)
  io.release.bits.opcode := task_r.opcode
  io.release.bits.param := task_r.param
  io.release.bits.source := task_r.source
  io.release.bits.size := task_r.size
  io.release.bits.corrupt := false.B
  io.release.bits.user.lift(PreferCacheKey).foreach(_ := true.B)
  io.release.bits.echo.lift(DirtyKey).foreach(_ := task_r.dirty)

  val w_fire = io.bs_waddr.fire() && !io.bs_waddr.bits.noop || io.release.fire()
  when(w_fire) {
    w_counter := w_counter + 1.U
  }
  val w_done = (w_counter === (beats - 1).U) && w_fire
  when(w_done || busy && task_r.drop) {
    w_counter := 0.U
    busy := false.B
    beatVals(task_r.bufIdx).foreach(_ := false.B)
  }
}
