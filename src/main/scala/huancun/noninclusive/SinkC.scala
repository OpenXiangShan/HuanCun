package huancun.noninclusive

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.{TLBundleC, TLMessages}
import huancun._
import utility.MemReqSource

class SinkC(implicit p: Parameters) extends BaseSinkC {

  val beats = blockBytes / beatBytes
  val buffer = Reg(Vec(sinkCbufBlocks, Vec(beats, UInt((beatBytes * 8).W))))
  val bufferTag = Reg(Vec(sinkCbufBlocks, UInt(tagBits.W)))
  val bufferSet = Reg(Vec(sinkCbufBlocks, UInt(setBits.W)))
  val bufferSetVals = RegInit(VecInit(Seq.fill(sinkCbufBlocks)(false.B)))
  val beatValsSave = RegInit(VecInit(Seq.fill(sinkCbufBlocks) {
    VecInit(Seq.fill(beats) { false.B })
  }))
  val beatValsThrough = RegInit(VecInit(Seq.fill(sinkCbufBlocks) {
    VecInit(Seq.fill(beats) { false.B })
  }))
  val beatVals = VecInit(Seq.fill(sinkCbufBlocks) {
    VecInit(Seq.fill(beats) { false.B })
  })
  val beatValsTimer = RegInit(VecInit(Seq.fill(sinkCbufBlocks)(0.U(16.W))))
  beatVals.zipWithIndex.map {
    case (b, i) =>
      b.zip(beatValsSave(i).zip(beatValsThrough(i))).map {
        case (a, (s, t)) =>
          a := s || t
      }
  }
  beatValsTimer.map { b =>
    when(b =/= 0.U) {
      b := b + 1.U;
      assert(b < 20000.U, "Buffer leak in SinkC")
    }
  }
  val bufVals = VecInit(beatVals.map(_.asUInt.orR)).asUInt
  val full = bufVals.andR
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
  val insertIdxReg = RegEnable(insertIdx, c.fire && first)
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
  io.alloc.bits.mask := 0.U // DontCare
  io.alloc.bits.bufIdx := insertIdx
  io.alloc.bits.needHint.foreach(_ := false.B)
  io.alloc.bits.isPrefetch.foreach(_ := false.B)
  io.alloc.bits.isBop.foreach(_ := false.B)
  io.alloc.bits.alias.foreach(_ := 0.U)
  io.alloc.bits.preferCache := true.B
  io.alloc.bits.isHit := true.B
  io.alloc.bits.dirty := c.bits.echo.lift(DirtyKey).getOrElse(true.B)
  io.alloc.bits.fromProbeHelper := false.B
  io.alloc.bits.fromCmoHelper := false.B
  io.alloc.bits.needProbeAckData.foreach(_ := false.B)
  io.alloc.bits.reqSource := MemReqSource.NoWhere.id.U // Ignore
  assert(!io.alloc.fire || c.fire && first, "alloc fire, but c channel not fire!")

  io.resp.valid := c.valid && (!noSpace || !first) && isResp
  io.resp.bits.hasData := hasData
  io.resp.bits.param := c.bits.param
  io.resp.bits.source := c.bits.source
  io.resp.bits.last := last
  io.resp.bits.set := set
  io.resp.bits.bufIdx := Mux(first, insertIdx, insertIdxReg)

  val task = io.task.bits
  val task_r = RegEnable(io.task.bits, io.task.fire)
  val busy = RegInit(false.B) // busy also serve as task_r.valid
  val setMatchVec = RegInit(0.U(sinkCbufBlocks.W))

  // buffer write
  when(c.fire && hasData) {
    when(first) {
      buffer(insertIdx)(count) := c.bits.data
      beatValsSave(insertIdx)(count) := true.B
      beatValsThrough(insertIdx)(count) := true.B
      beatValsTimer(insertIdx) := 1.U
      when(isProbeAckData) {
        bufferSetVals(insertIdx) := true.B
        bufferSet(insertIdx) := set
        bufferTag(insertIdx) := tag
      }
    }.otherwise({
      buffer(insertIdxReg)(count) := c.bits.data
      beatValsSave(insertIdxReg)(count) := true.B
      beatValsThrough(insertIdxReg)(count) := true.B
    })
  }

  when(c.fire && first && isProbeAckData) {
    setMatchVec := Cat((bufferSetVals.zipWithIndex).zip(bufferSet.zip(bufferTag)).map{
      case ((v, i), (s, t)) =>
        Mux(busy && i.U === task_r.bufIdx, false.B, (t === tag) && (s === set) && v) // do not clean buffer of ongoing task
    }.reverse)
  }
  when(setMatchVec.orR) {
    assert(PopCount(setMatchVec) === 1.U, "SinkC: ProbeAckData cleaner detects multiple data")
    val bufIdx = OHToUInt(setMatchVec)
    beatValsSave(bufIdx).foreach(_ := false.B)
    beatValsThrough(bufIdx).foreach(_ := false.B)
    bufferSetVals(bufIdx) := false.B
    beatValsTimer(bufIdx) := 0.U
    setMatchVec := 0.U
  }

  val w_counter_save = RegInit(0.U(beatBits.W))
  val w_counter_through = RegInit(0.U(beatBits.W))
  val task_w_safe = !(io.sourceD_r_hazard.valid &&
    io.sourceD_r_hazard.bits.safe(task.set, task.way))

  val w_save_done_r = RegInit(false.B)
  val w_through_done_r = RegInit(false.B)

  io.task.ready := !busy && task_w_safe
  when(io.task.fire) {
    assert(task.save || task.drop || task.release, "Null Task!")
    busy := true.B
    when(!task.save && !task.drop) {
      beatValsSave(task.bufIdx).foreach(_ := false.B)
      w_save_done_r := true.B
    }
    when(!task.release && !task.drop) {
      beatValsThrough(task.bufIdx).foreach(_ := false.B)
      w_through_done_r := true.B
    }
  }

  io.bs_waddr.valid := busy && task_r.save && !w_save_done_r
  io.bs_waddr.bits.way := task_r.way
  io.bs_waddr.bits.set := task_r.set
  io.bs_waddr.bits.beat := w_counter_save
  io.bs_waddr.bits.write := true.B
  io.bs_waddr.bits.noop := !beatValsSave(task_r.bufIdx)(w_counter_save)
  io.bs_wdata.data := buffer(task_r.bufIdx)(w_counter_save)
  io.bs_wdata.corrupt := false.B

  io.release.valid := busy && task_r.release && beatValsThrough(task_r.bufIdx)(w_counter_through) && !w_through_done_r
  io.release.bits.address := Cat(task_r.tag, task_r.set, 0.U(offsetBits.W))
  io.release.bits.data := buffer(task_r.bufIdx)(w_counter_through)
  io.release.bits.opcode := task_r.opcode
  io.release.bits.param := task_r.param
  io.release.bits.source := task_r.source
  io.release.bits.size := offsetBits.U
  io.release.bits.corrupt := false.B
  io.release.bits.user.lift(PreferCacheKey).foreach(_ := true.B)
  io.release.bits.user.lift(utility.ReqSourceKey).foreach(_ := MemReqSource.NoWhere.id.U)
  io.release.bits.echo.lift(DirtyKey).foreach(_ := task_r.dirty)

  val w_fire_save = io.bs_waddr.fire && !io.bs_waddr.bits.noop
  val w_fire_through = io.release.fire
  val w_fire = w_fire_save || w_fire_through
  when(w_fire_save) { w_counter_save := w_counter_save + 1.U }
  when(w_fire_through) { w_counter_through := w_counter_through + 1.U }

  when((w_counter_save === (beats-1).U) && w_fire_save) { w_save_done_r := true.B }
  when((w_counter_through === (beats-1).U) && w_fire_through) { w_through_done_r := true.B }

  val w_done =
      ((w_counter_save === (beats-1).U) && (w_counter_through === (beats-1).U) && w_fire_save && w_fire_through) ||
      ((w_counter_save === (beats-1).U) && w_through_done_r && w_fire_save) ||
      (w_save_done_r && (w_counter_through === (beats-1).U) && w_fire_through)

  when(w_done || busy && task_r.drop) {
    w_counter_save := 0.U
    w_counter_through := 0.U
    busy := false.B
    beatValsSave(task_r.bufIdx).foreach(_ := false.B)
    beatValsThrough(task_r.bufIdx).foreach(_ := false.B)
    bufferSetVals(task_r.bufIdx) := false.B
    beatValsTimer(task_r.bufIdx) := 0.U
    w_save_done_r := false.B
    w_through_done_r := false.B
  }

  io.taskack.bits.sink := RegNext(task_r.source)
  io.taskack.valid := RegNext(busy && (w_done || busy && task_r.drop), false.B)
}
