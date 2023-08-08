package huancun.inclusive

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import huancun._

class SinkC(implicit p: Parameters) extends BaseSinkC {
  /*
      Release/ReleaseData
      ProbeAck/ProbeAckData
   */
  val releaseBuf = Reg(Vec(bufBlocks, Vec(blockBytes / beatBytes, UInt((beatBytes * 8).W))))
  val beatValids = RegInit(VecInit(Seq.fill(bufBlocks) { VecInit(Seq.fill(blockBytes / beatBytes)(false.B)) }))
  val bufValids = RegInit(VecInit(Seq.fill(bufBlocks)(false.B)))
  val bufFull = Cat(bufValids).andR
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
  val w_done = (w_counter === ((blockBytes / beatBytes) - 1).U) && (io.bs_waddr.ready && !io.bs_waddr.bits.noop)

  val task_r = RegEnable(io.task.bits, io.task.fire)
  val busy_r = RegInit(false.B)
  val do_release = io.task.fire || busy_r

  when(w_done) {
    busy_r := false.B
  }.elsewhen(io.task.fire) {
    busy_r := true.B
  }

  val noSpace = hasData && bufFull

  val can_recv_req = Mux(first, io.alloc.ready && !noSpace, !noSpace)
  val can_recv_resp = Mux(do_release, false.B, !hasData || io.bs_waddr.ready)

  c.ready := Mux(isResp, can_recv_resp, can_recv_req)

  val (tag, set, off) = parseAddress(c.bits.address)

  assert(!c.valid || (c.bits.size === log2Up(blockBytes).U && off === 0.U), "SinkC must receive aligned message!")

  io.alloc.valid := c.valid && !noSpace && isReq && first
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
  io.alloc.bits.alias.foreach(_ := 0.U)
  io.alloc.bits.preferCache := true.B
  io.alloc.bits.dirty := c.bits.echo.lift(DirtyKey).getOrElse(true.B)
  io.alloc.bits.fromProbeHelper := false.B
  io.alloc.bits.fromCmoHelper := false.B

  if (cacheParams.enableDebug) {
    when(c.fire) {
      when(isRelease) {
        printf("release: addr:[%x]\n", c.bits.address)
      }
      when(isReleaseData) {
        printf("release data: addr:[%x] data[%x]\n", c.bits.address, c.bits.data)
      }
    }
  }

  val insertIdxReg = RegEnable(insertIdx, c.fire && isReleaseData && first)
  when(c.fire && isReleaseData) {
    when(first) {
      releaseBuf(insertIdx)(count) := c.bits.data
      beatValids(insertIdx)(count) := true.B
    }.otherwise({
      releaseBuf(insertIdxReg)(count) := c.bits.data
      beatValids(insertIdxReg)(count) := true.B
    })
    when(last) {
      bufValids(insertIdxReg) := true.B
    }
  }
  when((w_done || task_r.drop) && busy_r) { // release data write done
    bufValids(task_r.bufIdx) := false.B
    beatValids(task_r.bufIdx).foreach(_ := false.B)
  }

  when(io.bs_waddr.fire && !io.bs_waddr.bits.noop || io.release.fire) {
    w_counter := w_counter + 1.U
  }
  when(w_done) {
    w_counter := 0.U
  }

  val bs_w_task = Mux(busy_r, task_r, io.task.bits)
  val task_w_safe = !(io.sourceD_r_hazard.valid &&
    io.sourceD_r_hazard.bits.safe(io.task.bits.set, io.task.bits.way))

  val isProbeAckDataReg = RegEnable(isProbeAckData, io.c.fire)
  val resp_way = Mux(io.c.valid, io.way, RegEnable(io.way, io.c.fire))
  val resp_set = Mux(io.c.valid, set, RegEnable(set, io.c.fire))
  val resp_w_valid = (io.c.valid && !do_release && isProbeAckData) || (!first && isProbeAckDataReg) // ProbeAckData
  val req_w_valid =
    (io.task.fire && io.task.bits.save) || (busy_r && task_r.save)

  io.task.ready := first && !busy_r && task_w_safe // TODO: flow here

  io.bs_waddr.valid := req_w_valid || resp_w_valid
  io.bs_waddr.bits.way := Mux(do_release, bs_w_task.way, resp_way)
  io.bs_waddr.bits.set := Mux(do_release, bs_w_task.set, resp_set) // TODO: do we need io.set?
  io.bs_waddr.bits.beat := w_counter
  io.bs_waddr.bits.write := true.B
  io.bs_waddr.bits.noop := Mux(do_release, !beatValids(bs_w_task.bufIdx)(w_counter), !c.valid)
  io.bs_wdata.data := Mux(do_release, releaseBuf(bs_w_task.bufIdx)(w_counter), c.bits.data)
  io.bs_wdata.corrupt := false.B

  io.release.valid := busy_r && task_r.release
  io.release.bits.address := Cat(task_r.tag, task_r.set, task_r.off)
  io.release.bits.data := releaseBuf(task_r.bufIdx)(w_counter)
  io.release.bits.opcode := task_r.opcode
  io.release.bits.param := task_r.param
  io.release.bits.source := task_r.source
  io.release.bits.size := task_r.size
  io.release.bits.corrupt := false.B
  io.release.bits.user.lift(PreferCacheKey).foreach(_ := true.B)
  io.release.bits.echo.lift(DirtyKey).foreach(_ := true.B) // this is useless

  io.resp.valid := c.valid && isResp && can_recv_resp
  io.resp.bits.hasData := hasData
  io.resp.bits.param := c.bits.param
  io.resp.bits.source := c.bits.source
  io.resp.bits.last := last
  io.resp.bits.set := set
  io.resp.bits.bufIdx := DontCare // not used in inclusive cache
}
