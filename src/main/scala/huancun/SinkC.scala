/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkC(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    val way = Input(UInt(wayBits.W))
    val alloc = DecoupledIO(new MSHRRequest)
    val resp = ValidIO(new SinkCResp)
    val task = Flipped(DecoupledIO(new SinkCReq))
    val bs_waddr = DecoupledIO(new DSAddress)
    val bs_wdata = Output(new DSData)
    val sourceD_r_hazard = Flipped(ValidIO(new SourceDHazard))
  })
  /*
      Release/ReleaseData
      ProbeAck/ProbeAckData
   */
  val releaseBuf = Reg(Vec(bufBlocks, Vec(blockBytes / beatBytes, UInt((beatBytes * 8).W))))
  val beatValids = RegInit(VecInit(Seq.fill(bufBlocks) { VecInit(Seq.fill(blockBytes / beatBytes)(false.B)) }))
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
  val w_done = (w_counter === ((blockBytes / beatBytes) - 1).U) && (io.bs_waddr.ready && !io.bs_waddr.bits.noop)

  val task_r = RegEnable(io.task.bits, io.task.fire())
  val busy_r = RegInit(false.B)
  val do_release = io.task.fire() || busy_r

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

  assert(!c.valid || (c.bits.size === log2Up(blockBytes).U && off === 0.U), "SinkC must receive aligned message!")

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

  if (cacheParams.enableDebug) {
    when(c.fire()) {
      when(isRelease) {
        printf("release: addr:[%x]\n", c.bits.address)
      }
      when(isReleaseData) {
        printf("release data: addr:[%x] data[%x]\n", c.bits.address, c.bits.data)
      }
    }
  }

  val insertIdxReg = RegEnable(insertIdx, c.fire() && isReleaseData && first)
  when(c.fire() && isReleaseData) {
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
  when(w_done && busy_r) { // release data write done
    bufValids(task_r.bufIdx) := false.B
    beatValids(task_r.bufIdx).foreach(_ := false.B)
  }

  when(io.bs_waddr.fire() && !io.bs_waddr.bits.noop) {
    w_counter := w_counter + 1.U
  }
  when(w_done) {
    w_counter := 0.U
  }

  val bs_w_task = Mux(busy_r, task_r, io.task.bits)
  val task_w_safe = !(io.sourceD_r_hazard.valid &&
    io.sourceD_r_hazard.bits.safe(io.task.bits.set, io.task.bits.way))

  val isProbeAckDataReg = RegEnable(isProbeAckData, false.B, io.c.fire() && first)

  val req_w_valid = io.task.fire() || busy_r // ReleaseData
  val resp_w_valid = io.c.valid && can_recv_resp && isProbeAckData || isProbeAckDataReg // ProbeAckData

  when(done) {
    isProbeAckDataReg := false.B
  }

  io.task.ready := !isProbeAckDataReg && !busy_r && task_w_safe // TODO: flow here

  io.bs_waddr.valid := req_w_valid || resp_w_valid
  io.bs_waddr.bits.way := Mux(req_w_valid, bs_w_task.way, io.way)
  io.bs_waddr.bits.set := Mux(req_w_valid, bs_w_task.set, set) // TODO: do we need io.set?
  io.bs_waddr.bits.beat := w_counter
  io.bs_waddr.bits.write := true.B
  io.bs_waddr.bits.noop := Mux(req_w_valid, !beatValids(bs_w_task.bufIdx)(w_counter), !c.valid)
  io.bs_wdata.data := Mux(req_w_valid, releaseBuf(bs_w_task.bufIdx)(w_counter), c.bits.data)

  io.resp.valid := c.valid && isResp && can_recv_resp
  io.resp.bits.hasData := hasData
  io.resp.bits.param := c.bits.param
  io.resp.bits.source := c.bits.source
  io.resp.bits.last := last
  io.resp.bits.set := set
}
