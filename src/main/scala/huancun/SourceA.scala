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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink._
import huancun.utils.HoldUnless
import utility.MemReqSource

class SourceA(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val a = DecoupledIO(new TLBundleA(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceAReq))
    // putbuffer interface
    val pb_pop = DecoupledIO(new PutBufferPop)
    val pb_beat = Input(new PutBufferBeatEntry)
  })

  val a = io.a
  val a_acquire = Wire(a.cloneType)
  val a_put = Wire(a.cloneType)
  val beats = blockBytes / beatBytes
  val busy = RegInit(false.B)

  // io.task.ready := Mux(io.task.bits.putData, !busy, a_acquire.ready)  // TODO: not ready until all beats of Put fire
  io.task.ready := a_acquire.ready && !busy

  when (io.task.fire && io.task.bits.putData) {
    busy := true.B
  }

  a_acquire.bits.opcode := io.task.bits.opcode
  a_acquire.bits.param := io.task.bits.param
  a_acquire.bits.size := offsetBits.U
  a_acquire.bits.source := io.task.bits.source
  a_acquire.bits.address := Cat(io.task.bits.tag, io.task.bits.set, 0.U(offsetBits.W))
  a_acquire.bits.mask := Fill(edgeOut.manager.beatBytes, 1.U(1.W))
  a_acquire.bits.data := DontCare
  a_acquire.bits.corrupt := false.B
  a_acquire.bits.user.lift(PreferCacheKey).foreach(_ := false.B)
  a_acquire.bits.user.lift(utility.ReqSourceKey).foreach(_ := io.task.bits.reqSource)
  a_acquire.bits.echo.lift(DirtyKey).foreach(_ := true.B)
  a_acquire.valid := io.task.valid && !io.task.bits.putData

  val s1_ready = Wire(Bool())
  val s1_full = RegInit(false.B)

  // S0: read putBuffer
  val s0_task = RegEnable(io.task.bits, io.task.fire && io.task.bits.putData)
  val s0_count = RegInit(0.U(beatBits.W))
  // TODO: make beat calculation configurable
  require(blockBytes / beatBytes == 2)
  val s0_last = Mux(s0_task.size === log2Ceil(blockBytes).U, s0_count === (beats-1).U, s0_count === (1-1).U)
  val s0_valid = io.pb_pop.fire

  io.pb_pop.valid := busy && s1_ready
  io.pb_pop.bits.bufIdx := s0_task.bufIdx
  io.pb_pop.bits.count := s0_count
  io.pb_pop.bits.last  := s0_last

  when (io.pb_pop.fire) {
    s0_count := s0_count + 1.U
    when (s0_last) {
      busy := false.B
      s0_count := 0.U
    }
  }

  // S1: get putBuffer and transfer to outer A
  val s1_latch = s0_valid && s1_ready
  val s1_count = RegEnable(s0_count, s1_latch)
  val s1_task = RegEnable(s0_task, s1_latch)
  val s1_cango = Mux(a_put.valid, a_put.ready, false.B)
  val s1_pb_latch = HoldUnless(io.pb_beat, RegNext(s1_latch))

  s1_ready := s1_cango || !s1_full

  when(s1_full && s1_cango) { s1_full := false.B }
  when(s1_latch) { s1_full := true.B }

  a_put.bits.opcode := s1_task.opcode
  a_put.bits.param := s1_task.param
  a_put.bits.size := s1_task.size
  a_put.bits.source := s1_task.source
  a_put.bits.address := Cat(s1_task.tag, s1_task.set, s1_task.off)
  a_put.bits.mask := s1_pb_latch.mask
  a_put.bits.data := s1_pb_latch.data
  a_put.bits.corrupt := false.B
  a_put.bits.user.lift(PreferCacheKey).foreach(_ := false.B)
  a_put.bits.user.lift(utility.ReqSourceKey).foreach(_ := MemReqSource.NoWhere.id.U) //TODO: where does Put comes from
  a_put.bits.echo.lift(DirtyKey).foreach(_ := true.B)
  a_put.valid := s1_full

  TLArbiter.lowest(edgeIn, io.a, a_put, a_acquire)

}
