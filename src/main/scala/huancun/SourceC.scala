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
import freechips.rocketchip.tilelink._
import utility.{MemReqSource, ReqSourceKey}

class SourceCPipe(implicit p: Parameters) extends HuanCunBundle {
  val task = new SourceCReq
  val beat = UInt(beatBits.W)
  def apply(task: SourceCReq, beat: UInt): Unit = {
    this.task := task
    this.beat := beat
  }
}

class SourceC(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = DecoupledIO(new TLBundleC(edge.bundle))
    val bs_raddr = DecoupledIO(new DSAddress)
    val bs_rdata = Input(new DSData)
    val task = Flipped(DecoupledIO(new SourceCReq))
  })

  val queue_size = 4 + sramLatency
  val queue_flow = true

  val bs_busy = RegInit(false.B)
  val back_pressure = RegInit(false.B)
  val queue = Module(new Queue(chiselTypeOf(io.c.bits), entries = queue_size, flow = queue_flow))
  back_pressure := queue.io.count >= (queue_size - sramLatency - beatSize).U // 2 in pipeline and beatSize in pending

  // Handle task
  val beat = RegInit(0.U(beatBits.W))
  when(io.bs_raddr.fire) {
    beat := beat + 1.U
  }
  val task_latch = RegEnable(io.task.bits, !bs_busy && io.task.valid)
  val task = Mux(!bs_busy, io.task.bits, task_latch)
  val taskWithData = io.task.valid && !back_pressure && io.task.bits.opcode(0)
  when(taskWithData) { bs_busy := true.B }
  when(io.bs_raddr.fire && beat === ~0.U(beatBits.W)) { bs_busy := false.B }
  io.task.ready := !bs_busy && !back_pressure

  // Read Datastorage
  val has_data = taskWithData || bs_busy
  io.bs_raddr.valid := has_data
  io.bs_raddr.bits.way := task.way
  io.bs_raddr.bits.set := task.set
  io.bs_raddr.bits.beat := beat
  io.bs_raddr.bits.write := false.B
  io.bs_raddr.bits.noop := false.B

  // Stage 0 => Stage 1
  val task_handled = Mux(has_data, io.bs_raddr.ready, io.task.fire)
  val s1_task = RegInit(0.U.asTypeOf(io.task.bits.cloneType))
  val s1_beat = RegInit(0.U(beatBits.W))
  val s1_valid = RegInit(false.B)
  when(s1_valid){
    s1_valid := false.B
  }
  when(task_handled) {
    s1_valid := true.B
    s1_task := task
    s1_beat := beat
  }

  val s1_info = Wire(Valid(new SourceCPipe))
  s1_info.valid := s1_valid
  s1_info.bits.apply(s1_task, s1_beat)

  val pipeOut = Pipe(s1_info, sramLatency-1)

  queue.io.enq.valid := pipeOut.valid
  queue.io.enq.bits.opcode := pipeOut.bits.task.opcode
  queue.io.enq.bits.param := pipeOut.bits.task.param
  queue.io.enq.bits.size := offsetBits.U
  queue.io.enq.bits.source := pipeOut.bits.task.source
  queue.io.enq.bits.address := Cat(pipeOut.bits.task.tag, pipeOut.bits.task.set, 0.U(offsetBits.W))
  queue.io.enq.bits.data := io.bs_rdata.data
  queue.io.enq.bits.corrupt := io.bs_rdata.corrupt
  queue.io.enq.bits.user.lift(PreferCacheKey).foreach(_ := true.B)
  queue.io.enq.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.NoWhere.id.U)
  queue.io.enq.bits.echo.lift(DirtyKey).foreach(_ := pipeOut.bits.task.dirty)
  queue.io.enq.bits.user.lift(IsHitKey).foreach(_ := true.B)

  io.c <> queue.io.deq
}
