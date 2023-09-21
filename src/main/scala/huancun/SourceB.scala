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
import utility.ParallelPriorityMux

class SourceB(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val b = DecoupledIO(new TLBundleB(edgeIn.bundle))
    val task = Flipped(DecoupledIO(new SourceBReq))
  })

  /* Resolve task */
  val workVec = RegInit(0.U(clientBits.W))
  when(io.task.fire) {
    workVec := io.task.bits.clients
  }
  val busy = workVec.orR
  io.task.ready := !busy
  val pendingClient = Mux(busy, workVec, io.task.bits.clients)
  val chosenClient = ParallelPriorityMux(pendingClient.asBools.zipWithIndex.map {
    case (sel, i) => sel -> UIntToOH(i.U, width = clientBits)
  })
  when(io.b.fire) {
    workVec := pendingClient & ~chosenClient
  }

  /* Handle B channel */
  val taskLatch = Mux(!busy, io.task.bits, RegEnable(io.task.bits, io.task.fire))
  val probe_alias = taskLatch.alias.map(alias_vec => Mux1H(chosenClient, alias_vec))
  io.b.valid := busy || io.task.valid
  io.b.bits.opcode := TLMessages.Probe
  io.b.bits.param := taskLatch.param
  io.b.bits.size := offsetBits.U
  io.b.bits.source := getSourceId(chosenClient)
  io.b.bits.address := Cat(taskLatch.tag, taskLatch.set, 0.U(offsetBits.W))
  io.b.bits.mask := ~0.U(beatBytes.W)
  io.b.bits.data := Cat(
    probe_alias.getOrElse(0.U(clientBits.W)),
    taskLatch.needData.getOrElse(false.B).asUInt
  )
  io.b.bits.corrupt := 0.U
}
