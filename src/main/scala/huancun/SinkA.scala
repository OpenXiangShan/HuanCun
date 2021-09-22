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

class SinkA(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
    val task = Flipped(DecoupledIO(new SinkAReq))
  })

  // TODO: Handle task
  io.task.ready := false.B

  val a = io.a
  val first = edgeIn.first(a)
  val hasData = edgeIn.hasData(a.bits)
  when(a.valid) {
    assert(!hasData)
  }
  val (tag, set, offset) = parseAddress(a.bits.address)

  io.alloc.valid := a.valid && first
  a.ready := io.alloc.ready

  val allocInfo = io.alloc.bits
  allocInfo.channel := 1.U(3.W)
  allocInfo.opcode := a.bits.opcode
  allocInfo.param := a.bits.param
  allocInfo.size := a.bits.size
  allocInfo.source := a.bits.source
  allocInfo.set := set
  allocInfo.tag := tag
  allocInfo.off := offset
  allocInfo.bufIdx := DontCare
  allocInfo.needHint.foreach(_ := a.bits.user.lift(PrefetchKey).getOrElse(false.B))
  allocInfo.isPrefetch.foreach(_ := false.B)
  allocInfo.alias.foreach(_ := a.bits.user.lift(AliasKey).getOrElse(0.U))
  allocInfo.preferCache := a.bits.user.lift(PreferCacheKey).getOrElse(true.B)
  allocInfo.fromProbeHelper := false.B
  allocInfo.needProbeAckData.foreach(_ := false.B)
}
