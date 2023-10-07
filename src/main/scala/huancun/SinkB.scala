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
import utility.MemReqSource

class SinkB(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val b = Flipped(DecoupledIO(new TLBundleB(edge.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
  })

  io.b.ready := io.alloc.ready
  io.alloc.valid := io.b.valid

  val (tag, set, off) = parseAddress(io.b.bits.address)

  io.alloc.bits.opcode := io.b.bits.opcode
  io.alloc.bits.param := io.b.bits.param
  io.alloc.bits.channel := "b010".U
  io.alloc.bits.size := io.b.bits.size
  io.alloc.bits.source := io.b.bits.source
  io.alloc.bits.set := set
  io.alloc.bits.tag := tag
  io.alloc.bits.off := off
  io.alloc.bits.mask := io.b.bits.mask
  io.alloc.bits.bufIdx := 0.U
  io.alloc.bits.needHint.foreach(_ := false.B)
  io.alloc.bits.isPrefetch.foreach(_ := false.B)
  io.alloc.bits.isBop.foreach(_ := false.B)
  io.alloc.bits.alias.foreach(_ := 0.U)
  io.alloc.bits.preferCache := true.B
  io.alloc.bits.dirty := false.B // ignored
   io.alloc.bits.isHit := true.B // ignored
  io.alloc.bits.fromProbeHelper := false.B
  io.alloc.bits.fromCmoHelper := false.B
  io.alloc.bits.needProbeAckData.foreach(_ := io.b.bits.data(0))
  io.alloc.bits.reqSource := MemReqSource.NoWhere.id.U // Ignore
}
