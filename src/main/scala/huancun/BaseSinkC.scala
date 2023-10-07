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

abstract class BaseSinkC(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    val way = Input(UInt(wayBits.W))
    val alloc = DecoupledIO(new MSHRRequest)
    val resp = ValidIO(new SinkCResp)
    val task = Flipped(DecoupledIO(new SinkCReq))
    val taskack = ValidIO(new SinkCAck)
    val bs_waddr = DecoupledIO(new DSAddress)
    val bs_wdata = Output(new DSData)
    val sourceD_r_hazard = Flipped(ValidIO(new SourceDHazard))
    // directly release inner data to next level
    val release = DecoupledIO(new TLBundleC(edgeOut.bundle))
  })
  io.alloc.bits.isPrefetch.foreach(_ := false.B)

  when (io.c.fire) {
    assert(io.c.bits.opcode =/= 3.U) // opcode 3 is reserved for C channel
  }
}
