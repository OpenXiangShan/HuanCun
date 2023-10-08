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
import TLMessages.{AccessAckData, ReleaseAck}

class SinkD(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val d = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val bs_waddr = DecoupledIO(new DSAddress)
    val bs_wdata = Output(new DSData)
    val bypass_write = Flipped(new SinkDBufferWrite)
    val way = Input(UInt(wayBits.W))
    val set = Input(UInt(setBits.W))
    val inner_grant = Input(Bool())  // sourceD will use bypass data
    val save_data_in_bs = Input(Bool())
    val resp = ValidIO(new SinkDResp)
    val sourceD_r_hazard = Flipped(ValidIO(new SourceDHazard))
  })

  val (first, last, _, beat) = edge.count(io.d)
  val cache = io.save_data_in_bs
  val needData = io.d.bits.opcode(0)

  val source_latch = RegEnable(io.d.bits.source, io.d.valid)
  val first_resp = RegInit(true.B)
  when (io.d.valid) {
    first_resp := false.B
  }
  val new_source = first_resp || io.d.bits.source =/= source_latch
  val indexed_set = RegEnable(io.set, io.d.valid)
  val indexed_way = RegEnable(io.way, io.d.valid)
  val w_safe = !new_source && !(io.sourceD_r_hazard.valid && io.sourceD_r_hazard.bits.safe(indexed_set, indexed_way))

  assert(!io.d.valid || !needData || io.d.bits.size === log2Up(blockBytes).U, "SinkD must receive aligned message when needData")

  val bypass_ready = io.inner_grant && needData && io.bypass_write.ready
  val bs_ready = (needData && w_safe || !first) &&
    cache && io.bs_waddr.ready &&
    (bypass_ready || !io.inner_grant)

  io.d.ready := !needData || bs_ready || !cache && bypass_ready || (!cache && !io.inner_grant)

  // Generate resp
  io.resp.valid := (first || last) && io.d.fire // MSHR need to be notified when both first & last
  io.resp.bits.last := last
  io.resp.bits.opcode := io.d.bits.opcode
  io.resp.bits.param := io.d.bits.param
  io.resp.bits.source := io.d.bits.source
  io.resp.bits.sink := io.d.bits.sink
  io.resp.bits.denied := io.d.bits.denied
  io.resp.bits.dirty := io.d.bits.echo.lift(DirtyKey).getOrElse(false.B)
  io.resp.bits.bufIdx := io.bypass_write.id

  // Save data to Datastorage
  io.bs_waddr.valid := (io.d.valid && needData && w_safe || !first) && cache &&
    (bypass_ready || !io.inner_grant)
  io.bs_waddr.bits.way := io.way
  io.bs_waddr.bits.set := io.set
  io.bs_waddr.bits.beat := Mux(io.d.valid, beat, RegEnable(beat + io.bs_waddr.ready.asUInt, io.d.valid))
  io.bs_waddr.bits.write := true.B
  io.bs_waddr.bits.noop := !io.d.valid
  io.bs_wdata.data := io.d.bits.data
  io.bs_wdata.corrupt := false.B
  io.bypass_write.valid := io.d.valid && bypass_ready && (!cache || io.bs_waddr.ready && (w_safe || !first))
  io.bypass_write.beat := beat
  io.bypass_write.data := io.bs_wdata
}
