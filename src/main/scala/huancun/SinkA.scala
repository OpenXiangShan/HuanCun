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

class SinkA(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
    val task = Flipped(DecoupledIO(new SinkAReq))
    // SourceD
    val d_pb_pop = Flipped(DecoupledIO(new PutBufferPop))
    val d_pb_beat = Output(new PutBufferBeatEntry)
    // SourceA
    val a_pb_pop = Flipped(DecoupledIO(new PutBufferPop))
    val a_pb_beat = Output(new PutBufferBeatEntry)
  })

  // TODO: Does task for SinkA necessary?
  io.task.ready := false.B

  val a = io.a
  val (first, last, done, count) = edgeIn.count(a)
  val hasData = edgeIn.hasData(a.bits)

  val beats = blockBytes / beatBytes
  val putBuffer = Reg(Vec(bufBlocks, Vec(beats, new PutBufferBeatEntry())))
  val beatVals = RegInit(VecInit(Seq.fill(bufBlocks) {
    VecInit(Seq.fill(beats) { false.B })
  }))
  val bufVals = VecInit(beatVals.map(_.asUInt.orR)).asUInt
  val full = bufVals.andR
  val noSpace = full && hasData
  val insertIdx = PriorityEncoder(~bufVals)
  val insertIdxReg = RegEnable(insertIdx, a.fire && first)

  when(a.fire && hasData) {
    when(first) {
      putBuffer(insertIdx)(count).data := a.bits.data
      putBuffer(insertIdx)(count).mask := a.bits.mask
      putBuffer(insertIdx)(count).corrupt := a.bits.corrupt
      beatVals(insertIdx)(count) := true.B
    }.otherwise({
      putBuffer(insertIdxReg)(count).data := a.bits.data
      putBuffer(insertIdxReg)(count).mask := a.bits.mask
      putBuffer(insertIdxReg)(count).corrupt := a.bits.corrupt
      beatVals(insertIdxReg)(count) := true.B
    })
  }

  val bufferLeakCnt = RegInit(0.U(12.W)) // check buffer leak for index 0
  dontTouch(bufferLeakCnt)
  when(bufVals(0)) {
    bufferLeakCnt := bufferLeakCnt + 1.U
  }.otherwise {
    bufferLeakCnt := 0.U
  }

  when(bufferLeakCnt === 800.U) {
    assert(false.B, "buffer leak at index 0")
  }

  val (tag, set, offset) = parseAddress(a.bits.address)

  io.alloc.valid := a.valid && first && !noSpace
  a.ready := Mux(first, io.alloc.ready && !noSpace, true.B)

  val allocInfo = io.alloc.bits
  allocInfo.channel := 1.U(3.W)
  allocInfo.opcode := a.bits.opcode
  allocInfo.param := a.bits.param
  allocInfo.size := a.bits.size
  allocInfo.source := a.bits.source
  allocInfo.set := set
  allocInfo.tag := tag
  allocInfo.off := offset
  allocInfo.mask := a.bits.mask
  allocInfo.bufIdx := insertIdx
  allocInfo.needHint.foreach(_ := a.bits.user.lift(PrefetchKey).getOrElse(false.B))
  allocInfo.isPrefetch.foreach(_ := a.bits.opcode === TLMessages.Hint)
  allocInfo.isBop.foreach(_ := false.B)
  allocInfo.alias.foreach(_ := a.bits.user.lift(AliasKey).getOrElse(0.U))
  // allocInfo.preferCache := Mux((a.bits.opcode === TLMessages.Get || a.bits.opcode(2,1) === 0.U), true.B, a.bits.user.lift(PreferCacheKey).getOrElse(true.B))
  if (cacheParams.level == 2) {
    allocInfo.preferCache := a.bits.user.lift(PreferCacheKey).getOrElse(true.B)
  } else {
    allocInfo.preferCache := Mux((a.bits.opcode === TLMessages.Get || a.bits.opcode(2,1) === 0.U), true.B, a.bits.user.lift(PreferCacheKey).getOrElse(true.B))
  }
  allocInfo.dirty := false.B // ignored
  allocInfo.isHit := true.B // ignored
  allocInfo.fromProbeHelper := false.B
  allocInfo.fromCmoHelper := false.B
  allocInfo.needProbeAckData.foreach(_ := false.B)
  allocInfo.reqSource := a.bits.user.lift(utility.ReqSourceKey).getOrElse(MemReqSource.NoWhere.id.U)

  io.d_pb_pop.ready := beatVals(io.d_pb_pop.bits.bufIdx)(io.d_pb_pop.bits.count)
  io.d_pb_beat := RegEnable(putBuffer(io.d_pb_pop.bits.bufIdx)(io.d_pb_pop.bits.count), io.d_pb_pop.fire)
  when(io.d_pb_pop.fire && io.d_pb_pop.bits.last) {
    beatVals(io.d_pb_pop.bits.bufIdx).foreach(_ := false.B)
  }

  io.a_pb_pop.ready := beatVals(io.a_pb_pop.bits.bufIdx)(io.a_pb_pop.bits.count)
  io.a_pb_beat := RegEnable(putBuffer(io.a_pb_pop.bits.bufIdx)(io.a_pb_pop.bits.count), io.a_pb_pop.fire)
  when(io.a_pb_pop.fire && io.a_pb_pop.bits.last) {
    beatVals(io.a_pb_pop.bits.bufIdx).foreach(_ := false.B)
  }
}
