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
import freechips.rocketchip.tilelink.TLMessages.{AcquireBlock, AcquirePerm, ReleaseAck}
import huancun.utils._


class SourceD(implicit p: Parameters) extends HuanCunModule {

  // TODO: set this param correctly
  def SRAM_LATENCY = 2

  /*
      Message         Operation       Channel          Data
      -------------|---------------|------------|--------------
      AccessAck       Put                A           Y/N TODO: put may be done in SinkA?
      AccessAckData   Get/Atomic         A            Y
      HintAck         Hint               A            N
      Grant           Acquire            A            N
      GrantData       Acquire            A            Y
      ReleaseAck      Release            C            N
   */
  val io = IO(new Bundle() {
    val d = DecoupledIO(new TLBundleD(edgeIn.bundle))
    val task = Flipped(DecoupledIO(new SourceDReq))
    val bs_raddr = DecoupledIO(new DSAddress)
    val bs_rdata = Input(new DSData)
    val bypass_read = Flipped(new SourceDBufferRead)
    val bs_waddr = DecoupledIO(new DSAddress)
    val bs_wdata = Output(new DSData)
    // data hazards
    val sourceD_r_hazard = ValidIO(new SourceDHazard)
    // putbuffer interface
    val pb_pop = DecoupledIO(new PutBufferPop)
    val pb_beat = Input(new PutBufferBeatEntry)
  })

  val d = io.d
  val s1_valid = Wire(Bool())
  val s2_valid, s2_ready = Wire(Bool())
  val s3_valid, s3_ready = Wire(Bool())
  val s4_ready = Wire(Bool())

  def needData(req: SourceDReq): Bool = {
    req.fromA && (
      req.opcode === TLMessages.GrantData ||
        req.opcode === TLMessages.AccessAckData ||
        req.opcode === TLMessages.AccessAck && !req.bypassPut
      )
  }

  // stage1
  val busy = RegInit(false.B)
  val s1_block_r = RegInit(false.B)
  val s1_req_reg = RegEnable(io.task.bits, io.task.fire())
  val s1_req = Mux(busy, s1_req_reg, io.task.bits)
  val s1_needData = needData(s1_req)
  val s1_need_pb = s1_req.fromA && (s1_req.opcode === TLMessages.AccessAck && !s1_req.bypassPut)
  val s1_counter = RegInit(0.U(beatBits.W)) // how many beats have been sent
  val s1_total_beats = Mux(s1_needData, totalBeats(s1_req.size), 0.U(beatBits.W))
  val s1_beat = startBeat(s1_req.off) | s1_counter
  val s1_valid_r = (busy || io.task.valid) && s1_needData && !s1_block_r
  val s1_last = s1_counter === s1_total_beats
  val s1_bypass_hit_reg = RegInit(false.B)
  val s1_bypass_hit_wire = io.bypass_read.r_valid && io.bypass_read.buffer_hit
  val s1_bypass_hit = Mux(busy,
    s1_bypass_hit_reg && s1_bypass_hit_wire,
    s1_bypass_hit_wire
  )
  val s1_bypass_data = io.bypass_read.buffer_data
  val data_from_refill_buffer = s1_req.useBypass

  val s1_queue = Module(new Queue(new DSData, 2, flow = false, pipe = false))
  s1_queue.io.enq.valid := s1_bypass_hit
  s1_queue.io.enq.bits := s1_bypass_data
  assert(!s1_queue.io.enq.valid || s1_queue.io.enq.ready)

  io.bs_raddr.valid := s1_valid_r && !s1_req.useBypass
  io.bs_raddr.bits.way := s1_req.way
  io.bs_raddr.bits.set := s1_req.set
  io.bs_raddr.bits.beat := s1_beat // TODO: support unaligned address
  io.bs_raddr.bits.write := false.B
  io.bs_raddr.bits.noop := false.B

  io.bypass_read.r_valid := s1_valid_r && s1_req.useBypass
  io.bypass_read.r_id := s1_req.sinkId
  io.bypass_read.r_beat := s1_beat

  when(io.task.fire()) {
    busy := true.B
    s1_bypass_hit_reg := s1_bypass_hit_wire
  }
  when(Mux(data_from_refill_buffer, s1_bypass_hit, io.bs_raddr.fire())){
    s1_block_r := true.B
  }
  when(s1_valid && s2_ready) {
    s1_counter := s1_counter + 1.U
    s1_block_r := false.B
    when(s1_last) {
      s1_counter := 0.U
      busy := false.B
      s1_bypass_hit_reg := false.B
    }
  }
  io.task.ready := !busy
  s1_valid := (busy || io.task.valid) && (
    !s1_valid_r ||
      Mux(data_from_refill_buffer,
        s1_bypass_hit,                    // wait data from refill buffer
        io.bs_raddr.ready                 // wait data from bankedstore
      )
    )

  // stage2
  val s2_latch = s1_valid && s2_ready
  val s2_req = RegEnable(s1_req, s2_latch)
  val s2_needData = RegEnable(s1_needData, s2_latch)
  val s2_last = RegEnable(s1_last, s2_latch)
  val s2_counter = RegEnable(s1_counter, s2_latch)
  val s2_full = RegInit(false.B)
  val s2_releaseAck = s2_req.opcode === ReleaseAck
  val s2_bypass_hit = RegEnable(
    Mux(busy, s1_bypass_hit_reg, s1_bypass_hit_wire),
    false.B, s2_latch
  )
  val s2_d = Wire(io.d.cloneType)
  val s2_need_pb = RegEnable(s1_need_pb, s2_latch)
  val s2_need_d = RegEnable(!s1_need_pb || s1_counter === 0.U, s2_latch) // AccessAck for Put should only be fired once
  val s2_valid_pb = RegInit(false.B) // put buffer is valid, wait put buffer fire
  val s2_pdata_raw = io.pb_beat
  val pb_ready = io.pb_pop.ready
  val s2_pdata = HoldUnless(s2_pdata_raw, s2_valid_pb)

  io.pb_pop.valid := s2_valid_pb && s2_req.fromA
  io.pb_pop.bits.bufIdx := s2_req.bufIdx
  io.pb_pop.bits.count := s2_counter
  io.pb_pop.bits.last  := s2_last

  when (pb_ready) { s2_valid_pb := false.B }
  when (s2_latch) { s2_valid_pb := s1_need_pb }

  s1_queue.io.deq.ready := s2_full && s2_bypass_hit && s2_d.ready
  s2_d.valid := s2_full && ((s2_bypass_hit && s1_queue.io.deq.valid) || !s2_needData)
  s2_d.bits.opcode := s2_req.opcode
  s2_d.bits.param := Mux(s2_releaseAck, 0.U, s2_req.param)
  s2_d.bits.sink := s2_req.sinkId
  s2_d.bits.size := s2_req.size
  s2_d.bits.source := s2_req.sourceId
  s2_d.bits.denied := false.B
  s2_d.bits.data := s1_queue.io.deq.bits.data
  s2_d.bits.corrupt := false.B
  s2_d.bits.echo.lift(DirtyKey).foreach(_ := s2_req.dirty)

  val s2_can_go = Mux(s2_d.valid, s2_d.ready, s3_ready && (!s2_valid_pb || pb_ready))
  when(s2_full && s2_can_go) { s2_full := false.B }
  when(s2_latch) { s2_full := true.B }

  s2_valid := s2_full && !s2_d.valid && (!s2_valid_pb || pb_ready)
  s2_ready := !s2_full || s2_can_go

  class PipeInfo extends Bundle {
    val counter = UInt(beatBits.W)
    val pdata = new PutBufferBeatEntry
    val needPb = Bool()
    val need_d = Bool()
    val isReleaseAck = Bool()
    val req = new SourceDReq
  }

  // we read data at s1, -1 here because s2 is hard-written
  val pipe = Module(new Pipeline(new PipeInfo, SRAM_LATENCY - 1))

  pipe.io.in.valid := s2_valid
  pipe.io.in.bits.counter := s2_counter
  pipe.io.in.bits.pdata := s2_pdata
  pipe.io.in.bits.needPb := s2_need_pb
  pipe.io.in.bits.need_d := s2_need_d
  pipe.io.in.bits.isReleaseAck := s2_releaseAck
  pipe.io.in.bits.req := s2_req
  s3_ready := pipe.io.in.ready

  // stage3
  val s3_regs = pipe.io.out.bits
  val s3_req = s3_regs.req
  val s3_counter = s3_regs.counter
  val s3_pdata = s3_regs.pdata
  val s3_need_pb = s3_regs.needPb
  val s3_releaseAck = s3_regs.isReleaseAck
  val s3_d = Wire(io.d.cloneType)
  // stage1 may read two beats, so +1 here
  val s3_queue = Module(new Queue(new DSData, SRAM_LATENCY + 1, flow = true))

  assert(!s3_valid || needData(s3_regs.req), "Only data task can go to stage3!")

  val s3_rdata = s3_queue.io.deq.bits.data
  s3_d.valid := s3_valid
  s3_d.bits.opcode := s3_req.opcode
  s3_d.bits.param := Mux(s3_releaseAck, 0.U, s3_req.param)
  s3_d.bits.sink := s3_req.sinkId
  s3_d.bits.size := s3_req.size
  s3_d.bits.source := s3_req.sourceId
  s3_d.bits.denied := false.B
  s3_d.bits.data := s3_rdata
  s3_d.bits.corrupt := false.B
  s3_d.bits.echo.lift(DirtyKey).foreach(_ := s3_req.dirty)

  s3_queue.io.enq.valid := (0 until SRAM_LATENCY - 1).foldLeft(
    RegNext(io.bs_raddr.fire() && !Mux(busy, s1_bypass_hit_reg, s1_bypass_hit_wire), false.B)
  )((prev, _) => RegNext(prev, false.B))
  s3_queue.io.enq.bits := io.bs_rdata
  assert(!s3_queue.io.enq.valid || s3_queue.io.enq.ready)
  s3_queue.io.deq.ready := s3_d.ready && s3_valid

  pipe.io.out.ready := !s3_valid || s3_d.ready
  s3_valid := pipe.io.out.valid

  // stage4
  val s4_latch = s3_valid && s4_ready
  val s4_req = RegEnable(s3_req, s4_latch)
  val s4_rdata = RegEnable(s3_rdata, s4_latch)
  val s4_pdata = RegEnable(s3_pdata, s4_latch)
  val s4_need_pb = RegEnable(s3_need_pb, s4_latch)
  val s4_beat = RegEnable(s3_counter, s4_latch)
  val s4_full = RegInit(false.B)

  when (io.bs_waddr.ready || !s4_need_pb) { s4_full := false.B }
  when (s4_latch) { s4_full := true.B }

  val selects = s4_pdata.mask.asBools
  val mergedData = Cat(selects.zipWithIndex.map { case (s, i) =>
    VecInit(Seq(s4_rdata, s4_pdata.data).map(_((i + 1) * 8 - 1, i * 8)))(s)
  }.reverse)  // merge data according to mask

  io.bs_waddr.valid := s4_full && s4_need_pb
  io.bs_waddr.bits.noop := false.B
  io.bs_waddr.bits.way  := s4_req.way
  io.bs_waddr.bits.set  := s4_req.set
  io.bs_waddr.bits.beat := s4_beat
  io.bs_waddr.bits.write := true.B
  io.bs_wdata.data := mergedData

  s4_ready := !s4_full || io.bs_waddr.ready || !s4_need_pb

  TLArbiter.lowest(edgeIn, io.d, s3_d, s2_d)

  io.sourceD_r_hazard.valid := busy && s1_needData
  io.sourceD_r_hazard.bits.set := s1_req_reg.set
  io.sourceD_r_hazard.bits.way := s1_req_reg.way
}
