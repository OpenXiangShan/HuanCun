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
import huancun.utils._
import utility.{ParallelOR, ParallelPriorityMux}
import freechips.rocketchip.tilelink._

class MSHRSelector(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val idle = Input(Vec(mshrs, Bool()))
    val out = ValidIO(UInt(mshrs.W))
  })
  io.out.valid := ParallelOR(io.idle)
  io.out.bits := ParallelPriorityMux(io.idle.zipWithIndex.map {
    case (b, i) => (b, (1 << i).U)
  })
}

class MSHRAlloc(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    // requests
    val a_req = Flipped(DecoupledIO(new MSHRRequest))
    val b_req = Flipped(DecoupledIO(new MSHRRequest))
    val c_req = Flipped(DecoupledIO(new MSHRRequest))
    // From MSHRs
    val status = Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus)))
    // To MSHRs
    val alloc = Vec(mshrsAll, ValidIO(new MSHRRequest))
    // To directory
    val dirRead = DecoupledIO(new DirRead)
    val bc_mask = ValidIO(Vec(mshrsAll, Bool()))
    val c_mask = ValidIO(Vec(mshrsAll, Bool()))

  })

  // Allocate one MSHR per cycle
  assert(PopCount(io.alloc.map(_.valid)) <= 1.U)

  /* case1: selected request matches set of pending MSHR => stall
   * case2: selected request needs new MSHR but no room left => stall
   * case3: selected request needs new MSHR but dir not ready => stall
   * case4: selected request needs new MSHR and dir ready => good!
   */

  /* Select one request from a_req/b_req/c_req */
  val request = Wire(ValidIO(new MSHRRequest()))

  /* Whether selected request can be accepted */

  def get_match_vec(req: MSHRRequest, granularity: Int = setBits): Vec[Bool] = {
    VecInit(io.status.map(s => s.valid && s.bits.set(granularity - 1, 0) === req.set(granularity - 1, 0)))
  }

//  val c_block_vec = get_match_vec(io.c_req.bits, block_granularity)
//  val b_block_vec = get_match_vec(io.b_req.bits, block_granularity)
//  val a_block_vec = get_match_vec(io.a_req.bits, block_granularity)

  val c_match_vec = get_match_vec(io.c_req.bits, block_granularity)
  val b_match_vec = get_match_vec(io.b_req.bits, block_granularity)
  val a_match_vec = get_match_vec(io.a_req.bits, block_granularity)

  val nestC_vec = VecInit(
    io.status.map(s => s.valid && s.bits.nestC).init :+ false.B
  )
  val nestB_vec = VecInit(
    io.status.map(s => s.valid && s.bits.nestB).init.init ++ Seq(false.B, false.B)
  )

  val conflict_c = c_match_vec.asUInt.orR
  val conflict_b = b_match_vec.asUInt.orR
  val conflict_a = a_match_vec.asUInt.orR

  val abc_mshr_status = io.status.init.init
  val bc_mshr_status = io.status.init.last
  val c_mshr_status = io.status.last

  val double_nest = Cat(c_match_vec.init.init).orR && c_match_vec.init.last
  val may_nestC = (c_match_vec.asUInt & nestC_vec.asUInt).orR && !(double_nest && !bc_mshr_status.bits.nestC)
  val may_nestB = (b_match_vec.asUInt & nestB_vec.asUInt).orR

  val abc_mshr_alloc = io.alloc.init.init
  val bc_mshr_alloc = io.alloc.init.last
  val c_mshr_alloc = io.alloc.last

  val nestC = may_nestC && !c_mshr_status.valid
  val nestB = may_nestB && !bc_mshr_status.valid && !c_mshr_status.valid

  val dirRead = io.dirRead
  val mshrFree = Cat(abc_mshr_status.map(s => !s.valid)).orR

  //val can_accept_c = (mshrFree && !conflict_c) || nestC
  val can_accept_c = (!conflict_c && (mshrFree || !c_mshr_status.valid)) || nestC
  val accept_c = io.c_req.valid && can_accept_c

  val can_accept_b = (!conflict_b && (mshrFree || !bc_mshr_status.valid) || nestB) && !io.c_req.valid
  val accept_b = io.b_req.valid && can_accept_b

  val can_accept_a = mshrFree && !conflict_a && !io.c_req.valid && !io.b_req.valid
  val accept_a = io.a_req.valid && can_accept_a

  request.valid := io.c_req.valid || io.b_req.valid || io.a_req.valid
  request.bits := Mux(io.c_req.valid,
    io.c_req.bits,
    Mux(io.b_req.valid, io.b_req.bits, io.a_req.bits)
  )

  /* Provide signals for outer components*/
  io.c_req.ready := dirRead.ready && can_accept_c
  io.b_req.ready := dirRead.ready && can_accept_b
  io.a_req.ready := dirRead.ready && can_accept_a

  val mshrSelector = Module(new MSHRSelector())
  mshrSelector.io.idle := abc_mshr_status.map(s => !s.valid)
  val selectedMSHROH = mshrSelector.io.out.bits
  for ((mshr, i) <- abc_mshr_alloc.zipWithIndex) {
    mshr.valid := (
      mshrFree && dirRead.ready && (
        io.c_req.valid && !conflict_c ||
        io.b_req.valid && !conflict_b && !io.c_req.valid ||
        io.a_req.valid && !conflict_a && !io.b_req.valid && !io.c_req.valid
      )
    ) && selectedMSHROH(i)
    mshr.bits := request.bits
  }

  val nestB_valid = io.b_req.valid && nestB && !io.c_req.valid
  val tmpB_valid = io.b_req.valid && !mshrFree && !conflict_b && !bc_mshr_status.valid && !io.c_req.valid
  val nestC_valid = io.c_req.valid && nestC
  val tmpC_valid = io.c_req.valid && !mshrFree && !conflict_c && !c_mshr_status.valid

  bc_mshr_alloc.valid := (nestB_valid || tmpB_valid) && dirRead.ready
  bc_mshr_alloc.bits := io.b_req.bits
  c_mshr_alloc.valid := (nestC_valid || tmpC_valid) && dirRead.ready
  c_mshr_alloc.bits := io.c_req.bits

  io.bc_mask.valid := bc_mshr_alloc.valid
  io.bc_mask.bits := b_match_vec
  io.c_mask.valid := c_mshr_alloc.valid
  io.c_mask.bits := c_match_vec

  dirRead.valid := request.valid && Cat(accept_c, accept_b, accept_a).orR && dirRead.ready
  dirRead.bits.source := request.bits.source
  dirRead.bits.tag := request.bits.tag
  dirRead.bits.set := request.bits.set
  dirRead.bits.idOH := Cat(
    nestC_valid || tmpC_valid,
    nestB_valid || tmpB_valid,
    Mux(nestC_valid || tmpC_valid || nestB_valid || tmpB_valid, 0.U(mshrs.W), selectedMSHROH)
  )
  dirRead.bits.replacerInfo.channel := request.bits.channel
  dirRead.bits.replacerInfo.opcode := request.bits.opcode
  dirRead.bits.replacerInfo.reqSource := request.bits.reqSource
  dirRead.bits.wayMode := false.B
  dirRead.bits.way := DontCare

  val cntStart = RegInit(false.B)
  when(dirRead.ready) {
    cntStart := true.B
  }

  if (cacheParams.enablePerf) {
    val mshrCnt = RegInit(VecInit(Seq.fill(mshrsAll)(0.U(16.W))))
    for ((cnt, i) <- mshrCnt.zipWithIndex) {
      when(!cntStart) {
        cnt := 0.U
      }
      when(io.status(i).valid) {
        cnt := cnt + 1.U
      }.elsewhen(cnt =/= 0.U) {
        cnt := 0.U
      }
      val cntEnable =
        !io.status(i).valid && cnt =/= 0.U && cntStart && cnt < 5000.U // Ignore huge cnt during L3 dir reset
      XSPerfHistogram(cacheParams, "mshr_latency_" + Integer.toString(i, 10), cnt, cntEnable, 0, 300, 10, rStrict = true)
      XSPerfHistogram(cacheParams, "mshr_latency_" + Integer.toString(i, 10), cnt, cntEnable, 300, 1000, 50, lStrict = true)
      XSPerfMax(cacheParams, "mshr_latency", cnt, cntEnable)
    }
  }

  val pretch_block_vec = VecInit(io.status.map(s =>
    s.valid && s.bits.is_prefetch &&
      (s.bits.set(block_granularity - 1, 0) === io.a_req.bits.set(block_granularity - 1, 0))
  ))

  XSPerfAccumulate(cacheParams, "nrWorkingABCmshr", PopCount(io.status.init.init.map(_.valid)))
  XSPerfAccumulate(cacheParams, "nrWorkingBmshr", io.status.take(mshrs+1).last.valid)
  XSPerfAccumulate(cacheParams, "nrWorkingCmshr", io.status.last.valid)
  XSPerfAccumulate(cacheParams, "conflictA", io.a_req.valid && conflict_a)
  XSPerfAccumulate(cacheParams, "conflictByPrefetch", io.a_req.valid && Cat(pretch_block_vec).orR)
  XSPerfAccumulate(cacheParams, "conflictB", io.b_req.valid && conflict_b)
  XSPerfAccumulate(cacheParams, "conflictC", io.c_req.valid && conflict_c)
  //val perfinfo = IO(new Bundle(){
  //  val perfEvents = Output(new PerfEventsBundle(numPCntHcMSHR))
  //})
  val perfinfo = IO(Output(Vec(numPCntHcMSHR, (UInt(6.W)))))
  val perfEvents = Seq(
    ("nrWorkingABCmshr    ", PopCount(io.status.init.init.map(_.valid)) ),
    ("nrWorkingBmshr      ", io.status.take(mshrs+1).last.valid         ),
    ("nrWorkingCmshr      ", io.status.last.valid                       ),
    ("conflictA           ", io.a_req.valid && conflict_a               ),
    ("conflictByPrefetch  ", io.a_req.valid && Cat(pretch_block_vec).orR),
    ("conflictB           ", io.b_req.valid && conflict_b               ),
    ("conflictC           ", io.c_req.valid && conflict_c               ),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.zip(perfEvents).zipWithIndex) {
    perf_out := RegNext(perf)
  }
}
