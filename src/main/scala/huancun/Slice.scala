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
import huancun.utils.ParallelMux
import huancun.prefetch._

class Slice()(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle {
    val in = Flipped(TLBundle(edgeIn.bundle))
    val out = TLBundle(edgeOut.bundle)
  })

  // Inner channels
  val sinkA = Module(new SinkA)
  val sourceB = Module(new SourceB)
  val sinkC = Module(new SinkC)
  val sourceD = Module(new SourceD)
  val sinkE = Module(new SinkE)

  sinkA.io.a <> io.in.a
  io.in.b <> sourceB.io.b
  sinkC.io.c <> io.in.c
  io.in.d <> sourceD.io.d
  sinkE.io.e <> io.in.e

  // Outer channles
  val sourceA = Module(new SourceA(edgeOut))
  val sinkB = Module(new SinkB(edgeOut))
  val sourceC = Module(new SourceC(edgeOut))
  val sinkD = Module(new SinkD(edgeOut))
  val sourceE = Module(new SourceE(edgeOut))

  io.out.a <> sourceA.io.a
  sinkB.io.b <> io.out.b
  io.out.c <> sourceC.io.c
  sinkD.io.d <> io.out.d
  io.out.e <> sourceE.io.e

  // MSHRs
  val ms = Seq.fill(mshrsAll) { Module(new MSHR()) }
  require(mshrsAll == mshrs + 2)
  val ms_abc = ms.init.init
  val ms_bc = ms.init.last
  val ms_c = ms.last

  val dataStorage = Module(new DataStorage())

  dataStorage.io.sinkD_wdata := sinkD.io.bs_wdata
  dataStorage.io.sinkD_waddr <> sinkD.io.bs_waddr
  sourceC.io.bs_rdata := dataStorage.io.sourceC_rdata
  dataStorage.io.sourceC_raddr <> sourceC.io.bs_raddr
  sourceD.io.bs_rdata := dataStorage.io.sourceD_rdata
  dataStorage.io.sourceD_raddr <> sourceD.io.bs_raddr
  dataStorage.io.sourceD_waddr <> sourceD.io.bs_waddr
  dataStorage.io.sourceD_wdata <> sourceD.io.bs_wdata
  dataStorage.io.sinkC_waddr <> sinkC.io.bs_waddr
  dataStorage.io.sinkC_wdata <> sinkC.io.bs_wdata

  val mshrAlloc = Module(new MSHRAlloc)

  mshrAlloc.io.a_req <> sinkA.io.alloc
  mshrAlloc.io.b_req <> sinkB.io.alloc
  mshrAlloc.io.c_req <> sinkC.io.alloc

  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      mshr.io.id := i.U
      mshr.io.alloc := mshrAlloc.io.alloc(i)
      mshrAlloc.io.status(i) := mshr.io.status
  }

  val c_mshr = ms.last
  val bc_mshr = ms.init.last
  val abc_mshr = ms.init.init

  val select_c = c_mshr.io.status.valid
  val select_bc = bc_mshr.io.status.valid

  val nestedWb = Wire(new NestedWriteback)
  nestedWb.set := Mux(select_c, c_mshr.io.status.bits.set, bc_mshr.io.status.bits.set)
  nestedWb.tag := Mux(select_c, c_mshr.io.status.bits.tag, bc_mshr.io.status.bits.tag)
  nestedWb.b_toN := select_bc &&
    bc_mshr.io.tasks.dir_write.valid &&
    bc_mshr.io.tasks.dir_write.bits.data.state === MetaData.INVALID
  nestedWb.b_toB := select_bc &&
    bc_mshr.io.tasks.dir_write.valid &&
    bc_mshr.io.tasks.dir_write.bits.data.state === MetaData.BRANCH
  nestedWb.b_clr_dirty := select_bc && bc_mshr.io.tasks.dir_write.valid
  nestedWb.c_set_dirty := select_c &&
    c_mshr.io.tasks.dir_write.valid &&
    c_mshr.io.tasks.dir_write.bits.data.dirty

  abc_mshr.foreach(_.io.nestedwb := nestedWb)

  bc_mshr.io.nestedwb := 0.U.asTypeOf(nestedWb)
  bc_mshr.io.nestedwb.set := c_mshr.io.status.bits.set
  bc_mshr.io.nestedwb.tag := c_mshr.io.status.bits.tag
  bc_mshr.io.nestedwb.c_set_dirty := nestedWb.c_set_dirty

  c_mshr.io.nestedwb := 0.U.asTypeOf(nestedWb)

  val directory = Module(new Directory)
  directory.io.reads <> mshrAlloc.io.dirReads

  // Send tasks
  directory.io.dirWReqs <> ms.map(_.io.tasks.dir_write)
  arbTasks(sourceA.io.task, ms.map(_.io.tasks.source_a), Some("sourceA"))
  arbTasks(sourceB.io.task, ms.map(_.io.tasks.source_b), Some("sourceB"))
  arbTasks(sourceC.io.task, ms.map(_.io.tasks.source_c), Some("sourceC"))
  arbTasks(sourceD.io.task, ms.map(_.io.tasks.source_d), Some("sourceD"))
  arbTasks(sourceE.io.task, ms.map(_.io.tasks.source_e), Some("sourceE"))
  arbTasks(sinkA.io.task, ms.map(_.io.tasks.sink_a), Some("sinkA"))
  arbTasks(sinkC.io.task, ms.map(_.io.tasks.sink_c), Some("sinkC"))
  arbTasks(directory.io.tagWReq, ms.map(_.io.tasks.tag_write), Some("tagWrite"))

  // arbTasks(pft.io.train, abc_mshr.map(_.io.tasks.prefetch_train), Some("prefetchTrain"))

  def arbTasks[T <: Bundle](out: DecoupledIO[T], in: Seq[DecoupledIO[T]], name: Option[String] = None) = {
    if (in.size == mshrsAll) {
      val abc = in.init.init
      val bc = in.init.last
      val c = in.last
      val arbiter = Module(new RRArbiter[T](chiselTypeOf(out.bits), abc.size))
      if (name.nonEmpty) arbiter.suggestName(s"${name.get}_task_arb")
      for ((arb, req) <- arbiter.io.in.zip(abc)) {
        arb <> req
      }
      out.valid := c.valid || bc.valid || arbiter.io.out.valid
      out.bits := Mux(c.valid, c.bits, Mux(bc.valid, bc.bits, arbiter.io.out.bits))
      c.ready := out.ready
      bc.ready := out.ready && !c.valid
      arbiter.io.out.ready := out.ready && !c.valid && !bc.valid
    } else {
      val arbiter = Module(new RRArbiter[T](chiselTypeOf(out.bits), in.size))
      for ((arb, req) <- arbiter.io.in.zip(in)) {
        arb <> req
      }
      out <> arbiter.io.out
    }
  }

  if (enablePrefetch) {
    val pft = Module(new Prefetcher)

    val alloc_A_arb = Module(new Arbiter(new MSHRRequest, 2))
    alloc_A_arb.io.in(0) <> sinkA.io.alloc
    alloc_A_arb.io.in(1) <> pft.io.req
    mshrAlloc.io.a_req <> alloc_A_arb.io.out

    arbTasks(
      pft.io.train,
      abc_mshr.map(_.io.tasks.prefetch_train.getOrElse(0.U.asTypeOf(DecoupledIO(new PrefetchTrain)))),
      Some("prefetchTrain")
    )
    arbTasks(
      pft.io.resp,
      abc_mshr.map(_.io.tasks.prefetch_resp.getOrElse(0.U.asTypeOf(DecoupledIO(new PrefetchResp)))),
      Some("prefetchResp")
    )

    for (mshr <- Seq(bc_mshr, c_mshr)) {
      mshr.io.tasks.prefetch_train.map(_.ready := true.B)
      mshr.io.tasks.prefetch_resp.map(_.ready := true.B)
    }
  }

  // Resps to MSHRs
  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      mshr.io.resps.sink_c.valid := sinkC.io.resp.valid && sinkC.io.resp.bits.set === mshr.io.status.bits.set
      mshr.io.resps.sink_d.valid := sinkD.io.resp.valid && sinkD.io.resp.bits.source === i.U
      mshr.io.resps.sink_e.valid := sinkE.io.resp.valid && sinkE.io.resp.bits.sink === i.U
      mshr.io.resps.sink_c.bits := sinkC.io.resp.bits
      mshr.io.resps.sink_d.bits := sinkD.io.resp.bits
      mshr.io.resps.sink_e.bits := sinkE.io.resp.bits
  }

  // Directory read results to MSHRs
  def regFn[T <: Data](x: Valid[T]): Valid[T] = {
    if (cacheParams.dirReg) {
      val v = RegNext(x.valid, false.B)
      val bits = RegEnable(x.bits, x.valid)
      val ret = Wire(x.cloneType)
      ret.valid := v
      ret.bits := bits
      ret
    } else x
  }
  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      val dirResultMatch = directory.io.results.map(r => r.valid && r.bits.idOH(i))
      val dirResult = Wire(Valid(new DirResult))
      dirResult.valid := Cat(dirResultMatch).orR()
      dirResult.bits := Mux1H(dirResultMatch.zip(directory.io.results.map(_.bits)))
      mshr.io.dirResult := regFn(dirResult)
  }

  // Provide MSHR info for sinkC, sinkD
  sinkC.io.way := Mux(
    bc_mshr.io.status.valid &&
      bc_mshr.io.status.bits.set === sinkC.io.resp.bits.set,
    bc_mshr.io.status.bits.way,
    Mux1H(
      abc_mshr.map(m => m.io.status.valid && m.io.status.bits.set === sinkC.io.resp.bits.set),
      abc_mshr.map(m => m.io.status.bits.way)
    )
  )

  VecInit(ms.map(_.io.status.bits.way))(sinkC.io.resp.bits.source)
  sinkD.io.way := VecInit(ms.map(_.io.status.bits.way))(sinkD.io.resp.bits.source)
  sinkD.io.set := VecInit(ms.map(_.io.status.bits.set))(sinkD.io.resp.bits.source)

  sinkC.io.sourceD_r_hazard <> sourceD.io.sourceD_r_hazard
  sinkD.io.sourceD_r_hazard <> sourceD.io.sourceD_r_hazard

}
