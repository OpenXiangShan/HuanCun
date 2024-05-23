/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
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
import freechips.rocketchip.util.leftOR
import huancun.noninclusive.{MSHR, ProbeHelper, SliceCtrl, DirResult}
import huancun.prefetch._
import utility._

class Slice()(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle {
    val in = Flipped(TLBundle(edgeIn.bundle))
    val out = TLBundle(edgeOut.bundle)
    val prefetch = prefetchOpt.map(_ => Flipped(new PrefetchIO))
    val ms_status = topDownOpt.map(_ => Vec(mshrsAll, ValidIO(new MSHRStatus)))
    val dir_result = topDownOpt.map(_ => ValidIO(new DirResult))
    val ctl_req = Flipped(DecoupledIO(new CtrlReq()))
    val ctl_resp = DecoupledIO(new CtrlResp())
    val ctl_ecc = DecoupledIO(new EccInfo())
  })
  println(s"clientBits: $clientBits")

  val ctrl = cacheParams.ctrl.map(_ => Module(new SliceCtrl()))

  def ctrl_arb[T <: Data](source: DecoupledIO[T], ctrl: Option[DecoupledIO[T]]): DecoupledIO[T] ={
    if(ctrl.nonEmpty){
      val arbiter = Module(new Arbiter(chiselTypeOf(source.bits), 2))
      arbiter.io.in(0) <> ctrl.get
      arbiter.io.in(1) <> source
      arbiter.io.out
    } else {
      source
    }
  }

  // Inner channels
  val sinkA = Module(new SinkA)
  val sourceB = Module(new SourceB)
  val sinkC = Module(if (cacheParams.inclusive) new inclusive.SinkC else new noninclusive.SinkC)
  val sourceD = Module(new SourceD)
  val sinkE = Module(new SinkE)

  val inBuf = cacheParams.innerBuf
  sinkA.io.a <> inBuf.a(io.in.a)
  io.in.b <> inBuf.b(sourceB.io.b)
  sinkC.io.c <> inBuf.c(io.in.c)
  io.in.d <> inBuf.d(sourceD.io.d)
  sinkE.io.e <> inBuf.e(io.in.e)

  // Outer channles
  val sourceA = Module(new SourceA(edgeOut))
  val sinkB = Module(new SinkB(edgeOut))
  val sourceC = Module(new SourceC(edgeOut))
  val sinkD = Module(new SinkD(edgeOut))
  val sourceE = Module(new SourceE(edgeOut))

  val refillBuffer = Module(new RefillBuffer)

  refillBuffer.io.r <> sourceD.io.bypass_read
  refillBuffer.io.w <> sinkD.io.bypass_write

  val outBuf = cacheParams.outerBuf
  io.out.a <> outBuf.a(sourceA.io.a)
  sinkB.io.b <> outBuf.b(io.out.b)
  val out_c = Wire(io.out.c.cloneType)
  TLArbiter.lowest(edgeOut, out_c, sinkC.io.release, sourceC.io.c)
  io.out.c <> outBuf.c(out_c)
  sinkD.io.d <> outBuf.d(io.out.d)
  io.out.e <> outBuf.e(sourceE.io.e)

  // MSHRs
  val ms = Seq.fill(mshrsAll) {
    if (cacheParams.inclusive)
      Module(new inclusive.MSHR())
    else Module(new noninclusive.MSHR())
  }
  require(mshrsAll == mshrs + 2)
  val ms_abc = ms.init.init
  val ms_bc = ms.init.last
  val ms_c = ms.last

  val dataStorage = Module(new DataStorage())

  dataStorage.io.sinkD_wdata := sinkD.io.bs_wdata
  dataStorage.io.sinkD_waddr <> sinkD.io.bs_waddr
  sourceC.io.bs_rdata := dataStorage.io.sourceC_rdata
  if(ctrl.nonEmpty){
    ctrl.get.io.bs_r_data := dataStorage.io.sourceC_rdata
  }
  sourceD.io.bs_rdata := dataStorage.io.sourceD_rdata
  dataStorage.io.sourceD_raddr <> sourceD.io.bs_raddr
  dataStorage.io.sourceD_waddr <> sourceD.io.bs_waddr
  dataStorage.io.sourceD_wdata <> sourceD.io.bs_wdata
  dataStorage.io.sourceC_raddr <> ctrl_arb(sourceC.io.bs_raddr, ctrl.map(_.io.bs_r_addr))
  dataStorage.io.sinkC_waddr <> ctrl_arb(sinkC.io.bs_waddr, ctrl.map(_.io.bs_w_addr))
  dataStorage.io.sinkC_wdata := (if(ctrl.nonEmpty){
    Mux(ctrl.get.io.bs_w_addr.valid,
      ctrl.get.io.bs_w_data,
      sinkC.io.bs_wdata
    )
  } else sinkC.io.bs_wdata)


  val mshrAlloc = Module(new MSHRAlloc)
  val a_req_buffer = Module(new RequestBuffer(entries = 4))
  val probeHelperOpt = if(cacheParams.inclusive) None else {
    Some(Module(new ProbeHelper(enqDelay = if (cacheParams.sramClkDivBy2) 3 else (if(cacheParams.dirReg) 2 else 1))))
  }

  val a_req = Wire(DecoupledIO(new MSHRRequest()))
  if(cacheParams.inclusive){
    a_req <> sinkA.io.alloc
    mshrAlloc.io.b_req <> sinkB.io.alloc
  } else {
    val probeHelper = probeHelperOpt.get
    block_decoupled(a_req, sinkA.io.alloc, probeHelper.io.full)
    val b_arb = Module(new Arbiter(new MSHRRequest, 2))
    b_arb.io.in(0) <> probeHelper.io.probe
    b_arb.io.in(1) <> sinkB.io.alloc
    mshrAlloc.io.b_req <> b_arb.io.out
  }
  if(prefetchOpt.nonEmpty){
    io.prefetch.get.recv_addr := DontCare
    val alloc_A_arb = Module(new Arbiter(new MSHRRequest, 2))
    alloc_A_arb.io.in(0) <> a_req
    alloc_A_arb.io.in(1) <> pftReqToMSHRReq(io.prefetch.get.req)
    a_req_buffer.io.in <> alloc_A_arb.io.out
  } else {
    a_req_buffer.io.in <> a_req
  }
  mshrAlloc.io.a_req <> a_req_buffer.io.out
  if(ctrl.nonEmpty) { // LLC
    val cmo_req = Pipeline(ctrl.get.io.cmo_req)
    sinkC.io.alloc.ready := mshrAlloc.io.c_req.ready
    cmo_req.ready := !sinkC.io.alloc.valid && mshrAlloc.io.c_req.ready
    mshrAlloc.io.c_req.valid := sinkC.io.alloc.valid || cmo_req.valid
    mshrAlloc.io.c_req.bits := Mux(sinkC.io.alloc.valid,
      sinkC.io.alloc.bits,
      cmo_req.bits
    )
  } else {
    mshrAlloc.io.c_req <> sinkC.io.alloc
  }

  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      mshr.io.id := i.U
      mshr.io.alloc := mshrAlloc.io.alloc(i)
      mshrAlloc.io.status(i) := mshr.io.status
  }
  val c_mshr = ms.last
  val bc_mshr = ms.init.last
  val abc_mshr = ms.init.init

  abc_mshr.zipWithIndex.foreach{
    case (mshr, i) =>
      a_req_buffer.io.mshr_status(i) := mshr.io.status
  }

  val block_bc = c_mshr.io.status.valid
  val block_abc = block_bc || bc_mshr.io.status.valid

  val select_c = c_mshr.io.status.valid
  val select_bc = bc_mshr.io.status.valid

  val bc_mask_latch = RegInit(0.U.asTypeOf(mshrAlloc.io.bc_mask.bits))
  val c_mask_latch = RegInit(0.U.asTypeOf(mshrAlloc.io.c_mask.bits))
  when(mshrAlloc.io.bc_mask.valid) {
    bc_mask_latch := mshrAlloc.io.bc_mask.bits
  }
  when(mshrAlloc.io.c_mask.valid) {
    c_mask_latch := mshrAlloc.io.c_mask.bits
  }
  abc_mshr.zipWithIndex.foreach {
    case (mshr, i) =>
      val bc_disable = bc_mask_latch(i) && select_bc
      val c_disable = c_mask_latch(i) && select_c
      mshr.io.enable := !(bc_disable || c_disable)
  }
  bc_mshr.io.enable := !(c_mask_latch(mshrsAll-2) && select_c)
  c_mshr.io.enable := true.B

  def non_inclusive[T <: RawModule](m: T): noninclusive.MSHR = {
    m.asInstanceOf[noninclusive.MSHR]
  }

  abc_mshr.foreach {
    case mshr: noninclusive.MSHR =>
      mshr.io_c_status.set := c_mshr.io.status.bits.set
      mshr.io_c_status.tag := c_mshr.io.status.bits.tag
      mshr.io_c_status.way := c_mshr.io.status.bits.way
      mshr.io_c_status.nestedReleaseData :=
        c_mshr.io.status.valid && non_inclusive(c_mshr).io_is_nestedReleaseData
      mshr.io_c_status.metaValid := non_inclusive(c_mshr).io_metaValid
      mshr.io_c_status.reqDirty := non_inclusive(c_mshr).io_reqDirty
      mshr.io_b_status.set := bc_mshr.io.status.bits.set
      mshr.io_b_status.tag := bc_mshr.io.status.bits.tag
      mshr.io_b_status.way := bc_mshr.io.status.bits.way
      mshr.io_b_status.nestedProbeAckData :=
        bc_mshr.io.status.valid && non_inclusive(bc_mshr).io_is_nestedProbeAckData
      mshr.io_b_status.probeHelperFinish :=
        bc_mshr.io.status.valid && non_inclusive(bc_mshr).io_probeHelperFinish
      mshr.io_releaseThrough := false.B
      mshr.io_probeAckDataThrough := false.B
    case _: inclusive.MSHR =>
  }

  bc_mshr match {
    case mshr: noninclusive.MSHR =>
      mshr.io_c_status.set := c_mshr.io.status.bits.set
      mshr.io_c_status.tag := c_mshr.io.status.bits.tag
      mshr.io_c_status.way := c_mshr.io.status.bits.way
      mshr.io_c_status.nestedReleaseData :=
        c_mshr.io.status.valid && non_inclusive(c_mshr).io_is_nestedReleaseData
      mshr.io_c_status.metaValid := non_inclusive(c_mshr).io_metaValid
      mshr.io_c_status.reqDirty := non_inclusive(c_mshr).io_reqDirty
      mshr.io_b_status.set := 0.U
      mshr.io_b_status.tag := 0.U
      mshr.io_b_status.way := 0.U
      mshr.io_b_status.nestedProbeAckData := false.B
      mshr.io_b_status.probeHelperFinish := false.B
    case _: inclusive.MSHR =>
  }

  c_mshr match
  {
    case mshr: noninclusive.MSHR =>
      mshr.io_c_status.set := 0.U
      mshr.io_c_status.tag := 0.U
      mshr.io_c_status.way := 0.U
      mshr.io_c_status.nestedReleaseData := false.B
      mshr.io_c_status.metaValid := false.B
      mshr.io_c_status.reqDirty := false.B
      mshr.io_b_status.set := 0.U
      mshr.io_b_status.tag := 0.U
      mshr.io_b_status.way := 0.U
      mshr.io_b_status.nestedProbeAckData := false.B
      mshr.io_b_status.probeHelperFinish := false.B
    case _: inclusive.MSHR =>
  }

  val nestedWb = Wire(new NestedWriteback)
  nestedWb := DontCare
  nestedWb.set := Mux(select_c, c_mshr.io.status.bits.set, bc_mshr.io.status.bits.set)
  nestedWb.tag := Mux(select_c, c_mshr.io.status.bits.tag, bc_mshr.io.status.bits.tag)

  val bc_wb_state = bc_mshr match {
    case mshr: inclusive.MSHR =>
      mshr.io.tasks.dir_write.bits.data.state
    case mshr: noninclusive.MSHR =>
      mshr.io.tasks.dir_write.bits.data.state
  }
  val bc_wb_dirty = bc_mshr match {
    case mshr: inclusive.MSHR =>
      mshr.io.tasks.dir_write.bits.data.dirty
    case mshr: noninclusive.MSHR =>
      mshr.io.tasks.dir_write.bits.data.dirty
  }
  val c_wb_dirty = c_mshr match {
    case mshr: inclusive.MSHR =>
      mshr.io.tasks.dir_write.bits.data.dirty
    case mshr: noninclusive.MSHR =>
      mshr.io.tasks.dir_write.bits.data.dirty
  }
  nestedWb.b_toN := select_bc && !select_c &&
    bc_mshr.io.tasks.dir_write.valid &&
    bc_wb_state === MetaData.INVALID
  nestedWb.b_toB := select_bc && !select_c &&
    bc_mshr.io.tasks.dir_write.valid &&
    bc_wb_state === MetaData.BRANCH
  nestedWb.b_clr_dirty := select_bc && !select_c &&
    bc_mshr.io.tasks.dir_write.valid &&
    !MetaData.isT(bc_wb_state)
  nestedWb.b_set_dirty := select_bc && !select_c &&
    bc_mshr.io.tasks.dir_write.valid &&
    bc_wb_dirty
  nestedWb.c_set_dirty := select_c &&
    c_mshr.io.tasks.dir_write.valid &&
    c_wb_dirty
  val nestedWb_c_set_hit = c_mshr match {
    case c: inclusive.MSHR =>
      ms.map {
        m =>
          select_c && c.io.tasks.tag_write.valid &&
            c.io.tasks.tag_write.bits.tag === m.io.status.bits.tag
      }
    case c: noninclusive.MSHR =>
      ms.map {
        m =>
          select_c && c.io.tasks.tag_write.valid &&
            c.io.tasks.tag_write.bits.tag === m.io.status.bits.tag
      }
  }

  // nested client dir write
  (bc_mshr, c_mshr) match {
    case (bc_mshr: noninclusive.MSHR, c_mshr: noninclusive.MSHR) =>
      nestedWb.clients.get.zipWithIndex.foreach {
        case (n, i) =>
          n.isToN := Mux(
            select_c,
            c_mshr.io.tasks.client_dir_write.valid &&
              c_mshr.io.tasks.client_dir_write.bits.data(i).state === MetaData.INVALID,
            bc_mshr.io.tasks.client_dir_write.valid &&
              bc_mshr.io.tasks.client_dir_write.bits.data(i).state === MetaData.INVALID
          )
          n.isToB := Mux(
            select_c,
            c_mshr.io.tasks.client_dir_write.valid &&
              c_mshr.io.tasks.client_dir_write.bits.data(i).state === MetaData.BRANCH,
            bc_mshr.io.tasks.client_dir_write.valid &&
              bc_mshr.io.tasks.client_dir_write.bits.data(i).state === MetaData.BRANCH
          )
      }
    case (bc_mshr: inclusive.MSHR, c_mshr: inclusive.MSHR) =>
    // skip
    case _ =>
      assert(false)
  }

  abc_mshr.foreach(_.io.nestedwb := nestedWb)
  abc_mshr.zip(nestedWb_c_set_hit.init.init).foreach {
    case (m, set_hit) =>
      m.io.nestedwb.c_set_hit := set_hit
  }

  bc_mshr.io.nestedwb := 0.U.asTypeOf(nestedWb)
  bc_mshr.io.nestedwb.set := c_mshr.io.status.bits.set
  bc_mshr.io.nestedwb.tag := c_mshr.io.status.bits.tag
  bc_mshr.io.nestedwb.c_set_dirty := nestedWb.c_set_dirty
  bc_mshr.io.nestedwb.c_set_hit := nestedWb_c_set_hit.init.last
  bc_mshr match {
    case mshr: noninclusive.MSHR =>
      mshr.io_releaseThrough := false.B
      mshr.io_probeAckDataThrough := Cat(
        abc_mshr.map(non_inclusive).map(_.io_b_status.probeAckDataThrough)
      ).orR
    case _ => // skip
  }

  when(select_c) {
    bc_mshr.io.nestedwb.clients.zip(nestedWb.clients).map {
      case (m, n) =>
        m := n
    }
  }

  c_mshr.io.nestedwb := 0.U.asTypeOf(nestedWb)
  c_mshr match {
    case mshr: noninclusive.MSHR =>
      mshr.io_probeAckDataThrough := false.B
      mshr.io_releaseThrough := Cat(
        (abc_mshr :+ bc_mshr).map(non_inclusive).map(_.io_c_status.releaseThrough)
      ).orR
    case _: inclusive.MSHR =>
  }

  val directory = Module({
    if (cacheParams.inclusive) new inclusive.Directory()
    else new noninclusive.Directory()
  })
  directory.io.read <> ctrl_arb(mshrAlloc.io.dirRead, ctrl.map(_.io.dir_read))
  ctrl.map(c => {
    c.io.dir_result.valid := directory.io.result.valid && directory.io.result.bits.idOH(1, 0) === "b11".U
    c.io.dir_result.bits := directory.io.result.bits
  })

  // Send tasks

  def is_blocked(idx: Int): Bool = {
    if (idx < mshrs) block_abc else if (idx < mshrsAll - 1) block_bc else false.B
  }

  def block_decoupled[T <: Data](sink: DecoupledIO[T], source: DecoupledIO[T], block: Bool) = {
    sink.valid := !block && source.valid
    sink.bits := source.bits
    source.ready := !block && sink.ready
  }

  def block_decoupled[T <: Data](sinks: Seq[DecoupledIO[T]], sources: Seq[DecoupledIO[T]]): Unit = {
    require(sinks.size == sources.size)
    for (((x, y), i) <- sinks.zip(sources).zipWithIndex) {
      block_decoupled(x, y, is_blocked(i))
    }
  }

  def block_b_c[T <: Data](sink: DecoupledIO[T], sources: Seq[DecoupledIO[T]]): Unit = {
    val c_src = sources.last
    val b_src = sources.init.last
    val abc_src = sources.init.init
    val arbiter = Module(new FastArbiter[T](chiselTypeOf(sink.bits), sources.size))
    arbiter.io.in.init.init.zip(abc_src).foreach(x => x._1 <> x._2)
    block_decoupled(arbiter.io.in.init.last, b_src, select_c)
    arbiter.io.in.last <> c_src
    sink <> arbiter.io.out
  }

  def add_ctrl[T <: Data](xs: Seq[DecoupledIO[T]], ctrl: Option[DecoupledIO[T]]): Seq[DecoupledIO[T]] = {
    val last = if(ctrl.nonEmpty) ctrl_arb(xs.last, ctrl) else xs.last
    xs.init :+ last
  }

  // don't allow b write back when c is valid to simplify 'NestedWriteBack'
  block_b_c(
    Pipeline.pipeTo(directory.io.dirWReq),
    add_ctrl(ms.map(_.io.tasks.dir_write), ctrl.map(_.io.s_dir_w))
  )
  arbTasks(sourceA.io.task, ms.map(_.io.tasks.source_a), Some("sourceA"), latch=true)
  arbTasks(sourceB.io.task, ms.map(_.io.tasks.source_b), Some("sourceB"), latch=true)
  arbTasks(sourceC.io.task, ms.map(_.io.tasks.source_c), Some("sourceC"), latch=true)
  arbTasks(sourceD.io.task, ms.map(_.io.tasks.source_d), Some("sourceD"), latch=true)
  arbTasks(sourceE.io.task, ms.map(_.io.tasks.source_e), Some("sourceE"), latch=true)
  arbTasks(sinkA.io.task, ms.map(_.io.tasks.sink_a), Some("sinkA"), latch=true)
  arbTasks(sinkC.io.task, ms.map(_.io.tasks.sink_c), Some("sinkC"), latch=true)
  arbTasks(
    Pipeline.pipeTo(directory.io.tagWReq),
    add_ctrl(ms.map(_.io.tasks.tag_write), ctrl.map(_.io.s_tag_w)),
    Some("tagWrite")
  )
  (directory, ms) match {
    case (dir: noninclusive.Directory, ms: Seq[noninclusive.MSHR]) =>
      block_b_c(
        Pipeline.pipeTo(dir.io.clientDirWReq),
        add_ctrl(
          ms.map(_.io.tasks.client_dir_write),
          ctrl.map(_.io.c_dir_w)
        )
      )
      arbTasks(
        Pipeline.pipeTo(dir.io.clientTagWreq),
        add_ctrl(
          ms.map(_.io.tasks.client_tag_write),
          ctrl.map(_.io.c_tag_w)
        )
      )
    case (_: inclusive.Directory, _: Seq[inclusive.MSHR]) =>
    // skip
    case _ =>
      assert(false)
  }

  def arbTasks[T <: Bundle](
    out:    DecoupledIO[T],
    in:     Seq[DecoupledIO[T]],
    name:   Option[String] = None,
    strict: Boolean = false,
    latch:  Boolean = false
  ) = {
    require(!strict || in.size == mshrsAll)
    if (in.size == mshrsAll) {
      val abc = in.init.init
      val bc = in.init.last
      val c = in.last
      val arbiter = Module(if (latch) new LatchFastArbiter[T](chiselTypeOf(out.bits), abc.size)
                           else new FastArbiter[T](chiselTypeOf(out.bits), abc.size))
      if (name.nonEmpty) arbiter.suggestName(s"${name.get}_task_arb")
      for ((arb, req) <- arbiter.io.in.zip(abc)) {
        arb <> req
      }
      if (strict) {
        out.valid := c.valid ||
        !block_bc && bc.valid ||
        !block_abc && arbiter.io.out.valid
        out.bits := Mux(c.valid, c.bits, Mux(bc.valid, bc.bits, arbiter.io.out.bits))
        c.ready := out.ready
        bc.ready := out.ready && !block_bc
        arbiter.io.out.ready := out.ready && !block_abc
      } else {
        val bc_bits_latch = RegEnable(bc.bits, bc.valid)
        val bc_valid_latch = RegNext(bc.valid)
        val c_bits_latch = RegEnable(c.bits, c.valid)
        val c_valid_latch = RegNext(c.valid)
        val bc_real_valid = bc.valid && bc_valid_latch
        val c_real_valid = c.valid && c_valid_latch
        out.valid := c_real_valid || bc_real_valid || arbiter.io.out.valid
        out.bits := Mux(c_real_valid, c_bits_latch, Mux(bc_real_valid, bc_bits_latch, arbiter.io.out.bits))
        c.ready := out.ready && c_valid_latch
        bc.ready := out.ready && bc_valid_latch && !c_real_valid
        arbiter.io.out.ready := out.ready && !c_real_valid && !bc_real_valid
      }
    } else {
      val arbiter = Module(new FastArbiter[T](chiselTypeOf(out.bits), in.size))
      if (name.nonEmpty) arbiter.suggestName(s"${name.get}_task_arb")
      for ((arb, req) <- arbiter.io.in.zip(in)) {
        arb <> req
      }
      out <> arbiter.io.out
    }
  }

  io.prefetch.foreach { pft =>

    // connect abc mshrs to prefetcher
    arbTasks(
      pft.train,
      abc_mshr.map(_.io.tasks.prefetch_train.get),
      Some("prefetchTrain")
    )
    arbTasks(
      pft.resp,
      abc_mshr.map(_.io.tasks.prefetch_resp.get),
      Some("prefetchResp")
    )
    for (mshr <- Seq(bc_mshr, c_mshr)) {
      mshr.io.tasks.prefetch_train.foreach(_.ready := true.B)
      mshr.io.tasks.prefetch_resp.foreach(_.ready := true.B)
    }
  }

  // Resps to MSHRs
  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      mshr.io.resps.sink_c.valid := sinkC.io.resp.valid && sinkC.io.resp.bits.set === mshr.io.status.bits.set
      mshr.io.resps.sink_d.valid := sinkD.io.resp.valid && sinkD.io.resp.bits.source === i.U
      mshr.io.resps.sink_e.valid := sinkE.io.resp.valid && sinkE.io.resp.bits.sink === i.U
      mshr.io.resps.source_d.valid := sourceD.io.resp.valid && sourceD.io.resp.bits.sink === i.U
      mshr.io.resps.sink_c_ack.valid := sinkC.io.taskack.valid && sinkC.io.taskack.bits.sink === i.U
      mshr.io.resps.sink_c.bits := sinkC.io.resp.bits
      mshr.io.resps.sink_d.bits := sinkD.io.resp.bits
      mshr.io.resps.sink_e.bits := sinkE.io.resp.bits
      mshr.io.resps.source_d.bits := sourceD.io.resp.bits
      mshr.io.resps.sink_c_ack.bits := sinkC.io.taskack.bits
  }
  c_mshr.io.resps.sink_c.valid := false.B

  // Directory read results to MSHRs (deprecated)
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

  val is_ctrl_dir_res = directory.io.result.bits.idOH(1, 0) === "b11".U
  val dirReg = directory.io.result
  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      val dirResultMatch = !is_ctrl_dir_res && directory.io.result.valid && directory.io.result.bits.idOH(i)
      mshr.io.dirResult.bits := dirReg.bits
      mshr.io.dirResult.valid := RegNext(dirResultMatch, false.B)
  }
  probeHelperOpt.foreach(h => {
    h.io.dirResult.bits := dirReg.bits
    h.io.dirResult.valid := RegNext(dirReg.valid, false.B)
  })

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

  // there must be 1 match so we can use Mux1H
  val sinkD_status = Mux1H(ms.map(_.io.status).zipWithIndex.map{
    case (s, i) => (i.U === sinkD.io.resp.bits.source) -> s
  })
  sinkD.io.way := sinkD_status.bits.way_reg
  sinkD.io.set := sinkD_status.bits.set
  sinkD.io.inner_grant := sinkD_status.bits.will_grant_data
  sinkD.io.save_data_in_bs := sinkD_status.bits.will_save_data

  sinkC.io.sourceD_r_hazard <> sourceD.io.sourceD_r_hazard
  sinkD.io.sourceD_r_hazard <> sourceD.io.sourceD_r_hazard

  sinkA.io.d_pb_pop <> sourceD.io.pb_pop
  sinkA.io.d_pb_beat <> sourceD.io.pb_beat

  sinkA.io.a_pb_pop <> sourceA.io.pb_pop
  sinkA.io.a_pb_beat <> sourceA.io.pb_beat

  val tag_err = RegNext(Cat(ms.map(m => m.io.ecc.valid)).orR, false.B)
  val tag_err_info = RegNext(MuxCase(
    ms.head.io.ecc.bits,
    ms.map(m => (m.io.status.valid && m.io.ecc.valid, m.io.ecc.bits))
  ))
  val data_err = RegNext(dataStorage.io.ecc.valid, false.B)
  val data_err_info = RegNext(dataStorage.io.ecc.bits)

  io.ctl_ecc.bits := RegNext(Mux(tag_err, tag_err_info, data_err_info))
  io.ctl_ecc.valid := RegNext(tag_err | data_err, false.B)
  if (ctrl.nonEmpty) {
    ctrl.get.io.req <> io.ctl_req
    io.ctl_resp <> ctrl.get.io.resp
  } else {
    io.ctl_req <> DontCare
    io.ctl_resp <> DontCare
    io.ctl_req.ready := false.B
    io.ctl_resp.valid := false.B
  }

  def pftReqToMSHRReq(pftReq: DecoupledIO[PrefetchReq]): DecoupledIO[MSHRRequest] = {
    val mshrReq = Wire(DecoupledIO(new MSHRRequest()))
    val address = Cat(pftReq.bits.tag, pftReq.bits.set, 0.U(offsetBits.W))
    val (tag, set, off) = parseAddress(address)
    mshrReq.valid := pftReq.valid
    mshrReq.bits.opcode := TLMessages.Hint
    mshrReq.bits.param := Mux(pftReq.bits.needT, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ)
    mshrReq.bits.size := log2Up(blockBytes).U
    mshrReq.bits.source := pftReq.bits.source
    mshrReq.bits.tag := tag
    mshrReq.bits.set := set
    mshrReq.bits.off := off
    mshrReq.bits.mask := Fill(edgeOut.manager.beatBytes, 1.U(1.W))
    mshrReq.bits.channel := "b001".U
    mshrReq.bits.needHint.foreach(_ := false.B)
    mshrReq.bits.isPrefetch.foreach(_ := true.B)
    mshrReq.bits.isBop.foreach(_ := pftReq.bits.isBOP)
    mshrReq.bits.alias.foreach(_ := DontCare)
    mshrReq.bits.preferCache := true.B
    mshrReq.bits.fromProbeHelper := false.B
    mshrReq.bits.fromCmoHelper := false.B
    mshrReq.bits.bufIdx := DontCare
    mshrReq.bits.dirty := false.B
    mshrReq.bits.isHit := true.B
    mshrReq.bits.needProbeAckData.foreach(_ := false.B)
    mshrReq.bits.reqSource := pftReq.bits.pfSource
    pftReq.ready := mshrReq.ready
    mshrReq
  }

  topDownOpt.foreach (
    _ => {
      io.ms_status.get.zip(ms).foreach {
        case (out, mshr) =>
          out := mshr.io.status
      }
      io.dir_result.get := directory.io.result
    }
  )

  val perfinfo = IO(Output(Vec(numPCntHc, (UInt(6.W)))))
  perfinfo := DontCare
  if(!cacheParams.inclusive){
    val reqbuff_perf     = a_req_buffer.perfEvents.map(_._1).zip(a_req_buffer.perfinfo)
    val mshralloc_perf   = mshrAlloc.perfEvents.map(_._1).zip(mshrAlloc.perfinfo)
    val probq_perf       = probeHelperOpt.get.perfEvents.map(_._1).zip(probeHelperOpt.get.perfinfo)
    val direct_perf      = directory.asInstanceOf[noninclusive.Directory].perfEvents.map(_._1).zip(directory.asInstanceOf[noninclusive.Directory].perfinfo)
    val huancun_perf = reqbuff_perf ++ mshralloc_perf ++ probq_perf ++ direct_perf

    for (((perf_out,(perf_name,perf)),i) <- perfinfo.zip(huancun_perf).zipWithIndex) {
      perf_out := perf
      if(print_hcperfcounter){
        println(s"Huancun perf $i: $perf_name")
      }
    }
  }
}
