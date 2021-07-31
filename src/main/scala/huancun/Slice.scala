package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import huancun.utils.ParallelMux

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

  def arbTasks[T <: Bundle](out: DecoupledIO[T], in: Seq[DecoupledIO[T]], name: Option[String] = None) = {
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
  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      val dirResultMatch = directory.io.results.map(r => r.valid && r.bits.idOH(i))
      mshr.io.dirResult.valid := Cat(dirResultMatch).orR()
      mshr.io.dirResult.bits := Mux1H(dirResultMatch.zip(directory.io.results.map(_.bits)))
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
