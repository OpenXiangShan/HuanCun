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

  val directory = Module(new Directory)
  directory.io.reads <> mshrAlloc.io.dirReads

  // Send tasks
  val mshrGroups = ms.grouped((ms.size + dirWritePorts - 1) / dirWritePorts)
  for ((grp, i) <- mshrGroups.zipWithIndex) {
    val dirWriteArb = Module(new Arbiter(new DirWrite, grp.size))
    dirWriteArb.io.in <> VecInit(grp.map(_.io.tasks.dir_write))
    directory.io.dirWReqs(i) <> dirWriteArb.io.out
  }

  arbTasks(sourceA.io.task, ms.map(_.io.tasks.source_a), Some("sourceA"))
  arbTasks(sourceB.io.task, ms.map(_.io.tasks.source_b), Some("sourceB"))
  arbTasks(sourceC.io.task, ms.map(_.io.tasks.source_c), Some("sourceC"))
  arbTasks(sourceD.io.task, ms.map(_.io.tasks.source_d), Some("sourceD"))
  arbTasks(sourceE.io.task, ms.map(_.io.tasks.source_e), Some("sourceE"))
  arbTasks(sinkA.io.task, ms.map(_.io.tasks.sink_a), Some("sinkA"))
  arbTasks(sinkC.io.task, ms.map(_.io.tasks.sink_c), Some("sinkC"))
  arbTasks(directory.io.tagWReq, ms.map(_.io.tasks.tag_write), Some("tagWrite"))

  def arbTasks[T <: Bundle](out: DecoupledIO[T], in: Seq[DecoupledIO[T]], name: Option[String] = None) = {
    val arbiter = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) arbiter.suggestName(s"${name.get}_task_arb")
    for ((arb, req) <- arbiter.io.in.zip(in)) {
      arb <> req
    }
    out <> arbiter.io.out
    arbiter
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

  // Provide MSHR info for sinkD
  sinkD.io.way := VecInit(ms.map(_.io.status.bits.way))(sinkD.io.resp.bits.source)
  sinkD.io.set := VecInit(ms.map(_.io.status.bits.set))(sinkD.io.resp.bits.source)

}
