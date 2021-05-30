package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy.IdRange
import huancun.utils.ParallelMux

class Slice(inputIds: Seq[IdRange])(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle {
    val inSeq = MixedVec(edgeInSeq.map(e => Flipped(TLBundle(e.bundle))))
    val out = TLBundle(edgeOut.bundle)
  })

  val (cohEdges, cohIns) = edgeInSeq.zip(io.inSeq).filter(_._1.client.anySupportProbe).unzip
  val cohIds = edgeInSeq.zip(inputIds).filter(_._1.client.anySupportProbe).map(_._2)

  // Inner channels
  val sinkAs = edgeInSeq.map(e => Module(new SinkA(e)))
  val sourceBs = cohEdges.map(e => Module(new SourceB(e)))
  val sinkCs = cohEdges.map(e => Module(new SinkC(e)))
  val sourceDs = edgeInSeq.map(e => Module(new SourceD(e)))
  val sinkEs = cohEdges.map(e => Module(new SinkE(e)))

  io.inSeq.zip(sinkAs).foreach { case (bus, mod) => mod.io.a <> bus.a }
  cohIns.zip(sourceBs).foreach { case (bus, mod) => bus.b <> mod.io.b }
  cohIns.zip(sinkCs).foreach { case (bus, mod) => mod.io.c <> bus.c }
  io.inSeq.zip(sourceDs).foreach { case (bus, mod) => bus.d <> mod.io.d }
  cohIns.zip(sinkEs).foreach { case (bus, mod) => mod.io.e <> bus.e }

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

  // Connect to MSHR Allocator
  val mshrAlloc = Module(new MSHRAlloc(edgeInSeq.size - cohEdges.size, cohEdges.size))
  mshrAlloc.io.cohClints.map(_.a).zip(sinkAs.filter(_.edge.client.anySupportProbe)).foreach(c => c._1 <> c._2.io.alloc)
  mshrAlloc.io.cohClints.map(_.b).zip(Seq(sinkB)).foreach(c => c._1 <> c._2.io.alloc)
  mshrAlloc.io.cohClints.map(_.c).zip(sinkCs).foreach(c => c._1 <> c._2.io.alloc)
  mshrAlloc.io.relaxClints
    .map(_.a)
    .zip(sinkAs.filter(!_.edge.client.anySupportProbe))
    .foreach(c => c._1 <> c._2.io.alloc)

  // MSHRs
  val ms = Seq.fill(mshrsAll) { Module(new MSHR()) }
  require(mshrsAll == mshrs + 2)
  val ms_abc = ms.init.init
  val ms_bc = ms.init.last
  val ms_c = ms.last

  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      mshr.io.id := i.U
      mshr.io.alloc := mshrAlloc.io.alloc(i)
      mshrAlloc.io.status(i) := mshr.io.status
  }

  mshrAlloc.io.alloc <> VecInit(ms.map(_.io.alloc))

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
  arbTasks(sourceC.io.task, ms.map(_.io.tasks.source_c), Some("sourceC"))
  arbTasks(sourceE.io.task, ms.map(_.io.tasks.source_e), Some("sourceE"))
  arbTasks(directory.io.tagWReq, ms.map(_.io.tasks.tag_write), Some("tagWrite"))

  def validSourceId(x: UInt, range: IdRange): Bool = {
    require(isPow2(range.start) || range.start == 0)
    require(isPow2(range.size))
    /*  eg:
        10xxxx (32, 48) bit5 == 0
        01xxxx (16, 32) bit4 =0 0
        00xxxx (0, 16)  (bit5, bit4) == 00
     */
    range.start match {
      case 0 =>
        val highBits = x.getWidth - log2Up(range.size)
        if (highBits > 0) !x.head(highBits).orR() else true.B
      case n =>
        x(log2Up(n))
    }
  }

  def validInnerTask[T <: InnerTask](t: DecoupledIO[T], idRange: IdRange): Bool = {
    t.valid && validSourceId(t.bits.sourceId, idRange)
  }

  def arbInnerTasks[T <: InnerTask](
    out:     DecoupledIO[T],
    in:      Seq[DecoupledIO[T]],
    idRange: IdRange,
    name:    Option[String] = None
  ) = {
    for ((arb, req) <- arbTasks(out, in, name).io.in.zip(in)) {
      arb.valid := validInnerTask(req, idRange)
    }
  }

  def arbTasks[T <: Bundle](out: DecoupledIO[T], in: Seq[DecoupledIO[T]], name: Option[String] = None) = {
    val arbiter = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) arbiter.suggestName(s"${name.get}_task_arb")
    for ((arb, req) <- arbiter.io.in.zip(in)) {
      arb <> req
    }
    out <> arbiter.io.out
    arbiter
  }

  for ((sinkA, idRange) <- sinkAs.zip(inputIds)) {
    arbInnerTasks(sinkA.io.task, ms.map(_.io.tasks.sink_a), idRange, Some("sinkA"))
  }
  for ((sourceD, idRange) <- sourceDs.zip(inputIds)) {
    arbInnerTasks(sourceD.io.task, ms.map(_.io.tasks.source_d), idRange, Some("sourceD"))
  }
  // for ((sourceB, idRange) <- sourceBs.zip(cohIds)) {
  //   arbInnerTasks(sourceB.io.task, ms.map(_.io.tasks.source_b), idRange, Some("sourceB"))
  // }
  sourceBs.zipWithIndex.foreach { case (sourceB, i) =>
    val arbiter = arbTasks(sourceB.io.task, ms.map(_.io.tasks.source_b), Some("sourceB"))
    for ((in, req) <- arbiter.io.in.zip(ms.map(_.io.tasks.source_b))) {
      in.valid := req.valid && req.bits.clients(i).asBool
    }
  }
  for ((sinkC, idRange) <- sinkCs.zip(cohIds)) {
    arbInnerTasks(sinkC.io.task, ms.map(_.io.tasks.sink_c), idRange, Some("sinkC"))
  }

  // Resps to MSHRs
  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      val sinkCRespMatch = sinkCs.map(s => s.io.resp.valid && s.io.resp.bits.set === mshr.io.status.bits.set)
      val sinkERespMatch = sinkEs.map(s => s.io.resp.valid && s.io.resp.bits.sink === i.U)
      mshr.io.resps.sink_c.valid := Cat(sinkCRespMatch).orR
      mshr.io.resps.sink_d.valid := sinkD.io.resp.valid && sinkD.io.resp.bits.source === i.U
      mshr.io.resps.sink_e.valid := Cat(sinkERespMatch).orR
      mshr.io.resps.sink_c.bits := ParallelMux(sinkCRespMatch.zip(sinkCs.map(_.io.resp.bits)))
      mshr.io.resps.sink_d.bits := sinkD.io.resp.bits
      mshr.io.resps.sink_e.bits := ParallelMux(sinkERespMatch.zip(sinkEs.map(_.io.resp.bits)))
  }

  // Directory read results to MSHRs
  ms.zipWithIndex.foreach {
    case (mshr, i) =>
      val dirResultMatch = directory.io.results.map(r => r.valid && r.bits.idOH(i))
      mshr.io.dirResult.valid := Cat(dirResultMatch).orR()
      mshr.io.dirResult.bits := ParallelMux(dirResultMatch.zip(directory.io.results.map(_.bits)))
  }

}
