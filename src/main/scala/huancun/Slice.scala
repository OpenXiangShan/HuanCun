package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy.{IdRange}

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
  val mshrAlloc = Module(new MSHRAlloc(sinkAs.size, 1, sinkCs.size))
  mshrAlloc.io.inA.zip(sinkAs).foreach { case (m, a) => m <> a.io.alloc }
  mshrAlloc.io.inB.head <> sinkB.io.alloc
  mshrAlloc.io.inC.zip(sinkCs).foreach { case (m, c) => m <> c.io.alloc }

  // MSHRs
  val ms = Seq.fill(mshrs) { Module(new MSHR()) }
  ms.zipWithIndex.map {
    case (mshr, i) =>
      mshr.io.id := i.U
  }

  // Send tasks
  val sourceATaskArb = Module(new Arbiter(new SourceAReq, mshrs))
  val sourceCTaskArb = Module(new Arbiter(new SourceCReq, mshrs))
  val sourceETaskArb = Module(new Arbiter(new SourceEReq, mshrs))

  for (i <- 0 until mshrs) {
    sourceATaskArb.io.in(i) <> ms(i).io.tasks.source_a
    sourceCTaskArb.io.in(i) <> ms(i).io.tasks.source_c
    sourceETaskArb.io.in(i) <> ms(i).io.tasks.source_e
  }

  sourceA.io.task <> sourceATaskArb.io.out
  sourceC.io.task <> sourceCTaskArb.io.out
  sourceE.io.task <> sourceETaskArb.io.out

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
    val arbiter = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) arbiter.suggestName(s"${name.get}_task_arb")
    for ((arb, req) <- arbiter.io.in.zip(in)) {
      arb <> req
      arb.valid := validInnerTask(req, idRange)
    }
    out <> arbiter.io.out
  }

  for ((sinkA, idRange) <- sinkAs.zip(inputIds)) {
    arbInnerTasks(sinkA.io.task, ms.map(_.io.tasks.sink_a), idRange, Some("sinkA"))
  }
  for ((sourceD, idRange) <- sourceDs.zip(inputIds)) {
    arbInnerTasks(sourceD.io.task, ms.map(_.io.tasks.source_d), idRange, Some("sourceD"))
  }
  for ((sourceB, idRange) <- sourceBs.zip(cohIds)) {
    arbInnerTasks(sourceB.io.task, ms.map(_.io.tasks.source_b), idRange, Some("sourceB"))
  }
  for ((sinkC, idRange) <- sinkCs.zip(cohIds)) {
    arbInnerTasks(sinkC.io.task, ms.map(_.io.tasks.sink_c), idRange, Some("sinkC"))
  }

}
