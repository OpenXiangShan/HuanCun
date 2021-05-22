package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{DecoupledIO, MixedVec, ValidIO, Arbiter}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy.{IdRange}

class Slice(inputIds: Seq[IdRange])(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle {
    val inSeq = MixedVec(edgeInSeq.map(e => Flipped(TLBundle(e.bundle))))
    val out = TLBundle(edgeOut.bundle)
  })

  val (cohEdges, cohIns) = edgeInSeq.zip(io.inSeq).filter(_._1.client.anySupportProbe).unzip
  val cohIds = edgeInSeq.zip(inputIds).filter(_._1.client.anySupportProbe).unzip._2

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
  ms.zipWithIndex.map { case (mshr, i) =>
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


  for ((sinkA, idRange) <- sinkAs.zip(inputIds)) {
    val sinkATaskArb = Module(new Arbiter(new SinkAReq, mshrs))
    for (i <- 0 until mshrs) {
      sinkATaskArb.io.in(i) <> ms(i).io.tasks.sink_a
      sinkATaskArb.io.in(i).valid := ms(i).io.tasks.sink_a.valid && idRange.contains(ms(i).io.tasks.sink_a.bits.sourceId)
    }
    sinkA.io.task <> sinkATaskArb.io.out
  }

  for ((sourceD, idRange) <- sourceDs.zip(inputIds)) {
    val sourceDTaskArb = Module(new Arbiter(new SourceDReq, mshrs))
    for (i <- 0 until mshrs) {
      sourceDTaskArb.io.in(i) <> ms(i).io.tasks.source_d
      sourceDTaskArb.io.in(i).valid := ms(i).io.tasks.source_d.valid && idRange.contains(ms(i).io.tasks.source_d.bits.sourceId)
    }
    sourceD.io.task <> sourceDTaskArb.io.out
  }

  for ((sourceB, cohIdRange) <- sourceBs.zip(cohIds)) {
    val sourceBTaskArb = Module(new Arbiter(new SourceBReq, mshrs))
    for (i <- 0 until mshrs) {
      sourceBTaskArb.io.in(i) <> ms(i).io.tasks.source_b
      sourceBTaskArb.io.in(i).valid := ms(i).io.tasks.source_b.valid && cohIdRange.contains(ms(i).io.tasks.source_b.bits.sourceId)
    }
    sourceB.io.task <> sourceBTaskArb.io.out
  }

  for ((sinkC, cohIdRange) <- sinkCs.zip(cohIds)) {
    val sinkCTaskArb = Module(new Arbiter(new SinkCReq, mshrs))
    for (i <- 0 until mshrs) {
      sinkCTaskArb.io.in(i) <> ms(i).io.tasks.sink_c
      sinkCTaskArb.io.in(i).valid := ms(i).io.tasks.sink_c.valid && cohIdRange.contains(ms(i).io.tasks.sink_c.bits.sourceId)
    }
    sinkC.io.task <> sinkCTaskArb.io.out
  }

}
