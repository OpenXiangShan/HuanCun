package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{DecoupledIO, MixedVec, ValidIO}
import freechips.rocketchip.tilelink._

class Slice()(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle {
    val inSeq = MixedVec(edgeInSeq.map(e => Flipped(TLBundle(e.bundle))))
    val out = TLBundle(edgeOut.bundle)
  })

  val (cohEdges, cohIns) = edgeInSeq.zip(io.inSeq).filter(_._1.client.anySupportProbe).unzip

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

}
