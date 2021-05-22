package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{DecoupledIO, MixedVec, ValidIO}
import freechips.rocketchip.tilelink.{
  TLArbiter,
  TLBundle,
  TLBundleA,
  TLBundleB,
  TLBundleC,
  TLBundleD,
  TLBundleE,
  TLBundleParameters,
  TLEdgeIn,
  TLEdgeOut
}

class MSHRRequest extends Bundle

class SinkAReq extends Bundle
class SinkA(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val a = Flipped(DecoupledIO(new TLBundleA(edge.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
    val task = Flipped(DecoupledIO(new SinkAReq))
  })
}

class SourceBReq extends Bundle

class SourceB(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val b = DecoupledIO(new TLBundleB(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceBReq))
  })
}

class SinkCReq extends Bundle
class SinkC(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = Flipped(DecoupledIO(new TLBundleC(edge.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
    val resp = ValidIO(new TLBundleC(edge.bundle))
    val task = Flipped(DecoupledIO(new SinkCReq))
  })
}

class SourceDReq extends Bundle
class SourceD(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val d = DecoupledIO(new TLBundleD(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceDReq))
  })
}

class SinkE(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val e = Flipped(DecoupledIO(new TLBundleE(edge.bundle)))
    val resp = ValidIO(new TLBundleE(edge.bundle))
  })
}

class SourceAReq extends Bundle
class SourceA(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val a = DecoupledIO(new TLBundleA(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceAReq))
  })
}

class SinkB(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val b = Flipped(DecoupledIO(new TLBundleB(edge.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
  })
}

class SourceCReq extends Bundle
class SourceC(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = DecoupledIO(new TLBundleC(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceCReq))
  })
}

class SinkD(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val d = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val resp = ValidIO(new TLBundleD(edge.bundle))
  })
}

class SourceEReq extends Bundle
class SourceE(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val e = DecoupledIO(new TLBundleE(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceEReq))
  })
}

class MSHRStatus extends Bundle
class MSHRTasks extends Bundle {
  // inner
  val sink_a = DecoupledIO(new SinkAReq) // put
  val source_b = DecoupledIO(new SourceBReq) // probe
  val sink_c = DecoupledIO(new SinkCReq) // inner release
  val source_d = DecoupledIO(new SourceDReq) // grant & atomics
  // outer
  val source_a = DecoupledIO(new SourceAReq) // acquire
  val source_c = DecoupledIO(new SourceCReq) // outer release & probe ack
  val source_e = DecoupledIO(new SourceEReq) // grant ack
  // direcotry write
  val dir_write = DecoupledIO(new DirWrite)
}

class DirWrite extends Bundle
class DirRead extends Bundle
class DirResult extends Bundle

class MSHR()(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val id = Input(UInt())
    val alloc = Flipped(ValidIO(new MSHRRequest))
    val status = Output(new MSHRStatus)
    val tasks = new MSHRTasks
    val dirResult = Flipped(ValidIO(new DirResult))
  })
}

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
}
