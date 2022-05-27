package huancun.mbist

import chisel3._
import chisel3.util.{HasBlackBoxInline, MixedVec}

import scala.collection.mutable

class JTAGInterface extends Bundle{
  val tck = Input(Bool())
  val reset = Input(Bool())
  val ce = Input(Bool())
  val se = Input(Bool())
  val ue = Input(Bool())
  val sel = Input(Bool())
  val si = Input(Bool())
  val so = Input(Bool())
  val diag_done = Output(Bool())
}

class FUSEInterface extends Bundle{
  val trim_fuse = Input(UInt(11.W))
  val sleep_fuse = Input(UInt(2.W))
}

class FSCANInputInterface extends Bundle {
  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
}

class MBISTController
(
  mbistParams: Seq[MBISTBusParams],
  fscanPortNum: Int,
  prefix: Seq[String],
  sim: Boolean = false
)
  extends BlackBox with HasBlackBoxInline with genBlackBoxVerilogFile{
  require(mbistParams.nonEmpty)
  require(mbistParams.length == prefix.length, "mbist params number not match prefixes number")
  override val desiredName = "mbist_controller_" + prefix.reduce(_+_) + "_dfx_top"
  val io = IO(new Bundle{
    val mbist_ijtag = new JTAGInterface
    val mbist = MixedVec(prefix.indices.map(idx => Flipped(new MbitsStandardInterface(mbistParams(idx)))))
    val fscan_ram = Vec(prefix.length, Flipped(new MbitsFscanInterface))
    val static = Vec(prefix.length, Flipped(new MbitsStaticInterface))
    val hd2prf_in = new FUSEInterface
    val hsuspsr_in = new FUSEInterface
    val fscan_in = Vec(fscanPortNum,new FSCANInputInterface)
    val fscan_clkungate = Input(Bool())
    val clock = Input(Bool())
  })
  genV(io, sim)
}

class MBISTControllerDFXWrapperTestTop
(
  mbistParams: Seq[MBISTBusParams],
  fscanPortNum: Int,
  prefix: Seq[String]
)
  extends RawModule {
  require(mbistParams.nonEmpty)
  require(mbistParams.length == prefix.length, "mbist params number not match prefixes number")
  val numOfController = mbistParams.length
  val mbist_ijtag = IO(new JTAGInterface)
  val mbist = IO(MixedVec(prefix.indices.map(idx => Flipped(new MbitsStandardInterface(mbistParams(idx))))))
  val fscan_ram = IO(Vec(prefix.length, Flipped(new MbitsFscanInterface)))
  val static = IO(Vec(prefix.length, Flipped(new MbitsStaticInterface)))
  val hd2prf_in = IO(new FUSEInterface)
  val hsuspsr_in = IO(new FUSEInterface)
  val fscan_in = IO(Vec(fscanPortNum,new FSCANInputInterface))
  val fscan_clkungate = IO(Input(Bool()))
  val clock = IO(Input(Bool()))

  val ctrl = Module(new MBISTController(mbistParams, fscanPortNum, prefix))
  ctrl.io.mbist_ijtag <> mbist_ijtag
  ctrl.io.mbist.zip(mbist).foreach({ case (a,b) => a <> b })
  ctrl.io.fscan_ram.zip(fscan_ram).foreach({ case (a,b) => a <> b })
  ctrl.io.static.zip(static).foreach({ case (a,b) => a <> b })
  ctrl.io.hd2prf_in <> hd2prf_in
  ctrl.io.hsuspsr_in <> hsuspsr_in
  ctrl.io.fscan_in.zip(fscan_in).foreach({ case (a,b) => b <> a })
  ctrl.io.clock := clock
  ctrl.io.fscan_clkungate := fscan_clkungate
}
