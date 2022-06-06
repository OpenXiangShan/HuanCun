package huancun.mbist

import chisel3._
import chisel3.util.experimental.BoringUtils
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
  val so = Output(Bool())
  val diag_done = Output(Bool())
}

class FUSEInterface extends Bundle{
  val trim_fuse = Input(UInt(20.W))
  val sleep_fuse = Input(UInt(2.W))
}

class FSCANInputInterface extends Bundle {
  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
}
object MBISTController{
  def connectRepair(ports:List[RepairBundle],nodes:Seq[RepairNode]) = {
    val newNodes = nodes.map({
      node =>
        val bd = Wire(new RepairBundle)
        bd := DontCare
        dontTouch(bd)
        val res = new RepairNode(bd,node.prefix)
        val source_elms = res.sink_elms
        source_elms.foreach(sigName => BoringUtils.addSource(res.bd.elements(sigName), res.prefix + sigName))
        res
    })
    ports.zip(newNodes).foreach({
      case(port,node) =>
        node.bd := port
    })
  }
}
class MBISTController
(
  mbistParams: Seq[MBISTBusParams],
  fscanPortNum: Int,
  prefix: Seq[String],
  repairNodes:Option[Seq[RepairNode]]
) extends RawModule {
  require(mbistParams.nonEmpty)
  override val desiredName = s"mbist_controller_${prefix.reduce(_ + _)}_dfx_top"

  val io = IO(new Bundle{
    val mbist_ijtag = new JTAGInterface
    val mbist = MixedVec(mbistParams.indices.map(idx => Flipped(new MbitsStandardInterface(mbistParams(idx)))))
    val fscan_ram = Vec(mbistParams.length, Flipped(new MbitsFscanInterface))
    val static = Vec(mbistParams.length, Flipped(new MbitsStaticInterface))
    val hd2prf_in = new FUSEInterface
    val hsuspsr_in = new FUSEInterface
    val fscan_in = Vec(fscanPortNum, new FSCANInputInterface)
    val fscan_clkungate = Input(Bool())
    val clock = Input(Clock())
  })
  dontTouch(io)

  val regex = """(bankedData|dataEcc)\d{1,2}_bank\d{1,2}""".r
  val repairPort = if(repairNodes.isDefined) Some(List.fill(repairNodes.get.length)(IO(Flipped(new RepairBundle)))) else None
  if(repairPort.isDefined) {
    repairPort.get.zip(repairNodes.get).foreach({
      case (port, node) => port.suggestName("slice_" + regex.findFirstIn(node.prefix).get)
    })
    repairPort.get.foreach(port => {
      port := DontCare
      dontTouch(port)
    })
  }
  io := DontCare
}