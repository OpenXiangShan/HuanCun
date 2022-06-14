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

class FSCANInputInterface extends Bundle {
  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
}

class BISRInputInterface extends Bundle {
  val shift_en = Input(Bool())
  val clock = Input(Bool())
  val reset = Input(Bool())
  val scan_in = Input(Bool())
  val scan_out = Input(Bool())
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
  mbistName:Seq[String],
  prefix: Seq[String],
  repairNodes:Option[Seq[RepairNode]]
) extends RawModule {
  require(mbistParams.nonEmpty)
  require(mbistParams.length == mbistName.length)
  override val desiredName = s"mbist_controller_${prefix.reduce(_ + _)}_dfx_top"

  val io = IO(new Bundle{
    val mbist_ijtag = new JTAGInterface
    val hd2prf_out = Flipped(new MbitsFuseInterface(isSRAM = false))
    val hsuspsr_out = Flipped(new MbitsFuseInterface(isSRAM = true))
    val uhdusplr_out = Flipped(new MbitsFuseInterface(isSRAM = true))
    val hduspsr_out = Flipped(new MbitsFuseInterface(isSRAM = true))
    val hd2prf_in = new MbitsFuseInterface(isSRAM = false)
    val hsuspsr_in = new MbitsFuseInterface(isSRAM = true)
    val uhdusplr_in = new MbitsFuseInterface(isSRAM = true)
    val hduspsr_in = new MbitsFuseInterface(isSRAM = true)
    val xsx_fscan_in = new FSCANInputInterface
    val xsl2_fscan_in = new FSCANInputInterface
    val fscan_clkungate = Input(Bool())
    val clock = Input(Clock())
    val bisr = if(repairNodes.isDefined) Some(new BISRInputInterface) else None
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

  val mbist = mbistParams.indices.map(idx => IO(Flipped(new MbitsStandardInterface(mbistParams(idx)))))
  mbist.zip(mbistName).foreach({
    case(port,name) =>
      port.suggestName("io_" + name)
      port := DontCare
      dontTouch(port)
  })

  val fscan_ram = prefix.indices.map(idx => IO(Flipped(new MbitsFscanInterface)))
  fscan_ram.zip(prefix).foreach({
    case(port,name) =>
      port.suggestName("io_fscan_ram_" + name)
      port := DontCare
      dontTouch(port)
  })

  io := DontCare
}