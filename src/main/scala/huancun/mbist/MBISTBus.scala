package huancun.mbist

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import huancun.utils.SramType

trait MBISTBundleLike {
  this: Bundle =>
  def sink_elms: Seq[String]
  def source_elms: Seq[String]

  def elm_add_source(elm: String, prefix: String) = {
//    println(s"source: $prefix$elm")
    BoringUtils.addSource(elements(elm), prefix + elm)
  }

  def elm_add_sink(elm: String, prefix: String) = {
//    println(s"sink: $prefix$elm")
    BoringUtils.addSink(elements(elm), prefix + elm)
  }
}
abstract class MBISTCommonBundle(val sramType:Int) extends Bundle with MBISTBundleLike{
  val isRF = sramType == SramType.hd2prf.id
  val hd2prf_trim_fuse = Input(UInt(11.W))
  val hd2prf_sleep_fuse = Input(UInt(2.W))
  val hsuspsr_trim_fuse = Input(UInt(20.W))
  val hsuspsr_sleep_fuse = Input(UInt(2.W))
  val uhdusplr_trim_fuse = Input(UInt(20.W))
  val uhdusplr_sleep_fuse = Input(UInt(2.W))
  val hduspsr_trim_fuse = Input(UInt(20.W))
  val hduspsr_sleep_fuse = Input(UInt(2.W))
  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
  val clkungate = Input(Bool())
  val IP_RESET_B = Input(Bool())
  val WRAPPER_RD_CLK_EN = Input(UInt((if(isRF) 1 else 0).W))
  val WRAPPER_WR_CLK_EN = Input(UInt((if(isRF) 1 else 0).W))
  val WRAPPER_CLK_EN = Input(UInt((if(!isRF) 1 else 0).W))
  val OUTPUT_RESET = Input(Bool())
  val PWR_MGNT_IN = Input(UInt((if(isRF) 4 else 5).W))

  val typeSpecificSignal = if(isRF)
    Seq("WRAPPER_RD_CLK_EN","WRAPPER_WR_CLK_EN")
  else
    Seq("WRAPPER_CLK_EN")

  override def sink_elms =
    Seq(
      "hd2prf_trim_fuse","hd2prf_sleep_fuse",
      "hsuspsr_trim_fuse","hsuspsr_sleep_fuse",
      "uhdusplr_trim_fuse","uhdusplr_sleep_fuse",
      "hduspsr_trim_fuse","hduspsr_sleep_fuse",
      "bypsel","wdis_b","rdis_b","init_en","init_val","clkungate",
      "IP_RESET_B","OUTPUT_RESET", "PWR_MGNT_IN"
  ) ++ typeSpecificSignal
}

case class MBISTBusParams
(
  array: Int,
  set: Int,
  dataWidth: Int,
  maskWidth: Int,
  sramType: Int,
  domainName:String = "Unkown"
) {
  val isRF = sramType == SramType.hd2prf.id
  val arrayWidth = log2Up(array + 1)
  val addrWidth = log2Up(set + 1)
}

class MBISTBus(val params: MBISTBusParams) extends MBISTCommonBundle(params.sramType){
  // control signals
  val mbist_array = Input(UInt(params.arrayWidth.W))
  val mbist_all, mbist_req = Input(Bool())
  val mbist_ack = Output(Bool())
  // write
  val mbist_writeen = Input(Bool())
  val mbist_be = Input(UInt(params.maskWidth.W))
  val mbist_addr = Input(UInt(params.addrWidth.W))
  val mbist_indata = Input(UInt(params.dataWidth.W))
  // read
  val mbist_readen = Input(Bool())
  val mbist_addr_rd = Input(UInt(params.addrWidth.W)) // not used for single port srams
  val mbist_outdata = Output(UInt(params.dataWidth.W))

  override def sink_elms: Seq[String] = super.sink_elms ++ Seq(
    "mbist_array", "mbist_all", "mbist_req", "mbist_writeen", "mbist_be",
    "mbist_addr", "mbist_indata", "mbist_readen", "mbist_addr_rd"
  )

  override def source_elms: Seq[String] = Seq("mbist_ack", "mbist_outdata")
}

case class RAM2MBISTParams
(
  set: Int,
  dataWidth: Int,
  maskWidth: Int,
  singlePort: Boolean,
  vname:String,
  hierarchyName:String,
  sramType:Int,
  nodeNum:Int,
  maxArrayId:Int
) {
  val isRF = sramType == SramType.hd2prf.id
  val addrWidth = log2Up(set + 1)
  val arrayWidth = log2Up(maxArrayId + 1)
  def getAllNodesParams():Seq[RAM2MBISTParams] = {
    val res = Seq.tabulate(nodeNum)(idx => {
      RAM2MBISTParams(set,dataWidth,maskWidth,singlePort,vname,hierarchyName + s"node${idx}", sramType, nodeNum, maxArrayId)
    })
    res
  }
}

class RAM2MBIST(val params: RAM2MBISTParams) extends MBISTCommonBundle(params.sramType){
  val addr, addr_rd = Input(UInt(params.addrWidth.W))
  val wdata = Input(UInt(params.dataWidth.W))
  val wmask = Input(UInt(params.maskWidth.W))
  val re, we = Input(Bool())
  val rdata = Output(UInt(params.dataWidth.W))
  val ack = Input(Bool())
  val selectedOH = Input(UInt(params.nodeNum.W))
  val array = Input(UInt(params.arrayWidth.W))
  override def sink_elms: Seq[String] =  super.sink_elms ++ Seq(
    "addr", "addr_rd", "wdata", "wmask", "re", "we","ack","selectedOH","array"
  )
  override def source_elms: Seq[String] = Seq("rdata")
}
