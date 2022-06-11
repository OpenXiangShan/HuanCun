package huancun.mbist

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

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
abstract class MBISTCommonBundle(val isRF:Boolean) extends Bundle with MBISTBundleLike{
  val sram_trim_fuse = Input(UInt((if(!isRF) 20 else 0).W))
  val sram_sleep_fuse = Input(UInt((if(!isRF) 2 else 0).W))
  val rf_trim_fuse = Input(UInt((if(isRF) 11 else 0).W))
  val rf_sleep_fuse = Input(UInt((if(isRF) 2 else 0).W))
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
    Seq("rf_trim_fuse","rf_sleep_fuse","WRAPPER_RD_CLK_EN","WRAPPER_WR_CLK_EN")
  else
    Seq("sram_trim_fuse","sram_sleep_fuse","WRAPPER_CLK_EN")

  override def sink_elms =
    Seq("bypsel","wdis_b","rdis_b","init_en","init_val","clkungate",
    "IP_RESET_B","OUTPUT_RESET", "PWR_MGNT_IN") ++ typeSpecificSignal
}

case class MBISTBusParams
(
  array: Int,
  set: Int,
  dataWidth: Int,
  maskWidth: Int,
  isRF: Boolean,
  domainName:String = "Unkown"
) {

  val arrayWidth = log2Up(array)
  val addrWidth = log2Up(set)
}

class MBISTBus(val params: MBISTBusParams) extends MBISTCommonBundle(params.isRF){
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
  isRF:Boolean
) {
  val addrWidth = log2Up(set)
}

class RAM2MBIST(val params: RAM2MBISTParams) extends MBISTCommonBundle(params.isRF){
  val addr, addr_rd = Input(UInt(params.addrWidth.W))
  val wdata = Input(UInt(params.dataWidth.W))
  val wmask = Input(UInt(params.maskWidth.W))
  val re, we = Input(Bool())
  val rdata = Output(UInt(params.dataWidth.W))
  val ack = Input(Bool())

  override def sink_elms: Seq[String] =  super.sink_elms ++ Seq(
    "addr", "addr_rd", "wdata", "wmask", "re", "we","ack"
  )
  override def source_elms: Seq[String] = Seq("rdata")
}
