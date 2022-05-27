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
abstract class MBISTCommonBundle extends Bundle with MBISTBundleLike{
  val sram_trim_fuse = Input(UInt(11.W))
  val sram_sleep_fuse = Input(UInt(2.W))
  val rf_trim_fuse = Input(UInt(11.W))
  val rf_sleep_fuse = Input(UInt(2.W))
  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
  val clkungate = Input(Bool())
}

case class MBISTBusParams
(
  array: Int,
  set: Int,
  dataWidth: Int,
  maskWidth: Int
) {
  val arrayWidth = log2Up(array)
  val addrWidth = log2Up(set)
}

class MBISTBus(val params: MBISTBusParams) extends MBISTCommonBundle{
  // control signals
  val mbist_array = Input(UInt(params.arrayWidth.W))
  val mbist_all, mbist_req= Input(Bool())
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

  override def sink_elms: Seq[String] = Seq(
    "mbist_array", "mbist_all", "mbist_req", "mbist_writeen", "mbist_be",
    "mbist_addr", "mbist_indata", "mbist_readen", "mbist_addr_rd",
    "sram_trim_fuse","sram_sleep_fuse", "rf_trim_fuse","rf_sleep_fuse",
    "bypsel","wdis_b","rdis_b","init_en","init_val","clkungate"
  )

  override def source_elms: Seq[String] = Seq("mbist_ack", "mbist_outdata")
}

case class SRAM2MBISTParams
(
  set: Int,
  dataWidth: Int,
  maskWidth: Int,
  singlePort: Boolean
) {
  val addrWidth = log2Up(set)

  override def toString =
    f"Data width:${dataWidth} " +
    f"Address width:${addrWidth} " +
    f"Set num: ${set} " +
    f"BE width: ${maskWidth}"
}

class SRAM2MBIST(val params: SRAM2MBISTParams) extends MBISTCommonBundle{
  val addr, addr_rd = Input(UInt(params.addrWidth.W))
  val wdata = Input(UInt(params.dataWidth.W))
  val wmask = Input(UInt(params.maskWidth.W))
  val re, we = Input(Bool())
  val rdata = Output(UInt(params.dataWidth.W))
  val ack = Input(Bool())

  override def sink_elms: Seq[String] =  Seq(
    "addr", "addr_rd", "wdata", "wmask", "re", "we","ack",
    "sram_trim_fuse","sram_sleep_fuse", "rf_trim_fuse","rf_sleep_fuse",
    "bypsel","wdis_b","rdis_b","init_en","init_val","clkungate"
  )
  override def source_elms: Seq[String] = Seq("rdata")


}
