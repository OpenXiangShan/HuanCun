package huancun.mbist

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait MBISTBundleLike { this: Bundle =>
  def sink_elms: Seq[String]
  def source_elms: Seq[String]

  def elm_add_source(elm: String, prefix: String) = {
    println(s"source: $prefix + $elm")
    BoringUtils.addSource(elements(elm), prefix + elm)
  }

  def elm_add_sink(elm: String, prefix: String) = {
    println(s"sink: $prefix + $elm")
    BoringUtils.addSink(elements(elm), prefix + elm)
  }
}

case class MBISTBusParams
(
  array: Int,
  set: Int,
  dataWidth: Int
) {
  val arrayWidth = log2Up(array)
  val addrWidth = log2Up(set)
}

class MBISTBus(val params: MBISTBusParams) extends Bundle with MBISTBundleLike {
  // control signals
  val array = Input(UInt(params.arrayWidth.W))
  val all, req= Input(Bool())
  val ack = Output(Bool())
  // write
  val wen = Input(Bool())
  val addr = Input(UInt(params.addrWidth.W))
  val data_in = Input(UInt(params.dataWidth.W))
  // read
  val read_en = Input(Bool())
  val addr_rd = Input(UInt(params.addrWidth.W)) // not used for single port srams
  val data_out = Output(UInt(params.dataWidth.W))

  override def sink_elms: Seq[String] = Seq(
    "array", "all", "req", "wen", "addr", "data_in", "read_en", "addr_rd"
  )

  override def source_elms: Seq[String] = Seq("ack", "data_out")
}

case class SRAM2MBISTParams
(
  set: Int,
  dataWidth: Int
) {
  val addrWidth = log2Up(set)
}

class SRAM2MBIST(val params: SRAM2MBISTParams) extends Bundle with MBISTBundleLike {
  val addr, addr_rd = Input(UInt(params.addrWidth.W))
  val wdata = Input(UInt(params.dataWidth.W))
  val re, we = Input(Bool())
  val rdata = Output(UInt(params.dataWidth.W))

  override def sink_elms: Seq[String] =  Seq("addr", "addr_rd", "wdata", "re", "we")
  override def source_elms: Seq[String] = Seq("rdata")
}
