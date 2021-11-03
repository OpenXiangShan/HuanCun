package huancun.debug

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.StringParam
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleMap
import huancun.utils.GTimer

trait HasCLikeTypes {
  // c++ firendly data types
  def uint8_t = UInt(8.W)
  def uint32_t = UInt(32.W)
  def uint64_t = UInt(64.W)
}

class TLLog extends Bundle with HasCLikeTypes {
  // a b c d e
  // 0 1 2 3 4
  val channel = uint8_t
  val opcode = uint8_t
  val param = uint8_t
  val source = uint8_t
  val sink = uint8_t
  val address = uint64_t
  val data = Vec(4, uint64_t)
  val stamp = uint64_t
  val user = uint64_t
  val echo = uint64_t
}

class TLLogWriter(prefix: String) extends BlackBox(Map("prefix" -> StringParam(prefix))) with HasBlackBoxInline {
  val io = IO(Input(new TLLog {
    val wen = Bool()
    val clock = Clock()
    val reset = Reset()
  })).suggestName("io")

  // when integrate into other project as a submodule,
  // the path of 'resource' can't be found
  // so we have to inline the verilog file

  val verilog =
    """
      |import "DPI-C" function void tl_log_write_helper
      |(
      |    input byte channel,
      |    input byte opcode,
      |    input byte param,
      |    input byte source,
      |    input byte sink,
      |    input longint address,
      |    input longint data_0,
      |    input longint data_1,
      |    input longint data_2,
      |    input longint data_3,
      |    input longint stamp,
      |    input longint user,
      |    input longint echo,
      |    input string prefix
      |);
      |
      |module TLLogWriter(
      |    input [7:0] channel,
      |    input [7:0] opcode,
      |    input [7:0] param,
      |    input [7:0] source,
      |    input [7:0] sink,
      |    input [63:0] address,
      |    input [63:0] data_0,
      |    input [63:0] data_1,
      |    input [63:0] data_2,
      |    input [63:0] data_3,
      |    input [63:0] stamp,
      |    input [63:0] user,
      |    input [63:0] echo,
      |    input wen,
      |    input clock,
      |    input reset
      |);
      |    parameter string prefix;
      |
      |    always @(posedge clock) begin
      |        if(wen && !reset) begin
      |            tl_log_write_helper(
      |                channel, opcode, param, source, sink,
      |                address, data_0, data_1, data_2, data_3, stamp, user, echo, prefix
      |            );
      |        end
      |    end
      |
      |endmodule
      |""".stripMargin

  setInline("TLLogWriter", verilog)

}

class TLLogger(name: String)(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode()
  lazy val module = new TLLoggerImp(this, name)
}

class TLLoggerImp(outer: TLLogger, name: String) extends LazyModuleImp(outer) {
  val node = outer.node
  for (((in, edgeIn), (out, edgeOut)) <- node.in.zip(node.out)) {
    out <> in
    TLLogger.track(in, edgeIn, this.clock, this.reset)(name)
  }
}

object TLLogger {

  def a = 0.U
  def b = 1.U
  def c = 2.U
  def d = 3.U
  def e = 4.U // not used

  def writeChannel[T <: TLChannel](log: TLLog, chn: T, stamp: UInt): Unit = {
    for ((name, data) <- log.elements.filterNot(_._1 == "data")) {
      val e = chn.elements.find(_._1 == name)
      if (e.nonEmpty) {
        data := e.get._2.asUInt()
      } else {
        data := 0.U
      }
    }
    def bmp_to_uint(bmp: BundleMap): UInt = {
      if(bmp.fields.nonEmpty){
        bmp.asUInt()
      } else {
        0.U
      }
    }
    chn match {
      case a_chn: TLBundleA =>
        log.channel := a
        log.user := bmp_to_uint(a_chn.user)
        log.echo := bmp_to_uint(a_chn.echo)
      case _: TLBundleB =>
        log.channel := b
        log.user := 0.U
        log.echo := 0.U
      case c_chn: TLBundleC =>
        log.channel := c
        log.user := bmp_to_uint(c_chn.user)
        log.echo := bmp_to_uint(c_chn.echo)
      case d_chn: TLBundleD =>
        log.channel := d
        log.user := bmp_to_uint(d_chn.user)
        log.echo := bmp_to_uint(d_chn.echo)
    }
    log.stamp := stamp
  }

  def logA(log: TLLog, a: TLBundleA, stamp: UInt) = {
    writeChannel(log, a, stamp)
  }

  def logB(log: TLLog, b: TLBundleB, stamp: UInt) = {
    writeChannel(log, b, stamp)
  }

  def logC(log: TLLog, c: TLBundleC, stamp: UInt) = {
    writeChannel(log, c, stamp)
    log.data := c.data.asTypeOf(log.data)
  }

  def logD(log: TLLog, d: TLBundleD, stamp: UInt, addr: UInt) = {
    writeChannel(log, d, stamp)
    log.address := addr
    log.data := d.data.asTypeOf(log.data)
  }

  def track(in: TLBundle, edge: TLEdgeIn, clock: Clock, reset: Reset)(name: String) = {
    val numClients = edge.client.endSourceId

    // Acquire/Get -> Grant
    val a_d_addrs = Reg(Vec(numClients, UInt(edge.bundle.addressBits.W)))
    // Release -> ReleaseAck
    val c_d_addrs = Reg(Vec(numClients, UInt(edge.bundle.addressBits.W)))
    val a_log, b_log, c_log, d_log = WireInit(0.U.asTypeOf(new TLLog))
    val a_writer, b_writer, c_writer, d_writer = Module(new TLLogWriter(name))
    val timer = GTimer()

    def connect(writer: TLLogWriter, log: TLLog, wen: Bool) = {
      writer.io.channel := log.channel
      writer.io.opcode := log.opcode
      writer.io.param := log.param
      writer.io.source := log.source
      writer.io.sink := log.sink
      writer.io.address := log.address
      writer.io.data := log.data
      writer.io.stamp := log.stamp
      writer.io.user := log.user
      writer.io.echo := log.echo
      writer.io.wen := wen
      writer.io.clock := clock
      writer.io.reset := reset
    }

    connect(a_writer, a_log, in.a.fire())
    connect(b_writer, b_log, in.b.fire())
    connect(c_writer, c_log, in.c.fire())
    connect(d_writer, d_log, in.d.fire())

    when(in.a.fire()) {
      logA(a_log, in.a.bits, timer)
      a_d_addrs(in.a.bits.source) := in.a.bits.address
    }

    when(in.b.fire()) {
      logB(b_log, in.b.bits, timer)
    }

    when(in.c.fire()) {
      logC(c_log, in.c.bits, timer)
      c_d_addrs(in.c.bits.source) := in.c.bits.address
    }

    when(in.d.fire()) {
      val a_d = a_d_addrs(in.d.bits.source)
      val c_d = c_d_addrs(in.d.bits.source)
      val addr = Mux(in.d.bits.opcode === TLMessages.ReleaseAck, c_d, a_d)
      logD(d_log, in.d.bits, timer, addr)
    }

  }

  def apply(name: String, enable: Boolean = true)(implicit p: Parameters) = {
    if(enable){
      val logger = LazyModule(new TLLogger(name))
      logger.node
    } else {
      TLTempNode()
    }
  }

}
