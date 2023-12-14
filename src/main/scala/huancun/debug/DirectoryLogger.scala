package huancun.debug

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.{IntParam, StringParam}
import chisel3.util._
import huancun.HCCacheParamsKey
import utility._

object TypeId {
  val self_dir = 0
  val self_tag = 1
  val client_dir = 2
  val client_tag = 3
}

class DirectoryInfo extends Bundle with HasCLikeTypes {
  val tag = uint64_t
  val set = uint64_t
  val dir = uint64_t
  val stamp = uint64_t
  val way = uint8_t
  // 0 -> self dir
  // 1 -> self tag
  // 2 -> client dir
  // 3 -> client tag
  val typeId = uint8_t
}

class DirLogWriter(prefix: String)
    extends BlackBox(
      Map("prefix" -> StringParam(prefix))
    )
    with HasBlackBoxInline {
  val io = IO(Input(new DirectoryInfo {
    val wen = Bool()
    val clock = Clock()
    val reset = Reset()
  })).suggestName("io")

  val verilog =
    """
      |import "DPI-C" function void dir_log_write_helper
      |(
      |    input longint tag,
      |    input longint set_idx,
      |    input longint dir,
      |    input longint stamp,
      |    input byte way,
      |    input byte typeId,
      |    input string prefix
      |);
      |
      |module DirLogWriter(
      |    input [63:0] tag,
      |    input [63:0] set,
      |    input [63:0] dir,
      |    input [63:0] stamp,
      |    input [7:0] way,
      |    input [7:0] typeId,
      |    input wen,
      |    input clock,
      |    input reset
      |);
      |    parameter string prefix = "undefined";
      |
      |    always @(posedge clock) begin
      |        if(wen && !reset) begin
      |            dir_log_write_helper(
      |                tag, set, dir, stamp, way, typeId, prefix
      |            );
      |        end
      |    end
      |
      |endmodule
      |""".stripMargin

  setInline("DirLogWriter.v", verilog)
}

object DirectoryLogger {
  def apply[T <: Data](
    prefix: String,
    typeId: Int
  )(set:    UInt,
    way:    UInt,
    tag:    UInt,
    state:  T,
    stamp:  UInt,
    wen:    Bool,
    clock:  Clock,
    reset:  Reset
  )(
    implicit p: Parameters
  ) = {
    if(p(HCCacheParamsKey).enableDebug){
      val dirLogger = Module(new DirLogWriter(prefix))
      dirLogger.io.set := set
      dirLogger.io.way := way
      dirLogger.io.typeId := typeId.U
      dirLogger.io.stamp := stamp
      dirLogger.io.dir := state.asUInt
      dirLogger.io.tag := tag
      dirLogger.io.wen := wen
      dirLogger.io.clock := clock
      dirLogger.io.reset := reset
    }
  }
}
