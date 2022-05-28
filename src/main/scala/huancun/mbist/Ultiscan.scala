package huancun.mbist
import chisel3._
import chisel3.util._
import scala.collection.mutable

class UltiscanJTAGInterface extends Bundle {
  val fdfx_powergood = Input(Bool())
  val capture = Input(Bool())
  val reset_b = Input(Bool())
  val select = Input(Bool())
  val shift = Input(Bool())
  val si = Input(Bool())
  val tck = Input(Bool())
  val update = Input(Bool())
  val so = Output(Bool())
}

class UltiscanUscanInterface (
  NUM_CHANNELS_IN: Int,
  NUM_CHANNELS_OUT: Int
) extends Bundle {
  val state = Input(Bool())
  val edt_update = Input(Bool())
  val mode = Input(Bool())
  val scanclk = Input(Bool())
  val si = Input(UInt((NUM_CHANNELS_IN + 1).W))
  val so = Output(UInt((NUM_CHANNELS_OUT + 1).W))
}

class UltiscanIO (
  NUM_CHAINS: Int,
  NUM_CHANNELS_IN: Int,
  NUM_CHANNELS_OUT: Int,
  NUM_CLKGENCTRL: Int,
  NUM_CLKGENCTRLEN: Int
) extends Bundle {
  val fscan = new Bundle() {
    val mode = Output(Bool())
    val mode_atspeed = Output(Bool())
    val state = Output(Bool())

    val byplatrst = Output(Bool())
    val byplatrst_b = Output(Bool())
    val byprst = Output(Bool())
    val byprst_b = Output(Bool())
    val clkgenctrl = Output(UInt((NUM_CLKGENCTRL + 1).W))
    val clkgenctrlen = Output(UInt((NUM_CLKGENCTRLEN + 1).W))
    val clkungate = Output(Bool())
    val clkungate_syn = Output(Bool())
    val rstbypen = Output(Bool())
    val shiften = Output(Bool())

    val ram = new Bundle () {
      val bypsel = Output(Bool())
      val hold = Output(Bool())
      val init_en = Output(Bool())
      val init_val = Output(Bool())
      val mcp = Output(Bool())
      val odis_b = Output(Bool())
      val rddis_b = Output(Bool())
      val wrdis_b = Output(Bool())
    }
  }

  val ijtag = new UltiscanJTAGInterface

  val scanchains_so_end = Input(UInt((NUM_CHAINS - 2).W))
  val scanchains_si_bgn = Output(UInt((NUM_CHAINS - 2).W))

  val dftclken = Output(Bool())
  val core_clock_preclk = Input(Clock())
  val core_clock_postclk = Output(Clock())

  val uscan = new UltiscanUscanInterface(NUM_CHANNELS_IN, NUM_CHANNELS_OUT)
}


class Ultiscan (
  NUM_CHAINS: Int,
  NUM_CHANNELS_IN: Int,
  NUM_CHANNELS_OUT: Int,
  NUM_CLKGENCTRL: Int,
  NUM_CLKGENCTRLEN: Int,
  RSTVAL_CLKGENCTRL: Int,
  RSTVAL_CLKGENCTRLEN: Int,
  prefix: String,
  sim: Boolean = true
) extends RawModule {
  override val desiredName = prefix + "_ultiscan_top"

  val io = IO(new UltiscanIO(
    NUM_CHAINS,
    NUM_CHANNELS_IN,
    NUM_CHANNELS_OUT,
    NUM_CLKGENCTRL,
    NUM_CLKGENCTRLEN
  ))
  dontTouch(io)
  io.suggestName(prefix)

  io := DontCare
  io.core_clock_postclk := io.core_clock_preclk
}

class UltiscanTestTop extends RawModule {
  val xsl2_ultiscan = Module(new Ultiscan(3400, 20, 20, 1, 1, 0, 0, "xsl2"))
  val xsx_ultiscan = Module(new Ultiscan(1100, 10, 10, 1, 1, 0, 0, "xsx"))

  val xsl2 = IO(xsl2_ultiscan.io.cloneType)
  val xsx = IO(xsx_ultiscan.io.cloneType)

  xsl2 <> xsl2_ultiscan.io
  xsx <> xsx_ultiscan.io
}