package huancun.mbist
import chisel3._
import chisel3.util._
import scala.collection.mutable

class UltiscanJTAGInterface extends Bundle{
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

class UltiscanUscanInterface
(
  NUM_CHANNELS_IN: Int, NUM_CHANNELS_OUT: Int
)
  extends Bundle{
  val state = Input(Bool())
  val edt_update = Input(Bool())
  val mode = Input(Bool())
  val scanclk = Input(Bool())
  val si = Input(UInt((NUM_CHANNELS_IN + 1).W))
  val so = Output(UInt((NUM_CHANNELS_OUT + 1).W))
}

class UltiscanIO
(
  NUM_CHAINS: Int,
  NUM_CHANNELS_IN: Int,
  NUM_CHANNELS_OUT: Int,
  NUM_CLKGENCTRL: Int,
  NUM_CLKGENCTRLEN: Int
)
  extends Bundle {
  val fscan_mode = Output(Bool())
  val fscan_mode_atspeed = Output(Bool())
  val fscan_state = Output(Bool())

  val fscan_byplatrst = Output(Bool())
  val fscan_byplatrst_b = Output(Bool())
  val fscan_byprst = Output(Bool())
  val fscan_byprst_b = Output(Bool())
  val fscan_clkgenctrl = Output(UInt((NUM_CLKGENCTRL + 1).W))
  val fscan_clkgenctrlen = Output(UInt((NUM_CLKGENCTRLEN + 1).W))
  val fscan_clkungate = Output(Bool())
  val fscan_clkungate_syn = Output(Bool())
  val fscan_rstbypen = Output(Bool())
  val fscan_shiften = Output(Bool())

  val fscan_ram_bypsel = Output(Bool())
  val fscan_ram_hold = Output(Bool())
  val fscan_ram_init_en = Output(Bool())
  val fscan_ram_init_val = Output(Bool())
  val fscan_ram_mcp = Output(Bool())
  val fscan_ram_odis_b = Output(Bool())
  val fscan_ram_rddis_b = Output(Bool())
  val fscan_ram_wrdis_b = Output(Bool())

  val ijtag = new UltiscanJTAGInterface

  val scanchains_so_end = Input(UInt((NUM_CHAINS - 2).W))
  val scanchains_si_bgn = Output(UInt((NUM_CHAINS - 2).W))

  val dftclken = Output(Bool())
  val core_clock_preclk = Input(Clock())
  val core_clock_postclk = Output(Clock())

  val uscan = new UltiscanUscanInterface(NUM_CHANNELS_IN, NUM_CHANNELS_OUT)
}


class Ultiscan
(
  NUM_CHAINS: Int,
  NUM_CHANNELS_IN: Int,
  NUM_CHANNELS_OUT: Int,
  NUM_CLKGENCTRL: Int,
  NUM_CLKGENCTRLEN: Int,
  RSTVAL_CLKGENCTRL: Int,
  RSTVAL_CLKGENCTRLEN: Int,
  prefix: String,
  sim: Boolean = true
)
  extends BlackBox with HasBlackBoxInline with genBlackBoxVerilogFile {
  override val desiredName = prefix + "_ultiscan_top"
  val v_params = new mutable.HashMap[String, String]
  v_params("NUM_CHAINS")          = NUM_CHAINS.toString
  v_params("NUM_CHANNELS_IN")     = NUM_CHANNELS_IN.toString
  v_params("NUM_CHANNELS_OUT")    = NUM_CHANNELS_OUT.toString
  v_params("NUM_CLKGENCTRL")      = NUM_CLKGENCTRL.toString
  v_params("NUM_CLKGENCTRLEN")    = NUM_CLKGENCTRLEN.toString
  v_params("RSTVAL_CLKGENCTRL")   = RSTVAL_CLKGENCTRL.toString
  v_params("RSTVAL_CLKGENCTRLEN") = RSTVAL_CLKGENCTRLEN.toString

  val io = IO(new UltiscanIO(
    NUM_CHAINS,
    NUM_CHANNELS_IN,
    NUM_CHANNELS_OUT,
    NUM_CLKGENCTRL,
    NUM_CLKGENCTRLEN
  ))
  genV(io, v_params, sim)
}

class UltiscanTestTop extends RawModule {
  val xsl2_ultiscan = Module(new Ultiscan(3400, 20, 20, 1, 1, 0, 0, "xsl2"))
  val xsx_ultiscan = Module(new Ultiscan(1100, 10, 10, 1, 1, 0, 0, "xsx"))

  val xsl2 = IO(xsl2_ultiscan.io.cloneType)
  val xsx = IO(xsx_ultiscan.io.cloneType)

  xsl2 <> xsl2_ultiscan.io
  xsx <> xsx_ultiscan.io
}