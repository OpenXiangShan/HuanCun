package huancun

import chisel3._
import org.chipsalliance.cde.config.Config
import chisel3.simulator.scalatest.ChiselSim
import chisel3.simulator.HasSimulator
import svsim.CommonCompilationSettings
import svsim.CommonCompilationSettings.VerilogPreprocessorDefine
import svsim.verilator.Backend.CompilationSettings.{TraceKind, TraceStyle}
import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import huancun.prefetch._
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}

abstract class L2Tester extends AnyFlatSpec with ChiselSim with Matchers {
  behavior of "L2"
  implicit val defaultConfig: Config = new Config((site, here, up) => {
    case HCCacheParamsKey => HCCacheParameters(
      prefetch = Some(BOPParameters()),// None,
      inclusive = false,
      clientCaches = Seq(CacheParameters(sets = 32, ways = 8, blockGranularity = 5, name = "L2")),
      sramClkDivBy2 = true
    )
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(HCCacheParamsKey).enableDebug,
      here(HCCacheParamsKey).enablePerf,
      here(HCCacheParamsKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(HCCacheParamsKey).enablePerf && !here(HCCacheParamsKey).FPGAPlatform,
      false,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })
}

object L2Tester {
  def verilatorWithVcd: HasSimulator = HasSimulator.simulators.verilator(
      compilationSettings = CommonCompilationSettings(
        verilogPreprocessorDefines = Seq(
          VerilogPreprocessorDefine("RANDOMIZE_REG_INIT"),
          VerilogPreprocessorDefine("RANDOMIZE_MEM_INIT"),
          VerilogPreprocessorDefine("RANDOMIZE_GARBAGE_ASSIGN")
        )
      ),
      verilatorSettings = svsim.verilator.Backend.CompilationSettings(
        traceStyle = Some(TraceStyle(TraceKind.Vcd)),
        outputSplit = Some(30000),
        outputSplitCFuncs = Some(30000),
        disabledWarnings = Seq("STMTDLY", "WIDTH")
      )
    )
}
