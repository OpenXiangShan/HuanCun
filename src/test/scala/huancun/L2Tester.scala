package huancun

import chisel3._
import org.chipsalliance.cde.config.Config
import chiseltest._
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chiseltest.simulator.{VerilatorCFlags, VerilatorFlags}
import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import huancun.prefetch._

abstract class L2Tester extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {
  behavior of "L2"
  implicit val defaultConfig = new Config((_, _, _) => {
    case HCCacheParamsKey => HCCacheParameters(
      prefetch = Some(BOPParameters()),// None,
      inclusive = false,
      clientCaches = Seq(CacheParameters(sets = 32, ways = 8, blockGranularity = 5, name = "L2")),
      sramClkDivBy2 = true
    )
  })
}

trait HasTestAnnos {
  var testAnnos: AnnotationSeq = Seq()
}

trait UseVerilatorBackend { this: HasTestAnnos =>
  testAnnos = testAnnos ++ Seq(VerilatorBackendAnnotation)
}

trait RandomResetRegs { this: HasTestAnnos with UseVerilatorBackend =>
  testAnnos = testAnnos ++ Seq(
    VerilatorFlags(Seq(
      "+define+RANDOMIZE_REG_INIT",
      "+define+RANDOMIZE_MEM_INIT",
      "+define+RANDOMIZE_GARBAGE_ASSIGN",
      "+define+RANDOMIZE_DELAY=0"
    ))
  )
}

trait DumpVCD { this: HasTestAnnos =>
  testAnnos = testAnnos :+ WriteVcdAnnotation
}
