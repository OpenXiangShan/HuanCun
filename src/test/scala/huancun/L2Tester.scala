package huancun

import chipsalliance.rocketchip.config.Config
import chiseltest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chiseltest.legacy.backends.verilator.{VerilatorCFlags, VerilatorFlags}
import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation
import huancun.utils.FixSubModuleInputs
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

abstract class L2Tester extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {
  behavior of "L2"
  implicit val defaultConfig = new Config((_, _, _) => {
    case CacheParamsKey => CacheParameters()
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

trait WithFixSubModuleInputs { this: HasTestAnnos =>
  testAnnos = testAnnos :+ RunFirrtlTransformAnnotation(new FixSubModuleInputs)
}