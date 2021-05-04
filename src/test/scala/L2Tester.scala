package HuanCun

import chipsalliance.rocketchip.config.Config
import chiseltest._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import firrtl.AnnotationSeq
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
  testAnnos = testAnnos :+ VerilatorBackendAnnotation
}

trait DumpVCD { this: HasTestAnnos =>
  testAnnos = testAnnos :+ WriteVcdAnnotation
}