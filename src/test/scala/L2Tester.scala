package HuanCun

import chipsalliance.rocketchip.config.Config
import chiseltest._
import chiseltest.internal.VerilatorBackendAnnotation
import firrtl.AnnotationSeq
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

abstract class L2Tester extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "L2"
  implicit val defaultConfig = new Config((_, _, _) => {
    case CacheParamsKey => CacheParameters()
  })
  var testAnnos: AnnotationSeq = Seq()
}

trait WithVerilatorBackend { this: L2Tester =>
  testAnnos = testAnnos :+ VerilatorBackendAnnotation
}