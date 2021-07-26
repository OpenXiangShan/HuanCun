package huancun.tlctest

import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.LazyModule
import huancun.{DumpVCD, UseVerilatorBackend}
import tltest.{ScoreboardData, TLCTrans, TLMessagesBigInt}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ReleaseTester extends TLCTest with DumpVCD {
  it should "trigger release" in {
    test(testTop.module).withAnnotations(testAnnos) { dut =>
      for (i <- 0 until 5) {
        testTop.l1d.agent.addAcquire((i+1) * 0x2000, TLMessagesBigInt.NtoT)
      }
      while (testTop.l1d.agent.outerAcquire.nonEmpty) {
        testTop.l1d.update(dut.l1dio)
        dut.clock.step(1)
      }
    }
  }
}
