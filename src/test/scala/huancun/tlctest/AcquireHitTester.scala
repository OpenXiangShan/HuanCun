package huancun.tlctest

import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.LazyModule
import huancun.DumpVCD
import tltest.{ScoreboardData, TLCTrans, TLMessagesBigInt}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class AcquireHitTester extends TLCTest with DumpVCD{
  it should "second acuqire hit" in {
    test(testTop.module).withAnnotations(testAnnos) { dut =>
      testTop.l1d.agent.addAcquire(512, TLMessagesBigInt.toB)
      testTop.l1d.agent.addAcquire(512, TLMessagesBigInt.toT)
      while (testTop.l1d.agent.outerAcquire.nonEmpty) {
        testTop.l1d.update(dut.io)
        dut.clock.step(1)
      }
    }
  }
}
