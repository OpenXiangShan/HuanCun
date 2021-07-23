package huancun.tlctest

import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.LazyModule
import huancun.DumpVCD
import tltest.{ScoreboardData, TLCTrans, TLMessagesBigInt}

class BasicTester extends TLCTest with DumpVCD{
  it should "do single acquire" in {
    test(testTop.module).withAnnotations(testAnnos){ dut =>
      testTop.l1d.agent.addAcquire(512, TLMessagesBigInt.toT)
      while (testTop.l1d.agent.outerAcquire.nonEmpty) {
        testTop.l1d.agent.issueA()
        testTop.l1d.agent.issueC()
        testTop.l1d.update(dut.io)
        testTop.l1d.agent.step()
        dut.clock.step(1)
      }
    }
  }
}
