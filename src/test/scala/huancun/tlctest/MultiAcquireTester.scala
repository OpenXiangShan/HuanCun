package huancun.tlctest

import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import huancun.DumpVCD
import tltest.TLMessagesBigInt

class MultiAcquireTester extends TLCTest with DumpVCD {
  it should "do multiple acquire" in {
    test(testTop.module).withAnnotations(testAnnos) { dut =>
      for (i <- 0 until 5) {
        testTop.l1d.agent.addAcquire(512 * i, TLMessagesBigInt.toT)
      }
      while (testTop.l1d.agent.outerAcquire.nonEmpty) {
        testTop.l1d.update(dut.l1dio)
        dut.clock.step(1)
      }
    }
  }
}
