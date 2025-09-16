package huancun

import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import freechips.rocketchip.diplomacy.DisableMonitors

class AcquireTester extends L2Tester {

  it should "send outer acquire" in {

    implicit val sim = L2Tester.verilatorWithVcd

    val top = DisableMonitors(p => LazyModule(new ExampleSystem(l1dReq = 2)(p)))(defaultConfig)

    simulate(top.module) { dut =>
      var stepCount = 0
      val timeout = 2000
      while (!dut.success.peek().litToBoolean && stepCount < timeout) {
        dut.clock.step(1)
        stepCount += 1
      }
      if (stepCount >= timeout) {
        throw new Exception(s"timeout on ${dut.clock} at $timeout idle cycles")
      }
    }
  }

}
