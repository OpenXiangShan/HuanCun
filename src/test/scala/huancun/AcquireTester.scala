package huancun

import chisel3._
import chiseltest._
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLEphemeralNode, TLFragmenter, TLFuzzer, TLRAM, TLWidthWidget, TLXbar}

class AcquireTester extends L2Tester with DumpVCD with UseVerilatorBackend {

  it should "send outer acquire" in {

    val top = LazyModule(new ExampleSystem(l1dReq = 2))

    test(top.module).withAnnotations(testAnnos) { dut =>
      dut.clock.setTimeout(2000)
      while (!dut.success.peek().litToBoolean){
        dut.clock.step(1)
      }
    }
  }

}
