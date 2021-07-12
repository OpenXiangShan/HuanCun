package huancun
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLRAM, TLXbar}

class ConnectionTester extends L2Tester {

  it should "generate firrtl correctly" in {

    val top = LazyModule(new ExampleSystem(nBanks = 2))

    test(top.module).withAnnotations(testAnnos) { dut =>
      dut.clock.step(1)
    }
  }

}
