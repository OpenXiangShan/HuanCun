package huancun
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLAdapterNode, TLCacheCork, TLFuzzer, TLRAM, TLXbar}

class ConnectionTester extends L2Tester {

  val nBanks = 2

  class TestTop extends LazyModule {
    val l1d = LazyModule(new FakeL1D(nBanks))
    val l1i = LazyModule(new FakeL1I(nBanks))
    val ptw = LazyModule(new FakePTW(nBanks))
    val l2 = LazyModule(new HuanCun())

    /**
      *         xbar <= l2 bank0 <= { l1d_bank0, l1i_bank0 }
      *  ram <=
      *         xbar <= l2 bank1 <= { l1d_bank1, l1i_bank1 }
      */
    val ram = LazyModule(new TLRAM(AddressSet(0, 0xffff)))
    ram.node :=
      TLXbar() :=*
      BankBinder(nBanks, l2.cacheParams.blockBytes) :=*
      l2.node

    l2.node :=*
        l1d.node

    l2.node :=*
        l1i.node

    l2.node :=*
      ptw.node

    lazy val module = new LazyModuleImp(this) {}
  }

  it should "generate firrtl correctly" in {

    val top = LazyModule(new TestTop)

    test(top.module).withAnnotations(testAnnos) { dut =>
      dut.clock.step(1)
    }
  }

}
