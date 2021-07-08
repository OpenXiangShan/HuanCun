package huancun

import chiseltest._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLRAM, TLXbar}

class DSTest extends L2Tester {

  val nBanks = 1
  class System extends LazyModule {
    val l1d = LazyModule(new FakeL1D(nBanks))
    val l1i = LazyModule(new FakeL1I(nBanks))
    val ptw = LazyModule(new FakePTW(nBanks))
    val l2 = LazyModule(new HuanCun())

    /**
      *         xbar <= l2 bank0 <= { l1d_bank0, l1i_bank0 }
      *  ram <=
      *         xbar <= l2 bank1 <= { l1d_bank1, l1i_bank1 }
      */
    val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffffffL)))
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

  val system = LazyModule(new System)
  chisel3.stage.ChiselStage.elaborate(system.module)

  val datastorage = chisel3.aop.Select.collectDeep[DataStorage](system.module){
    case ds: DataStorage =>
      ds
  }.head

  it should "pass" in {
    test(new DataStorage()(datastorage.p)){ dut =>
      dut.clock.step(10)
    }
  }

}
