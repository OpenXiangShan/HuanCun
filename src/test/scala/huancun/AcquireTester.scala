package huancun

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLEphemeralNode, TLFuzzer, TLRAM, TLXbar}

class AcquireListener(outer: TLDebugModuleBase) extends LazyModuleImp(outer) {
  val success = IO(Output(Bool()))
  val flags = Seq.fill(outer.node.in.size){ RegInit(false.B) }
  flags.zip(outer.node.in.map(_._1)).foreach{
    case (f, channel) =>
      when(channel.a.fire()){ f := true.B }
  }
  success := flags.reduce(_ && _)
}

class AcquireTester extends L2Tester with DumpVCD with UseVerilatorBackend {

  val nBanks = 1

  class TestTop extends LazyModule {

    val l1d = LazyModule(new FakeL1D(nBanks, 0))
    val l1i = LazyModule(new FakeL1I(nBanks, 10))
    val ptw = LazyModule(new FakePTW(nBanks, 0))
    val l2 = LazyModule(new HuanCun())

    /**
      *         xbar <= l2 bank0 <= { l1d_bank0, l1i_bank0 }
      *  ram <=
      *         xbar <= l2 bank1 <= { l1d_bank1, l1i_bank1 }
      */
    val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffffffL)))
    ram.node :=
      TLCacheCork(unsafe = true) :=
      TLXbar() :=*
        l2.node

    l2.node :=*
      l1d.node

    l2.node :=*
      l1i.node

    l2.node :=*
      ptw.node

    lazy val module = new LazyModuleImp(this) {
      val success = IO(Output(Bool()))
      success := Seq(l1d, l1i, ptw).map(_.module.finish).reduce(_&&_)
    }
  }

  it should "send outer acquire" in {

    val top = LazyModule(new TestTop)

    test(top.module).withAnnotations(testAnnos) { dut =>
      while (!dut.success.peek().litToBoolean){
        dut.clock.step(1)
      }
    }
  }

}
