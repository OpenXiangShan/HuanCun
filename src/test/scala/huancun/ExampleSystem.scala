package huancun

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLFragmenter, TLRAM, TLWidthWidget, TLXbar}

class ExampleSystem(nBanks: Int = 1, l1dReq: Int = 0, l1iReq: Int = 0, ptwReq: Int = 0)(implicit p: Parameters) extends LazyModule {

  val l1d = LazyModule(new FakeL1D(nBanks, l1dReq))
  val l1i = LazyModule(new FakeL1I(nBanks, l1iReq))
  val ptw = LazyModule(new FakePTW(nBanks, ptwReq))
  val xbar = TLXbar()
  val l2 = LazyModule(new HuanCun())

  /**
    *         xbar <= l2 bank0 <= { l1d_bank0, l1i_bank0 }
    *  ram <=
    *         xbar <= l2 bank1 <= { l1d_bank1, l1i_bank1 }
    */
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))
  ram.node :=
    TLXbar() :=*
      BankBinder(nBanks, l2.cacheParams.blockBytes) :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      l2.node :=* xbar

  xbar := l1d.node
  xbar := l1i.node
  xbar := ptw.node

  class ExampleSystemImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val success = IO(Output(Bool()))
    success := Seq(l1d, l1i, ptw).map(_.module.finish).reduce(_&&_)
  }
  lazy val module = new ExampleSystemImp(this)
}