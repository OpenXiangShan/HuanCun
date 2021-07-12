package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLRAM, TLXbar}

class ExampleSystem(nBanks: Int = 1, l1dReq: Int = 0, l1iReq: Int = 0, ptwReq: Int = 0)(implicit p: Parameters) extends LazyModule {

  val l1d = LazyModule(new FakeL1D(nBanks, l1dReq))
  val l1i = LazyModule(new FakeL1I(nBanks, l1iReq))
  val ptw = LazyModule(new FakePTW(nBanks, ptwReq))
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

  lazy val module = new LazyModuleImp(this) {
    val success = IO(Output(Bool()))
    success := Seq(l1d, l1i, ptw).map(_.module.finish).reduce(_&&_)
  }
}