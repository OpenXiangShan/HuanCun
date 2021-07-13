package huancun

import chisel3._
import chiseltest._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLFuzzer, TLRAM, TLXbar}

class AllocatorTest extends L2Tester {

  val system = LazyModule(new ExampleSystem())
  chisel3.stage.ChiselStage.elaborate(system.module)

  val mshrAlloc = chisel3.aop.Select.collectDeep[MSHRAlloc](system.module){
    case alloc: MSHRAlloc =>
      alloc
  }.head

  it should "pass" in {
    test(new MSHRAlloc()(mshrAlloc.p)){ dut =>
      dut.clock.step(10)
    }
  }

}
