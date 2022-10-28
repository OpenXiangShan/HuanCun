package huancun

import chisel3._
import chiseltest._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLFuzzer, TLRAM, TLXbar}
import huancun.noninclusive.MSHR
import huancun.debug.DebugMSHR

class MSTest extends L2Tester {

  val system = LazyModule(new ExampleSystem())
  chisel3.stage.ChiselStage.elaborate(system.module)

  // get parameters of mshr in the system
  val mshr = chisel3.aop.Select.collectDeep[noninclusive.MSHR](system.module){
    case ms: noninclusive.MSHR =>
      ms
  }.head

  // and send them to the standalone MSHR module under test
  it should "pass" in {
    test(new debug.DebugMSHR()(mshr.p)){ ms =>
      ms.io.id.poke(0.U)
      ms.io.enable.poke(true.B)

      
      ms.clock.step(10)
      
      ms.io.status.valid.expect(false.B)
    }
  }
  println("Hello HuanCun")
}
