package huancun

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLCacheCork, TLFuzzer, TLRAM}

class BasicTester extends L2Tester {

  class TestTop extends LazyModule {
    val cache = LazyModule(new HuanCun())
    val fuzzer = LazyModule(new TLFuzzer(
      nOperations = 2 * cache.cacheParams.mshrs, inFlight = cache.cacheParams.mshrs
    ))
    val ram = LazyModule(new TLRAM(AddressSet(0, 0xffff)))

    ram.node := TLCacheCork() := cache.node := fuzzer.node
    lazy val module = new LazyModuleImp(this){
      val pass = IO(Output(Bool()))
      pass := fuzzer.module.io.finished
    }
  }

  it should "read and write TLRAM correctly" in {

    val top = LazyModule(new TestTop)

    test(top.module).withAnnotations(testAnnos) { dut =>
      while(!dut.pass.peek().litToBoolean){
        dut.clock.step(1)
      }
    }
  }

}
