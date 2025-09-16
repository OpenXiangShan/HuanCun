package huancun

import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}

class AllocatorTest extends L2Tester {

  val system = DisableMonitors(p =>LazyModule(new ExampleSystem()(p)))(defaultConfig)
  circt.stage.ChiselStage.elaborate(system.module)

  val mshrAlloc = chisel3.aop.Select.collectDeep[MSHRAlloc](system.module){
    case alloc: MSHRAlloc =>
      alloc
  }.head

  it should "pass" in {
    simulate(new MSHRAlloc()(mshrAlloc.p)){ dut =>
      dut.clock.step(10)
    }
  }

}
