package huancun

import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}

class DSTest extends L2Tester {

  val system = DisableMonitors(p => LazyModule(new ExampleSystem()(p)))(defaultConfig)
  circt.stage.ChiselStage.elaborate(system.module)

  val datastorage = chisel3.aop.Select.collectDeep[DataStorage](system.module){
    case ds: DataStorage =>
      ds
  }.head

  it should "pass" in {
    simulate(new DataStorage()(datastorage.p)){ dut =>
      dut.clock.step(10)
    }
  }

}
