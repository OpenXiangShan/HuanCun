package huancun

import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule

class DSTest extends L2Tester {

  val system = LazyModule(new ExampleSystem())
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
