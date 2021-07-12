package huancun

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLEphemeralNode, TLFragmenter, TLFuzzer, TLRAM, TLWidthWidget, TLXbar}

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

  it should "send outer acquire" in {

    val top = LazyModule(new ExampleSystem(l1iReq = 1))

    test(top.module).withAnnotations(testAnnos) { dut =>
      while (!dut.success.peek().litToBoolean){
        dut.clock.step(1)
      }
    }
  }

}
