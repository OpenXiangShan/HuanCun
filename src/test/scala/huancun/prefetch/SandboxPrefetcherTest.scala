package huancun.prefetch

import chisel3._
import chiseltest._
import chisel3.util.{Mux1H, OHToUInt}
import freechips.rocketchip.util.{Random, SeqToAugmentedSeq}
import freechips.rocketchip.diplomacy._
import chipsalliance.rocketchip.config._
import huancun.{HCCacheParameters, HCCacheParamsKey, CacheParameters}
import huancun.{L2Tester, ExampleSystem}

class SandboxPrefetcherTest extends L2Tester(Some(SandboxParameters())) {
  val system = LazyModule(new ExampleSystem())
  chisel3.stage.ChiselStage.elaborate(system.module)

  val prefetcher = chisel3.aop.Select.collectDeep[SandboxPrefetcher](system.module){
    case sp: SandboxPrefetcher =>
      sp
  }.head

  it should "pass" in {
    test(new SandboxPrefetcher()(prefetcher.p)){ dut =>

      def printOutput(): Unit = {
        println(dut.io.req.valid.peek())
        println(dut.io.req.bits.set.peek())
        println(dut.io.req.bits.tag.peek())
      }

      def streamAccess(count:Int, initAddr:Int=0x123061): Unit = {
        for (i <- 0 until count) {
          dut.io.train.valid.poke(true.B)
          dut.io.train.bits.tag.poke(1.U)
          dut.io.train.bits.set.poke((i%128).U)
          printOutput()
          dut.clock.step()
        }
      }
 
      def stepAndprint(n:Int): Unit = {
        for (i <- 0 until n) {
          printOutput()
          dut.clock.step()
        }
      }

      dut.io.req.ready.poke(true.B)
      streamAccess(64)
      dut.io.train.valid.poke(false.B)
      stepAndprint(5)
      streamAccess(2)
    }
  }

}
