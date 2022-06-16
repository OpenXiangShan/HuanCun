package huancun.prefetch

import chisel3._
import chiseltest._
import chisel3.util.{Mux1H, OHToUInt}
import freechips.rocketchip.util.{Random, SeqToAugmentedSeq}
import freechips.rocketchip.diplomacy._
import chipsalliance.rocketchip.config._
import huancun.{HCCacheParameters, HCCacheParamsKey, CacheParameters}
import huancun.{L2Tester, ExampleSystem}

class MatryoshkaTest extends L2Tester(Some(MatryoshkaParameters())) {
  val system = LazyModule(new ExampleSystem())
  chisel3.stage.ChiselStage.elaborate(system.module)

  val prefetcher = chisel3.aop.Select.collectDeep[Matryoshka](system.module){
    case sp: Matryoshka =>
    sp
  }.head

  it should "pass" in {
    test(new Matryoshka()(prefetcher.p)) { dut =>
      def printOutput(): Unit = {
        // println(dut.io.req.valid.peek())
        // println(dut.io.req.bits.set.peek())
        // println(dut.io.req.bits.tag.peek())
      }

      def streamAccess(count:Int, tag:Int): Unit = {
        for (i <- 0 until count) {
          dut.io.train.valid.poke(true.B)
          dut.io.train.bits.tag.poke(tag.U)
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
      streamAccess(10, 257)
      dut.io.train.valid.poke(false.B)
      dut.clock.step(4)
    }
  }
}