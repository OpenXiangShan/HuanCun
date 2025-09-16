package huancun.tlctest

import chisel3.util._
import huancun.{HCCacheParamsKey, L2Tester}

class DirConflictTester extends TLCTest with RandomSampleUtil {

  val param = defaultConfig(HCCacheParamsKey)
  val clientWays = param.clientCaches.head.ways
  val clientSets = param.clientCaches.head.sets
  val clientSetBits = log2Ceil(clientSets)
  val offsetBits = log2Ceil(param.blockBytes)

  it should "pass" in {
    implicit val sim = L2Tester.verilatorWithVcd
    simulate(testTop.module) { dut =>
      val l1d = testTop.l1d_list.head
      for(i <- 0 until clientWays * 2){
        //   | tag | 0...0 |
        val addr = i << (clientSetBits + offsetBits)
        l1d.agent.addAcquire(addr, tltest.TLMessagesBigInt.trunk)
      }
      while (l1d.agent.outerAcquire.nonEmpty){
        l1d.update(dut.l1d_io.head)
        dut.clock.step(1)
      }
    }
  }

}
