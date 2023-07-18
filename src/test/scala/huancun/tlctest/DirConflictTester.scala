package huancun.tlctest

import org.chipsalliance.cde.config.Parameters
import huancun.noninclusive.HasClientInfo
import huancun.{DumpVCD, HCCacheParamsKey, HasHuanCunParameters, UseVerilatorBackend}
import tltest.TLMessagesBigInt.trunk
import chiseltest._
import chisel3.util._

class DirConflictTester extends TLCTest
  with RandomSampleUtil
  with DumpVCD
  with UseVerilatorBackend
{

  val param = defaultConfig(HCCacheParamsKey)
  val clientWays = param.clientCaches.head.ways
  val clientSets = param.clientCaches.head.sets
  val clientSetBits = log2Ceil(clientSets)
  val offsetBits = log2Ceil(param.blockBytes)

  it should "pass" in {
    test(testTop.module).withAnnotations(testAnnos) { dut =>
      val l1d = testTop.l1d_list.head
      for(i <- 0 until clientWays * 2){
        //   | tag | 0...0 |
        val addr = i << (clientSetBits + offsetBits)
        l1d.agent.addAcquire(addr, trunk)
      }
      while (l1d.agent.outerAcquire.nonEmpty){
        l1d.update(dut.l1d_io.head)
        dut.clock.step(1)
      }
    }
  }

}
