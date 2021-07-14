package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLBundle, TLBundleParameters, TLCacheCork, TLFragmenter, TLRAM, TLXbar}
import tltest.{ScoreboardData, TLCTrans, TLMessagesBigInt}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TestTop
(serialList: ArrayBuffer[(Int, TLCTrans)],
 scoreboard: mutable.Map[BigInt, ScoreboardData]
)(implicit p: Parameters) extends LazyModule {

  val l1d = LazyModule(new MasterAgent(
    id = 0,
    name = "l1d",
    probe = true,
    serialList,
    scoreboard
  ))

  val l1i = LazyModule(new FakeL1I(nBanks = 1))
  val ptw = LazyModule(new FakePTW(nBanks = 1))

  val l2 = LazyModule(new HuanCun())
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))
  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      l2.node

  l2.node :=*
    l1d.node

  l2.node :=* l1i.node
  l2.node :=* ptw.node

  lazy val module = new LazyModuleImp(this){
    val io = IO(Flipped(l1d.module.tl_master_io.cloneType))
    l1d.module.tl_master_io <> io
  }

}


class TLCTest extends L2Tester with DumpVCD {

  it should "do single acquire" in {
    val serialList = ArrayBuffer[(Int, TLCTrans)]()
    val scoreboard = mutable.Map[BigInt, ScoreboardData]()
    val testTop = LazyModule(new TestTop(serialList, scoreboard))
    test(testTop.module).withAnnotations(testAnnos){ dut =>
      testTop.l1d.agent.addAcquire(512, TLMessagesBigInt.toT)
      while (testTop.l1d.agent.outerAcquire.nonEmpty) {
        testTop.l1d.agent.issueA()
        testTop.l1d.agent.issueC()
        testTop.l1d.update(dut.io)
        testTop.l1d.agent.step()
        dut.clock.step(1)
      }
    }
  }

  it should "do multiple acquire" in {
    val serialList = ArrayBuffer[(Int, TLCTrans)]()
    val scoreboard = mutable.Map[BigInt, ScoreboardData]()
    val testTop = LazyModule(new TestTop(serialList, scoreboard))
    test(testTop.module).withAnnotations(testAnnos){ dut =>
      for (i <- 0 until 5) {
        testTop.l1d.agent.addAcquire(512*i, TLMessagesBigInt.toT)
      }
      while (testTop.l1d.agent.outerAcquire.nonEmpty) {
        testTop.l1d.agent.issueA()
        testTop.l1d.agent.issueC()
        testTop.l1d.update(dut.io)
        testTop.l1d.agent.step()
        dut.clock.step(1)
      }
    }
  }

  // TODO: has bugs here
  it should "second acuqire hit" in {
    val serialList = ArrayBuffer[(Int, TLCTrans)]()
    val scoreboard = mutable.Map[BigInt, ScoreboardData]()
    val testTop = LazyModule(new TestTop(serialList, scoreboard))
    test(testTop.module).withAnnotations(testAnnos) { dut =>
      testTop.l1d.agent.addAcquire(512, TLMessagesBigInt.toT)
      testTop.l1d.agent.addAcquire(512, TLMessagesBigInt.toT)
      while (testTop.l1d.agent.outerAcquire.nonEmpty) {
        testTop.l1d.agent.issueA()
        testTop.l1d.agent.issueC()
        testTop.l1d.update(dut.io)
        testTop.l1d.agent.step()
        dut.clock.step(1)
      }
    }
  }

  it should "trigger release" in {
    val serialList = ArrayBuffer[(Int, TLCTrans)]()
    val scoreboard = mutable.Map[BigInt, ScoreboardData]()
    val testTop = LazyModule(new TestTop(serialList, scoreboard))
    test(testTop.module).withAnnotations(testAnnos) { dut =>
      for (i <- 0 until 5) {
        testTop.l1d.agent.addAcquire(i * 0x2000, TLMessagesBigInt.toT)
      }
      while (testTop.l1d.agent.outerAcquire.nonEmpty) {
        testTop.l1d.agent.issueA()
        testTop.l1d.agent.issueC()
        testTop.l1d.update(dut.io)
        testTop.l1d.agent.step()
        dut.clock.step(1)
      }
    }
  }
}
