package huancun.tlctest

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import freechips.rocketchip.diplomacy.{AddressSet, DisableMonitors, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLDelayer, TLFragmenter, TLRAM, TLWidthWidget, TLXbar}
import huancun._
import tltest.{ScoreboardData, TLCTrans, TLMessagesBigInt}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TestTop
(serialList: ArrayBuffer[(Int, TLCTrans)],
 scoreboard: mutable.Map[BigInt, ScoreboardData]
)(implicit p: Parameters) extends LazyModule {

  val delayFactor = 0

  val l1d = LazyModule(new MasterAgent(
    id = 0,
    name = "l1d",
    probe = true,
    serialList,
    scoreboard
  ))

  val l1i = LazyModule(new MasterULAgent(
    id = 1,
    name = "l1i",
    probe = false,
    serialList,
    scoreboard
  ))

  val ptw = LazyModule(new FakePTW(nBanks = 1))
  val xbar = TLXbar()

  val l2 = LazyModule(new HuanCun())
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))
  ram.node :=
    TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      TLBuffer() :=*
      TLDelayer(delayFactor) :=*
      TLBuffer() :=*
      l2.node :=* xbar

  xbar := TLBuffer() := TLDelayer(delayFactor) := TLBuffer() := l1d.node
  xbar := TLBuffer() := TLDelayer(delayFactor) := TLBuffer() := l1i.node
  xbar := TLBuffer() := TLDelayer(delayFactor) := TLBuffer() := ptw.node

  lazy val module = new LazyModuleImp(this) {
    val l1dio = IO(Flipped(l1d.module.tl_master_io.cloneType))
    l1d.module.tl_master_io <> l1dio
    val l1iio = IO(Flipped(l1i.module.tl_master_io.cloneType))
    l1i.module.tl_master_io <> l1iio
  }

}

abstract class TLCTest extends L2Tester {
  val serialList = ArrayBuffer[(Int, TLCTrans)]()
  val scoreboard = mutable.Map[BigInt, ScoreboardData]()
  val testTop = DisableMonitors { p =>
    LazyModule(new TestTop(serialList, scoreboard)(p))
  }
}
