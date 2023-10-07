package huancun.tlctest

import org.chipsalliance.cde.config.Parameters
import chisel3._
import freechips.rocketchip.diplomacy.{AddressSet, DisableMonitors, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLDelayer, TLFragmenter, TLRAM, TLWidthWidget, TLXbar}
import huancun._
import tltest.{ScoreboardData, TLCMasterAgent, TLCTrans, TLMessagesBigInt, TLULMasterAgent}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TestTop
(serialList: ArrayBuffer[(Int, TLCTrans)],
 scoreboard: mutable.Map[BigInt, ScoreboardData],
 dcacheNum: Int = 2,
 icacheNum: Int = 1
)(implicit p: Parameters) extends LazyModule {

  val delayFactor = 0.2

  var id = 0
  def get_id_and_inc() = {val ret = id; id = id + 1; ret}

  val l1d_list = List.fill(dcacheNum) {
    val id = get_id_and_inc()
    LazyModule(new MasterAgent(
      id = id,
      name = s"l1d$id",
      probe = true,
      serialList,
      scoreboard
    ))
  }

  val l1i_list = List.fill(icacheNum) {
    val id = get_id_and_inc()
    LazyModule(new MasterULAgent(
      id = id,
      name = s"l1i$id",
      probe = false,
      serialList,
      scoreboard
    ))
  }

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

  for(agent <- l1d_list ++ l1i_list) {
    xbar := TLBuffer() := TLDelayer(delayFactor) := TLBuffer() := agent.node
  }
  xbar := TLBuffer() := TLDelayer(delayFactor) := TLBuffer() := ptw.node

  class TestTopImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val l1d_io = IO(Vec(dcacheNum, Flipped(l1d_list.head.module.tl_master_io.cloneType)))
    l1d_list.zip(l1d_io).foreach{
      case (agent, tl_master_io) => agent.module.tl_master_io <> tl_master_io
    }
    val l1i_io = IO(Vec(icacheNum, Flipped(l1i_list.head.module.tl_master_io.cloneType)))
    l1i_list.zip(l1i_io).foreach{
      case (agent, tl_master_io) => agent.module.tl_master_io <> tl_master_io
    }
  }

  lazy val module = new TestTopImp(this)
}

abstract class TLCTest extends L2Tester {
  val serialList = ArrayBuffer[(Int, TLCTrans)]()
  val scoreboard = mutable.Map[BigInt, ScoreboardData]()
  val testTop = LazyModule(new TestTop(serialList, scoreboard, dcacheNum = 2))

  def l1d_map(func: TLCMasterAgent => Unit) = testTop.l1d_list.map(_.agent).foreach(func)
  def l1i_map(func: TLULMasterAgent => Unit) = testTop.l1i_list.map(_.agent).foreach(func)

}
