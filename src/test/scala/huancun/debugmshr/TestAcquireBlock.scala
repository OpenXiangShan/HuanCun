package huancun.debugmshr

import chisel3._
import chiseltest._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLFuzzer, TLRAM, TLXbar}
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.TLHints._
import chipsalliance.rocketchip.config.Parameters
import huancun.{L2Tester, TestTop_L2}
import huancun.MetaData._
import huancun.noninclusive.{MSHR, DirResult}
import huancun.debug.DebugMSHR

class TestAcquireBlock extends L2Tester {

  val system = LazyModule(new TestTop_L2())
  chisel3.stage.ChiselStage.elaborate(system.module)

  // get parameters of mshr in the system
  val mshr = chisel3.aop.Select.collectDeep[huancun.noninclusive.MSHR](system.module){
    case ms: huancun.noninclusive.MSHR =>
      ms
  }.head

  println("AcquireBlock.")
  // and send them to the standalone MSHR module under test
  it should "AcquireBlock, both miss" in {
    test(new huancun.debug.DebugMSHR()(mshr.p)){ ms =>
      // input basics
      ms.io.id.poke(0.U)
      ms.io.enable.poke(true.B)

      // input req alloc: AcquireBlock
      print("#1   alloc req\n")
      ms.clock.step(1)
      ms.io.alloc.valid.poke(true.B)
      val allocInfo = ms.io.alloc.bits
      allocInfo.channel.poke(1.U(3.W))
      allocInfo.opcode.poke(AcquireBlock)
      allocInfo.param.poke(NtoT)
      allocInfo.size.poke(6.U)    // &
      allocInfo.source.poke(0.U)  // &
      allocInfo.set.poke(0x0.U)   // &
      allocInfo.tag.poke(0x1.U)   // &
      allocInfo.off.poke(0.U)
      allocInfo.mask.poke(0xFFFF.U(32.W))
      allocInfo.bufIdx.poke(0.U)
      allocInfo.needHint.foreach(_.poke(false.B))
      allocInfo.isPrefetch.foreach(_.poke(false.B))
      allocInfo.alias.foreach(_.poke(0.U))
      allocInfo.preferCache.poke(true.B)
      allocInfo.dirty.poke(false.B)
      allocInfo.fromProbeHelper.poke(false.B)
      allocInfo.fromCmoHelper.poke(false.B)
      allocInfo.needProbeAckData.foreach(_.poke(false.B))
      ms.clock.step(1)
      ms.io.alloc.valid.poke(false.B)

//===========================================================
//-----------------------------------------------------------
// input dir result: self miss, clients miss
//-----------------------------------------------------------
      // print("#10    get dirResult: both miss\n")
      // ms.clock.step(10)
      // ms.io.dirResult.valid.poke(true.B)
      // val dirr = ms.io.dirResult.bits
      // dirr.sourceId.poke(0.U)
      // dirr.self.dirty.poke(0.U)
      // dirr.self.state.poke(INVALID)
      // dirr.self.clientStates.foreach { _.poke(INVALID) }
      // // dirr.self.prefetch.poke(Some(false.B))
      // dirr.self.hit.poke(0.U)
      // dirr.self.way.poke(0.U)
      // dirr.self.tag.poke(0x0.U)
      // dirr.self.error.poke(0.U)
      // dirr.clients.states.map { case s =>
      //   s.state.poke(INVALID)
      //   // s.alias.poke(0.U)
      //   s.hit.poke(0.U)
      // }
      // dirr.clients.tag.poke(0.U)
      // dirr.clients.way.poke(0.U)
      // dirr.clients.error.poke(0.U)
      // ms.clock.step(1)
      // ms.io.dirResult.valid.poke(false.B)

//--------------------------------------------------------------------------
// another case: self miss but not INVALID, clients miss, may release first.
//--------------------------------------------------------------------------
      print("#10    get dirResult: self miss but not INVALID, may release first.\n")
      ms.clock.step(10)
      ms.io.dirResult.valid.poke(true.B)
      val dirr = ms.io.dirResult.bits
      dirr.sourceId.poke(0.U)
      dirr.self.hit.poke(0.U)   // miss
      dirr.self.way.poke(0.U)
      dirr.self.tag.poke(0x0.U)
      dirr.self.dirty.poke(0.U)
      dirr.self.state.poke(TIP) // but has data
      dirr.self.clientStates.foreach { _.poke(INVALID) }
      // dirr.self.prefetch.poke(Some(false.B))
      dirr.self.error.poke(0.U)
      dirr.clients.states.map { case s =>
        s.state.poke(INVALID)
        // s.alias.poke(0.U)
        s.hit.poke(0.U)
      }
      dirr.clients.tag.poke(0.U)
      dirr.clients.way.poke(0.U)
      dirr.clients.error.poke(0.U)
      ms.clock.step(1)
      ms.io.dirResult.valid.poke(false.B)

      print("#11    meta_valid is set\n")
      ms.clock.step(1)

    }
  }
}
