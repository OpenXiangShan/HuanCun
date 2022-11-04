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

class TestProbe extends L2Tester {

  val system = LazyModule(new TestTop_L2())
  chisel3.stage.ChiselStage.elaborate(system.module)

  // get parameters of mshr in the system
  val mshr = chisel3.aop.Select.collectDeep[huancun.noninclusive.MSHR](system.module){
    case ms: huancun.noninclusive.MSHR =>
      ms
  }.head

  // and send them to the standalone MSHR module under test
  it should "AcquireBlock, self miss, clients hit" in {
    test(new huancun.debug.DebugMSHR()(mshr.p)){ ms =>
      print("AcquireBlock, self miss, clients hit, should probe!\n")
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


      print("#10  get dirResult.\n")
//===========================================================
//-----------------------------------------------------------
// input dir result:
//  self miss.
//  client0: INV; client1: Branch
//-----------------------------------------------------------
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
      // dirr.self.tag.poke(0x1.U)
      // dirr.self.error.poke(0.U)
      // dirr.clients.states(0).state.poke(INVALID)
      // dirr.clients.states(0).hit.poke(0.U)
      // dirr.clients.states(1).state.poke(BRANCH)
      // dirr.clients.states(1).hit.poke(1.U)
      // dirr.clients.tag.poke(1.U)
      // dirr.clients.way.poke(0.U)
      // dirr.clients.error.poke(0.U)
      // ms.clock.step(1)
      // ms.io.dirResult.valid.poke(false.B)

//-----------------------------------------------------------
// input dir result: 
//  self miss.
//  client0: INV; client1: T
//-----------------------------------------------------------
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
      // dirr.self.tag.poke(0x1.U)
      // dirr.self.error.poke(0.U)
      // dirr.clients.states(0).state.poke(INVALID)
      // dirr.clients.states(0).hit.poke(0.U)
      // dirr.clients.states(1).state.poke(TIP)
      // dirr.clients.states(1).hit.poke(1.U)
      // dirr.clients.tag.poke(1.U)
      // dirr.clients.way.poke(0.U)
      // dirr.clients.error.poke(0.U)
      // ms.clock.step(1)
      // ms.io.dirResult.valid.poke(false.B)

//-----------------------------------------------------------
// input dir result: 
//  self miss.
//  client0: B; client1: INV
//-----------------------------------------------------------
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
      // dirr.self.tag.poke(0x1.U)
      // dirr.self.error.poke(0.U)
      // dirr.clients.states(0).state.poke(BRANCH)
      // dirr.clients.states(0).hit.poke(1.U)
      // dirr.clients.states(1).state.poke(INVALID)
      // dirr.clients.states(1).hit.poke(0.U)
      // dirr.clients.tag.poke(1.U)
      // dirr.clients.way.poke(0.U)
      // dirr.clients.error.poke(0.U)
      // ms.clock.step(1)
      // ms.io.dirResult.valid.poke(false.B)

//-----------------------------------------------------------
// input dir result: 
//  self miss.
//  client0: T; client1: INV
//-----------------------------------------------------------
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
      // dirr.self.tag.poke(0x1.U)
      // dirr.self.error.poke(0.U)
      // dirr.clients.states(0).state.poke(TIP)
      // dirr.clients.states(0).hit.poke(1.U)
      // dirr.clients.states(1).state.poke(INVALID)
      // dirr.clients.states(1).hit.poke(0.U)
      // dirr.clients.tag.poke(1.U)
      // dirr.clients.way.poke(0.U)
      // dirr.clients.error.poke(0.U)
      // ms.clock.step(1)
      // ms.io.dirResult.valid.poke(false.B)


//-----------------------------------------------------------
// input dir result: 
//  self miss.
//  client0: B; client1: B
//-----------------------------------------------------------
      ms.clock.step(10)
      ms.io.dirResult.valid.poke(true.B)
      val dirr = ms.io.dirResult.bits
      dirr.sourceId.poke(0.U)
      dirr.self.dirty.poke(0.U)
      dirr.self.state.poke(INVALID)
      dirr.self.clientStates.foreach { _.poke(INVALID) }
      // dirr.self.prefetch.poke(Some(false.B))
      dirr.self.hit.poke(0.U)
      dirr.self.way.poke(0.U)
      dirr.self.tag.poke(0x1.U)
      dirr.self.error.poke(0.U)
      dirr.clients.states(0).state.poke(BRANCH)
      dirr.clients.states(0).hit.poke(1.U)
      dirr.clients.states(1).state.poke(BRANCH)
      dirr.clients.states(1).hit.poke(1.U)
      dirr.clients.tag.poke(1.U)
      dirr.clients.way.poke(0.U)
      dirr.clients.error.poke(0.U)
      ms.clock.step(1)
      ms.io.dirResult.valid.poke(false.B)

      print("#11  meta_valid is set, should send tasks.\n")
      ms.clock.step(1)

    }
  }
}
