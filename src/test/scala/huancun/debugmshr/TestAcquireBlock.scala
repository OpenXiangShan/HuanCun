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


  // and send them to the standalone MSHR module under test
  it should "AcquireBlock" in {
    print("req_channel,req_opcode,req_param,req_source,req_tag,req_set,req_offset,req_mask,req_dirty,req_preferCache,req_fromPH,req_fromCH,req_needProbeAckData,")
    print("self_hit,self_state,client_1_hit,client_1_state,client_0_hit,client_0_state,")
    print("s_acquire,s_probe,s_release,s_probeack,s_execute,s_grantack,s_wbselfdir,s_wbselftag,s_wbclientsdir,s_wbclientstag,s_transferput,s_writerelease,s_writeprobe,s_triggerprefetch,s_prefetchack,w_probeackfirst,w_probeacklast,w_probeack,w_grantfirst,w_grantlast,w_grant,w_releaseack,w_grantack,w_putwritten,")
    print("new_self_state,new_self_dirty,new_self_clientState_1,new_self_clientState_0,new_client_1_hit,new_client_1_state,new_client_0_hit,new_client_0_state,")
    print("oa_opcode,oa_param,oa_tag,oa_set,oa_offset,oa_source,oa_needData,oa_putData,oa_bufIdx,oa_size,")
    print("ob_param,ob_tag,ob_set,ob_alias,ob_clients,ob_needData,")
    print("oc_opcode,oc_param,oc_tag,oc_set,oc_way,oc_dirty,oc_source,")
    print("od_opcode,od_param,od_channel,od_tag,od_set,od_way,od_offset,od_size,od_dirty,od_denied,od_bufIdx,od_bypassPut,od_sourceId,od_sinkId,od_useBypass,")
    print("oe_sink\n")
    test(new huancun.debug.DebugMSHR()(mshr.p)){ ms =>
      // input basics
      ms.io.id.poke(0.U)
      ms.io.enable.poke(true.B)

      // input req alloc: AcquireBlock
      // print("#1   alloc req\n")
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
// input dir result:
//  self miss but not INVALID, may release first.
//  clients miss.
//--------------------------------------------------------------------------
      // print("#10    get dirResult: self miss but not INVALID, may release first.\n")
      // ms.clock.step(10)
      // ms.io.dirResult.valid.poke(true.B)
      // val dirr = ms.io.dirResult.bits
      // dirr.sourceId.poke(0.U)
      // dirr.self.hit.poke(0.U)   // miss
      // dirr.self.way.poke(0.U)
      // dirr.self.tag.poke(0x0.U)
      // dirr.self.dirty.poke(0.U)
      // dirr.self.state.poke(TIP) // but has data
      // dirr.self.clientStates.foreach { _.poke(INVALID) }
      // // dirr.self.prefetch.poke(Some(false.B))
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

// -----------------------------------------------------------
// input dir result: 
//  self hit.
//  clients miss.
// -----------------------------------------------------------
//       ms.clock.step(10)
//       ms.io.dirResult.valid.poke(true.B)
//       val dirr = ms.io.dirResult.bits
//       dirr.sourceId.poke(0.U)
//       dirr.self.dirty.poke(0.U)
//       dirr.self.state.poke(TIP)
//       dirr.self.clientStates.foreach { _.poke(INVALID) }
//       // dirr.self.prefetch.poke(Some(false.B))
//       dirr.self.hit.poke(1.U)
//       dirr.self.way.poke(0.U)
//       dirr.self.tag.poke(0x1.U)
//       dirr.self.error.poke(0.U)
//       dirr.clients.states(0).state.poke(INVALID)
//       dirr.clients.states(0).hit.poke(0.U)
//       dirr.clients.states(1).state.poke(INVALID)
//       dirr.clients.states(1).hit.poke(0.U)
//       dirr.clients.tag.poke(1.U)
//       dirr.clients.way.poke(0.U)
//       dirr.clients.error.poke(0.U)
//       ms.clock.step(1)
//       ms.io.dirResult.valid.poke(false.B)


// -----------------------------------------------------------
// input dir result: 
//  self hit.
//  clients hit.
// -----------------------------------------------------------
      ms.clock.step(10)
      ms.io.dirResult.valid.poke(true.B)
      val dirr = ms.io.dirResult.bits
      dirr.sourceId.poke(0.U)
      dirr.self.dirty.poke(0.U)
      dirr.self.state.poke(TIP)
      dirr.self.hit.poke(1.U)
      dirr.self.clientStates.foreach { _.poke(INVALID) }
      // dirr.self.prefetch.poke(Some(false.B))
      dirr.self.way.poke(0.U)
      dirr.self.tag.poke(0x1.U)
      dirr.self.error.poke(0.U)
      dirr.clients.states(0).state.poke(INVALID)
      dirr.clients.states(0).hit.poke(0.U)
      dirr.clients.states(1).state.poke(BRANCH)
      dirr.clients.states(1).hit.poke(1.U)
      dirr.clients.tag.poke(1.U)
      dirr.clients.way.poke(0.U)
      dirr.clients.error.poke(0.U)
      ms.clock.step(1)
      ms.io.dirResult.valid.poke(false.B)

      // print("#11    meta_valid is set\n")
      ms.clock.step(1)

    }
  }
}
