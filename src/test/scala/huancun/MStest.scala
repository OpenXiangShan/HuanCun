package huancun

import chisel3._
import chiseltest._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{BankBinder, TLCacheCork, TLFuzzer, TLRAM, TLXbar}
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.TLHints._
import chipsalliance.rocketchip.config.Parameters
import huancun.noninclusive.MSHR
import huancun.debug.DebugMSHR
import huancun.noninclusive.DirResult

class MSTest extends L2Tester {

  val system = LazyModule(new ExampleSystem())
  chisel3.stage.ChiselStage.elaborate(system.module)

  // get parameters of mshr in the system
  val mshr = chisel3.aop.Select.collectDeep[noninclusive.MSHR](system.module){
    case ms: noninclusive.MSHR =>
      ms
  }.head

  // and send them to the standalone MSHR module under test
  it should "pass" in {
    test(new debug.DebugMSHR()(mshr.p)){ ms =>
      // input basics
      ms.io.id.poke(0.U)
      ms.io.enable.poke(true.B)

      // input req alloc
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


      // input dir result
      ms.clock.step(10)
      ms.io.dirResult.valid.poke(true.B)
      val dirr = ms.io.dirResult.bits
      dirr.sourceId.poke(0.U)



      ms.clock.step(2)
      ms.io.status.valid.expect(false.B)
    }
  }
  println("Hello HuanCun")
}
