package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun.utils.{XSPerfAccumulate, XSPerfHistogram}

class TopDownMonitor()(implicit p: Parameters) extends HuanCunModule {
  val banks = 1 << bankBits
  val io = IO(new Bundle() {
    val cpuAddr  = Flipped(ValidIO(UInt(39.W))) // CPU using ExcitingUtils to give addr, this deprecated
    val msStatus = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus))))
  })

  val pAddr = WireInit(0.U.asTypeOf(Valid(UInt(36.W)))) // TODO: hand written to match PAddrBits in SoC.scala
  ExcitingUtils.addSink(pAddr, "rob_head_paddr_0", ExcitingUtils.Perf) // TODO: add core1

  /* ====== PART ONE ======
   * Check whether the Addr given by core is a Miss in Cache
  */
  val addrMatchVec = io.msStatus.zipWithIndex.map {
    case(slice, i) =>
      slice.map {
        ms =>
          val msBlockAddr = if(bankBits == 0) Cat(ms.bits.tag, ms.bits.set)
                            else Cat(ms.bits.tag, ms.bits.set, i.U(bankBits-1, 0))
          val pBlockAddr  = (pAddr.bits >> 6.U).asUInt

          val isCPUReq = ms.valid && ms.bits.fromA && !ms.bits.is_prefetch // TODO: whether we need fromA
          val isMiss   = ms.bits.is_miss
          pAddr.valid && msBlockAddr === pBlockAddr && isCPUReq && isMiss
      }
  }

  val addrMatch = Cat(addrMatchVec.flatten).orR

  val perfName = if(cacheParams.name == "L2") "L2MissMatch" else "L3MissMatch"

  XSPerfAccumulate(cacheParams, perfName, addrMatch)
  ExcitingUtils.addSource(addrMatch, perfName, ExcitingUtils.Perf)
  ExcitingUtils.addSink(WireDefault(addrMatch), perfName, ExcitingUtils.Perf)

  /* ====== PART TWO ======
   * Count the parallel misses, and divide them into CPU/Prefetch
  */
  def allMSHRMatchVec(cond: MSHRStatus => Bool): IndexedSeq[Bool] = {
    io.msStatus.zipWithIndex.flatMap {
      case (slice, i) =>
        slice.map {
          ms => ms.valid && cond(ms.bits)
        }
    }
  }

  val missVecCPU  = allMSHRMatchVec(s => s.fromA && s.is_miss && !s.is_prefetch)
  val missVecPref = allMSHRMatchVec(s => s.fromA && s.is_miss &&  s.is_prefetch)
  // val missVecAll      = allMSHRMatchVec(s => s.fromA && s.is_miss)

  val totalMSHRs = banks * mshrsAll
  XSPerfHistogram(cacheParams, "parallel_misses_CPU" , PopCount(missVecCPU), true.B, 0, totalMSHRs, 1)
  XSPerfHistogram(cacheParams, "parallel_misses_Pref", PopCount(missVecPref), true.B, 0, totalMSHRs, 1)
  XSPerfHistogram(cacheParams, "parallel_misses_All" , PopCount(missVecCPU)+PopCount(missVecPref), true.B, 0, 32, 1)
}
