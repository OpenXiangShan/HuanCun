package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun.utils.{XSPerfAccumulate, XSPerfHistogram}

class TopDownMonitor()(implicit p: Parameters) extends HuanCunModule {
  val banks = 1 << bankBits
  val io = IO(new Bundle() {
    val msStatus = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus))))
  })

  /* ====== PART ONE ======
  * Check whether the Addr given by core is a Miss in Cache
  */
  for (hartId <- cacheParams.hartIds) {
    val perfName = s"${cacheParams.name}MissMatch_${hartId}"

    val pAddr = WireInit(0.U.asTypeOf(Valid(UInt(36.W)))) // TODO: hand written to match PAddrBits in SoC.scala
    ExcitingUtils.addSink(pAddr, s"rob_head_paddr_${hartId}", ExcitingUtils.Perf)

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

    XSPerfAccumulate(cacheParams, perfName, addrMatch)
    ExcitingUtils.addSource(addrMatch, perfName, ExcitingUtils.Perf)
  }

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

  /* ====== PART THREE ======
 * Distinguish req sources and count num & miss
 */
  // TODO: the following comparison have extensive logic
  for (i <- 0 until MemReqSource.ReqSourceCount.id) {
    // use will_free to make sure each req is counted only once
    val sourceMatchVec     = allMSHRMatchVec(s => s.will_free && s.fromA && s.reqSource === i.U)
    val sourceMatchVecMiss = allMSHRMatchVec(s => s.will_free && s.fromA && s.reqSource === i.U && s.is_miss)
    XSPerfAccumulate(cacheParams, s"${cacheParams.name}AReqSource_${i}_Total", PopCount(sourceMatchVec))
    XSPerfAccumulate(cacheParams, s"${cacheParams.name}AReqSource_${i}_Miss",  PopCount(sourceMatchVecMiss))
  }

  // or we can io.msStatus.flatten.map(_.reqSource) to get all reqSource
  // then io.msStatus.flatten.map(_.valid && _.will_free) as enable
  // finally use XSPerfHistogram

}
