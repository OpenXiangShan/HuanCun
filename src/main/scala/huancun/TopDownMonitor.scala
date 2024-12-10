package huancun

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import huancun.noninclusive.DirResult
import huancun.utils.{XSPerfAccumulate, XSPerfHistogram}
import utility.MemReqSource

class TopDownMonitor()(implicit p: Parameters) extends HuanCunModule {
  val banks = 1 << bankBits
  val io = IO(new Bundle() {
    val dirResult = Vec(banks, Flipped(ValidIO(new DirResult)))
    val msStatus = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus))))
    val debugTopDown = new Bundle {
      val robHeadPaddr = Vec(cacheParams.hartIds.length, Flipped(Valid(UInt(36.W))))
      val addrMatch = Vec(cacheParams.hartIds.length, Output(Bool()))
    }
  })

  /* ====== PART ONE ======
  * Check whether the Addr given by core is a Miss in Cache
  */
  for (((hartId, pAddr), addrMatch) <- cacheParams.hartIds zip io.debugTopDown.robHeadPaddr zip io.debugTopDown.addrMatch) {
    val addrMatchVec = io.msStatus.zipWithIndex.map {
      case(slice, i) =>
        slice.map {
          ms =>
            val msBlockAddr = if(bankBits == 0) Cat(ms.bits.tag, ms.bits.set)
                              else Cat(ms.bits.tag, ms.bits.set, i.U(bankBits-1, 0))
            val pBlockAddr  = (pAddr.bits >> 6.U).asUInt

            val isMiss   = ms.valid && ms.bits.is_miss
            pAddr.valid && msBlockAddr === pBlockAddr && isMiss
        }
    }

    addrMatch := Cat(addrMatchVec.flatten).orR
    XSPerfAccumulate(cacheParams, s"${cacheParams.name}MissMatch_${hartId}", addrMatch)
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
  // count releases
  val releaseCnt = allMSHRMatchVec(s => s.will_free && s.fromC)
  XSPerfAccumulate(cacheParams, s"${cacheParams.name}C_ReleaseCnt_Total", PopCount(releaseCnt))

  // we can follow the counting logic of Directory to count
  // add reqSource in replacerInfo, set in MSHRAlloc, passes in Directory and get the result in DirResult
  // dirResult.valid is given 1 cycle ahead of bits, and we count A req only
  def dirResultMatchVec(cond: DirResult => Bool): IndexedSeq[Bool] = {
    io.dirResult.map {
      r => RegNext(r.valid, false.B) && r.bits.replacerInfo.channel === 1.U && cond(r.bits)
    }
  }

  for (i <- 0 until MemReqSource.ReqSourceCount.id) {
    val sourceMatchVec = dirResultMatchVec(r => r.replacerInfo.reqSource === i.U)
    val sourceMatchVecMiss = dirResultMatchVec(r => r.replacerInfo.reqSource === i.U && !r.self.hit)

    val sourceName = MemReqSource.apply(i).toString
    XSPerfAccumulate(cacheParams, s"E2_${cacheParams.name}AReqSource_${sourceName}_Total", PopCount(sourceMatchVec))
    XSPerfAccumulate(cacheParams, s"E2_${cacheParams.name}AReqSource_${sourceName}_Miss", PopCount(sourceMatchVecMiss))
  }
}
