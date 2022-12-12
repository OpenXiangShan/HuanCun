package huancun

import chisel3._
import chisel3.util._
import huancun.utils.XSPerfAccumulate
import chipsalliance.rocketchip.config.Parameters

class MSHRPfStats extends Bundle {
  val pf_issued = Bool()
  val pf_useful = Bool()
  val pf_usefulButMiss = Bool()
  val demand_miss = Bool()
}

class StatsUnit(nSlices: Int)(params: HCCacheParameters) extends Module {
  val io = IO(new Bundle() {
    // from prefetcher
    val pf_received = Input(Bool())
    // from MSHR
    val mshr_stats = Input(Vec(nSlices, new MSHRPfStats))
  })
  XSPerfAccumulate(params, "pf_received", io.pf_received)
  for((stats, i) <- io.mshr_stats.zipWithIndex) {
    XSPerfAccumulate(params, s"slice_${i}_pf_issued", stats.pf_issued)
    XSPerfAccumulate(params, s"slice_${i}_pf_useful", stats.pf_useful)
    XSPerfAccumulate(params, s"slice_${i}_pf_usefulButMiss", stats.pf_usefulButMiss)
    XSPerfAccumulate(params, s"slice_${i}_demand_miss", stats.demand_miss)
  }
  XSPerfAccumulate(params, s"pf_issued", PopCount(io.mshr_stats.map(_.pf_issued)))
  XSPerfAccumulate(params, s"pf_useful", PopCount(io.mshr_stats.map(_.pf_useful)))
  XSPerfAccumulate(params, s"pf_usefulButMiss", PopCount(io.mshr_stats.map(_.pf_usefulButMiss)))
  XSPerfAccumulate(params, s"pf_demand_miss", PopCount(io.mshr_stats.map(_.demand_miss)))
}
