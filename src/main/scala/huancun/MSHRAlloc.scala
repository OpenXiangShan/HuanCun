package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{DecoupledIO, PopCount, ValidIO, log2Up}

class MSHRAlloc(nrA: Int, nrB: Int, nrC: Int)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    // From channels
    val inA = Vec(nrA, Flipped(DecoupledIO(new MSHRRequest)))
    val inB = Vec(nrB, Flipped(DecoupledIO(new MSHRRequest)))
    val inC = Vec(nrC, Flipped(DecoupledIO(new MSHRRequest)))
    // From MSHRs
    val status = Vec(mshrsAll, new MSHRStatus)
    // To MSHRs
    val alloc = Vec(mshrsAll, ValidIO(new MSHRRequest))
    // To directory
    val dirReads = Vec(dirReadPorts, DecoupledIO(new DirRead))
  })

  // nrA-allocs  nrB-allocs  nrC-allocs
  //         allocated to
  //  [ABC MSHRs] [BC MSHRs] [C MSHRs]

  require(nrB == 1)
  require(nrC == 1)

  assert(PopCount(io.alloc.map(_.valid)) <= dirReadPorts.U)

  val inHandleMask = 0.U(log2Up(nrA+nrB+nrC).W) // TODO


}
