package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

class MSHRAlloc(implicit p: Parameters) extends HuanCunModule with DontCareInnerLogic {
  val io = IO(new Bundle() {
    // requests
    val a_req = Flipped(DecoupledIO(new MSHRRequest))
    val b_req = Flipped(DecoupledIO(new MSHRRequest))
    val c_req = Flipped(DecoupledIO(new MSHRRequest))
    // From MSHRs
    val status = Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus)))
    // To MSHRs
    val alloc = Vec(mshrsAll, ValidIO(new MSHRRequest))
    // To directory
    val dirReads = Vec(dirReadPorts, DecoupledIO(new DirRead))
  })

}
