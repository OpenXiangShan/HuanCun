package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkE(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule with DontCareInnerLogic {
  val io = IO(new Bundle() {
    val e = Flipped(DecoupledIO(new TLBundleE(edge.bundle)))
    val resp = ValidIO(new SinkEResp)
  })
  io.e.ready := true.B
}
