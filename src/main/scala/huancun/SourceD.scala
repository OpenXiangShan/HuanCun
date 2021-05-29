package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SourceD(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule with DontCareInnerLogic {
  val io = IO(new Bundle() {
    val d = DecoupledIO(new TLBundleD(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceDReq))
  })
}
