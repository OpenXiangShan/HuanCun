package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkC(edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = Flipped(DecoupledIO(new TLBundleC(edge.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
    val resp = ValidIO(new TLBundleC(edge.bundle))
    val task = Flipped(DecoupledIO(new SinkCReq))
  })
}
