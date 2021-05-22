package huancun
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkB(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val b = Flipped(DecoupledIO(new TLBundleB(edge.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
  })
}
