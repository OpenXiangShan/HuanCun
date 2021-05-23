package huancun
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkD(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val d = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val resp = ValidIO(new SinkDResp)
  })
}
