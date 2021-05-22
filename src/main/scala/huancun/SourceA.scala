package huancun
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SourceA(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val a = DecoupledIO(new TLBundleA(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceAReq))
  })
}
