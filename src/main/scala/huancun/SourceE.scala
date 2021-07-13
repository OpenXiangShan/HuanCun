package huancun
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SourceE(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val e = DecoupledIO(new TLBundleE(edge.bundle))
    val task = Flipped(DecoupledIO(new SourceEReq))
  })
  val e = io.e
  e.valid := io.task.valid
  e.bits.sink := io.task.bits.sink
  io.task.ready := e.ready
}
