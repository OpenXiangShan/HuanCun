package huancun
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkB(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule with DontCareInnerLogic {
  val io = IO(new Bundle() {
    val b = Flipped(DecoupledIO(new TLBundleB(edge.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
  })

  io.b.ready := io.alloc.ready
  io.alloc.valid := io.b.valid

  val (tag, set, off) = parseAddress(io.b.bits.address)

  io.alloc.bits.opcode := io.b.bits.opcode
  io.alloc.bits.param := io.b.bits.param
  io.alloc.bits.size := io.b.bits.size
  io.alloc.bits.source := io.b.bits.source
  io.alloc.bits.set := set
  io.alloc.bits.tag := tag
  io.alloc.bits.off := off
  io.alloc.bits.bufIdx := 0.U
}
