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

  val a = io.a

  dontTouch(a)
  dontTouch(io.task)
  io.task.ready := a.ready
  a.valid := io.task.valid

  a.bits.opcode := io.task.bits.opcode
  a.bits.param := io.task.bits.param
  a.bits.size := offsetBits.U
  a.bits.source := io.task.bits.source
  a.bits.address := Cat(io.task.bits.tag, io.task.bits.set, 0.U(offsetBits.W))
  a.bits.mask := Fill(edgeOut.manager.beatBytes, 1.U(1.W))
  a.bits.data := DontCare
  a.bits.corrupt := false.B

}
