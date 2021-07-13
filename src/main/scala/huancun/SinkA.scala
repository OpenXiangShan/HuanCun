package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkA(val edge: TLEdgeIn)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val a = Flipped(DecoupledIO(new TLBundleA(edge.bundle)))
    val alloc = DecoupledIO(new MSHRRequest)
    val task = Flipped(DecoupledIO(new SinkAReq))
  })
  dontTouch(io)

  // TODO: Handle task
  io.task.ready := false.B

  val a = io.a
  val first = edge.first(a)
  val hasData = edge.hasData(a.bits)
  when(a.valid) {
    assert(!hasData)
  }
  val (tag, set, offset) = parseAddress(a.bits.address)

  io.alloc.valid := a.valid && first
  a.ready := io.alloc.ready

  val allocInfo = io.alloc.bits
  allocInfo.channel := 1.U(3.W)
  allocInfo.opcode := a.bits.opcode
  allocInfo.param := a.bits.param
  allocInfo.size := a.bits.size
  allocInfo.source := a.bits.source
  allocInfo.set := set
  allocInfo.tag := tag
  allocInfo.off := offset
  allocInfo.bufIdx := DontCare
}
