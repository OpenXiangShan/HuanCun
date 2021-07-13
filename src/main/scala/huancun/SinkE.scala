package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkE(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val e = Flipped(DecoupledIO(new TLBundleE(edgeIn.bundle)))
    val resp = ValidIO(new SinkEResp)
  })
  val e = io.e
  e.ready := true.B
  io.resp.valid := e.fire()
  io.resp.bits.sink := e.bits.sink
}
