package huancun.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import huancun._
import utility.{MemReqSource, Pipeline}

case class PrefetchReceiverParams(n: Int = 32) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val inflightEntries: Int = n
}

case class L3PrefetchReceiverParams(n: Int = 32) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val inflightEntries: Int = n
}

class PrefetchReceiver()(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO())
  // just ignore train reqs
  io.train.ready := true.B
  io.resp.ready := true.B

  io.req.bits.tag := parseFullAddress(io.recv_addr.bits)._1
  io.req.bits.set := parseFullAddress(io.recv_addr.bits)._2
  io.req.bits.needT := false.B
  io.req.bits.source := 0.U // TODO: ensure source 0 is dcache
  io.req.bits.pfSource := MemReqSource.Prefetch2L2Stream.id.U // TODO: add L3 pfSource
  io.req.valid := io.recv_addr.valid

}
