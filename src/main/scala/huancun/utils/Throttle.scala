package huancun.utils

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class Throttle(threshold: Int)(implicit p: Parameters) extends LazyModule {
  val node = TLIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, edgeIn) = node.in.head
    val (out, edgeOut) = node.out.head

    // TL-UL is guaranteed
    assert(!in.b.fire() && !in.c.fire() && !in.e.fire())
    assert(!out.b.fire() && !out.c.fire() && !out.e.fire())

    val pendingCnt = RegInit(0.U(32.W))
    val (a_first, _, _, _) = edgeIn.count(in.a)
    val (d_first, _, _, _) = edgeIn.count(in.d)

    val new_req = in.a.fire() && a_first
    val new_resp = in.d.fire() && d_first

    when(new_req) {
      when(!new_resp) {
        // assert(pendingCnt < threshold)
        pendingCnt := pendingCnt + 1.U
      }
    }.elsewhen(new_resp) {
      assert(pendingCnt > 0.U)
      pendingCnt := pendingCnt - 1.U
    }

    val do_throttle = pendingCnt >= threshold.U
    when(do_throttle) {
      in.a.ready := false.B
      out.a.valid := false.B
    }
  }
}

object Throttle {
  def apply(threshold: Int)(implicit p: Parameters): TLIdentityNode = {
    val throttle = LazyModule(new Throttle(threshold))
    throttle.node
  }
}