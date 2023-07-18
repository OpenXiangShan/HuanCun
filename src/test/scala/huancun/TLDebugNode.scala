package huancun

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.TLEphemeralNode

trait HasTLEphemeralNode { this: LazyModule =>
  val node = TLEphemeralNode()
}

abstract class TLDebugModuleBase(implicit p: Parameters) extends LazyModule with HasTLEphemeralNode

class TLDebugNode[T <: LazyModuleImp](gen: TLDebugModuleBase => T)(implicit p: Parameters)
  extends TLDebugModuleBase {
  lazy val module = gen(this)
}

object TLDebugNode {
  def apply[T <: LazyModuleImp](gen: TLDebugModuleBase => T)(implicit p: Parameters) = {
    val debugNode = LazyModule(new TLDebugNode(gen))
    debugNode.node
  }
}
