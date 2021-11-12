package huancun.utils

import chisel3._
import chisel3.util._

class Pipeline[T <: Data](gen: T, depth: Int = 1) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO[T](gen.cloneType))
    val out = DecoupledIO[T](gen.cloneType)
  })
  val stages = (0 until depth).map(_ => Module(new Queue[T](gen, 1, pipe = true, flow = false)))

  stages.foldLeft(io.in)((in, q) => {
    q.io.enq <> in
    q.io.deq
  })

  io.out <> stages.last.io.deq
}

object RegNextN {
  def apply[T <: Data](in: T, n: Int, initOpt: Option[T] = None): T = {
    (0 until n).foldLeft(in){
      (prev, _) =>
        initOpt match {
          case Some(init) => RegNext(prev, init)
          case None => RegNext(prev)
        }
    }
  }
}