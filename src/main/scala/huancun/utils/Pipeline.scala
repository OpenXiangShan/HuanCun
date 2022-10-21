package huancun.utils

import chisel3._
import chisel3.util._

class Pipeline[T <: Data](gen: T, depth: Int = 1, pipe: Boolean = true) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO[T](gen.cloneType))
    val out = DecoupledIO[T](gen.cloneType)
  })
  val stages = (0 until depth).map(_ => Module(new Queue[T](gen, 1, pipe = pipe, flow = false)))

  stages.foldLeft(io.in)((in, q) => {
    q.io.enq <> in
    q.io.deq
  })

  io.out <> stages.last.io.deq
}

object Pipeline {
  def pipeTo[T <: Data](out: DecoupledIO[T], depth: Int = 1, pipe: Boolean = true, name: Option[String] = None): DecoupledIO[T] = {
    val pipeline = Module(new Pipeline[T](out.bits.cloneType, depth, pipe))
    name.map(n => pipeline.suggestName(n))
    out <> pipeline.io.out
    pipeline.io.in
  }
  def apply[T <: Data](in: DecoupledIO[T], depth: Int = 1, pipe: Boolean = true, name: Option[String] = None): DecoupledIO[T] = {
    val pipeline = Module(new Pipeline[T](in.bits.cloneType, depth, pipe))
    name.map(n => pipeline.suggestName(n))
    pipeline.io.in <> in
    pipeline.io.out
  }
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

