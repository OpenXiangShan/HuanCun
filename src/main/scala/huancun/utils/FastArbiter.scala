package huancun.utils

import chisel3._
import chisel3.util._

class FastArbiter[T <: Data](val gen: T, val n: Int) extends Module {

  val io = IO(new ArbiterIO[T](gen, n))

  def maskToOH(seq: Seq[Bool]) = {
    seq.zipWithIndex.map{
      case (b, 0) => b
      case (b, i) => b && !Cat(seq.take(i)).orR()
    }
  }

  val rev = RegInit(false.B)
  rev := !rev

  val firstOneOH = VecInit(maskToOH(io.in.map(_.valid)))
  val lastOneOH = VecInit(maskToOH(io.in.map(_.valid).reverse).reverse)

  val selOH = Mux(rev, lastOneOH, firstOneOH)

  io.chosen := OHToUInt(selOH)

  io.in.zip(selOH).foreach{
    case (in, g) => in.ready := g && io.out.ready
  }

  io.out.valid := Cat(io.in.map(_.valid)).orR()
  io.out.bits := Mux1H(selOH, io.in.map(_.bits))

}

