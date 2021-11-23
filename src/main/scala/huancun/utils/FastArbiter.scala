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

  val chosenOH = Wire(UInt(n.W))
  val valids = VecInit(io.in.map(_.valid)).asUInt()
  // the reqs that we didn't choose in last cycle
  val pendingMask = RegEnable(
    valids & (~chosenOH).asUInt(), // make IDEA happy ...
    0.U(n.W),
    io.out.fire()
  )
  // select a req from pending reqs by RR
  /*
      Eg: chosenOH  0100
       rrGrantMask  0011
   */
  val rrGrantMask = RegEnable(VecInit((0 until n) map { i =>
    if(i == 0) false.B else chosenOH(i - 1, 0).orR()
  }).asUInt(), 0.U(n.W), io.out.fire())
  val rrSelOH = VecInit(maskToOH((rrGrantMask & pendingMask).asBools())).asUInt()
  val firstOneOH = VecInit(maskToOH(valids.asBools())).asUInt()
  val rrValid = (rrSelOH & valids).orR()
  chosenOH := Mux(rrValid, rrSelOH, firstOneOH)

  io.out.valid := valids.orR()
  io.out.bits := Mux1H(chosenOH, io.in.map(_.bits))

  io.in.map(_.ready).zip(chosenOH.asBools()).foreach{
    case (rdy, grant) => rdy := grant && io.out.ready
  }

  io.chosen := OHToUInt(chosenOH)

}

