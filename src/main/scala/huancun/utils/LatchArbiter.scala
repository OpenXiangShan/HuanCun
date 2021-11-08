package huancun.utils

import chisel3._
import chisel3.util._

private object ArbiterCtrl {
  def apply(request: Seq[Bool]): Seq[Bool] = request.length match {
    case 0 => Seq()
    case 1 => Seq(true.B)
    case _ => true.B +: request.tail.init.scanLeft(request.head)(_ || _).map(!_)
  }
}

class LatchArbiter[T <: Data](val gen: T, val n: Int) extends Module {
  val io = IO(new ArbiterIO(gen, n))
  val inLatch = RegInit(VecInit(Seq.fill(n)(false.B)))
  val inLatchValid = RegInit(false.B)
  val valids = Mux(inLatchValid, inLatch, VecInit(io.in.map(_.valid)))

  io.chosen := (n-1).asUInt
  io.out.bits := io.in(n-1).bits
  for (i <- n-2 to 0 by -1) {
    when(valids(i)) {
      io.chosen := i.asUInt
      io.out.bits := io.in(i).bits
    }
  }
  val grant = ArbiterCtrl(valids)
  for ((in, g) <- io.in zip grant)
    in.ready := g && io.out.ready
  io.out.valid := !grant.last || valids.last

  val outFire = io.out.valid && io.out.ready
  val maskedInLatch = VecInit(inLatch.zipWithIndex.map {
    case (t,i) => Mux(i.U === io.chosen, false.B, t)
  })
  val maskedIn = VecInit(io.in.zipWithIndex.map {
    case (t,i) => Mux(i.U === io.chosen, false.B, t)
  })

  val inLatchNext = VecInit(Seq.fill(n)(false.B))
  when (!inLatchValid) {
    inLatchNext := Mux(outFire, maskedIn, VecInit(io.in.map(_.valid)))
  }.otherwise{
    inLatchNext := Mux(outFire, maskedInLatch, inLatch)
  }
  inLatchValid := inLatchNext.foldLeft(false.B)(_ || _)
  inLatch := inLatchNext
}
