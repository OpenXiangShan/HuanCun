package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun.utils.ParallelOR

class CohClint(implicit p: Parameters) extends HuanCunBundle() {
  val a = Flipped(DecoupledIO(new MSHRRequest))
  val b = Flipped(DecoupledIO(new MSHRRequest))
  val c = Flipped(DecoupledIO(new MSHRRequest))
}

class RelaxClint(implicit p: Parameters) extends HuanCunBundle() {
  val a = Flipped(DecoupledIO(new MSHRRequest()))
}

class MSHRAlloc(nrRelaxClints: Int, nrCohClints: Int)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    // From clients
    val relaxClints = Vec(nrRelaxClints, new RelaxClint)
    val cohClints = Vec(nrCohClints, new CohClint)
    // From MSHRs
    val status = Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus)))
    // To MSHRs
    val alloc = Vec(mshrsAll, ValidIO(new MSHRRequest))
    // To directory
    val dirReads = Vec(dirReadPorts, DecoupledIO(new DirRead))
  })

  /* nrA-allocs  nrB-allocs  nrC-allocs
   *         allocated to
   *  [ABC MSHRs] [BC MSHRs] [C MSHRs]
   */

  /* Requirements */

  require(nrRelaxClints == 2)
  require(nrCohClints == 1)
  assert(PopCount(io.alloc.map(_.valid)) <= dirReadPorts.U)

  /* Judge collision mutually */

  val cohClient = io.cohClints.head

  // elimination mask

  val relaxMaskVec1 = io.relaxClints.zipWithIndex.map {
    case (_, index) =>
      val allA = io.relaxClints.map(_.a) ++ io.cohClints.map(_.a)
      val highPrioA = allA.zipWithIndex.filter(_._2 > index)
      cohClient.c.valid || cohClient.b.valid || highPrioA.foldLeft(false.B) { case (result, c) => result || c._1.valid }
  }

  /*
   * Judge collision with MSHRs
   */

  val relaxMaskVec2 = io.relaxClints.map(_.a).map(c => ParallelOR(io.status.map(s => s.bits.set === c.bits.set)))

  // TODO: implement nest & block logic
  val nestB = false.B
  val nestC = false.B
  val blockB = false.B
  val blockC = false.B

  /*
   * Find idle MSHR entries
   */
  val mshrIdle = Wire(Vec(dirReadPorts, ValidIO(UInt(log2Up(mshrsAll).W)))) // TODO: refill logic here
  mshrIdle := DontCare
  /*
   * Arbitration & allocation of directory read ports
   */

  // Collision mask
  val relaxMask = relaxMaskVec1.zip(relaxMaskVec2).map(c => c._1 || c._2)
  val cohMask = VecInit(Seq.fill(3)(false.B)) // TODO

  // Arbiter relaxReqs
  val relaxReqs = Wire(Vec(nrRelaxClints, DecoupledIO(new MSHRRequest)))
  relaxReqs.zip(io.relaxClints.map(_.a)).zip(relaxMask).foreach {
    case ((r, c), mask) =>
      r.bits := c.bits
      r.valid := c.valid && !mask
      c.ready := r.ready && !mask
  }

  // Arbiter cohReqs, priority select C > B > A
  val cohReqs = Wire(Vec(nrCohClints, DecoupledIO(new MSHRRequest)))
  val cohReq = cohReqs.head
  cohReq.valid := (cohClient.c.valid && !cohMask(0)) ||
    (!cohClient.c.valid && cohClient.b.valid && !cohMask(1)) ||
    (!cohClient.c.valid && !cohClient.b.valid && cohClient.a.valid && !cohMask(2))
  cohReq.bits := Mux(cohClient.c.valid, cohClient.c.bits, Mux(cohClient.b.valid, cohClient.b.bits, cohClient.a.bits))
  cohClient.c.ready := !cohMask(0) && cohReq.ready
  cohClient.b.ready := !cohMask(1) && cohReq.ready && !cohClient.c.valid
  cohClient.a.ready := !cohMask(2) && cohReq.ready && !cohClient.c.valid && !cohClient.b.valid

  io.dirReads.zip(relaxReqs ++ cohReqs).zip(mshrIdle).foreach {
    case ((dir, req), alloc) =>
      dir.valid := req.valid && alloc.valid
      dir.bits.tag := req.bits.tag
      dir.bits.set := req.bits.set
      dir.bits.id := alloc.bits
      req.ready := dir.ready && alloc.valid
  }

  /*
   * Provide MSHR allocation response
   */
  io.alloc.foreach(_.valid := false.B)
  mshrIdle.foreach { c =>
    io.alloc(UIntToOH(c.bits)).valid := c.valid
  }
  // TODO: assign io.alloc.bits
}
