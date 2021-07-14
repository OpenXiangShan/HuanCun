package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun.utils._

class MSHRSelector(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val idle = Input(Vec(mshrs, Bool()))
    val out = ValidIO(UInt(mshrs.W))
  })
  io.out.valid := ParallelOR(io.idle)
  io.out.bits := ParallelPriorityMux(io.idle.zipWithIndex.map{
    case (b, i) => (b, (1 << i).U)
  })
}

class MSHRAlloc(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    // requests
    val a_req = Flipped(DecoupledIO(new MSHRRequest))
    val b_req = Flipped(DecoupledIO(new MSHRRequest))
    val c_req = Flipped(DecoupledIO(new MSHRRequest))
    // From MSHRs
    val status = Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus)))
    // To MSHRs
    val alloc = Vec(mshrsAll, ValidIO(new MSHRRequest))
    // To directory
    val dirReads = Vec(dirReadPorts, DecoupledIO(new DirRead))
  })

  // Allocate one MSHR per cycle
  assert(PopCount(io.alloc.map(_.valid)) <= 1.U)
  // Only use the first dir read port
  assert(io.dirReads.takeRight(dirReadPorts-1).map(_.valid).reduce(_||_) === false.B)

  /* case1: selected request matches set of pending MSHR => stall
   * case2: selected request needs new MSHR but no room left => stall
   * case3: selected request needs new MSHR but dir not ready => stall
   * case4: selected request needs new MSHR and dir ready => good!
   */

  /* Select one request from a_req/b_req/c_req */
  val request = Wire(ValidIO(new MSHRRequest()))
  request.valid := io.c_req.valid || io.b_req.valid || io.a_req.valid
  request.bits := Mux(io.c_req.valid, io.c_req.bits, Mux(io.b_req.valid, io.b_req.bits, io.a_req.bits))

  /* Whether selected request can be accepted */

  def set_conflict(req: MSHRRequest): Bool = {
    Cat(io.status.map(s => s.valid && s.bits.set === req.set)).orR()
  }

  val conflict_c = set_conflict(io.c_req.bits)
  val conflict_b = set_conflict(io.b_req.bits)
  val conflict_a = set_conflict(io.a_req.bits)

  val dirRead = io.dirReads.head
  val mshrFree = Cat(io.status.map(!_.valid)).orR()

  val can_accept_c = mshrFree && !conflict_c
  val can_accept_b = mshrFree && !conflict_b && !io.c_req.valid
  val can_accept_a = mshrFree && !conflict_a && !io.c_req.valid && !io.b_req.valid

  val accept_c = io.c_req.valid && can_accept_c
  val accept_b = io.b_req.valid && can_accept_b
  val accept_a = io.a_req.valid && can_accept_a

  /* Provide signals for outer components*/
  io.c_req.ready := dirRead.ready && can_accept_c
  io.b_req.ready := dirRead.ready && can_accept_b
  io.a_req.ready := dirRead.ready && can_accept_a

  val recv_req = io.c_req.fire() || io.b_req.fire() || io.a_req.fire()
  val mshrSelector = Module(new MSHRSelector())
  mshrSelector.io.idle := io.status.take(mshrs).map(!_.valid)
  val selectedMSHROH = mshrSelector.io.out.bits
  for ((mshr, i) <- io.alloc.take(mshrs).zipWithIndex) {
    mshr.valid := recv_req && selectedMSHROH(i)
    mshr.bits := request.bits
  }

  dirRead.valid := request.valid && Cat(accept_c, accept_b, accept_a).orR()
  dirRead.bits.tag := request.bits.tag
  dirRead.bits.set := request.bits.set
  dirRead.bits.idOH := selectedMSHROH

  io.alloc.drop(mshrs).map {
    a =>
      a.valid := false.B
      a.bits <> DontCare
  }
  io.dirReads.drop(1).map {
    d =>
      d.valid := false.B
      d.bits <> DontCare
  }
}
