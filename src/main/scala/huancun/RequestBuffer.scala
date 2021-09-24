package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLBundleA

class RequestBuffer(flow: Boolean = true, entries: Int = 16)(implicit p: Parameters) extends HuanCunModule {

  def bundle = new TLBundleA(edgeIn.bundle)

  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(bundle))
    val out = DecoupledIO(bundle)
    val mshr_status = Vec(mshrs, Flipped(ValidIO(new MSHRStatus)))
    val mshr_alloc = Vec(mshrs, Flipped(ValidIO(new MSHRRequest)))
  })

  val buffer = Mem(entries, bundle)
  val valids = RegInit(VecInit(Seq.fill(entries){ false.B }))
  // which mshr the entry is waiting for
  val wait_table = Reg(Vec(entries, UInt(mshrs.W)))
  val rdys = RegInit(VecInit(Seq.fill(entries){ false.B }))

  val issue_arb = Module(new RRArbiter(bundle, entries))
  for (i <- 0 until entries){
    issue_arb.io.in(i).valid := valids(i) && rdys(i)
    issue_arb.io.in(i).bits := buffer(i)
    when(issue_arb.io.in(i).fire()){
      valids(i) := false.B
    }
  }

  val full = Cat(valids).andR()
  val empty = !Cat(valids).orR()
  io.out.bits := Mux(empty && flow.B, io.in.bits, issue_arb.io.out.bits)
  io.out.valid := (flow.B && empty && io.in.valid) | issue_arb.io.out.valid
  issue_arb.io.out.ready := io.out.ready

  io.in.ready := !full

  val (_, input_set, _) = parseAddress(io.in.bits.address)

  def set_conflict(set_a: UInt, set_b: UInt): Bool = {
    set_a(block_granularity - 1, 0) === set_b(block_granularity - 1, 0)
  }
  val conflict_mask = (0 until mshrs) map { i =>
    val s = io.mshr_status(i)
    val s_conflict = s.valid && set_conflict(s.bits.set, input_set) && !s.bits.will_free
    val a = io.mshr_alloc(i)
    val a_conflict = a.valid && set_conflict(a.bits.set, input_set)
    s_conflict | a_conflict
  }
  val conflict = Cat(conflict_mask).orR()
  val insert_idx = PriorityEncoder(~valids.asUInt())
  when(!full && io.in.valid && !(flow.B && empty && io.out.ready)){
    buffer(insert_idx) := io.in.bits
    valids(insert_idx) := true.B
    wait_table(insert_idx) := VecInit(conflict_mask).asUInt()
    assert(PopCount(conflict_mask) <= 1.U)
    rdys(insert_idx) := !conflict
  }

  val free_mask = VecInit(io.mshr_status.map(_.bits.will_free)).asUInt()
  for (i <- 0 until entries){
    val wait_mask = wait_table(i).asUInt()
    when((wait_mask & free_mask).orR()){
      rdys(i) := true.B
    }
  }

}
