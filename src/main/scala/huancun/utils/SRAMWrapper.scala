package huancun.utils

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.Pow2ClockDivider
import utility.ClockGate

class SRAMWrapper[T <: Data]
(
  gen: T, set: Int, n: Int = 1,
  clk_div_by_2: Boolean = false
) extends Module {

  val io = IO(new Bundle() {
    val r = Flipped(new SRAMReadBus(gen, set, 1))
    val w = Flipped(new SRAMWriteBus(gen, set, 1))
  })

  val innerSet = set / n
  val selBits = log2Ceil(n)
  val innerSetBits = log2Up(set) - selBits
  val r_setIdx = io.r.req.bits.setIdx.head(innerSetBits)
  val r_sel = if(n == 1) 0.U else io.r.req.bits.setIdx(selBits - 1, 0)
  val w_setIdx = io.w.req.bits.setIdx.head(innerSetBits)
  val w_sel = if(n == 1) 0.U else io.w.req.bits.setIdx(selBits - 1, 0)

  val banks = (0 until n).map{ i =>
    val ren = if(n == 1) true.B else i.U === r_sel
    val wen = if(n == 1) true.B else i.U === w_sel
    val sram = Module(new SRAMTemplate[T](
      gen, innerSet, 1, singlePort = true, input_clk_div_by_2 = clk_div_by_2
    ))

    if (clk_div_by_2) {
      val clk_en = RegInit(false.B)
      clk_en := ~clk_en
      val masked_clock = ClockGate(false.B, clk_en, clock)
      sram.clock := masked_clock
    }
    sram.io.r.req.valid := io.r.req.valid && ren
    sram.io.r.req.bits.apply(r_setIdx)
    sram.io.w.req.valid := io.w.req.valid && wen
    sram.io.w.req.bits.apply(io.w.req.bits.data(0), w_setIdx, 1.U)
    sram
  }

  val ren_vec_0 = VecInit(banks.map(_.io.r.req.fire))
  val ren_vec_1 = RegNext(ren_vec_0, 0.U.asTypeOf(ren_vec_0))
  val ren_vec = if(clk_div_by_2){
    RegNext(ren_vec_1, 0.U.asTypeOf(ren_vec_0))
  } else ren_vec_1

  io.r.req.ready := Cat(banks.map(_.io.r.req.ready)).andR
  io.r.resp.data := Mux1H(ren_vec, banks.map(_.io.r.resp.data))

  io.w.req.ready := Cat(banks.map(_.io.w.req.ready)).andR

}
