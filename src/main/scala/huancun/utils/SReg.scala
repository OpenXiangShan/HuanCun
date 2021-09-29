package huancun.utils

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun.HCCacheParamsKey

object SReg {
  def get_sram_cycle()(implicit p: Parameters): (Int, Int) = {
    val cycles = p(HCCacheParamsKey).sramCycleFactor
    require(isPow2(cycles))
    val cycle_bits = log2Ceil(cycles)
    (cycles, cycle_bits)
  }
  def sren()(implicit p: Parameters): Bool = {
    val (cycle, cycle_bits) = get_sram_cycle()
    if(cycle == 1) return true.B
    val counter = RegInit(0.U(cycle_bits.W))
    counter := counter + 1.U
    counter === 0.U
  }
  def pipe[T <: Data](x: T)(implicit p: Parameters): T = {
    val (cycles, _) = get_sram_cycle()
    if(cycles == 1) return RegNext(x)
    val regs = List.tabulate[T](cycles){_ => Reg(chiselTypeOf(x))}
    regs.fold(x)((a, b) => { b := a; b })
  }
  def pipe[T <: Data](x: T, init: T)(implicit p: Parameters): T = {
    val (cycles, _) = get_sram_cycle()
    if(cycles == 1) return RegNext(x, init)
    val regs = List.tabulate[T](cycles){_ => RegInit(chiselTypeOf(x), init)}
    regs.fold(x)((a, b) => { b := a; b })
  }
}
