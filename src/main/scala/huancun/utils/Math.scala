package huancun.utils

import chisel3._

object Min
{
  def apply(values: Seq[UInt], flags: UInt, valueBits: Int): Seq[Bool] = {
    val minFlags = Wire(Vec(values.length, Bool()))
    val digits   = Wire(Vec(valueBits, Vec(values.length, Bool())))
    val cmpFlags = Wire(Vec(valueBits, UInt(values.length.W)))
    for (i <- 0 until valueBits) {
      for (j <- values.indices) {
        digits(i)(j) := values(j)(i)
      }
    }
    cmpFlags(valueBits-1) := Mux(((~flags) | digits(valueBits-1).asUInt()) === (-1).S(values.length.W).asUInt(), flags, flags &(~digits(valueBits-1).asUInt()))
    for (i <- 0 until valueBits-1) {
      cmpFlags(i) := Mux(((~cmpFlags(i+1)) | digits(i).asUInt()) === (-1).S(values.length.W).asUInt(), cmpFlags(i+1), cmpFlags(i+1) &(~digits(i).asUInt()))
    }
    for (i <- 0 until values.length-1) {
      minFlags(i) := cmpFlags(0)(i) & (~(cmpFlags(0)(values.length-1, i+1).asBools().reduce(_|_)))
    }
    minFlags(values.length-1) := cmpFlags(0)(values.length-1)
    minFlags
  }
}

object Max {
  def apply(values: Seq[UInt], flags: UInt, valueBits: Int): Seq[Bool] = {
    val remainingValues = values map { i =>
      (~i).asUInt()
    }
    Min(remainingValues, flags, valueBits)
  }
}