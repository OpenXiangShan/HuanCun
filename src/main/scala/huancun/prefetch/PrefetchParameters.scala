package huancun.prefetch

import chipsalliance.rocketchip.config.{Parameters}
import chisel3._
import chisel3.util._
import huancun._

trait HasPrefetchParameters extends HasHuanCunParameters {
  // Best offset
  val rrTableEntries = bopParams.rrTableEntries
  val rrTagBits = bopParams.rrTagBits
  val scoreBits = bopParams.scoreBits
  val roundMax = bopParams.roundMax
  val badScore = bopParams.badScore
  val offsetList = bopParams.offsetList
  val inflightEntries = bopParams.inflightEntries

  val scores = offsetList.length
  val offsetWidth = log2Up(offsetList(scores - 1)) + 1
  val rrIdxBits = log2Up(rrTableEntries)
  val roundBits = log2Up(roundMax)
  val scoreMax = (1 << scoreBits) - 1
  val scoreTableIdxBits = log2Up(scores)
  // val prefetchIdWidth = log2Up(inflightEntries)
}

abstract class PrefetchBundle(implicit val p: Parameters) extends Bundle with HasPrefetchParameters
abstract class PrefetchModule(implicit val p: Parameters) extends Module with HasPrefetchParameters
