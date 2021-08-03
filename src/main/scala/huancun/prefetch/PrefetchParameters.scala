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

class PrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  // val addr = UInt(addressBits.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  // val id = UInt(sourceIdBits.W)
}

class PrefetchResp(implicit p: Parameters) extends PrefetchBundle {
  // val id = UInt(sourceIdBits.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
}

class PrefetchTrain(implicit p: Parameters) extends PrefetchBundle {
  // val addr = UInt(addressBits.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  // prefetch only when L2 receives a miss or prefetched hit req
  // val miss = Bool()
  // val prefetched = Bool()
}

class PrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  val train = Flipped(DecoupledIO(new PrefetchTrain))
  val req = DecoupledIO(new PrefetchReq)
  val resp = Flipped(DecoupledIO(new PrefetchResp))
}