package huancun.prefetch

import chipsalliance.rocketchip.config.{Parameters, Field}
import chisel3._
import chisel3.util._
import huancun._

case object PCParamsKey extends Field[PCParameters]

case class PCParameters (
  sets: Int = 64 * 1024, // 64K
  ways: Int = 4
) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val inflightEntries: Int = 16
  override val needCommitInfo: Boolean = true
}

trait HasPCParams extends HasHuanCunParameters {
  val pcParams = prefetchOpt.get.asInstanceOf[PCParameters]
  val commitWidth = cacheParams.clientCaches.head.commitWidth
  val vaddrBits = cacheParams.clientCaches.head.vaddrBits

  // pointer cache
  val pcSets = pcParams.sets
  val pcWays = pcParams.ways

  val wordOffBits = log2Up(64 / 8) // TODO: parameterize this
  val pcIdxBits = log2Up(pcSets)
  val pcTagBits = 10 // partial tag
  val diffAddrBits = pcIdxBits + pcTagBits
  require((diffAddrBits + wordOffBits) <= vaddrBits)
  // The pointer cache store only if the address of the pointer and the address of the object
  // it points to fall within the range of the heap.
  val heapTagBits = 6 // the N most significant bits of vaddr
}

abstract class PCBundle(implicit val p: Parameters) extends Bundle with HasPCParams
abstract class PCModule(implicit val p: Parameters) extends Module with HasPCParams