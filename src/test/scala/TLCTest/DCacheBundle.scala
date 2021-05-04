package TLCTest

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

trait L1CacheParameters {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def blockBytes:    Int
}

trait CoreParameters {
  val XLEN = 64
  val PAddrBits = 40
  val l1BusDataWidth = 256
  val DataBits = XLEN
  val dcacheParameters = DCacheParameters(
    tagECC = Some("secded"),
    dataECC = Some("secded"),
    replacer = Some("setplru"),
    nMissEntries = 16,
    nProbeEntries = 16,
    nReleaseEntries = 16,
    nStoreReplayEntries = 16
  )
  val LRSCCycles = 100
}

trait HasL1CacheParameters extends CoreParameters {
  val cacheParams: L1CacheParameters

  def nSets = cacheParams.nSets
  def nWays = cacheParams.nWays
  def blockBytes = cacheParams.blockBytes
  def blockBits = blockBytes * 8

  def idxBits = log2Up(cacheParams.nSets)
  def wayBits = log2Up(nWays)
  def blockOffBits = log2Up(cacheParams.blockBytes)

  def untagBits = blockOffBits + idxBits
  // 4K page
  def pgIdxBits = 12
  def pgUntagBits = untagBits min pgIdxBits
  def tagBits = PAddrBits - pgUntagBits

  // the basic unit at which we store contents
  // SRAM bank width
  def rowBits = cacheParams.rowBits
  def rowBytes = rowBits/8
  def rowOffBits = log2Up(rowBytes)
  // the number of rows in a block
  def blockRows = blockBytes / rowBytes

  // outer bus width
  def beatBits = l1BusDataWidth
  def beatBytes = beatBits / 8
  def refillCycles = blockBytes / beatBytes
  def beatOffBits = log2Up(beatBytes)

  // inner bus width(determined by XLEN)
  def wordBits = DataBits
  def wordBytes = wordBits / 8
  def wordOffBits = log2Up(wordBytes)
  // the number of words in a block
  def blockWords = blockBytes / wordBytes

  def idxMSB = untagBits-1
  def idxLSB = blockOffBits
  def offsetmsb = idxLSB-1
  def offsetlsb = wordOffBits

  def get_tag(addr: UInt) = (addr >> untagBits).asUInt()
  def get_idx(addr: UInt) = addr(untagBits-1, blockOffBits)
  def get_block(addr: UInt) = addr >> blockOffBits
  def get_block_addr(addr: UInt) = (addr >> blockOffBits) << blockOffBits

  def get_beat(addr: UInt) = addr(blockOffBits - 1, beatOffBits)
  def get_row(addr: UInt) = addr(blockOffBits - 1, rowOffBits)
  def get_word(addr: UInt) = addr(blockOffBits - 1, wordOffBits)

  def beatRows = beatBits/rowBits
  def rowWords = rowBits/wordBits

  def full_divide(a: Int, b: Int) = a >= b && isPow2(a / b)
}

case class DCacheParameters
(
  nSets: Int = 64,
  nWays: Int = 8,
  rowBits: Int = 128,
  nTLBEntries: Int = 32,
  tagECC: Option[String] = None,
  dataECC: Option[String] = None,
  replacer: Option[String] = Some("random"),
  nMissEntries: Int = 1,
  nProbeEntries: Int = 1,
  nReleaseEntries: Int = 1,
  nStoreReplayEntries: Int = 1,
  nMMIOEntries: Int = 1,
  nMMIOs: Int = 1,
  blockBytes: Int = 64
) extends L1CacheParameters {}

trait HasDCacheParameters extends HasL1CacheParameters {
  val cacheParams = dcacheParameters
  val cfg = cacheParams

  def lrscCycles = LRSCCycles // ISA requires 16-insn LRSC sequences to succeed
  def lrscBackoff = 3 // disallow LRSC reacquisition briefly
  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant
  def nIOMSHRs = cacheParams.nMMIOs

  def maxUncachedInFlight = cacheParams.nMMIOs

  def nSourceType = 3

  def sourceTypeWidth = log2Up(nSourceType)

  def LOAD_SOURCE = 0

  def STORE_SOURCE = 1

  def AMO_SOURCE = 2

  // each source use a id to distinguish its multiple reqs
  def reqIdWidth = 64

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(rowBits, wordBits), s"rowBits($rowBits) must be multiple of wordBits($wordBits)")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
  // this is a VIPT L1 cache
  require(pgIdxBits >= untagBits, s"page aliasing problem: pgIdxBits($pgIdxBits) < untagBits($untagBits)")
  // require(rowWords == 1, "Our DCache Implementation assumes rowWords == 1")
}

abstract class L1CacheBundle(implicit p: Parameters) extends Bundle
  with HasL1CacheParameters

abstract class DCacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasDCacheParameters