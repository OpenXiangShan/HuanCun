package huancun.noninclusive

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun._

trait HasClientInfo { this: HasHuanCunParameters =>
  val clientCacheParams = cacheParams.clientCache.get
  val clientSets = clientCacheParams.sets
  val clientWays = clientCacheParams.ways
  val clientSetBits = log2Ceil(clientSets)
  val clientWayBits = log2Ceil(clientWays)
  val clientTagBits = addressBits - clientSetBits - offsetBits
}

class SelfDirEntry(implicit p: Parameters) extends HuanCunBundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clientStates = Vec(clientBits, UInt(stateBits.W))
  val prefetch = if (hasPrefetchBit) Some(Bool()) else None // whether the block is prefetched
}

class ClientDirEntry(implicit p: Parameters) extends HuanCunBundle {
  val state = UInt(stateBits.W)
}

class SelfDirResult(implicit p: Parameters) extends SelfDirEntry {
  val hit = Bool()
  val way = UInt(wayBits.W)
  val tag = UInt(tagBits.W)
}

class ClientDirResult(implicit p: Parameters) extends ClientDirEntry with HasClientInfo {
  val hit = Bool()
  val way = UInt(clientWays.W)
  val tag = UInt(clientTagBits.W)
}

class DirResult(implicit p: Parameters) extends BaseDirResult {
  val self = new SelfDirResult
  val client = new ClientDirResult
}

class TagWriteBundle(val setBits: Int, val wayBits: Int, val tagBits: Int) extends Bundle {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val tag = UInt(tagBits.W)
}

class TagWrite(implicit p: Parameters) extends BaseTagWrite with HasClientInfo {
  val self = new TagWriteBundle(setBits, wayBits, tagBits)
  val client = new TagWriteBundle(clientSetBits, clientWayBits, clientTagBits)
}

class SelfDirWrite(implicit p: Parameters) extends HuanCunBundle {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val data = new SelfDirEntry
}

class ClientDirWrite(implicit p: Parameters) extends HuanCunBundle with HasClientInfo {
  val set = UInt(clientSetBits.W)
  val way = UInt(clientWayBits.W)
  val data = new ClientDirEntry()
}

class DirWrite(implicit p: Parameters) extends BaseDirWrite {
  val self = new SelfDirWrite
  val client = new ClientDirWrite
}

class DirectoryIO(implicit p: Parameters) extends BaseDirectoryIO[DirResult, DirWrite, TagWrite] {
  val reads = Vec(dirReadPorts, Flipped(DecoupledIO(new DirRead)))
  val results = Vec(dirReadPorts, ValidIO(new DirResult))
  val dirWReqs = Vec(mshrsAll, Flipped(DecoupledIO(new DirWrite)))
  val tagWReq = Flipped(DecoupledIO(new TagWrite))
}

class Directory(implicit p: Parameters) extends BaseDirectory[DirResult, DirWrite, TagWrite] with DontCareInnerLogic {
  val io = IO(new DirectoryIO())
}
