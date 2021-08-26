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
  val clients = Vec(clientBits, new ClientDirResult)
}

class SelfTagWrite(implicit p: Parameters) extends BaseTagWrite {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val tag = UInt(tagBits.W)
}

class ClientTagWrite(implicit p: Parameters) extends HuanCunBundle with HasClientInfo {
  val set = UInt(clientSetBits.W)
  val way = UInt(clientWayBits.W) // log2
  val tag = UInt(clientTagBits.W)

  def apply(lineAddr: UInt, way: UInt) = { // way is in oneHot form
    val w = Wire(this)
    w.set := lineAddr(clientSetBits - 1, 0)
    w.way := OHToUInt(way)
    w.tag := lineAddr(clientSetBits + clientTagBits - 1, clientSetBits)
    w
  }
}

class SelfDirWrite(implicit p: Parameters) extends BaseDirWrite {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val data = new SelfDirEntry
}

class ClientDirWrite(implicit p: Parameters) extends HuanCunBundle with HasClientInfo {
  val set = UInt(clientSetBits.W)
  val way = UInt(clientWayBits.W)
  val data = new ClientDirEntry()

  def apply(lineAddr: UInt, way: UInt, data: UInt) = { // way is in oneHot form
    val w = Wire(this)
    w.set := lineAddr(clientSetBits - 1, 0)
    w.way := OHToUInt(way)
    w.data.state := data
    w
  }
}

class DirectoryIO(implicit p: Parameters) extends BaseDirectoryIO[DirResult, SelfDirWrite, SelfTagWrite] {
  val reads = Vec(dirReadPorts, Flipped(DecoupledIO(new DirRead)))
  val results = Vec(dirReadPorts, ValidIO(new DirResult))
  val dirWReqs = Vec(mshrsAll, Flipped(DecoupledIO(new SelfDirWrite)))
  val tagWReq = Flipped(DecoupledIO(new SelfTagWrite))
  val clientDirWReqs = Vec(clientBits, Vec(mshrsAll, Flipped(DecoupledIO(new ClientDirWrite))))
  val clientTagWreq = Vec(clientBits, Flipped(DecoupledIO(new ClientTagWrite)))
}

class Directory(implicit p: Parameters)
    extends BaseDirectory[DirResult, SelfDirWrite, SelfTagWrite]
    with DontCareInnerLogic {
  val io = IO(new DirectoryIO())
}
