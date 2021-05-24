package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._

abstract class InnerTask(implicit p: Parameters) extends HuanCunBundle {
  val sourceId = UInt(sourceIdBits.W)
}

class SinkAReq(implicit p: Parameters) extends InnerTask
class SourceBReq(implicit p: Parameters) extends InnerTask
class SinkCReq(implicit p: Parameters) extends InnerTask
class SourceDReq(implicit p: Parameters) extends InnerTask

class SourceAReq(implicit p: Parameters) extends HuanCunBundle
class SourceCReq(implicit p: Parameters) extends HuanCunBundle
class SourceEReq(implicit p: Parameters) extends HuanCunBundle

class SinkCResp(implicit p: Parameters) extends HuanCunBundle {
  // ProbeAck
  val set = UInt(setBits.W) // The target address of the transfer, but only set is enough
}
class SinkDResp(implicit p: Parameters) extends HuanCunBundle {
  // Grant / AccessAck / ReleaseAck
  val source = UInt(mshrBits.W) // The master source id receiving the resp
}
class SinkEResp(implicit p: Parameters) extends HuanCunBundle {
  // GrantAck
  val sink = UInt(mshrBits.W) // The slave sink id accepting this resp
}

class MSHRRequest(implicit p: Parameters) extends HuanCunBundle {
  val channel = UInt(3.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val size = UInt(msgSizeBits.W)
  val source = UInt(sourceIdBits.W)
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)

  def fromA = channel(0).asBool
  def fromB = channel(1).asBool
  def fromC = channel(2).asBool
}

class MSHRStatus(implicit p: Parameters) extends HuanCunBundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val reload = Bool()
}

class TagWrite(implicit p: Parameters) extends HuanCunBundle {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val tag = UInt(tagBits.W)
}

class DirectoryEntry(implicit p: Parameters) extends HuanCunBundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clients = UInt(clientBits.W)
}

class DirWrite(implicit p: Parameters) extends HuanCunBundle {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val data = new DirectoryEntry
}

class DirRead(implicit p: Parameters) extends HuanCunBundle {
  val id = UInt(mshrsAll.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
}

class DirResult(implicit p: Parameters) extends DirectoryEntry {
  val id = UInt(mshrsAll.W)
  val hit = Bool()
  val way = UInt(wayBits.W)
}
