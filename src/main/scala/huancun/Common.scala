package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._

class SinkAReq(implicit p: Parameters) extends HuanCunBundle {
  val id = UInt()
}
class SourceBReq(implicit p: Parameters) extends HuanCunBundle {
  val id = UInt()
}
class SinkCReq(implicit p: Parameters) extends HuanCunBundle {
  val id = UInt()
}
class SourceDReq(implicit p: Parameters) extends HuanCunBundle {
  val id = UInt()
}
class SourceAReq(implicit p: Parameters) extends HuanCunBundle
class SourceCReq(implicit p: Parameters) extends HuanCunBundle
class SourceEReq(implicit p: Parameters) extends HuanCunBundle

class MSHRRequest(implicit p: Parameters) extends HuanCunBundle
class MSHRStatus(implicit p: Parameters) extends HuanCunBundle

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
