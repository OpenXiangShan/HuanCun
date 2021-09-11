package huancun.inclusive

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SetAssocLRU
import huancun._
import huancun.utils._

class TagWrite(implicit p: Parameters) extends BaseTagWrite {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val tag = UInt(tagBits.W)
}

class DirectoryEntry(implicit p: Parameters) extends HuanCunBundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clients = UInt(clientBits.W)
  val prefetch = if (hasPrefetchBit) Some(Bool()) else None // whether the block is prefetched
}

class DirWrite(implicit p: Parameters) extends BaseDirWrite {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val data = new DirectoryEntry
}

class DirResult(implicit p: Parameters) extends DirectoryEntry with BaseDirResult {
  val hit = Bool()
  val way = UInt(wayBits.W)
  val tag = UInt(tagBits.W)
}

class DirectoryIO(implicit p: Parameters) extends BaseDirectoryIO[DirResult, DirWrite, TagWrite] {
  val reads = Vec(dirReadPorts, Flipped(DecoupledIO(new DirRead)))
  val results = Vec(dirReadPorts, ValidIO(new DirResult))
  val dirWReqs = Vec(mshrsAll, Flipped(DecoupledIO(new DirWrite)))
  val tagWReq = Flipped(DecoupledIO(new TagWrite))
}

class Directory(implicit p: Parameters) extends BaseDirectory[DirResult, DirWrite, TagWrite] {

  val io = IO(new DirectoryIO())

  val dir = Module(
    new RandomSubDirectory[DirectoryEntry](
      rports = dirReadPorts,
      wports = mshrsAll,
      sets = cacheParams.sets,
      ways = cacheParams.ways,
      tagBits = tagBits,
      dir_init_fn = () => {
        val init = Wire(new DirectoryEntry())
        init := DontCare
        init.state := MetaData.INVALID
        init
      },
      dir_hit_fn = x => x.state =/= MetaData.INVALID
    )
  )

  for (i <- 0 until dirReadPorts) {
    val rport = dir.io.reads(i)
    val req = io.reads(i)
    rport.valid := req.valid
    rport.bits.set := req.bits.set
    rport.bits.tag := req.bits.tag
    req.ready := rport.ready
    val reqIdOHReg = RegEnable(req.bits.idOH, req.fire())
    val resp = io.results(i)
    val selfResp = dir.io.resps(i)
    resp.valid := selfResp.valid
    resp.bits.idOH := reqIdOHReg
    resp.bits.hit := selfResp.bits.hit
    resp.bits.way := selfResp.bits.way
    resp.bits.tag := selfResp.bits.tag
    resp.bits.dirty := selfResp.bits.dir.dirty
    resp.bits.state := selfResp.bits.dir.state
    resp.bits.clients := selfResp.bits.dir.clients
    resp.bits.prefetch.foreach(p => p := selfResp.bits.dir.prefetch.get)
  }
  // Self Tag Write
  dir.io.tag_w.valid := io.tagWReq.valid
  dir.io.tag_w.bits.tag := io.tagWReq.bits.tag
  dir.io.tag_w.bits.set := io.tagWReq.bits.set
  dir.io.tag_w.bits.way := io.tagWReq.bits.way
  io.tagWReq.ready := dir.io.tag_w.ready
  for ((req, wport) <- io.dirWReqs.zip(dir.io.dir_w)) {
    wport.valid := req.valid
    wport.bits.set := req.bits.set
    wport.bits.way := req.bits.way
    wport.bits.dir := req.bits.data
    req.ready := wport.ready
  }

}
