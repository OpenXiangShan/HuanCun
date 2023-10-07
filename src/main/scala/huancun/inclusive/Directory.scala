package huancun.inclusive

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SetAssocLRU
import huancun._
import huancun.utils._
import utility.{ParallelMax, ParallelPriorityMux}

// TODO: inclusive may have cache aliase too

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
  val error = Bool()
}

class DirectoryIO(implicit p: Parameters) extends BaseDirectoryIO[DirResult, DirWrite, TagWrite] {
  val read = Flipped(DecoupledIO(new DirRead))
  val result = ValidIO(new DirResult)
  val dirWReq = Flipped(DecoupledIO(new DirWrite))
  val tagWReq = Flipped(DecoupledIO(new TagWrite))
}

class Directory(implicit p: Parameters) extends BaseDirectory[DirResult, DirWrite, TagWrite] {

  val io = IO(new DirectoryIO())

  def invalid_way_sel(metaVec: Seq[DirectoryEntry], repl: UInt) = {
    val invalid_vec = metaVec.map(_.state === MetaData.INVALID)
    val has_invalid_way = Cat(invalid_vec).orR
    val way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(wayBits.W)))
    (has_invalid_way, way)
  }

  val dir = Module(
    new SubDirectoryDoUpdate[DirectoryEntry](
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
      dir_hit_fn = x => x.state =/= MetaData.INVALID,
      invalid_way_sel = invalid_way_sel,
      replacement = cacheParams.replacement
    ) with UpdateOnAcquire
  )
  val rport = dir.io.read
  val req = io.read
  rport.valid := req.valid
  rport.bits.set := req.bits.set
  rport.bits.tag := req.bits.tag
  rport.bits.replacerInfo := req.bits.replacerInfo
  rport.bits.wayMode := false.B
  rport.bits.way := DontCare
  req.ready := rport.ready
  val reqIdOHReg = RegEnable(req.bits.idOH, req.fire)
  val resp = io.result
  val selfResp = dir.io.resp
  resp.valid := selfResp.valid
  resp.bits.idOH := reqIdOHReg
  resp.bits.hit := selfResp.bits.hit
  resp.bits.way := selfResp.bits.way
  resp.bits.tag := selfResp.bits.tag
  resp.bits.dirty := selfResp.bits.dir.dirty
  resp.bits.state := selfResp.bits.dir.state
  resp.bits.clients := selfResp.bits.dir.clients
  resp.bits.prefetch.foreach(p => p := selfResp.bits.dir.prefetch.get)
  resp.bits.error := selfResp.bits.error
  // Self Tag Write
  dir.io.tag_w.valid := io.tagWReq.valid
  dir.io.tag_w.bits.tag := io.tagWReq.bits.tag
  dir.io.tag_w.bits.set := io.tagWReq.bits.set
  dir.io.tag_w.bits.way := io.tagWReq.bits.way
  io.tagWReq.ready := dir.io.tag_w.ready
  // Self Dir Write
  dir.io.dir_w.valid := io.dirWReq.valid
  dir.io.dir_w.bits.set := io.dirWReq.bits.set
  dir.io.dir_w.bits.way := io.dirWReq.bits.way
  dir.io.dir_w.bits.dir := io.dirWReq.bits.data
  io.dirWReq.ready := dir.io.dir_w.ready
}
