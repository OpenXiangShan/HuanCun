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
  val idOH = UInt(mshrsAll.W)
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

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  val metaArray = Mem(cacheParams.sets, Vec(cacheParams.ways, new DirectoryEntry))

  val replacer = new SetAssocLRU(cacheParams.sets, cacheParams.ways, cacheParams.replacement)

  when(!resetFinish) {
    metaArray(resetIdx).foreach(w => w.state := MetaData.INVALID)
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }

  io.tagWReq.ready := true.B // let tag write block tag read
  io.reads.foreach(_.ready := !io.tagWReq.valid && resetFinish)

  val tagArray = Array.fill(dirReadPorts) {
    Module(new SRAMTemplate(UInt(tagBits.W), cacheParams.sets, cacheParams.ways, singlePort = true))
  }

  val tagRead = Seq.fill(dirReadPorts) {
    Wire(Vec(cacheParams.ways, UInt(tagBits.W)))
  }
  tagArray
    .zip(io.reads)
    .zip(tagRead)
    .foreach({
      case ((sram, rreq), rdata) =>
        val wen = io.tagWReq.fire()
        sram.io.w(wen, io.tagWReq.bits.tag, io.tagWReq.bits.set, UIntToOH(io.tagWReq.bits.way))
        assert(!wen || (wen && sram.io.w.req.ready))
        rdata := sram.io.r(!wen, rreq.bits.set).resp.data
        assert(wen || (!wen && sram.io.r.req.ready))
    })

  val rreqIds = io.reads.map(r => RegEnable(r.bits.idOH, enable = r.fire()))
  val rreqTags = io.reads.map(r => RegEnable(r.bits.tag, enable = r.fire()))
  val rreqSets = io.reads.map(r => RegEnable(r.bits.set, enable = r.fire()))
  val rreqValids = io.reads.map(r => RegNext(r.fire(), false.B))
  val rreqReplacerInfo = io.reads.map(r => RegEnable(r.bits.replaceInfo, enable = r.fire()))

  val hitVec = Seq.fill(dirReadPorts)(Wire(Vec(cacheParams.ways, Bool())))
  for ((result, i) <- io.results.zipWithIndex) {
    result.valid := rreqValids(i)
    result.bits.idOH := rreqIds(i)
    val tags = tagRead(i)
    val metas = metaArray(rreqSets(i))
    val tagMatchVec = tags.map(_ === rreqTags(i))
    val metaValidVec = metas.map(_.state =/= MetaData.INVALID)
    hitVec(i) := tagMatchVec.zip(metaValidVec).map(a => a._1 && a._2)
    val hitWay = OHToUInt(hitVec(i))
    val replaceWay = replacer.way(rreqSets(i))
    val invalidWay = ParallelPriorityMux(metaValidVec.zipWithIndex.map(a => (!a._1, a._2.U)))
    val chosenWay = Mux(Cat(metaValidVec).andR(), replaceWay, invalidWay)

    result.bits.hit := Cat(hitVec(i)).orR()
    XSPerfAccumulate(cacheParams, "nrmiss", !result.bits.hit && result.valid)
    result.bits.way := Mux(result.bits.hit, hitWay, chosenWay)
    val meta = metas(result.bits.way)
    result.bits.dirty := meta.dirty
    result.bits.state := meta.state
    result.bits.clients := meta.clients
    result.bits.prefetch.foreach(_ := meta.prefetch.get)
    result.bits.tag := tags(result.bits.way)

    // update replacer for req from channel A
    when(result.valid && rreqReplacerInfo(i).channel(0) && !rreqReplacerInfo(i).isHint) {
      replacer.access(rreqSets(i), result.bits.way)
    }
  }

  for (dirWReq <- io.dirWReqs) {
    when(dirWReq.fire()) {
      metaArray(dirWReq.bits.set)(dirWReq.bits.way) := dirWReq.bits.data
    }
    dirWReq.ready := true.B
  }

}
