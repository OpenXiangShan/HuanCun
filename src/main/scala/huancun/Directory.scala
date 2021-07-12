package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun.utils.{ParallelMux, SRAMTemplate}

class Directory(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val reads = Vec(dirReadPorts, Flipped(DecoupledIO(new DirRead)))
    val results = Vec(dirReadPorts, ValidIO(new DirResult))
    val dirWReqs = Vec(dirWritePorts, Flipped(DecoupledIO(new DirWrite)))
    val tagWReq = Flipped(DecoupledIO(new TagWrite))
  })

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  val metaArray = Mem(cacheParams.sets, Vec(cacheParams.ways, new DirectoryEntry))

  when(!resetFinish) {
    metaArray(resetIdx).foreach(w => w.state := MetaData.INVALID)
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) { resetFinish := true.B }

  val writeFactor = 10
  val writeCounter = RegInit(writeFactor.U)

  when(io.tagWReq.valid && resetFinish) {
    when(io.tagWReq.ready) {
      writeCounter := writeFactor.U
    }.otherwise({
      writeCounter := writeCounter - 1.U
    })
  }
  io.tagWReq.ready := writeCounter === 0.U

  io.reads.foreach(_.ready := !io.tagWReq.fire() && resetFinish)

  val tagArray = Array.fill(dirReadPorts) {
    Module(new SRAMTemplate(UInt(tagBits.W), cacheParams.sets, cacheParams.ways, singlePort = true))
  }

  val tagRead = Seq.fill(dirReadPorts) { Wire(Vec(cacheParams.ways, UInt(tagBits.W))) }
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

  for ((result, i) <- io.results.zipWithIndex) {
    result.valid := rreqValids(i)
    result.bits.idOH := rreqIds(i)
    val tags = tagRead(i)
    val hitVec = tags.map(t => t === rreqTags(i))
    val hitWay = OHToUInt(hitVec)
    result.bits.hit := Cat(hitVec).orR()
    result.bits.way := hitWay // TODO: add replace way
    val meta = ParallelMux(hitVec.zip(metaArray(rreqSets(i))))
    result.bits.dirty := meta.dirty
    result.bits.state := meta.state
    result.bits.clients := meta.clients
    result.bits.tag := 0.U // ParallelMux(hitVec.zip(tags)) // TODO
  }

  for (dirWReq <- io.dirWReqs) {
    when(dirWReq.fire()) {
      metaArray(dirWReq.bits.set)(dirWReq.bits.way) := dirWReq.bits.data
    }
    dirWReq.ready := true.B
  }

}
