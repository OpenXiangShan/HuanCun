/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{ReplacementPolicy, SetAssocLRU}
import huancun.utils.{ParallelMux, ParallelPriorityMux, SRAMTemplate}

class Directory(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val reads = Vec(dirReadPorts, Flipped(DecoupledIO(new DirRead)))
    val results = Vec(dirReadPorts, ValidIO(new DirResult))
    val dirWReqs = Vec(mshrsAll, Flipped(DecoupledIO(new DirWrite)))
    val tagWReq = Flipped(DecoupledIO(new TagWrite))
  })

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  val metaArray = Mem(cacheParams.sets, Vec(cacheParams.ways, new DirectoryEntry))

  val replacer = new SetAssocLRU(cacheParams.sets, cacheParams.ways, cacheParams.replacement)

  when(!resetFinish) {
    metaArray(resetIdx).foreach(w => w.state := MetaData.INVALID)
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) { resetFinish := true.B }

  io.tagWReq.ready := true.B // let tag write block tag read
  io.reads.foreach(_.ready := !io.tagWReq.valid && resetFinish)

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
    result.bits.way := Mux(result.bits.hit, hitWay, chosenWay)
    val meta = metas(result.bits.way)
    result.bits.dirty := meta.dirty
    result.bits.state := meta.state
    result.bits.clients := meta.clients
    result.bits.prefetch.map(_ := meta.prefetch.getOrElse(false.B))
    result.bits.tag := tags(result.bits.way)
  }
  // TODO: should we update replacer when cache hit?
  when(io.tagWReq.fire()) {
    replacer.access(io.tagWReq.bits.set, io.tagWReq.bits.way)
  }

  for (dirWReq <- io.dirWReqs) {
    when(dirWReq.fire()) {
      metaArray(dirWReq.bits.set)(dirWReq.bits.way) := dirWReq.bits.data
    }
    dirWReq.ready := true.B
  }

}
