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
import chisel3.util.random.LFSR
import freechips.rocketchip.tilelink.TLMessages
import huancun.utils._

trait BaseDirResult extends HuanCunBundle {
  val idOH = UInt(mshrsAll.W) // which mshr the result should be sent to
}
trait BaseDirWrite extends HuanCunBundle
trait BaseTagWrite extends HuanCunBundle

class DirRead(implicit p: Parameters) extends HuanCunBundle {
  val idOH = UInt(mshrsAll.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val replacerInfo = new ReplacerInfo()
  val source = UInt(sourceIdBits.W)
}

abstract class BaseDirectoryIO[T_RESULT <: BaseDirResult, T_DIR_W <: BaseDirWrite, T_TAG_W <: BaseTagWrite](
  implicit p: Parameters)
    extends HuanCunBundle {
  val reads:    Vec[DecoupledIO[DirRead]]
  val results:  Vec[Valid[T_RESULT]]
  val dirWReqs: Vec[DecoupledIO[T_DIR_W]]
  val tagWReq:  DecoupledIO[T_TAG_W]
}

abstract class BaseDirectory[T_RESULT <: BaseDirResult, T_DIR_W <: BaseDirWrite, T_TAG_W <: BaseTagWrite](
  implicit p: Parameters)
    extends HuanCunModule {
  val io: BaseDirectoryIO[T_RESULT, T_DIR_W, T_TAG_W]
}

class SubDirectory[T <: Data](
  rports:      Int,
  wports:      Int,
  sets:        Int,
  ways:        Int,
  tagBits:     Int,
  dir_init_fn: () => T,
  dir_hit_fn: T => Bool,
  invalid_way_sel: (Seq[T], UInt) => (Bool, UInt),
  replacement: String)
    extends MultiIOModule {

  val setBits = log2Ceil(sets)
  val wayBits = log2Ceil(ways)
  val dir_init = dir_init_fn()

  val io = IO(new Bundle() {
    val reads = Vec(
      rports,
      Flipped(DecoupledIO(new Bundle() {
        val tag = UInt(tagBits.W)
        val set = UInt(setBits.W)
        val replacerInfo = new ReplacerInfo()
      }))
    )
    val resps = Vec(
      rports,
      ValidIO(new Bundle() {
        val hit = Bool()
        val way = UInt(wayBits.W)
        val tag = UInt(tagBits.W)
        val dir = dir_init.cloneType
      })
    )
    val tag_w = Flipped(DecoupledIO(new Bundle() {
      val tag = UInt(tagBits.W)
      val set = UInt(setBits.W)
      val way = UInt(wayBits.W)
    }))
    val dir_w = Vec(
      wports,
      Flipped(DecoupledIO(new Bundle() {
        val set = UInt(setBits.W)
        val way = UInt(wayBits.W)
        val dir = dir_init.cloneType
      }))
    )
  })

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((sets - 1).U)
  val metaArray = Mem(sets, Vec(ways, dir_init.cloneType))

  when(!resetFinish) {
    metaArray(resetIdx).foreach(w => w := dir_init)
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }

  io.tag_w.ready := true.B
  io.reads.foreach(_.ready := !io.tag_w.valid && resetFinish)

  val tagArray = Array.fill(rports) {
    Module(new SRAMTemplate(UInt(tagBits.W), sets, ways, singlePort = true))
  }

  val tagRead = Seq.fill(rports) {
    Wire(Vec(ways, UInt(tagBits.W)))
  }
  tagArray
    .zip(io.reads)
    .zip(tagRead)
    .foreach({
      case ((sram, rreq), rdata) =>
        val wen = io.tag_w.fire()
        sram.io.w(wen, io.tag_w.bits.tag, io.tag_w.bits.set, UIntToOH(io.tag_w.bits.way))
        assert(!wen || (wen && sram.io.w.req.ready))
        rdata := sram.io.r(!wen, rreq.bits.set).resp.data
        assert(wen || (!wen && sram.io.r.req.ready))
    })

  val replacer = new SetAssocReplacer(sets, ways, replacement)

  val reqTags = io.reads.map(r => RegEnable(r.bits.tag, enable = r.fire()))
  val reqSets = io.reads.map(r => RegEnable(r.bits.set, enable = r.fire()))
  val reqValids = io.reads.map(r => RegNext(r.fire(), false.B))
  val reqReplacerInfo = io.reads.map(r => RegEnable(r.bits.replacerInfo, enable = r.fire()))

  val hitVec = Seq.fill(rports)(Wire(Vec(ways, Bool())))

  for ((result, i) <- io.resps.zipWithIndex) {
    result.valid := reqValids(i)
    val tags = tagRead(i)
    val metas = metaArray(reqSets(i))
    val tagMatchVec = tags.map(_ === reqTags(i))
    val metaValidVec = metas.map(dir_hit_fn)
    hitVec(i) := tagMatchVec.zip(metaValidVec).map(a => a._1 && a._2)
    val hitWay = OHToUInt(hitVec(i))
    val replaceWay = replacer.way(reqSets(i))
    val (inv, invalidWay) = invalid_way_sel(metas, replaceWay)
    val chosenWay = Mux(inv, invalidWay, replaceWay)

    result.bits.hit := Cat(hitVec(i)).orR()
    result.bits.way := Mux(result.bits.hit, hitWay, chosenWay)
    val meta = metas(result.bits.way)
    result.bits.dir := meta
    result.bits.tag := tags(result.bits.way)
  }

  for (req <- io.dir_w) {
    when(req.fire()) {
      metaArray(req.bits.set)(req.bits.way) := req.bits.dir
    }
    req.ready := true.B
  }

}

trait HasUpdate {
  def doUpdate(info: ReplacerInfo): Bool
}

trait UpdateOnRelease extends HasUpdate {
  override def doUpdate(info: ReplacerInfo) = {
    info.channel(2) && info.opcode === TLMessages.ReleaseData
  }
}

trait UpdateOnAcquire extends HasUpdate {
  override def doUpdate(info: ReplacerInfo) = {
    info.channel(0) && (info.opcode === TLMessages.AcquirePerm || info.opcode === TLMessages.AcquireBlock)
  }
}

abstract class SubDirectoryDoUpdate[T <: Data](
  rports:      Int,
  wports:      Int,
  sets:        Int,
  ways:        Int,
  tagBits:     Int,
  dir_init_fn: () => T,
  dir_hit_fn:  T => Bool,
  invalid_way_sel: (Seq[T], UInt) => (Bool, UInt),
  replacement: String)
    extends SubDirectory[T](
      rports, wports, sets, ways, tagBits,
      dir_init_fn, dir_hit_fn, invalid_way_sel,
      replacement
    ) with HasUpdate {

  for ((result, i) <- io.resps.zipWithIndex) {
    val updateReplacer = doUpdate(reqReplacerInfo(i))
    when(reqValids(i) && updateReplacer) {
      replacer.access(reqSets(i), result.bits.way)
    }
  }
}
