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
  val read:    DecoupledIO[DirRead]
  val result:  Valid[T_RESULT]
  val dirWReqs: Vec[DecoupledIO[T_DIR_W]]
  val tagWReq:  DecoupledIO[T_TAG_W]
}

abstract class BaseDirectory[T_RESULT <: BaseDirResult, T_DIR_W <: BaseDirWrite, T_TAG_W <: BaseTagWrite](
  implicit p: Parameters)
    extends HuanCunModule {
  val io: BaseDirectoryIO[T_RESULT, T_DIR_W, T_TAG_W]
}

class SubDirectory[T <: Data](
  wports:      Int,
  sets:        Int,
  ways:        Int,
  tagBits:     Int,
  dir_init_fn: () => T,
  dir_hit_fn: T => Bool,
  invalid_way_sel: (Seq[T], UInt) => (Bool, UInt),
  replacement: String)(implicit p: Parameters)
    extends MultiIOModule {

  val setBits = log2Ceil(sets)
  val wayBits = log2Ceil(ways)
  val dir_init = dir_init_fn()

  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new Bundle() {
      val tag = UInt(tagBits.W)
      val set = UInt(setBits.W)
      val replacerInfo = new ReplacerInfo()
    }))
    val resp = ValidIO(new Bundle() {
      val hit = Bool()
      val way = UInt(wayBits.W)
      val tag = UInt(tagBits.W)
      val dir = dir_init.cloneType
      val error = Bool()
    })
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
  val metaArray = Seq.fill(ways) {
    Module(new SyncDataModuleTemplate(dir_init.cloneType, sets, 1, wports))
  }

  val tag_wen = io.tag_w.valid
  val replacer_wen = WireInit(false.B)
  io.tag_w.ready := true.B
  io.read.ready := !tag_wen && !replacer_wen && resetFinish

  def tagCode: Code = Code.fromString(p(HCCacheParamsKey).tagECC)

  val eccTagBits = tagCode.width(tagBits)
  println(s"extra ECC Tagbits:${eccTagBits - tagBits}")
  val tagArray = Module(new SRAMTemplate(UInt(eccTagBits.W), sets, ways, singlePort = true))
  val tagRead = Wire(Vec(ways, UInt(eccTagBits.W)))

  tagArray.io.w(
    io.tag_w.fire(),
    tagCode.encode(io.tag_w.bits.tag),
    io.tag_w.bits.set,
    UIntToOH(io.tag_w.bits.way)
  )
  tagRead := tagArray.io.r(!io.tag_w.fire(), io.read.bits.set).resp.data

  val reqReg = RegEnable(io.read.bits, enable = io.read.fire())
  val reqValidReg = RegNext(io.read.fire(), false.B)

  val repl = ReplacementPolicy.fromString(replacement, ways)
  val replacer_sram = Module(new SRAMTemplate(UInt(repl.nBits.W), sets, singlePort = true))
  val repl_state = replacer_sram.io.r(!replacer_wen, io.read.bits.set).resp.data(0)
  val next_state = repl.get_next_state(repl_state, io.resp.bits.way)
  replacer_sram.io.w(replacer_wen, next_state, reqReg.set, 1.U)

  for (m <- metaArray) {
    m.io.raddr(0) := io.read.bits.set
  }

  io.resp.valid := reqValidReg
  val metas = VecInit(metaArray.map(m => m.io.rdata(0)))
  val tagMatchVec = tagRead.map(_(tagBits - 1, 0) === reqReg.tag)
  val metaValidVec = metas.map(dir_hit_fn)
  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)
  val replaceWay = repl.get_replace_way(repl_state)
  val (inv, invalidWay) = invalid_way_sel(metas, replaceWay)
  val chosenWay = Mux(inv, invalidWay, replaceWay)
  val meta = metas(io.resp.bits.way)
  val tag_decode = tagCode.decode(tagRead(io.resp.bits.way))
  val tag = tag_decode.uncorrected
  io.resp.bits.hit := Cat(hitVec).orR()
  io.resp.bits.way := Mux(io.resp.bits.hit, hitWay, chosenWay)
  io.resp.bits.dir := meta
  io.resp.bits.tag := tag
  io.resp.bits.error := tag_decode.error

  for ((req, i) <- io.dir_w.zipWithIndex) {
    for( (m, way) <- metaArray.zipWithIndex ){
      m.io.wen(i) := req.fire() && way.U === req.bits.way
      m.io.waddr(i) := req.bits.set
      m.io.wdata(i) := req.bits.dir
    }
    req.ready := true.B
  }

  for( m <- metaArray ) {
    when(!resetFinish){
      m.io.waddr(0) := resetIdx
      m.io.wdata(0) := dir_init
      m.io.wen(0) := true.B
    }
  }
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
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
  wports:      Int,
  sets:        Int,
  ways:        Int,
  tagBits:     Int,
  dir_init_fn: () => T,
  dir_hit_fn:  T => Bool,
  invalid_way_sel: (Seq[T], UInt) => (Bool, UInt),
  replacement: String)(implicit p: Parameters)
    extends SubDirectory[T](
      wports, sets, ways, tagBits,
      dir_init_fn, dir_hit_fn, invalid_way_sel,
      replacement
    ) with HasUpdate {

  val update = doUpdate(reqReg.replacerInfo)
  when(reqValidReg && update){
    replacer_wen := true.B
  }
}
