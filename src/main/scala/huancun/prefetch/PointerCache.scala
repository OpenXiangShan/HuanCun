package huancun.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun._
import huancun.utils.SRAMTemplate

class CommitInfo(implicit p: Parameters) extends HuanCunBundle {
  val vaddr = UInt(vaddrBits.W)
  val data = UInt(64.W) // TODO: parameterize this
}

class CoreCommitInfos(implicit p: Parameters) extends HuanCunBundle {
  val ld = Vec(commitWidth, Valid(new CommitInfo))
  val st = Vec(commitWidth, Valid(new CommitInfo))
}

class CoreMissInfo(implicit p: Parameters) extends HuanCunBundle {
  val vaddr = UInt(vaddrBits.W)
  val needT = Bool()
}

class TagReadReq(implicit p: Parameters) extends PCBundle {
  val idx = UInt(pcIdxBits.W)
  val way_en = UInt(pcWays.W)
}

class TagReadResp(implicit p: Parameters) extends PCBundle {
  val valids = Vec(pcWays, Bool())
  val tags = Vec(pcWays, UInt(pcTagBits.W))
}

class TagWriteReq(implicit p: Parameters) extends TagReadReq {
  val tag = UInt(pcTagBits.W)
}

class DataReadReq(implicit p: Parameters) extends PCBundle {
  val idx = UInt(pcIdxBits.W)
  val way_en = UInt(pcWays.W)
}

class DataReadResp(implicit p: Parameters) extends PCBundle {
  val diffAddr = UInt(diffAddrBits.W) // differential address field
}

class DataWriteReq(implicit p: Parameters) extends DataReadReq {
  val diffAddr = UInt(diffAddrBits.W)
}

class PointerCache(implicit p: Parameters) extends PCModule {
  val io = IO(new Bundle() {
    val tag_read = Flipped(DecoupledIO(new TagReadReq))
    val tag_resp = Output(new TagReadResp)
    val tag_write = Flipped(DecoupledIO(new TagWriteReq))

    val data_read = Flipped(DecoupledIO(new DataReadReq))
    val data_resp = Output(new DataReadResp)
    val data_write = Flipped(DecoupledIO(new DataWriteReq))
  })

  val valids = Reg(Vec(pcSets, Vec(pcWays, Bool())))
  when (reset.asBool()) {
    valids := 0.U.asTypeOf(valids.cloneType)
  }
  val tag_array = Seq.fill(pcWays)(Module(new SRAMTemplate(
    UInt(pcTagBits.W),
    set = pcSets,
    way = 1,
    shouldReset = false,
    holdRead = false,
    singlePort = true
  )))
  val data_array = Seq.fill(pcWays)(Module(new SRAMTemplate(
    UInt(diffAddrBits.W),
    set = pcSets,
    way = 1,
    shouldReset = false,
    holdRead = false,
    singlePort = true
  )))

  val tag_rwconflict = Wire(Vec(pcWays, Bool()))
  val tag_ren = io.tag_read.fire()
  tag_array.zipWithIndex.foreach {
    case (array, i) =>
      val write = io.tag_write
      val wen = write.valid && write.bits.way_en(i).asBool
      tag_rwconflict(i) := wen
      array.io.w.req.valid := wen
      array.io.w.req.bits.apply(
        data = write.bits.tag,
        setIdx = write.bits.idx,
        waymask = (-1).asSInt.asUInt
      )
      array.io.r.req.valid := tag_ren && io.tag_read.bits.way_en(i).asBool
      array.io.r.req.bits.apply(setIdx = io.tag_read.bits.idx)
  }

  val data_rwconflict = Wire(Vec(pcWays, Bool()))
  val data_ren = io.data_read.fire()
  assert(RegNext(!data_ren || PopCount(io.data_read.bits.way_en) <= 1.U))
  data_array.zipWithIndex.foreach {
    case (array, i) =>
      val write = io.data_write
      val wen = write.valid && write.bits.way_en(i).asBool
      data_rwconflict(i) := wen
      array.io.w.req.valid := wen
      array.io.w.req.bits.apply(
        data = write.bits.diffAddr,
        setIdx = write.bits.idx,
        waymask = (-1).asSInt.asUInt
      )
      array.io.r.req.valid := data_ren && io.data_read.bits.way_en(i).asBool
      array.io.r.req.bits.apply(setIdx = io.data_read.bits.idx)
  }

  io.tag_read.ready := !tag_rwconflict.asUInt.orR && !reset.asBool()
  io.tag_resp.tags := VecInit(tag_array.map(_.io.r.resp.data(0)))
  io.tag_resp.valids := RegNext(valids(io.tag_read.bits.idx))
  io.tag_write.ready := !reset.asBool()

  io.data_read.ready := !data_rwconflict.asUInt.orR && !reset.asBool()
  io.data_resp.diffAddr := PriorityMux(
    RegNext(io.data_read.bits.way_en).asBools,
    data_array.map(_.io.r.resp.data(0))
  )
  io.data_write.ready := !reset.asBool()
}