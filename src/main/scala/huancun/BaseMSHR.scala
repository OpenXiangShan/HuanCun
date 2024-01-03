package huancun

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import huancun.prefetch._

abstract class MSHRTasks[T_DIR_W <: BaseDirWrite, T_TAG_W <: BaseTagWrite](implicit p: Parameters)
    extends HuanCunBundle {
  // inner
  val sink_a = DecoupledIO(new SinkAReq) // put
  val source_b = DecoupledIO(new SourceBReq) // probe
  val sink_c = DecoupledIO(new SinkCReq) // inner release
  val source_d = DecoupledIO(new SourceDReq) // grant & atomics
  // outer
  val source_a = DecoupledIO(new SourceAReq) // acquire
  val source_c = DecoupledIO(new SourceCReq) // outer release & probe ack
  val source_e = DecoupledIO(new SourceEReq) // grant ack
  // direcotry & tag write
  val dir_write: DecoupledIO[T_DIR_W] // = DecoupledIO(new DirWrite)
  val tag_write: DecoupledIO[T_TAG_W] // = DecoupledIO(new TagWrite)
  // prefetcher
  val prefetch_train = prefetchOpt.map(_ => DecoupledIO(new PrefetchTrain))
  val prefetch_resp = prefetchOpt.map(_ => DecoupledIO(new PrefetchResp))
}

class MSHRResps(implicit p: Parameters) extends HuanCunBundle {
  val sink_c = ValidIO(new SinkCResp)
  val sink_d = ValidIO(new SinkDResp)
  val sink_e = ValidIO(new SinkEResp)
  val source_d = ValidIO(new SourceDResp)
  val sink_c_ack = ValidIO(new SinkCAck)
}

class NestedWriteback(implicit p: Parameters) extends HuanCunBundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val b_toN, b_toB, b_clr_dirty, b_set_dirty = Bool()
  val c_set_dirty = Bool()
  val c_set_hit = Bool()
  val clients =
    if (!cacheParams.inclusive)
      Some(
        Vec(
          clientBits,
          new Bundle {
            val isToN, isToB = Bool()
          }
        )
      )
    else None
}

abstract class BaseMSHRIO[T_RESULT <: BaseDirResult, T_DIR_W <: BaseDirWrite, T_TAG_W <: BaseTagWrite](
  implicit p: Parameters)
    extends HuanCunBundle {
  val id = Input(UInt(mshrBits.W))
  val enable = Input(Bool())
  val alloc = Flipped(ValidIO(new MSHRRequest))
  val status = ValidIO(new MSHRStatus)
  val tasks:     MSHRTasks[T_DIR_W, T_TAG_W] //= new MSHRTasks
  val dirResult: Valid[T_RESULT] // = Flipped(ValidIO(new DirResult))
  val resps = Flipped(new MSHRResps)
  val nestedwb = Input(new NestedWriteback)
  val ecc = Valid(new EccInfo)
}

abstract class BaseMSHR[T_RESULT <: BaseDirResult, T_DIR_W <: BaseDirWrite, T_TAG_W <: BaseTagWrite](
  implicit p: Parameters)
    extends HuanCunModule {
  val io: BaseMSHRIO[T_RESULT, T_DIR_W, T_TAG_W]
}
