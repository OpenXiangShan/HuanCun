package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class MSHRTasks(implicit p: Parameters) extends HuanCunBundle {
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
  val dir_write = DecoupledIO(new DirWrite)
  val tag_write = DecoupledIO(new TagWrite)
}

class MSHRResps(implicit p: Parameters) extends HuanCunBundle {
  val sink_c = ValidIO(new SinkCResp)
  val sink_d = ValidIO(new SinkDResp)
  val sink_e = ValidIO(new SinkEResp)
}

class MSHR()(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val id = Input(UInt())
    val alloc = Flipped(ValidIO(new MSHRRequest))
    val status = ValidIO(new MSHRStatus)
    val tasks = new MSHRTasks
    val dirResult = Flipped(ValidIO(new DirResult))
    val resps = Flipped(new MSHRResps)
  })
}
