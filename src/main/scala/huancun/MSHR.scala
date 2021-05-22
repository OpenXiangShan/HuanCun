package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{DecoupledIO, MixedVec, ValidIO}
import freechips.rocketchip.tilelink._

class MSHRTasks extends Bundle {
  // inner
  val sink_a = DecoupledIO(new SinkAReq) // put
  val source_b = DecoupledIO(new SourceBReq) // probe
  val sink_c = DecoupledIO(new SinkCReq) // inner release
  val source_d = DecoupledIO(new SourceDReq) // grant & atomics
  // outer
  val source_a = DecoupledIO(new SourceAReq) // acquire
  val source_c = DecoupledIO(new SourceCReq) // outer release & probe ack
  val source_e = DecoupledIO(new SourceEReq) // grant ack
  // direcotry write
  val dir_write = DecoupledIO(new DirWrite)
}

class MSHR()(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val id = Input(UInt())
    val alloc = Flipped(ValidIO(new MSHRRequest))
    val status = Output(new MSHRStatus)
    val tasks = new MSHRTasks
    val dirResult = Flipped(ValidIO(new DirResult))
  })
}
