package huancun.noninclusive

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun._

class MSHR()(implicit p: Parameters) extends BaseMSHR[DirResult, DirWrite, TagWrite] with DontCareInnerLogic {
  val io = IO(new BaseMSHRIO[DirResult, DirWrite, TagWrite] {
    override val tasks = new MSHRTasks[DirWrite, TagWrite] {
      override val dir_write: DecoupledIO[DirWrite] = DecoupledIO(new DirWrite())
      override val tag_write: DecoupledIO[TagWrite] = DecoupledIO(new TagWrite())
    }
    override val dirResult = Flipped(ValidIO(new DirResult()))
  })
}
