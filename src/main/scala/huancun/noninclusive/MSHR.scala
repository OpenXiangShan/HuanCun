package huancun.noninclusive

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun._

class MSHR()(implicit p: Parameters) extends BaseMSHR[DirResult, SelfDirWrite, SelfTagWrite] with DontCareInnerLogic {
  val io = IO(new BaseMSHRIO[DirResult, SelfDirWrite, SelfTagWrite] {
    override val tasks = new MSHRTasks[SelfDirWrite, SelfTagWrite] {
      override val dir_write: DecoupledIO[SelfDirWrite] = DecoupledIO(new SelfDirWrite())
      override val tag_write: DecoupledIO[SelfTagWrite] = DecoupledIO(new SelfTagWrite())
      val client_dir_write = Vec(clientBits, DecoupledIO(new ClientDirWrite()))
      val client_tag_write = Vec(clientBits, DecoupledIO(new ClientTagWrite()))
    }
    override val dirResult = Flipped(ValidIO(new DirResult()))
  })
}
