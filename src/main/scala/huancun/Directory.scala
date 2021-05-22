package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

class Directory(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val reads = Vec(dirReadPorts, Flipped(DecoupledIO(new DirRead)))
    val results = Vec(dirReadPorts, ValidIO(new DirResult))
    val dirWReqs = Vec(dirWritePorts, Flipped(DecoupledIO(new DirWrite)))
    val tagWReq = Flipped(DecoupledIO(new TagWrite))
  })
}
