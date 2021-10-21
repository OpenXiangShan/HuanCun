package huancun.utils

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class NoopNode()(implicit p: Parameters) extends LazyModule {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, 1)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new LazyModuleImp(this)
}