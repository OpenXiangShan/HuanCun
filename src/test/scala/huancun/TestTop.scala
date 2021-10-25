package huancun

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import scala.collection.mutable.ArrayBuffer

class TestTop()(implicit p: Parameters) extends LazyModule {


  val cacheParams = p(HCCacheParamsKey)

  def createClientNode(name: String, sources: Int) = {
    val masterNode = TLClientNode(Seq(
      TLMasterPortParameters.v2(
        masters = Seq(
          TLMasterParameters.v1(
            name = name,
            sourceId = IdRange(0, sources),
            supportsProbe = TransferSizes(cacheParams.blockBytes)
          )
        ),
        channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
        minLatency = 1,
        echoFields = cacheParams.echoField,
        requestFields = Seq(PrefetchField(), PreferCacheField(), DirtyField()),
        responseKeys = cacheParams.respKey
      )
    ))
    masterNode
  }

  val l1d_nodes = (0 until 2) map( i => createClientNode(s"l1d$i", 16))
  val master_nodes = l1d_nodes

  val l2 = LazyModule(new HuanCun())
  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

  for(l1d <- l1d_nodes){
    xbar := TLBuffer() := l1d
  }

  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      l2.node :=* xbar


  lazy val module = new LazyModuleImp(this){

    master_nodes.zipWithIndex.foreach{
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }


  }
}

object TestTop extends App {
  val config = new Config((_, _, _) => {
    case HCCacheParamsKey => HCCacheParameters(
      inclusive = false,
      clientCaches = Seq(CacheParameters(sets = 32, ways = 8, name = "L2"))
    )
  })
  val top = LazyModule(new TestTop()(config))

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

}
