package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import huancun.utils.SourceIdConverter

trait HasHuanCunParameters {
  val p: Parameters
  val cacheParams = p(CacheParamsKey)
  val blockBytes = cacheParams.blockBytes

  val mshrs = cacheParams.mshrs
  val mshrsAll = cacheParams.mshrs + 2
  val blocks = cacheParams.ways * cacheParams.sets
  val sizeBytes = blocks * blockBytes
  val dirReadPorts = cacheParams.dirReadPorts
  val dirWritePorts = cacheParams.dirWritePorts

  val wayBits = log2Ceil(cacheParams.ways)
  val setBits = log2Ceil(cacheParams.sets)
  val offsetBits = log2Ceil(blockBytes)

  val stateBits = 2

  lazy val edgeInSeq = p(EdgeInSeqKey)
  lazy val edgeOut = p(EdgeOutKey)

  lazy val clientBits = edgeInSeq.map(e => e.client.clients.count(_.supports.probe)).sum

  lazy val sourceIdBits = edgeInSeq.map(e => e.client.endSourceId).max

  lazy val addressBits = edgeOut.bundle.addressBits
  lazy val tagBits = addressBits - setBits - offsetBits
}

trait DontCareInnerLogic { this: Module =>
  override def IO[T <: Data](iodef: T): T = {
    val p = chisel3.experimental.IO.apply(iodef)
    p <> DontCare
    p
  }
}

abstract class HuanCunBundle(implicit val p: Parameters) extends Bundle with HasHuanCunParameters

abstract class HuanCunModule(implicit val p: Parameters)
    extends Module
    with HasHuanCunParameters
    with DontCareInnerLogic

/**
  * We want to create hierarchy like this:
  *   <= L2-bank0 <= { L1D-bank0, L1I-bank0, PTW-bank0 }
  *   <= L2-bank1 <= { L1D-bank1, L1I-bank1, PTW-bank1 }
  * So a custom node is needed.
  */
case class L2Node(
  clientPortParameters: TLClientPortParameters,
  managerFn:            TLManagerPortParameters => TLManagerPortParameters
)(
  implicit valName: ValName)
    extends TLCustomNode {

  def nClients: Int = iPorts.map(_._2).distinct.size
  def nBanks:   Int = iPorts.size / nClients

  override def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require(iStars == 0 && iKnown > 0)
    require((oStars == 0 && oKnown == nBanks) || oKnown == 0)
    val oStar = if (oStars == 0) 0 else nBanks
    (0, oStar)
  }

  override def mapParamsD(n: Int, p: Seq[TLClientPortParameters]): Seq[TLClientPortParameters] = {
    Seq.fill(n)(clientPortParameters)
  }

  override def mapParamsU(n: Int, p: Seq[TLManagerPortParameters]): Seq[TLManagerPortParameters] = {
    Seq.fill(nClients)(p.map(managerFn)).flatten
  }
}

class HuanCun(implicit p: Parameters) extends LazyModule with HasHuanCunParameters {

  val xfer = TransferSizes(blockBytes, blockBytes)
  val atom = TransferSizes(1, cacheParams.channelBytes.d.get)
  val access = TransferSizes(1, blockBytes)

  val clientPortParams = TLMasterPortParameters.v2(
    Seq(
      TLMasterParameters.v2(
        name = cacheParams.name,
        supports = TLSlaveToMasterTransferSizes(
          probe = xfer
        ),
        sourceId = IdRange(0, cacheParams.mshrs)
      )
    ),
    channelBytes = cacheParams.channelBytes,
    minLatency = 1,
    echoFields = cacheParams.echoField,
    requestFields = cacheParams.reqField,
    responseKeys = cacheParams.respKey
  )

  val node: L2Node = L2Node(
    clientPortParams,
    managerFn = { m =>
      TLSlavePortParameters.v1(
        m.managers.map { m =>
          m.v2copy(
            regionType = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
            supports = TLMasterToSlaveTransferSizes(
              acquireB = xfer,
              acquireT = if (m.supportsAcquireT) xfer else TransferSizes.none,
              arithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
              logical = if (m.supportsAcquireT) atom else TransferSizes.none,
              get = access,
              putFull = if (m.supportsAcquireT) access else TransferSizes.none,
              putPartial = if (m.supportsAcquireT) access else TransferSizes.none,
              hint = access
            ),
            fifoId = None
          )
        },
        beatBytes = 32,
        minLatency = 2,
        responseFields = cacheParams.respField,
        requestKeys = cacheParams.reqKey,
        endSinkId = mshrsAll
      )
    }
  )

  lazy val module = new LazyModuleImp(this) {

    println(s"====== ${cacheParams.name} ======")
    node.in.grouped(node.nBanks).toList.transpose.zip(node.out).zipWithIndex.foreach {
      case ((inGroup, (out, edgeOut)), idx) => {
        println(s"slice # $idx [${inGroup.flatMap(_._2.client.clients.map(_.name)).mkString(" ")}]")
        val (newIn, newEdgesIn) = SourceIdConverter.convert(inGroup).unzip
        val inputIds = TLXbar.mapInputIds(inGroup.unzip._2.map(_.client))
        val slice = Module(new Slice(inputIds)(p.alterPartial {
          case EdgeInSeqKey => newEdgesIn
          case EdgeOutKey   => edgeOut
        }))
        slice.io.inSeq.zip(newIn).foreach { case (x, y) => x <> y }
        out <> slice.io.out
      }
    }
  }

}
