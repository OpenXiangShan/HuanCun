package HuanCun

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleField

trait HasHuanCunParameters {
  val p: Parameters
  val cacheParams = p(CacheParamsKey)
  val blockBytes = cacheParams.blockBytes

  val mshrs = cacheParams.mshrs
  val blocks = cacheParams.ways * cacheParams.sets
  val sizeBytes = blocks * blockBytes
}

abstract class HuanCunModule(implicit p: Parameters) extends Module with HasHuanCunParameters

class HuanCun(implicit p: Parameters) extends LazyModule with HasHuanCunParameters {

  val xfer = TransferSizes(blockBytes, blockBytes)
  val atom = TransferSizes(1, cacheParams.channelBytes.d.get)
  val access = TransferSizes(1, blockBytes)

  val node: TLAdapterNode = TLAdapterNode(
    clientFn = { _ =>
      TLMasterPortParameters.v2(
        Seq(
          TLMasterParameters.v2(
            name = cacheParams.name,
            supports = TLSlaveToMasterTransferSizes(
              probe = xfer
            ),
            sourceId = IdRange(0, cacheParams.mshrs),
          )
        ),
        channelBytes = cacheParams.channelBytes,
        minLatency = 1,
        echoFields = cacheParams.echoField,
        requestFields = cacheParams.reqField,
        responseKeys = cacheParams.respKey
      )
    },
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
        endSinkId = cacheParams.mshrs + 2
      )
    }
  )

  lazy val module = new LazyModuleImp(this) {

    require(node.in.size == node.out.size)

    node.in.zip(node.out).foreach {
      case ((in, edgeIn), (out, edgeOut)) =>
        out <> in
        val header = s"======== HuanCun: ${cacheParams.name} ========"
        println(header)
        println("clients: " + edgeIn.client.clients.map(_.name).mkString(" "))
        println("managers: " + edgeOut.manager.managers.map(_.name).mkString(" "))
        println(header.map(_ => "=").mkString(""))
    }

  }
}
