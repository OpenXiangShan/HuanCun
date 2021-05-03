package HuanCun

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class HuanCun(val cache: CacheParameters)(implicit p: Parameters) extends LazyModule {

  val xfer = TransferSizes(cache.blockBytes, cache.blockBytes)
  val atom = TransferSizes(1, cache.beatBytes)
  val access = TransferSizes(1, cache.blockBytes)

  val node: TLAdapterNode = TLAdapterNode(
    clientFn = { _ =>
      TLClientPortParameters(
        Seq(
          TLClientParameters(
            name = "L2 HuanCun",
            supportsProbe = xfer
          )
        )
      )
    },
    managerFn = { m =>
      TLManagerPortParameters(
        managers = m.managers.map { m =>
          m.copy(
            regionType =
              if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED
              else m.regionType,
            supportsAcquireB = xfer,
            supportsAcquireT = if (m.supportsAcquireT) xfer else TransferSizes.none,
            supportsArithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
            supportsLogical = if (m.supportsAcquireT) atom else TransferSizes.none,
            supportsGet = access,
            supportsPutFull = if (m.supportsAcquireT) access else TransferSizes.none,
            supportsPutPartial = if (m.supportsAcquireT) access else TransferSizes.none,
            supportsHint = access,
            alwaysGrantsT = false,
            fifoId = None
          )
        },
        beatBytes = cache.beatBytes,
        minLatency = 2
      )
    }
  )

  lazy val module = new LazyModuleImp(this) {

    require(node.in.size == node.out.size)

    (node.in.zip(node.out)).foreach {
      case ((in, edgeIn), (out, edgeOut)) =>
        in.a <> out.a
        in.b <> out.b
        in.c <> out.c
        in.d <> out.d
        in.e <> out.e
    }

  }
}
