package huancun

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLChannelBeatBytes, TLClientNode, TLMasterParameters, TLMasterPortParameters}

class FakeClient(name: String, nBanks: Int, probe: Boolean = true)(implicit p: Parameters)
    extends LazyModule
    with HasHuanCunParameters {

  val xfer = TransferSizes(blockBytes)

  val node: TLClientNode = TLClientNode(Seq.tabulate(nBanks) { i =>
    TLMasterPortParameters.v2(
      masters = Seq(
        TLMasterParameters.v1(
          name = s"$name-bank$i",
          sourceId = IdRange(0, 10),
          supportsProbe = if(probe) xfer else TransferSizes(0),
        )
      ),
      channelBytes = TLChannelBeatBytes(blockBytes),
      minLatency = 1,
      echoFields = cacheParams.echoField,
      requestFields = cacheParams.reqField,
      responseKeys = cacheParams.respKey
    )
  })

  lazy val module = new LazyModuleImp(this) {}
}

class FakeL1D(nBanks: Int)(implicit p: Parameters) extends FakeClient("L1D", nBanks)
class FakeL1I(nBanks: Int)(implicit p: Parameters) extends FakeClient("L1I", nBanks, probe = false)
class FakePTW(nBanks: Int)(implicit p: Parameters) extends FakeClient("PTW", nBanks, probe = false)
