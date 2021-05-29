package huancun

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLChannelBeatBytes, TLClientNode, TLMasterParameters, TLMasterPortParameters, TLPermissions}

class FakeClient(name: String, nBanks: Int, probe: Boolean = true, reqs: Int = 0)(implicit p: Parameters)
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

  lazy val module = new LazyModuleImp(this) {
    for((out, edge) <- node.out) {
      val cnt = RegInit(reqs.U)
      if(probe){
        val (_, acquire) = edge.AcquireBlock(
          0.U,
          edge.manager.managers.head.address.head.base.U,
          offsetBits.U,
          TLPermissions.toT
        )
        out.a.bits := acquire
        out.a.valid := cnt =/= 0.U
        when(out.a.fire()){ cnt := cnt - 1.U }
      } else {
        val (_, get) = edge.Get(
          0.U,
          edge.manager.managers.head.address.head.base.U,
          offsetBits.U
        )
        out.a.bits := get
        out.a.valid := cnt =/= 0.U
        when(out.a.fire()){ cnt := cnt - 1.U }
      }
    }
  }
}

class FakeL1D(nBanks: Int, reqs: Int = 0)(implicit p: Parameters)
  extends FakeClient("L1D", nBanks, reqs = reqs)
class FakeL1I(nBanks: Int, reqs: Int = 0)(implicit p: Parameters)
  extends FakeClient("L1I", nBanks, probe = false, reqs = reqs)
class FakePTW(nBanks: Int, reqs: Int = 0)(implicit p: Parameters)
  extends FakeClient("PTW", nBanks, probe = false, reqs = reqs)
