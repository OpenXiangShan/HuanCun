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

    val finish = IO(Output(Bool()))

    val flags = Seq.fill(node.out.size){ RegInit(false.B) }

    finish := flags.reduce(_ && _)

    for(((out, edge), f) <- node.out.zip(flags)) {
      out.b.ready := true.B
      out.c.valid := false.B
      out.d.ready := true.B
      out.e.valid := false.B

      val cnt = RegInit(reqs.U)
      val addr = RegInit(0.U(edge.bundle.addressBits.W))
      when(out.a.fire()){
        cnt := cnt - 1.U
        addr := addr + blockBytes.U
      }
      val source = cnt - 1.U
      if(probe){
        val (_, acquire) = edge.AcquireBlock(
          source,
          addr,
          offsetBits.U,
          TLPermissions.toT
        )
        out.a.bits := acquire
        out.a.valid := cnt =/= 0.U
      } else {
        val (_, get) = edge.Get(
          source,
          addr,
          offsetBits.U
        )
        out.a.bits := get
        out.a.valid := cnt =/= 0.U
      }
      val grantCnt = RegInit(reqs.U)
      val grantAck = RegNext(edge.GrantAck(out.d.bits))
      when(RegNext(out.d.fire() && edge.last(out.d))){
        out.e.valid := probe.B
        out.e.bits := grantAck
      }
      when(out.e.fire()){
       grantCnt := grantCnt - 1.U
      }
      f := grantCnt === 0.U
    }
  }
}

class FakeL1D(nBanks: Int, reqs: Int = 0)(implicit p: Parameters)
  extends FakeClient("L1D", nBanks, reqs = reqs)
class FakeL1I(nBanks: Int, reqs: Int = 0)(implicit p: Parameters)
  extends FakeClient("L1I", nBanks, probe = false, reqs = reqs)
class FakePTW(nBanks: Int, reqs: Int = 0)(implicit p: Parameters)
  extends FakeClient("PTW", nBanks, probe = false, reqs = reqs)
