package huancun

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLBundle, TLChannelBeatBytes, TLClientNode, TLMasterParameters, TLMasterPortParameters, TLPermissions}
import tltest.{AddrState, ScoreboardData, TLCMasterAgent, TLCScalaB, TLCScalaD, TLCTrans, TLULMasterAgent}

import scala.collection.mutable.ArrayBuffer

abstract class BaseFakeClient(name: String, nBanks: Int, probe: Boolean = true)(implicit p: Parameters)
  extends LazyModule
    with HasHuanCunParameters {
  val xfer = TransferSizes(blockBytes)
  val node: TLClientNode = TLClientNode(Seq.tabulate(nBanks) { i =>
    TLMasterPortParameters.v2(
      masters = Seq(
        TLMasterParameters.v1(
          name = s"$name-bank$i",
          sourceId = IdRange(0, 10),
          supportsProbe = if (probe) xfer else TransferSizes(0)
        )
      ),
      channelBytes = TLChannelBeatBytes(blockBytes),
      minLatency = 1,
      echoFields = cacheParams.echoField,
      requestFields = Seq(PrefetchField(), PreferCacheField(), DirtyField()),
      responseKeys = cacheParams.respKey
    )
  })
}

class FakeClient(name: String, nBanks: Int, probe: Boolean = true, reqs: Int = 0)(implicit p: Parameters)
  extends BaseFakeClient(name, nBanks, probe) {

  class FakeClientImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {

    val finish = IO(Output(Bool()))

    val flags = Seq.fill(node.out.size) {
      RegInit(false.B)
    }

    finish := flags.reduce(_ && _)

    for (((out, edge), f) <- node.out.zip(flags)) {
      out.b.ready := true.B
      out.c.valid := false.B
      out.d.ready := true.B
      out.e.valid := false.B

      val cnt = RegInit(reqs.U)
      val addr = RegInit(0.U(edge.bundle.addressBits.W))
      when(out.a.fire) {
        cnt := cnt - 1.U
        addr := addr + blockBytes.U
      }
      val source = cnt - 1.U
      if (probe) {
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
      when(RegNext(out.d.fire && edge.last(out.d))) {
        out.e.valid := probe.B
        out.e.bits := grantAck
      }
      when(out.e.fire) {
        grantCnt := grantCnt - 1.U
      }
      f := grantCnt === 0.U
    }
  }

  lazy val module = new FakeClientImp(this)
}

class FakeL1D(nBanks: Int, reqs: Int = 0)(implicit p: Parameters) extends FakeClient("L1D", nBanks, reqs = reqs)

class FakeL1I(nBanks: Int, reqs: Int = 0)(implicit p: Parameters)
  extends FakeClient("L1I", nBanks, probe = false, reqs = reqs)

class FakePTW(nBanks: Int, reqs: Int = 0)(implicit p: Parameters)
  extends FakeClient("PTW", nBanks, probe = false, reqs = reqs)

class MasterAgent
(
  id: Int,
  name: String,
  probe: Boolean,
  serialList: ArrayBuffer[(Int, TLCTrans)],
  scoreboard: scala.collection.mutable.Map[BigInt, ScoreboardData]
)(
  implicit p: Parameters)
  extends BaseFakeClient(name, 1, probe) {
  val addrStateList = scala.collection.mutable.Map[BigInt, AddrState]()
  lazy val agent = new TLCMasterAgent(
    id,
    name,
    node.out.head._2.master.endSourceId,
    addrStateList,
    serialList,
    scoreboard,
    blockBytes,
    beatBytes
  )
  class MasterAgentImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val tl_master_io = IO(Flipped(new TLBundle(node.out.head._1.params)))
    node.out.head._1 <> tl_master_io
  }
  lazy val module = new MasterAgentImp(this)

  def peekFire[T <: Data](port: DecoupledIO[T]): Boolean = {
    port.valid.peek().litToBoolean && port.ready.peek().litToBoolean
  }

  def peekReady[T <: Data](port: DecoupledIO[T]): Boolean = port.ready.peek().litToBoolean

  def peekBigInt(sig: UInt): BigInt = sig.peek().litValue

  def peekBoolean(sig: Bool): Boolean = sig.peek().litToBoolean

  def update(io: TLBundle) = {
    // e channel
    val opt_e = agent.peekE()
    if (opt_e.isDefined) {
      val e = opt_e.get
      io.e.valid.poke(true.B)
      io.e.bits.sink.poke(e.sink.U)
    } else {
      io.e.valid.poke(false.B)
    }
    // d
    io.d.ready.poke(true.B)
    // c channel
    agent.issueC()
    val opt_c = agent.peekC()
    if (opt_c.isDefined) {
      val c = opt_c.get
      io.c.valid.poke(true.B)
      io.c.bits.opcode.poke(c.opcode.U)
      io.c.bits.param.poke(c.param.U)
      io.c.bits.size.poke(c.size.U)
      io.c.bits.source.poke(c.source.U)
      io.c.bits.address.poke(c.address.U)
      io.c.bits.data.poke(c.data.U)
    } else {
      io.c.valid.poke(false.B)
    }
    // b
    io.b.ready.poke(true.B) // TODO: random stall
    // a channel
    agent.issueA()
    val opt_a = agent.peekA()
    if (opt_a.isDefined) {
      val a = opt_a.get
      io.a.valid.poke(true.B)
      io.a.bits.opcode.poke(a.opcode.U)
      io.a.bits.param.poke(a.param.U)
      io.a.bits.size.poke(a.size.U)
      io.a.bits.source.poke(a.source.U)
      io.a.bits.address.poke(a.address.U)
      io.a.bits.mask.poke(a.mask.U)
      io.a.bits.data.poke(a.data.U)
      io.a.bits.user.lift(PrefetchKey).get.poke(true.B)
    } else {
      io.a.valid.poke(false.B)
    }

    if (peekFire(io.e)) {
      agent.fireE()
    }
    // d channel
    if (peekFire(io.d)) {
      val d = new TLCScalaD(
        opcode = peekBigInt(io.d.bits.opcode),
        param = peekBigInt(io.d.bits.param),
        size = peekBigInt(io.d.bits.size),
        source = peekBigInt(io.d.bits.source),
        sink = peekBigInt(io.d.bits.sink),
        denied = peekBoolean(io.d.bits.denied),
        data = peekBigInt(io.d.bits.data)
      )
      agent.fireD(d)
    }
    if (peekFire(io.c)) {
      agent.fireC()
    }
    // b channel
    if (peekFire(io.b)) {
      val b = new TLCScalaB(
        opcode = peekBigInt(io.b.bits.opcode),
        param = peekBigInt(io.b.bits.param),
        size = peekBigInt(io.b.bits.size),
        source = peekBigInt(io.b.bits.source),
        address = peekBigInt(io.b.bits.address),
        data = peekBigInt(io.b.bits.data),
        mask = peekBigInt(io.b.bits.mask)
      )
      agent.fireB(b)
    }
    agent.tickB()
    if (peekFire(io.a)) {
      agent.fireA()
    }
    agent.step()
  }
}

class MasterULAgent
(
  id: Int,
  name: String,
  probe: Boolean,
  serialList: ArrayBuffer[(Int, TLCTrans)],
  scoreboard: scala.collection.mutable.Map[BigInt, ScoreboardData]
)(
  implicit p: Parameters)
  extends BaseFakeClient(name, 1, probe) {
  val addrStateList = scala.collection.mutable.Map[BigInt, AddrState]()
  lazy val agent = new TLULMasterAgent(
    id,
    name,
    node.out.head._2.master.endSourceId,
    addrStateList,
    serialList,
    scoreboard,
    blockBytes,
    beatBytes
  )
  class MasterULAgent(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val tl_master_io = IO(Flipped(new TLBundle(node.out.head._1.params)))
    node.out.head._1 <> tl_master_io
  }
  lazy val module = new MasterULAgent(this)

  def peekFire[T <: Data](port: DecoupledIO[T]): Boolean = {
    port.valid.peek().litToBoolean && port.ready.peek().litToBoolean
  }

  def peekReady[T <: Data](port: DecoupledIO[T]): Boolean = port.ready.peek().litToBoolean

  def peekBigInt(sig: UInt): BigInt = sig.peek().litValue

  def peekBoolean(sig: Bool): Boolean = sig.peek().litToBoolean

  def update(io: TLBundle) = {
    // d
    io.d.ready.poke(true.B)
    // a channel
    agent.issueA()
    val opt_a = agent.peekA()
    if (opt_a.isDefined) {
      val a = opt_a.get
      io.a.valid.poke(true.B)
      io.a.bits.opcode.poke(a.opcode.U)
      io.a.bits.param.poke(a.param.U)
      io.a.bits.size.poke(a.size.U)
      io.a.bits.source.poke(a.source.U)
      io.a.bits.address.poke(a.address.U)
      io.a.bits.mask.poke(a.mask.U)
      io.a.bits.data.poke(a.data.U)
    } else {
      io.a.valid.poke(false.B)
    }
    // d channel
    if (peekFire(io.d)) {
      val d = new TLCScalaD(
        opcode = peekBigInt(io.d.bits.opcode),
        param = peekBigInt(io.d.bits.param),
        size = peekBigInt(io.d.bits.size),
        source = peekBigInt(io.d.bits.source),
        sink = peekBigInt(io.d.bits.sink),
        denied = peekBoolean(io.d.bits.denied),
        data = peekBigInt(io.d.bits.data)
      )
      agent.fireD(d)
    }
    if (peekFire(io.a)) {
      // TODO: check whether to fire-Get or fire-Put
      agent.fireAGet()
    }
    agent.step()
  }
}