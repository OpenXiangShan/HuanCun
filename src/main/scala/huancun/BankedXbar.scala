package huancun

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleField

case class BankedXbarNode
(
  banks: Int,
  clientFn: Seq[TLMasterPortParameters] => TLMasterPortParameters,
  managerFn: Seq[TLSlavePortParameters] => TLSlavePortParameters
)(implicit valName: ValName) extends TLCustomNode {

  override def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
//    println(iKnown, oKnown, iStars, oStars)
    (banks, banks)
  }

  override def mapParamsD(n: Int, p: Seq[TLClientPortParameters]): Seq[TLClientPortParameters] = {
//    println("mapD", n, p.size)
    require(p.size >= banks, "client banks should >= manager banks")
    val params = p.grouped(banks).toList.transpose.map(clientFn)
    val res = Seq.fill(n / banks){params}.flatten
    //println(res.mkString("\n"))
    res
  }

  override def mapParamsU(n: Int, p: Seq[TLManagerPortParameters]): Seq[TLManagerPortParameters] = {
//    println("mapU", n, p.size)
    val masters = n / banks
    val params = p.grouped(banks).toList.transpose.map(managerFn)
    val res = Seq.fill(masters){params}.flatten
    //println(res.mkString("\n"))
    res
  }

  def inGroups = in.grouped(banks).toList.transpose
  def outGroups = out.grouped(banks).toList.transpose

}

class BankedXbar(nBanks: Int, policy: TLArbiter.Policy)(implicit p: Parameters) extends LazyModule {
  val node = BankedXbarNode(
    nBanks,
    clientFn = { seq =>
      seq.head.v1copy(
        echoFields = BundleField.union(seq.flatMap(_.echoFields)),
        requestFields = BundleField.union(seq.flatMap(_.requestFields)),
        responseKeys = seq.flatMap(_.responseKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client =>
            client.v1copy(
              sourceId = client.sourceId.shift(range.start)
            )
          }
        }
      )
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      seq.head.v1copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require(port.beatBytes == seq.head.beatBytes,
            s"Xbar ($name with parent $parent) data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager =>
            manager.v1copy(
              fifoId = manager.fifoId.map(fifoIdMapper(_))
            )
          }
        }
      )
    })

  lazy val module = new LazyModuleImp(this) {

    val inOutGroups = node.inGroups.zip(node.outGroups)

    for((in, out) <- inOutGroups){
      val circuit = Module(new XbarCircuit(policy, in.map(_._2), out.map(_._2)))
      circuit.io.in.zip(in.map(_._1)).foreach(x => x._1 <> x._2)
      circuit.io.out.zip(out.map(_._1)).foreach(x => x._2 <> x._1)
    }
  }
}


object BankedXbar {
  def apply(nBanks: Int, policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) = {
    val bankedXbar = LazyModule(new BankedXbar(nBanks, policy))
    bankedXbar.node
  }
}

// wrap TLXbar.circuit into a module
class XbarCircuit
(
  policy: TLArbiter.Policy,
  edgeIn: Seq[TLEdge],
  edgeOut: Seq[TLEdge]
) extends Module {

  val io = IO(new Bundle{
    val in = MixedVec(edgeIn.map(e => Flipped(TLBundle(e.bundle))))
    val out = MixedVec(edgeOut.map(e => TLBundle(e.bundle)))
  })

  val inSeq = io.in.zip(edgeIn).toSeq
  val outSeq = io.out.zip(edgeOut).toSeq

  TLXbar.circuit(policy, inSeq, outSeq)

}
