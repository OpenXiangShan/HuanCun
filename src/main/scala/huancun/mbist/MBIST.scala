package huancun.mbist

import chisel3._
import chisel3.experimental.ChiselAnnotation
import firrtl.annotations.Annotation
import firrtl.transforms.NoDedupAnnotation

object MBIST {

  protected [mbist] var tempNodes = Seq[BaseNode]()

  sealed trait BaseNode {
    val bd: MBISTBundleLike
    val prefix: String
    val level: Int
  }
  sealed class SRAMNode(val bd: SRAM2MBIST, val prefix: String) extends BaseNode {
    override val level: Int = 0
  }
  sealed class ControllerNode(val bd: MBISTBus, val prefix: String, val level: Int) extends BaseNode {
    var children: Seq[BaseNode] = Seq()
    require(level > 0)
  }

  def inferMBITSBusParams(children: Seq[BaseNode]): MBISTBusParams = MBISTBusParams(
    children.size,
    children.map {
      case sram: SRAMNode => sram.bd.params.set
      case ctr: ControllerNode => ctr.bd.params.set
    }.max,
    (children map {
      case sram: SRAMNode => sram.bd.params.dataWidth
      case ctr: ControllerNode => ctr.bd.params.dataWidth
    }).max
  )

  def addSRAM(bd: SRAM2MBIST, prefix: String): SRAMNode = {
    val node = new SRAMNode(bd, prefix)
    tempNodes = tempNodes :+ node
    bd.source_elms.foreach(e => bd.elm_add_source(e, prefix))
    bd.sink_elms.foreach(e => bd.elm_add_sink(e, prefix))
    node
  }

  def isMaxLevel(level: Int) = level == Int.MaxValue

  def addController(prefix: String, level: Int): ControllerNode = {
    require(tempNodes.nonEmpty)
    require(tempNodes.map(_.level).max <= level)
    val children = tempNodes.filter(_.level < level)
    val params = inferMBITSBusParams(children)
    val bd = Wire(new MBISTBus(params))
    bd := DontCare
    val node = new ControllerNode(bd, prefix, level)
    node.children = children.map {
      case sram: SRAMNode =>
        val childBd = Wire(sram.bd.cloneType)
        childBd := DontCare
        new SRAMNode(childBd, sram.prefix)
      case ctr: ControllerNode =>
        val childBd = Wire(ctr.bd.cloneType)
        childBd := DontCare
        new ControllerNode(childBd, ctr.prefix, ctr.level)
    }
    tempNodes = tempNodes.filter(_.level == node.level) :+ node
    for(c <- node.children){
      c.bd.source_elms.foreach(src => c.bd.elm_add_sink(src, c.prefix))
      c.bd.sink_elms.foreach(sink => c.bd.elm_add_source(sink, c.prefix))
    }
    if(!isMaxLevel(level)){
      bd.source_elms.foreach(e => bd.elm_add_source(e, prefix))
      bd.sink_elms.foreach(e => bd.elm_add_sink(e, prefix))
    }
    node
  }

  def noDedup(m: Module): Unit = {
    chisel3.experimental.annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = NoDedupAnnotation(m.toTarget)
    })
  }
}
