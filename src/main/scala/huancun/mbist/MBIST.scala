package huancun.mbist

import chisel3._
import chisel3.experimental.ChiselAnnotation
import firrtl.annotations.Annotation
import firrtl.transforms.NoDedupAnnotation

object MBIST {

  protected [mbist] var tempNodes = Seq[BaseNode]()

  sealed trait BaseNode {
    val bd: MBISTCommonBundle
    val prefix: String
    val level: Int
    val array_id: Seq[Int]
    val array_depth: Seq[Int]
  }
  sealed class SRAMNode(val bd: SRAM2MBIST, val prefix: String, val id:Int) extends BaseNode {
    override val level: Int = 0
    override val array_id = Seq(id)
    override val array_depth = Seq(0)
  }
  sealed class PipelineNode(val bd: MBISTBus, val prefix: String, val level: Int, val array_id:Seq[Int], val array_depth: Seq[Int]) extends BaseNode {
    var children: Seq[BaseNode] = Seq()
    var sramParamsBelongToThis: Seq[SRAM2MBISTParams] = Seq()
    require(level > 0)
  }

  def inferMBITSBusParams(children: Seq[BaseNode]): MBISTBusParams =
    MBISTBusParams(
      children.map(_.array_id).reduce(_ ++ _).max,
      children.map {
        case sram: SRAMNode => sram.bd.params.set
        case ctr: PipelineNode => ctr.bd.params.set
      }.max,
      (children map {
        case sram: SRAMNode => sram.bd.params.dataWidth
        case ctr: PipelineNode => ctr.bd.params.dataWidth
      }).max,
      (children map {
        case sram: SRAMNode => sram.bd.params.maskWidth
        case ctr: PipelineNode => ctr.bd.params.maskWidth
      }).max
    )



  def addSRAM(bd: SRAM2MBIST, prefix: String, id:Int): SRAMNode = {
    val node = new SRAMNode(bd, prefix, id)
    tempNodes = tempNodes :+ node
    bd.source_elms.foreach(e => bd.elm_add_source(e, prefix))
    bd.sink_elms.foreach(e => bd.elm_add_sink(e, prefix))
    node
  }

  def isMaxLevel(level: Int) = level == Int.MaxValue

  def addController(prefix: String, level: Int): PipelineNode = {
    require(tempNodes.nonEmpty,"No nodes were created before implementing mbist controller!")
    val children = tempNodes.filter(_.level < level)
    val remain = tempNodes.filterNot(_.level < level)
    require(children.nonEmpty, "Mbist controller level setting is wrong or no children nodes were found!")
    val params = inferMBITSBusParams(children)
    val bd = Wire(new MBISTBus(params))
    bd := DontCare
    val ids = children.flatMap(_.array_id)
    val depth = children.flatMap(_.array_depth.map(_ + 1))
    val node = new PipelineNode(bd, prefix, level,ids,depth)
    node.children = children.map {
      case sram: SRAMNode =>
        val childBd = Wire(sram.bd.cloneType)
        childBd := DontCare
        new SRAMNode(childBd, sram.prefix,sram.array_id.head)
      case ctr: PipelineNode =>
        val childBd = Wire(ctr.bd.cloneType)
        childBd := DontCare
        new PipelineNode(childBd, ctr.prefix, ctr.level,ctr.array_id,ctr.array_depth)
    }
    node.sramParamsBelongToThis = children.flatMap ({
      case sram: SRAMNode =>
        Seq(sram.bd.params)
      case ctr: PipelineNode =>
        ctr.sramParamsBelongToThis
    })
    tempNodes = remain :+ node
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
