package huancun.mbist

import chisel3._
import chisel3.experimental.ChiselAnnotation
import firrtl.annotations.Annotation
import firrtl.transforms.NoDedupAnnotation

object MBIST {

  protected[mbist] var globalNodes = Seq[BaseNode]()

  sealed trait BaseNode {
    val bd: MBISTCommonBundle
    val prefix: String
    val level: Int
    val array_id: Seq[Int]
    val array_depth: Seq[Int]
  }
  sealed class RAMBaseNode
  (
    val bd: RAM2MBIST,
    val prefix: String,
    val id:Int
  ) extends BaseNode {
    override val level: Int = 0
    override val array_id = Seq(id)
    override val array_depth = Seq(0)
  }
  sealed class PipelineBaseNode
  (
    val bd: MBISTBus,
    val prefix: String,
    val level: Int,
    val array_id:Seq[Int],
    val array_depth: Seq[Int]
  ) extends BaseNode {
    var children: Seq[BaseNode] = Seq()
    var ramParamsBelongToThis: Seq[RAM2MBISTParams] = Seq()
    require(level > 0)
  }

  sealed class SRAMNode(bd: RAM2MBIST, prefix: String, id:Int)
    extends RAMBaseNode(bd, prefix, id)
  sealed class RFNode(bd: RAM2MBIST, prefix: String, id:Int)
    extends RAMBaseNode(bd, prefix, id)
  sealed class SRAMNodeRepair(bd: RAM2MBIST, prefix: String, id:Int)
    extends RAMBaseNode(bd, prefix, id)
  sealed class RFNodeRepair(bd: RAM2MBIST, prefix: String, id:Int)
    extends RAMBaseNode(bd, prefix, id)

  sealed class PipelineNodeSRAM(bd: MBISTBus, prefix: String, level: Int, array_id:Seq[Int], array_depth: Seq[Int])
    extends PipelineBaseNode(bd, prefix, level, array_id, array_depth)
  sealed class PipelineNodeRF(bd: MBISTBus, prefix: String, level: Int, array_id:Seq[Int], array_depth: Seq[Int])
    extends PipelineBaseNode(bd, prefix, level, array_id, array_depth)
  sealed class PipelineNodeSRAMRepair(bd: MBISTBus, prefix: String, level: Int, array_id:Seq[Int], array_depth: Seq[Int])
    extends PipelineBaseNode(bd, prefix, level, array_id, array_depth)
  sealed class PipelineNodeRFRepair(bd: MBISTBus, prefix: String, level: Int, array_id:Seq[Int], array_depth: Seq[Int])
    extends PipelineBaseNode(bd, prefix, level, array_id, array_depth)

  def inferMBITSBusParamsFromParams(children: Seq[MBISTBusParams]): MBISTBusParams =
    MBISTBusParams(
      children.map(_.array).max,
      children.map(_.set).max,
      children.map(_.dataWidth).max,
      children.map(_.maskWidth).max,
      sramType = children.head.sramType
    )

  def inferMBITSBusParams(children: Seq[BaseNode]): MBISTBusParams =
    MBISTBusParams(
      children.map(_.array_id).reduce(_ ++ _).max,
      children.map {
        case ram: RAMBaseNode => ram.bd.params.set
        case pl: PipelineBaseNode => pl.bd.params.set
      }.max,
      (children map {
        case ram: RAMBaseNode => ram.bd.params.dataWidth
        case pl: PipelineBaseNode => pl.bd.params.dataWidth
      }).max,
      (children map {
        case ram: RAMBaseNode => ram.bd.params.maskWidth
        case pl: PipelineBaseNode => pl.bd.params.maskWidth
      }).max,
      sramType = children.head.bd.sramType
    )

  def addRamNode(bd: RAM2MBIST, prefix: String, id:Int, isSRAM:Boolean, isRepair:Boolean): RAMBaseNode = {
    val node = (isSRAM,isRepair) match {
      case(false,false) => new RFNode (bd, prefix, id)
      case(false,true ) => new RFNodeRepair (bd, prefix, id)
      case(true,false)  => new SRAMNode (bd, prefix, id)
      case(true,true)   => new SRAMNodeRepair (bd, prefix, id)
    }
    globalNodes = globalNodes :+ node
    bd.source_elms.foreach(e => bd.elm_add_source(e, prefix))
    bd.sink_elms.foreach(e => bd.elm_add_sink(e, prefix))
    node
  }

  def isMaxLevel(level: Int) = level == Int.MaxValue

  def addController(prefix: String, level: Int, isSRAM: Boolean, isRepair:Boolean): PipelineBaseNode = {
    require(globalNodes.nonEmpty,"No nodes were created before implementing mbist controller!")
    val candidateNodes = (isSRAM,isRepair) match {
      case(false,false) => globalNodes.filter(inst => inst.isInstanceOf[RFNode] || inst.isInstanceOf[PipelineNodeRF])
      case(false,true ) => globalNodes.filter(inst => inst.isInstanceOf[RFNodeRepair] || inst.isInstanceOf[PipelineNodeRFRepair])
      case(true,false)  => globalNodes.filter(inst => inst.isInstanceOf[SRAMNode] || inst.isInstanceOf[PipelineNodeSRAM])
      case(true,true)   => globalNodes.filter(inst => inst.isInstanceOf[SRAMNodeRepair] || inst.isInstanceOf[PipelineNodeSRAMRepair])
    }
    val children = candidateNodes.filter(_.level < level)
    val remain = globalNodes.filterNot(children.contains(_))
    require(children.nonEmpty, "Mbist controller level setting is wrong or no children nodes were found!")
    val params = inferMBITSBusParams(children)
    val bd = Wire(new MBISTBus(params))
    bd := DontCare
    val ids = children.flatMap(_.array_id)
    val depth = children.flatMap(_.array_depth.map(_ + 1))
    val node = (isSRAM,isRepair) match {
      case(false,false) => new PipelineNodeRF (bd, prefix, level,ids,depth)
      case(false,true ) => new PipelineNodeRFRepair (bd, prefix, level,ids,depth)
      case(true,false)  => new PipelineNodeSRAM (bd, prefix, level,ids,depth)
      case(true,true)   => new PipelineNodeSRAMRepair (bd, prefix, level,ids,depth)
    }
    node.children = children.map {
      case ram: RAMBaseNode =>
        val childBd = Wire(ram.bd.cloneType)
        childBd := DontCare
        (isSRAM,isRepair) match {
          case(false,false) => new RFNode (childBd, ram.prefix, ram.array_id.head)
          case(false,true ) => new RFNodeRepair (childBd, ram.prefix, ram.array_id.head)
          case(true,false)  => new SRAMNode (childBd, ram.prefix, ram.array_id.head)
          case(true,true)   => new SRAMNodeRepair (childBd, ram.prefix, ram.array_id.head)
        }
      case pl: PipelineBaseNode =>
        val childBd = Wire(pl.bd.cloneType)
        childBd := DontCare
        (isSRAM,isRepair) match {
          case(false,false) => new PipelineNodeRF (childBd, pl.prefix, pl.level,pl.array_id,pl.array_depth)
          case(false,true ) => new PipelineNodeRFRepair (childBd, pl.prefix, pl.level,pl.array_id,pl.array_depth)
          case(true,false)  => new PipelineNodeSRAM (childBd, pl.prefix, pl.level,pl.array_id,pl.array_depth)
          case(true,true)   => new PipelineNodeSRAMRepair (childBd, pl.prefix, pl.level,pl.array_id,pl.array_depth)
        }
    }
    node.ramParamsBelongToThis = children.flatMap ({
      case ram: RAMBaseNode =>
        Seq(ram.bd.params)
      case pl: PipelineBaseNode =>
        pl.ramParamsBelongToThis
    })
    globalNodes = remain :+ node
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

  def checkSramChildrenExistence(level:Int):Boolean = {
    val sramChildren = globalNodes.filter(inst => inst.isInstanceOf[SRAMNode] || inst.isInstanceOf[PipelineNodeSRAM]).filter(_.level < level)
    sramChildren.nonEmpty
  }

  def checkRfChildrenExistence(level:Int):Boolean = {
    val rfChildren = globalNodes.filter(inst => inst.isInstanceOf[RFNode] || inst.isInstanceOf[PipelineNodeRF]).filter(_.level < level)
    rfChildren.nonEmpty
  }

  def checkSramRepairChildrenExistence(level:Int):Boolean = {
    val sramChildren = globalNodes.filter(inst => inst.isInstanceOf[SRAMNodeRepair] || inst.isInstanceOf[PipelineNodeSRAMRepair]).filter(_.level < level)
    sramChildren.nonEmpty
  }

  def checkRfRepairChildrenExistence(level:Int):Boolean = {
    val rfChildren = globalNodes.filter(inst => inst.isInstanceOf[RFNodeRepair] || inst.isInstanceOf[PipelineNodeRFRepair]).filter(_.level < level)
    rfChildren.nonEmpty
  }
}
