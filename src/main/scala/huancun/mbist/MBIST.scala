/** *************************************************************************************
  * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2022 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

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
    val ids:Seq[Int]
  ) extends BaseNode {
    override val level: Int = 0
    override val array_id = ids
    override val array_depth = Seq.fill(ids.length)(0)
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

  sealed class SRAMNode(bd: RAM2MBIST, prefix: String, ids:Seq[Int])
    extends RAMBaseNode(bd, prefix, ids)

  sealed class PipelineNodeSRAM(bd: MBISTBus, prefix: String, level: Int, array_id:Seq[Int], array_depth: Seq[Int])
    extends PipelineBaseNode(bd, prefix, level, array_id, array_depth)

  def inferMBITSBusParamsFromParams(children: Seq[MBISTBusParams]): MBISTBusParams =
    MBISTBusParams(
      children.map(_.array).max,
      children.map(_.set).max,
      children.map(_.dataWidth).max,
      children.map(_.maskWidth).max
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
    )

  def addRamNode(bd: RAM2MBIST, prefix: String, ids:Seq[Int]): RAMBaseNode = {
    val node = new SRAMNode (bd, prefix, ids)
    globalNodes = globalNodes :+ node
    bd.source_elms.foreach(e => bd.elm_add_source(e, prefix))
    bd.sink_elms.foreach(e => bd.elm_add_sink(e, prefix))
    node
  }

  def isMaxLevel(level: Int) = level == Int.MaxValue

  def addController(prefix: String, level: Int): PipelineBaseNode = {
    require(globalNodes.nonEmpty,"No nodes were created before implementing mbist controller!")
    val candidateNodes = globalNodes.filter(inst => inst.isInstanceOf[SRAMNode] || inst.isInstanceOf[PipelineNodeSRAM])
    val children = candidateNodes.filter(_.level < level)
    val remain = globalNodes.filterNot(children.contains(_))
    require(children.nonEmpty, "Mbist controller level setting is wrong or no children nodes were found!")
    val params = inferMBITSBusParams(children)
    val bd = Wire(new MBISTBus(params))
    bd := DontCare
    val ids = children.flatMap(_.array_id)
    val depth = children.flatMap(_.array_depth.map(_ + 1))
    val node = new PipelineNodeSRAM (bd, prefix, level,ids,depth)

    node.children = children.map {
      case ram: RAMBaseNode =>
        val childBd = Wire(ram.bd.cloneType)
        childBd := DontCare
        new SRAMNode (childBd, ram.prefix, ram.array_id)
      case pl: PipelineBaseNode =>
        val childBd = Wire(pl.bd.cloneType)
        childBd := DontCare
        new PipelineNodeSRAM (childBd, pl.prefix, pl.level,pl.array_id,pl.array_depth)
    }
    node.ramParamsBelongToThis = children.flatMap ({
      case ram: RAMBaseNode =>
        ram.bd.params.getAllNodesParams()
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

  def noDedup(m: RawModule): Unit = {
    chisel3.experimental.annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = NoDedupAnnotation(m.toTarget)
    })
  }

  def checkSramChildrenExistence(level:Int):Boolean = {
    val sramChildren = globalNodes.filter(inst => inst.isInstanceOf[SRAMNode] || inst.isInstanceOf[PipelineNodeSRAM]).filter(_.level < level)
    sramChildren.nonEmpty
  }
}
