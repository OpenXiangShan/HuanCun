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
import chisel3.experimental.IO

class RepairBundle extends Bundle{
  val rowRepair = Input(UInt(26.W))
  val colRepair = Input(UInt(13.W))
}

class RepairNode(val bd: RepairBundle, val prefix:String){
  val sink_elms: Seq[String] = Seq("rowRepair", "colRepair")
}

object Repair {
  var globalRepairNode = Seq[RepairNode]()

  def addRepairNodeToGlobal(bd:RepairBundle,repairName:String):Unit = {
    val node = new RepairNode(bd,repairName)
    globalRepairNode = globalRepairNode :+ node
    val sink_elms = node.sink_elms
    sink_elms.foreach(sigName => WiringUtils.addSink(node.bd.elements(sigName), node.prefix + sigName))
  }
}
