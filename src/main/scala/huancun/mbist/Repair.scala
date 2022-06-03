package huancun.mbist
import chisel3._
import chisel3.experimental.IO
import chisel3.util.experimental.BoringUtils
class RepairBundle extends Bundle{
  val rowRepair = Input(UInt(26.W))
  val colRepair = Input(UInt(14.W))
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
    sink_elms.foreach(sigName => BoringUtils.addSink(node.bd.elements(sigName), node.prefix + sigName))
  }
}
