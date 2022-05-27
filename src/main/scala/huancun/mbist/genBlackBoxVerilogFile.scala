package huancun.mbist

import chisel3._
import chisel3.util._

import scala.collection.mutable

object genBlackBoxVerilogFile{
  def getIOVerilogInfo(inVec: Vec[Data]): Seq[(String, Int, Int)] = {
    inVec.zipWithIndex.flatMap({
      case (data: Bundle, idx: Int) => getIOVerilogInfo(data).map(t => (f"${idx}_" + t._1, t._2, t._3))
      case (data: Vec[Data], idx: Int) => getIOVerilogInfo(data).map(t => (f"${idx}_" + t._1, t._2, t._3))
      case (data: Data, idx: Int) => Seq((f"${idx}", data.getWidth, chisel3Hacks.getDataDirection(data, false)))
    })
  }
  def getIOVerilogInfo(inBundle: Bundle): Seq[(String, Int, Int)] = {
    val isFlipped: Boolean = chisel3Hacks.getDataDirection(inBundle, false) == 1
    inBundle.elements.toList.reverse.flatMap({
      case (name: String, data: Bundle) => getIOVerilogInfo(data).map(t => (name + "_" + t._1, t._2, t._3))
      case (name: String, data: Vec[Data]) => getIOVerilogInfo(data).map(t => (name + "_" + t._1, t._2, t._3))
      case (name: String, data: MixedVec[Data]) => getIOVerilogInfo(data).map(t => (name + "_" + t._1, t._2, t._3))
      case (name: String, data: Data) => Seq((name, data.getWidth, chisel3Hacks.getDataDirection(data, isFlipped)))
    })
  }
  def getIOVerilogInfo(inMixedVec: MixedVec[Data]): Seq[(String, Int, Int)] = {
    inMixedVec.zipWithIndex.flatMap({
      case (data: Bundle, idx: Int) => getIOVerilogInfo(data).map(t => (f"${idx}_" + t._1, t._2, t._3))
      case (data: Vec[Data], idx: Int) => getIOVerilogInfo(data).map(t => (f"${idx}_" + t._1, t._2, t._3))
      case (data: MixedVec[Data], idx: Int) => getIOVerilogInfo(data).map(t => (f"${idx}_" + t._1, t._2, t._3))
      case (data: Data, idx: Int) => Seq((f"${idx}", data.getWidth, chisel3Hacks.getDataDirection(data, false)))
    })
  }
}

trait genBlackBoxVerilogFile{
  this:BlackBox with HasBlackBoxInline =>

  def getBasicInfo(targetIO: Bundle, sim: Boolean): (String, String, String, String) = {

    val ioInfo = genBlackBoxVerilogFile.getIOVerilogInfo(targetIO).map({
      case(name:String, width:Int,dir:Int) =>
        val direction = dir match{
          case 3 => "input  logic"
          case 2 => "output logic"
          case _ => "unknown"
        }
        val widthRange = if(width >1) f"[${width - 1}:0]" else ""
        "  %-16s%-12s%s,\n".format(direction,widthRange,name)
    })
    val finalIOInfo = ioInfo.reduce(_+_).dropRight(2) + "\n);\n"
    val modPrefix = if (sim) "" else "bosc_"
    val verilogHead = s"module $modPrefix" + desiredName + "(\n"
    val verilogTail = "\nendmodule"
    val verilogFile = s"$modPrefix" + desiredName + ".sv"
    (finalIOInfo, verilogHead, verilogTail, verilogFile)
  }

  def genV(targetIO: Bundle, sim: Boolean): Unit = {
    val (finalIOInfo, verilogHead, verilogTail, verilogFile) = getBasicInfo(targetIO, sim)
    setInline(verilogFile, verilogHead + finalIOInfo + verilogTail)
  }
  def genV(targetIO: Bundle, params: mutable.HashMap[String, String], sim: Boolean):Unit = {
    val (finalIOInfo, verilogHead, verilogTail, verilogFile) = getBasicInfo(targetIO, sim)
    val paramsInfo = params.toList.map(t => "  localparam %-24s = %s;\n".format(t._1,t._2)).reduce(_+_)
    setInline(verilogFile,verilogHead + finalIOInfo + paramsInfo + verilogTail)
  }
}