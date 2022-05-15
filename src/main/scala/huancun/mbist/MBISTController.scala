package huancun.mbist

import chisel3._
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import firrtl.annotations.Annotation
import firrtl.transforms.NoDedupAnnotation
import huancun.mbist.MBISTController.uniqueId

object MBISTController {
  private var uniqueId = 0
}

class MBISTController(level: Int) extends Module {

  uniqueId += 1

  val prefix = "MBISTArray_" + uniqueId + "_"

  println("=====")
  println(prefix)
  val node = MBIST.addController(prefix, level)
  for(c <- node.children){
    println(c.prefix)
  }
  println("*****")
  val bd = node.bd

  val io = IO(new Bundle() {
    val mbist = if(MBIST.isMaxLevel(level)) Some(new MBISTBus(bd.params)) else None
  })

  io.mbist.foreach(m => {
    dontTouch(m)
    m := DontCare
  })

  MBIST.noDedup(this)
}
