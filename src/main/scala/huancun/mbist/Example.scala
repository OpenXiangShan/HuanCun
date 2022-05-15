package huancun.mbist

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import huancun.utils.SRAMTemplate

class ModuleA(width: Int, set: Int, way: Int) extends Module {
  val io = IO(new Bundle() {
  })

  val sram_0, sram_1 = Module(new SRAMTemplate[UInt](
    UInt(width.W),
    set,
    way,
    mbist = true
  ))

  sram_0.io := DontCare
  sram_1.io := DontCare
}

class ModuleB(width: Int, set: Int, way: Int) extends Module {
  val io = IO(new Bundle() {})

  val a1, a2 = Module(new ModuleA(width, set, way))
  val ctr = Module(new MBISTController(level = 1))

}

class Example extends Module {

  /*
        Example -> b1 -> (a1, a2, ctr0)
                -> b2 -> (a1, a2, ctr1)
                -> ctr2
   */

  val b1, b2 = Module(new ModuleB(64,  1024, 4))

  val top_ctl = Module(new MBISTController(level = Int.MaxValue))

  val io = IO(new Bundle() {
    val mbist = top_ctl.io.mbist.get.cloneType
  })

  top_ctl.io.mbist.get <> io.mbist

}

object Example extends App {
  override def main(args: Array[String]): Unit = {
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new Example)
    ))
  }
}
