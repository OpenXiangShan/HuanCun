package huancun.mbist
import chisel3._
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import firrtl.annotations.Annotation
import firrtl.transforms.NoDedupAnnotation

class STD_CLKGT_func extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E  = Input(Bool())
    val CK = Input(Clock())
    val Q  = Output(Clock())
  })

  addResource("/STD_CLKGT_func.v")
}

class MBISTClockGateCell extends Module{
  val mbist = IO(new Bundle{
    val writeen = Input(Bool())
    val readen = Input(Bool())
    val req = Input(Bool())
  })
  val fscan_clkungate = IO(Input(Bool()))
  val out_clock = IO(Output(Clock()))

  val cg_en_reg = RegInit(false.B)
  cg_en_reg := !cg_en_reg

  val E = Mux(mbist.req, mbist.readen | mbist.writeen, cg_en_reg)
  val TE = fscan_clkungate

  val CG = Module(new STD_CLKGT_func)
  CG.io.E := E
  CG.io.TE := TE
  CG.io.CK := clock
  out_clock := CG.io.Q
}

