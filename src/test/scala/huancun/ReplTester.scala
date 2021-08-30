package huancun

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import freechips.rocketchip.util.ReplacementPolicy

class TestTop extends Module {
  val io = IO(new Bundle() {
    val valid = Input(Bool())
    val way = Input(UInt(log2Up(8).W))
  })

  val exp = 82.U(8.W)
  val sExp = exp.zext() - 127.S
  println(sExp.getWidth)
  println(exp.getWidth)
  printf("%d %b\n", sExp, sExp)

}

class ReplTester extends L2Tester with UseVerilatorBackend {
  it should "1" in test(new TestTop).withAnnotations(testAnnos){ c =>
    c.clock.step(5)
    def touch(way: Int) = {
      c.io.valid.poke(true.B)
      c.io.way.poke(way.U)
      c.clock.step(1)
      c.io.valid.poke(false.B)
    }
    touch(0)
    touch(1)
    touch(2)
    touch(3)
    c.clock.step(10)
    touch(0)
    touch(1)
    touch(2)
    touch(2)
    c.clock.step(10)
  }
}
