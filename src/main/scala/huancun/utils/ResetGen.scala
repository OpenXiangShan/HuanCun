/***************************************************************************************
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
 ***************************************************************************************/

package huancun.utils

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.AsyncResetSynchronizerShiftReg

abstract class ResetGen extends Module {
  val o_reset = IO(Output(AsyncReset()))
}

// Async reset requires carefully synchronization of the reset deassertion.
class ResetSyncNoDFT(SYNC_NUM: Int = 2) extends ResetGen {
  val pipe_reset = RegInit(((1L << SYNC_NUM) - 1).U(SYNC_NUM.W))
  pipe_reset := Cat(pipe_reset(SYNC_NUM - 2, 0), 0.U(1.W))

  // deassertion of the reset needs to be synchronized.
  o_reset := pipe_reset(SYNC_NUM - 1).asAsyncReset
}

class DFTResetGen extends Bundle {
  val dft_mode  = Bool()
  val dft_reset = Bool()
  val scan_mode = Bool()
}

class ResetSyncDFT(SYNC_NUM: Int = 2) extends ResetGen {
  val i = IO(Input(new DFTResetGen))

  val dft_reset = Mux(i.dft_mode, i.dft_reset, reset.asBool).asAsyncReset

  withClockAndReset(clock, dft_reset) {
    val pipe_reset = RegInit(((1L << SYNC_NUM) - 1).U(SYNC_NUM.W))
    pipe_reset := Cat(pipe_reset(SYNC_NUM - 2, 0), 0.U(1.W))

    o_reset := Mux(i.scan_mode, i.dft_reset, pipe_reset(SYNC_NUM - 1)).asAsyncReset
  }
}

trait ResetNode

case class ModuleNode(mod: Module) extends ResetNode

case class ResetGenNode(children: Seq[ResetNode]) extends ResetNode

object ResetGen {
  def apply(SYNC_NUM: Int, dft: Option[DFTResetGen]): AsyncReset = {
    if (dft.isDefined) {
      val resetSync = Module(new ResetSyncDFT(SYNC_NUM))
      resetSync.i := dft.get
      resetSync.o_reset
    }
    else {
      val resetSync = Module(new ResetSyncNoDFT(SYNC_NUM))
      resetSync.o_reset
    }
  }

  def apply(clock: Clock, reset: Reset, SYNC_NUM: Int, dft: Option[DFTResetGen]): AsyncReset = {
    withClockAndReset(clock, reset) {
      apply(SYNC_NUM, dft)
    }
  }

  def apply(resetTree: ResetNode, reset: Reset, sim: Boolean, dft: Option[DFTResetGen]): Unit = {
    if(!sim) {
      resetTree match {
        case ModuleNode(mod) =>
          mod.reset := reset
        case ResetGenNode(children) =>
          val next_rst = Wire(Reset())
          withReset(reset){
            next_rst := ResetGen(2, dft)
          }
          children.foreach(child => apply(child, next_rst, sim, dft))
      }
    }
  }

  def apply(resetChain: Seq[Seq[Module]], reset: Reset, sim: Boolean, dft: Option[DFTResetGen]): Seq[Reset] = {
    apply(resetChain.map(_.map(_.reset)), reset, sim, dft)
  }

  def apply(resetChain: Seq[Seq[Reset]], reset: Reset, sim: Boolean, dft: Option[DFTResetGen], dummy: Int = 0): Seq[Reset] = {
    val resetReg = Wire(Vec(resetChain.length + 1, Reset()))
    resetReg.foreach(_ := reset)
    for ((resetLevel, i) <- resetChain.zipWithIndex) {
      if (!sim) {
        withReset(resetReg(i)) {
          resetReg(i + 1) := ResetGen(2, dft)
        }
      }
      resetLevel.foreach(_ := resetReg(i + 1))
    }
    resetReg.tail
  }
}

class PulseClockSync3 extends Module {
  val d = IO(Input(Bool()))
  val pulse = IO(Output(Bool()))

  val q = AsyncResetSynchronizerShiftReg(d, 3)

  val q_prev = RegNext(q)

  pulse := q && !q_prev
}

object PulseClockSync3 {
  def apply(d: Bool): Bool = {
    val sync3 = Module(new PulseClockSync3)
    sync3.d := d
    sync3.pulse
  }
}