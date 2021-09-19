/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

// See LICENSE.SiFive for license details.

package huancun

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

object MetaData {
  val stateBits = 2
  def INVALID: UInt = 0.U(stateBits.W) // way is empty
  def BRANCH:  UInt = 1.U(stateBits.W) // outer slave cache is trunk
  def TRUNK:   UInt = 2.U(stateBits.W) // unique inner master cache is trunk
  def TIP:     UInt = 3.U(stateBits.W) // we are trunk, inner masters are branch

  // Does a request need trunk?
  def needT(opcode: UInt, param: UInt): Bool = {
    !opcode(2) ||
    (opcode === TLMessages.Hint && param === TLHints.PREFETCH_WRITE) ||
    ((opcode === TLMessages.AcquireBlock || opcode === TLMessages.AcquirePerm) && param =/= TLPermissions.NtoB)
  }
  // Does a request prove the client need not be probed?
  def skipProbeN(opcode: UInt): Bool = {
    // Acquire(toB) and Get => is N, so no probe
    // Acquire(*toT) => is N or B, but need T, so no probe
    // Hint => could be anything, so probe IS needed
    // Put* => is N or B, so probe IS needed
    opcode === TLMessages.AcquireBlock || opcode === TLMessages.AcquirePerm || opcode === TLMessages.Get
  }
  def isToN(param: UInt): Bool = {
    param === TLPermissions.TtoN || param === TLPermissions.BtoN || param === TLPermissions.NtoN
  }
  def isToB(param: UInt): Bool = {
    param === TLPermissions.TtoB || param === TLPermissions.BtoB
  }
  def isT(state: UInt): Bool = state(1)
  def isParamFromT(param: UInt): Bool = {
    param === TLPermissions.TtoN || param === TLPermissions.TtoB || param === TLPermissions.TtoT
  }
  def hintMiss(state: UInt, param: UInt): Bool = {
    param === TLHints.PREFETCH_WRITE && !isT(state) ||
    param === TLHints.PREFETCH_READ && state === INVALID
  }
  def growFrom(param: UInt): UInt = {
    Mux1H(
      Seq(TLPermissions.NtoB, TLPermissions.NtoT, TLPermissions.BtoT).map(param === _),
      Seq(INVALID, INVALID, BRANCH)
    )
  }
}
