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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{BundleField, BundleFieldBase, UIntToOH1}
import huancun.prefetch._
import huancun.utils.{FastArbiter, ParallelORR, Pipeline, ResetGen}

trait HasHuanCunLvnaParameters {
  val p: Parameters
  val cacheParams = p(HCCacheParamsKey)
  val hasDsid = cacheParams.LvnaEnable
  val dsidWidth = cacheParams.dsidWidth
  val hasLvnaCtrl = cacheParams.LvnaCtrlEnable
}

abstract class HuanCunLvnaBundle(implicit val p: Parameters) extends Bundle with HasHuanCunLvnaParameters

class WaymaskSetReq(implicit p: Parameters) extends HuanCunLvnaBundle
{
  val dsid = UInt(dsidWidth.W)
  val waymask = UInt(cacheParams.ways.W)
}

// from HuanCun point of view
class LvnaCtrlIO(implicit p: Parameters) extends HuanCunLvnaBundle
{
  val waymaskSetReq = Flipped(DecoupledIO(new WaymaskSetReq))
}

// from HuanCun point of view
class LvnaSliceReq(implicit p: Parameters) extends HuanCunLvnaBundle
{
  val broadcastWaymask = DecoupledIO(new WaymaskSetReq)
}

class HuancunLvnaCtrl(numSlices: Int)(implicit val p: Parameters) extends Module with HasHuanCunLvnaParameters {
  val io = IO(new Bundle {
    val fromCP = new LvnaCtrlIO()
    val toSlicesReq = Vec(numSlices, new LvnaSliceReq)
  })

  val waymasks = RegInit(VecInit(Seq.fill(1 << dsidWidth) {
    ((1L << cacheParams.ways) - 1).U
  }))
  when (io.fromCP.waymaskSetReq.fire){
    waymasks(io.fromCP.waymaskSetReq.bits.dsid) := io.fromCP.waymaskSetReq.bits.waymask
  }

  val waymaskSetQueue = Module(new Queue(new WaymaskSetReq,2, flow = true))
  waymaskSetQueue.io.enq <> io.fromCP.waymaskSetReq

  val broadcastTaskReg = RegInit(VecInit(Seq.fill(numSlices)(false.B)))
  val taskIdle = !Cat(broadcastTaskReg).orR
  waymaskSetQueue.io.deq.ready := taskIdle
  val taskReq = RegEnable(waymaskSetQueue.io.deq.bits, waymaskSetQueue.io.deq.fire)
  when (waymaskSetQueue.io.deq.fire){
    broadcastTaskReg.foreach(_ := true.B)
  }
  broadcastTaskReg zip io.toSlicesReq foreach{
    case (task, slicereq) =>
      slicereq.broadcastWaymask.valid := task
      slicereq.broadcastWaymask.bits := taskReq
      when (slicereq.broadcastWaymask.fire){
        task := false.B
      }
  }
}