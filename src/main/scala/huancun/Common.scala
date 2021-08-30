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

abstract class InnerTask(implicit p: Parameters) extends HuanCunBundle {
  val sourceId = UInt(sourceIdBits.W)
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
}

class SinkAReq(implicit p: Parameters) extends InnerTask {
  val size = UInt(msgSizeBits.W)
  val off = UInt(offsetBits.W)
}
class SourceBReq(implicit p: Parameters) extends HuanCunBundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val param = UInt(3.W)
  val clients = UInt(clientBits.W)
}
class SinkCReq(implicit p: Parameters) extends InnerTask {
  val size = UInt(msgSizeBits.W)
  val way = UInt(wayBits.W)
  val off = UInt(offsetBits.W)
  val bufIdx = UInt(bufIdxBits.W)
}
class SourceDReq(implicit p: Parameters) extends InnerTask with HasChannelBits {
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val size = UInt(msgSizeBits.W)
  val way = UInt(wayBits.W)
  val off = UInt(offsetBits.W)
  val denied = Bool()
  val sinkId = UInt(mshrBits.W)
}

class SourceAReq(implicit p: Parameters) extends HuanCunBundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(mshrBits.W)
  val needData = Bool()
}
class SourceCReq(implicit p: Parameters) extends HuanCunBundle {
  val opcode = UInt(3.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val param = UInt(3.W)
  val source = UInt(mshrBits.W)
  val way = UInt(wayBits.W)
  val dirty = Bool()
}
class SourceEReq(implicit p: Parameters) extends HuanCunBundle {
  val sink = UInt(outerSinkBits.W)
}

class SinkCResp(implicit p: Parameters) extends HuanCunBundle {
  // ProbeAck
  val hasData = Bool() // opcode(0)
  val param = UInt(3.W)
  val source = UInt(sourceIdBits.W)
  val last = Bool()
  val set = UInt(setBits.W) // The target address of the transfer, but only set is enough
}
class SinkDResp(implicit p: Parameters) extends HuanCunBundle {
  // Grant / AccessAck / ReleaseAck
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(mshrBits.W) // The master source id receiving the resp
  val sink = UInt(outerSinkBits.W)
  val last = Bool() // last beat
  val denied = Bool()
}
class SinkEResp(implicit p: Parameters) extends HuanCunBundle {
  // GrantAck
  val sink = UInt(mshrBits.W) // The slave sink id accepting this resp
}

trait HasChannelBits { this: Bundle =>
  val channel = UInt(3.W)
  def fromA = channel(0).asBool
  def fromB = channel(1).asBool
  def fromC = channel(2).asBool
}

class MSHRRequest(implicit p: Parameters) extends HuanCunBundle with HasChannelBits {
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val size = UInt(msgSizeBits.W)
  val source = UInt(sourceIdBits.W) // 0 when this req is a Hint
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val off = UInt(offsetBits.W)
  val bufIdx = UInt(bufIdxBits.W)
  val needHint = Bool()
}

class MSHRStatus(implicit p: Parameters) extends HuanCunBundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val way = UInt(wayBits.W)
  val reload = Bool()
  val blockB, blockC = Bool()
  val nestB, nestC = Bool()
}

class DSAddress(implicit p: Parameters) extends HuanCunBundle {
  val way = UInt(width = wayBits.W)
  val set = UInt(width = setBits.W)
  val beat = UInt(width = beatBits.W)
  val write = Bool()
  val noop = Bool()
}

class DSData(implicit p: Parameters) extends HuanCunBundle {
  val data = UInt((beatBytes * 8).W)
}

class SourceDHazard(implicit p: Parameters) extends HuanCunBundle {
  val way = UInt(width = wayBits.W)
  val set = UInt(width = setBits.W)

  def safe(s: UInt, w: UInt): Bool = {
    set === s && way === w
  }
}
