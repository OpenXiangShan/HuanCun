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
import freechips.rocketchip.util.{BundleMap, UIntToOH1}

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
  val alias = aliasBitsOpt.map(w => Vec(clientBits, UInt(w.W)))
  val needData = if (cacheParams.inclusive) None else Some(Bool())
}
class SinkCReq(implicit p: Parameters) extends InnerTask {
  val size = UInt(msgSizeBits.W)
  val way = UInt(wayBits.W)
  val off = UInt(offsetBits.W)
  val bufIdx = UInt(bufIdxBits.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(mshrBits.W)
  val save = Bool() // write into banked store
  val drop = Bool() // clear write buf (without writing banked store)
  val release = Bool() // send buffer contents to SourceC
  val dirty = Bool() // useful only when release = true
}
class SourceDReq(implicit p: Parameters) extends InnerTask with HasChannelBits {
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val size = UInt(msgSizeBits.W)
  val way = UInt(wayBits.W)
  val off = UInt(offsetBits.W)
  val useBypass = Bool()
  val bufIdx = UInt(bufIdxBits.W)
  val denied = Bool()
  val sinkId = UInt(mshrBits.W)
  val bypassPut = Bool()
  val dirty = Bool()
}

class SourceAReq(implicit p: Parameters) extends HuanCunBundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val off = UInt(offsetBits.W)
  val mask = UInt(beatBytes.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(mshrBits.W)
  val bufIdx = UInt(bufIdxBits.W)
  val size = UInt(msgSizeBits.W)
  val needData = Bool()
  val putData = Bool()
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
  val bufIdx = UInt(bufIdxBits.W)
}
class SinkDResp(implicit p: Parameters) extends HuanCunBundle {
  // Grant / AccessAck / ReleaseAck
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(mshrBits.W) // The master source id receiving the resp
  val sink = UInt(outerSinkBits.W)
  val last = Bool() // last beat
  val denied = Bool()
  val dirty = Bool()
  val bufIdx = UInt(bufIdxBits.W)
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
  val mask = UInt(beatBytes.W)
  val bufIdx = UInt(bufIdxBits.W)
  val needHint = prefetchOpt.map(_ => Bool())
  val isPrefetch = prefetchOpt.map(_ => Bool())
  val isBop = prefetchOpt.map(_ => Bool())
  val alias = aliasBitsOpt.map(_ => UInt(aliasBitsOpt.get.W))
  val preferCache = Bool()
  val dirty = Bool()
  val fromProbeHelper = Bool()
  val fromCmoHelper = Bool()
  val needProbeAckData = if (cacheParams.inclusive) None else Some(Bool())
}

class MSHRStatus(implicit p: Parameters) extends HuanCunBundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val way = UInt(wayBits.W)
  val way_reg = UInt(wayBits.W)
  val reload = Bool()
  val blockB, blockC = Bool()
  val nestB, nestC = Bool()
  /**
    *   for missed acquire, if 'will_grant_data' is true:
    *     sinkD must write refill buffer
    *     soruceD must read data from refill buffer
    */
  val will_grant_data = Bool()
  /**
    *   for missed acquire/get, if 'will_save_data' is true:
    *     sinkD must write bankedstore to keep it in cache
    *   else:
    *     sinkD just write to refill buffer,
    *     and the data will be bypassed to inner cache
    */
  val will_save_data = Bool()
  // the mshr will be free at next cycle
  val will_free = Bool()
  // for debug usage now
  val is_prefetch = Bool()
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
  val corrupt = Bool()
}

class SourceDHazard(implicit p: Parameters) extends HuanCunBundle {
  val way = UInt(width = wayBits.W)
  val set = UInt(width = setBits.W)

  def safe(s: UInt, w: UInt): Bool = {
    set === s && way === w
  }
}

class ReplacerInfo() extends Bundle {
  val channel = UInt(3.W)
  val opcode = UInt(3.W)
}

class PutBufferPop(implicit p: Parameters) extends HuanCunBundle {
  val bufIdx = UInt(bufIdxBits.W)
  val count = UInt(beatBits.W)
  val last = Bool()
}

class PutBufferBeatEntry(implicit p: Parameters) extends HuanCunBundle {
  val data = UInt((beatBytes * 8).W)
  val mask = UInt(beatBytes.W)
  val corrupt = Bool()
}

class PrefetchRecv extends Bundle {
  val addr = UInt(64.W)
  val addr_valid = Bool()
  val l2_pf_en = Bool()
}