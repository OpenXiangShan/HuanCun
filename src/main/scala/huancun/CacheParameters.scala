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

import chipsalliance.rocketchip.config.Field
import chisel3._
import freechips.rocketchip.tilelink.{TLChannelBeatBytes, TLEdgeIn, TLEdgeOut}
import freechips.rocketchip.util.{BundleField, BundleFieldBase, BundleKeyBase, ControlKey}
import huancun.prefetch.PrefetchParameters

case object CacheParamsKey extends Field[CacheParameters](CacheParameters())

case class ClientCacheParameters(
  sets:       Int,
  ways:       Int,
  blockBytes: Int)

case object PrefetchKey extends ControlKey[Bool]("needHint")

case class PrefetchField() extends BundleField(PrefetchKey) {
  override def data: Bool = Bool()

  override def default(x: Bool): Unit = false.B
}

case class CacheParameters(
  name:         String = "L2",
  level:        Int = 2,
  ways:         Int = 4,
  sets:         Int = 128,
  blockBytes:   Int = 64,
  replacement:  String = "plru",
  mshrs:        Int = 16,
  dirReadPorts: Int = 1,
  dirReg:       Boolean = true,
  enableDebug:  Boolean = false,
  enablePerf:   Boolean = false,
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  prefetch:     Option[PrefetchParameters] = None,
  clientCache:  Option[ClientCacheParameters] = None,
  inclusive:    Boolean = true,
  echoField:    Seq[BundleFieldBase] = Nil,
  reqField:     Seq[BundleFieldBase] = Nil, // master
  respKey:      Seq[BundleKeyBase] = Nil,
  reqKey:       Seq[BundleKeyBase] = Seq(PrefetchKey), // slave
  respField:    Seq[BundleFieldBase] = Nil) {
  require(ways > 0)
  require(sets > 0)
  require(channelBytes.d.get >= 8)
  require(dirReadPorts == 1, "now we only use 1 read port")
  if (!inclusive) {
    require(clientCache.nonEmpty, "Non-inclusive cache need to know client cache information")
  }
}

case object EdgeInKey extends Field[TLEdgeIn]
case object EdgeOutKey extends Field[TLEdgeOut]
