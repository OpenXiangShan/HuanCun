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

package huancun.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import huancun.HasHuanCunParameters

trait TPmetaParameters {
  val metaEntries: Int
  val metaAssoc: Int
  val busBytes: Int
  val deltaBits: Int
  val nrDelta: Int
}

case class DefaultTPmetaParameters() extends TPmetaParameters {
  override val metaEntries = 16384
  override val metaAssoc = 16
  override val busBytes = 256
  override val deltaBits = 36
  override val nrDelta = 16
}

trait HasTPmetaParameters extends HasHuanCunParameters {
  val metaEntries = tpmetaOpt.get.metaEntries
  val metaAssoc = tpmetaOpt.get.metaAssoc
  val nrSet = metaEntries / metaAssoc
  val SetBits = log2Ceil(nrSet)
  val busBytes = tpmetaOpt.get.busBytes
  val busBits = busBytes * 8
  // val nrTPBeat = cacheParams.blockBytes / busBytes
  val nrTPBeat = 1
  val tpbeatBits = if (nrTPBeat == 1) 1 else log2Ceil(nrTPBeat)
  val deltaBits = tpmetaOpt.get.deltaBits
  val nrDelta = tpmetaOpt.get.nrDelta
}

abstract class TPmetaBundle(implicit val p: Parameters) extends Bundle with HasTPmetaParameters
abstract class TPmetaModule(implicit val p: Parameters) extends Module with HasTPmetaParameters