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
import huancun.{TPmetaReq, TPmetaResp}
import utility._


class TPmetaIO(implicit p: Parameters) extends TPmetaBundle {
  val req = Flipped(DecoupledIO(new TPmetaReq))
  val resp = ValidIO(new TPmetaResp)
}

class metaEntry(implicit p:Parameters) extends TPmetaBundle {
  val rawData = Vec(16, UInt((36-6).W))
  val hartid = UInt(hartIdLen.W)
  // TODO: val compressedData = UInt(512.W)
}

class TPmeta(implicit p: Parameters) extends TPmetaModule
{
  val io = IO(new TPmetaIO())
  val tpDataTable = Module(
    new SRAMTemplate(new metaEntry(), set = nrSet, way = metaAssoc, shouldReset = false, singlePort = true)
  )

  val readReqValid = io.req.valid && !io.req.bits.wmode
  val writeReqValid = io.req.valid && io.req.bits.wmode

  tpDataTable.io.r.req.valid := readReqValid
  tpDataTable.io.r.req.bits.setIdx := io.req.bits.set

  val wdata = Wire(new metaEntry())
  wdata.rawData := io.req.bits.rawData
  wdata.hartid := io.req.bits.hartid
  tpDataTable.io.w.apply(
    valid = writeReqValid,
    data = wdata,
    setIdx = io.req.bits.set,
    waymask = UIntToOH(io.req.bits.way)
  )

  val readReqValidReg = RegNext(readReqValid, false.B)
  val readReqReg = RegEnable(io.req.bits, readReqValid)

  val rdata = Reg(new metaEntry())
  when(readReqValidReg) {
    rdata := tpDataTable.io.r.resp.data(readReqReg.way)
  }

  io.resp.valid := RegNext(readReqValidReg) && (rdata.hartid === RegNext(readReqReg).hartid)
  io.resp.bits.rawData := rdata.rawData
  io.resp.bits.hartid := RegNext(readReqReg).hartid
  io.req.ready := true.B
}
