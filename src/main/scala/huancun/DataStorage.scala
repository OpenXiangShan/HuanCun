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
import huancun.utils._

class DataStorage(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val sourceC_raddr = Flipped(DecoupledIO(new DSAddress))
    val sourceC_rdata = Output(new DSData)
    val sinkD_waddr = Flipped(DecoupledIO(new DSAddress))
    val sinkD_wdata = Input(new DSData)
    val sourceD_raddr = Flipped(DecoupledIO(new DSAddress))
    val sourceD_rdata = Output(new DSData)
    val sourceD_waddr = Flipped(DecoupledIO(new DSAddress))
    val sourceD_wdata = Input(new DSData)
    val sinkC_waddr = Flipped(DecoupledIO(new DSAddress))
    val sinkC_wdata = Input(new DSData)
  })

  /* Define some internal parameters */
  val nrStacks = 4
  val bankBytes = 8
  val rowBytes = nrStacks * beatBytes
  val nrRows = sizeBytes / rowBytes
  val nrBanks = rowBytes / bankBytes
  val rowBits = log2Ceil(nrRows)
  val stackSize = nrBanks / nrStacks
  val stackBits = log2Ceil(stackSize)
  val sramSinglePort = true

  // Suppose * as one bank
  // All banks can be grouped by nrStacks. We call such group as stack
  //     one row ==> ******** ******** ******** ********
  // If there's no conflict, one row can be accessed in parallel by nrStacks

  def dataCode: Code = Code.fromString(p(HCCacheParamsKey).dataECC)

  val eccDataBits = dataCode.width(8*bankBytes)
  println(s"extra ECC Databits:${eccDataBits - (8*bankBytes)}")

  val bankedData = Seq.fill(nrBanks)(
    Module(
      new SRAMTemplate(
        UInt(eccDataBits.W),
        set = nrRows,
        way = 1,
        shouldReset = false,
        holdRead = false,
        singlePort = sramSinglePort,
        cycleFactor = cacheParams.sramCycleFactor
      )
    )
  )

  /* Convert to internal request signals */
  class DSRequest extends HuanCunBundle {
    val wen = Bool()
    val index = UInt((rowBytes * 8).W)
    val bankSel = UInt(nrBanks.W)
    val bankSum = UInt(nrBanks.W)
    val bankEn = UInt(nrBanks.W)
    val data = Vec(nrBanks, UInt((8 * bankBytes).W))
  }

  def req(wen: Boolean, addr: DecoupledIO[DSAddress], data: DSData) = {
    // Remap address
    // [beat, set, way, block] => [way, set, beat, block]
    //                          aka [index, stack, block]
    val innerAddr = Cat(addr.bits.way, addr.bits.set, addr.bits.beat)
    val innerIndex = innerAddr >> stackBits
    val stackSel = UIntToOH(innerAddr(stackBits - 1, 0), stackSize) // Select which stack to access

    val out = Wire(new DSRequest)
    val accessVec = Cat(
      Seq
        .tabulate(nrStacks) { i =>
          !out.bankSum((i + 1) * stackSize - 1, i * stackSize).orR
        }
        .reverse
    )
    addr.ready := accessVec(innerAddr(stackBits - 1, 0)) && SReg.sren()

    out.wen := wen.B
    out.index := innerIndex
    // FillInterleaved: 0010 => 00000000 00000000 11111111 00000000
    out.bankSel := Mux(addr.valid, FillInterleaved(stackSize, stackSel), 0.U) // TODO: consider mask
    out.bankEn := Mux(addr.bits.noop, 0.U, out.bankSel & FillInterleaved(stackSize, accessVec))
    out.data := Cat(Seq.fill(nrStacks)(data.data)).asTypeOf(out.data.cloneType)
    out
  }

  /* Arbitrates r&w by bank according to priority */
  val sourceC_req = req(wen = false, io.sourceC_raddr, io.sourceC_rdata)
  val sourceD_rreq = req(wen = false, io.sourceD_raddr, io.sourceD_rdata)
  val sourceD_wreq = req(wen = true, io.sourceD_waddr, io.sourceD_wdata)
  val sinkD_wreq = req(wen = true, io.sinkD_waddr, io.sinkD_wdata)
  val sinkC_req = req(wen = true, io.sinkC_waddr, io.sinkC_wdata)

  val reqs =
    Seq(
      sinkC_req,
      sourceC_req,
      sinkD_wreq,
      sourceD_wreq,
      sourceD_rreq
    ) // TODO: add more requests with priority carefully
  reqs.foldLeft(0.U(nrBanks.W)) {
    case (sum, req) =>
      req.bankSum := sum
      req.bankSel | sum
  }

  val outData = Wire(Vec(nrBanks, UInt((8 * bankBytes).W)))

  for (i <- 0 until nrBanks) {
    val en = reqs.map(_.bankEn(i)).reduce(_ || _) && SReg.sren()
    val selectedReq = PriorityMux(reqs.map(_.bankSel(i)), reqs)
    // Write
    bankedData(i).io.w.req.valid := en && selectedReq.wen
    bankedData(i).io.w.req.bits.apply(
      setIdx = selectedReq.index,
      data = dataCode.encode(selectedReq.data(i)),
      waymask = 1.U
    )
    // Read
    bankedData(i).io.r.req.valid := en && !selectedReq.wen
    bankedData(i).io.r.req.bits.apply(setIdx = selectedReq.index)
    val decode = dataCode.decode(bankedData(i).io.r.resp.data(0))
    outData(i) := decode.uncorrected
  }

  val dataSelModules = Array.fill(stackSize){
    Module(new DataSel(nrStacks, 2, bankBytes * 8))
  }
  val data_grps = outData.grouped(stackSize).toList.transpose
  val d_sel = sourceD_rreq.bankEn.asBools().grouped(stackSize).toList.transpose
  val c_sel = sourceC_req.bankEn.asBools().grouped(stackSize).toList.transpose
  for(i <- 0 until stackSize){
    val dataSel = dataSelModules(i)
    dataSel.io.in := VecInit(data_grps(i))
    dataSel.io.sel(0) := Cat(d_sel(i).reverse)
    dataSel.io.sel(1) := Cat(c_sel(i).reverse)
    dataSel.io.en(0) := io.sourceD_raddr.fire()
    dataSel.io.en(1) := io.sourceC_raddr.fire()
  }

  io.sourceD_rdata.data := Cat(dataSelModules.map(_.io.out(0)).reverse)
  io.sourceC_rdata.data := Cat(dataSelModules.map(_.io.out(1)).reverse)

}

class DataSel(inNum: Int, outNum: Int, width: Int)(implicit p: Parameters) extends Module {

  val io = IO(new Bundle() {
    val in = Input(Vec(inNum, UInt(width.W)))
    val sel = Input(Vec(outNum, UInt(inNum.W))) // one-hot sel mask
    val en = Input(Vec(outNum, Bool()))
    val out = Output(Vec(outNum, UInt(width.W)))
  })

  for(i <- 0 until outNum){
    val sel_r = SReg.pipe(io.sel(i))
    val odata = Mux1H(sel_r, io.in)
    val en = SReg.pipe(io.en(i), false.B)
    io.out(i) := RegEnable(odata, en)
  }

}