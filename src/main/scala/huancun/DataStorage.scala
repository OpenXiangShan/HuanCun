/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import huancun.utils.{SRAMWrapper, XSPerfAccumulate}
import utility._

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
    val ecc = Valid(new EccInfo)
  })

  /* Define some internal parameters */
  val nrStacks = 2
  val stackBits = log2Ceil(nrStacks)
  val bankBytes = 8
  val rowBytes = nrStacks * beatBytes
  val nrRows = sizeBytes / rowBytes
  val nrBanks = rowBytes / bankBytes
  val rowBits = log2Ceil(nrRows)
  val stackSize = nrBanks / nrStacks
  val sramSinglePort = true

  // Suppose * as one bank
  // All banks can be grouped by nrStacks. We call such group as stack
  //     one row ==> ******** ******** ******** ********
  // If there's no conflict, one row can be accessed in parallel by nrStacks

  def dataCode: Code = Code.fromString(p(HCCacheParamsKey).dataECC)

  val eccBits = dataCode.width(8 * bankBytes) - 8 * bankBytes
  println(s"Data ECC bits:$eccBits")

  val bankedData = Seq.fill(nrBanks) {
    Module(
      new SRAMWrapper(
        gen = UInt((8 * bankBytes).W),
        set = nrRows,
        n = cacheParams.sramDepthDiv,
        clk_div_by_2 = cacheParams.sramClkDivBy2
      )
    )
  }
  val dataEccArray = if (eccBits > 0) {
    Seq.fill(nrStacks) {
      Module(new SRAMWrapper(
        gen = UInt((eccBits * stackSize).W),
        set = nrRows,
        n = cacheParams.sramDepthDiv,
        clk_div_by_2 = cacheParams.sramClkDivBy2
      ))
    }
  } else null

  val stackRdy = if (cacheParams.sramClkDivBy2) {
    RegInit(VecInit(Seq.fill(nrStacks) {
      true.B
    }))
  } else VecInit(Seq.fill(nrStacks) {
    true.B
  })

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
    //                            [index, stack, block]
    val innerAddr = Cat(addr.bits.way, addr.bits.set, addr.bits.beat)
    val innerIndex = innerAddr >> stackBits
    val stackIdx = innerAddr(stackBits - 1, 0)
    val stackSel = UIntToOH(stackIdx, stackSize) // Select which stack to access

    val out = Wire(new DSRequest)
    val accessVec = Cat(
      Seq
        .tabulate(nrStacks) { i =>
          !out.bankSum((i + 1) * stackSize - 1, i * stackSize).orR
        }
        .reverse
    )
    addr.ready := accessVec(stackIdx) && stackRdy(stackIdx)

    out.wen := wen.B
    out.index := innerIndex
    // FillInterleaved: 0010 => 00000000 00000000 11111111 00000000
    out.bankSel := Mux(addr.valid, FillInterleaved(stackSize, stackSel), 0.U) // TODO: consider mask
    out.bankEn := Mux(addr.bits.noop || !stackRdy(stackIdx),
      0.U,
      out.bankSel & FillInterleaved(stackSize, accessVec)
    )
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
      sourceC_req,
      sinkC_req,
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
  val eccData = if (eccBits > 0) Some(Wire(Vec(nrStacks, Vec(stackSize, UInt(eccBits.W))))) else None
  val bank_en = Wire(Vec(nrBanks, Bool()))
  val sel_req = Wire(Vec(nrBanks, new DSRequest))
  dontTouch(bank_en)
  dontTouch(sel_req)

  val cycleCnt = Counter(true.B, 2)
  // mark accessed banks as busy
  if (cacheParams.sramClkDivBy2) {
    bank_en.grouped(stackSize).toList
      .map(banks => Cat(banks).orR)
      .zip(stackRdy)
      .foreach {
        case (accessed, rdy) => rdy := !accessed && cycleCnt._1(0)
      }
  }

  for (i <- 0 until nrBanks) {
    val en = reqs.map(_.bankEn(i)).reduce(_ || _)
    val selectedReq = PriorityMux(reqs.map(_.bankSel(i)), reqs)
    bank_en(i) := en
    sel_req(i) := selectedReq
    if (cacheParams.sramClkDivBy2) {
      // Write
      val wen = en && selectedReq.wen
      val wen_latch = RegNext(wen, false.B)
      bankedData(i).io.w.req.valid := wen_latch
      bankedData(i).io.w.req.bits.apply(
        setIdx = RegNext(selectedReq.index),
        data = RegNext(selectedReq.data(i)),
        waymask = 1.U
      )
      // Read
      val ren = en && !selectedReq.wen
      val ren_latch = RegNext(ren, false.B)
      bankedData(i).io.r.req.valid := ren_latch
      bankedData(i).io.r.req.bits.apply(setIdx = RegNext(selectedReq.index))
    } else {
      // Write
      val wen = en && selectedReq.wen
      bankedData(i).io.w.req.valid := wen
      bankedData(i).io.w.req.bits.apply(
        setIdx = selectedReq.index,
        data = selectedReq.data(i),
        waymask = 1.U
      )
      // Read
      val ren = en && !selectedReq.wen
      bankedData(i).io.r.req.valid := ren
      bankedData(i).io.r.req.bits.apply(setIdx = selectedReq.index)
    }
    // Ecc
    outData(i) := bankedData(i).io.r.resp.data(0)
  }

  if (eccBits > 0) {
    for (((banks, ecc), eccArray) <-
           bankedData.grouped(stackSize).toList
             .zip(eccData.get)
             .zip(dataEccArray)
         ) {
      eccArray.io.w.req.valid := banks.head.io.w.req.valid
      eccArray.io.w.req.bits.apply(
        setIdx = banks.head.io.w.req.bits.setIdx,
        data = VecInit(banks.map(b =>
          dataCode.encode(b.io.w.req.bits.data(0)).head(eccBits)
        )).asUInt,
        waymask = 1.U
      )
      eccArray.io.r.req.valid := banks.head.io.r.req.valid
      eccArray.io.r.req.bits.apply(setIdx = banks.head.io.r.req.bits.setIdx)
      ecc := eccArray.io.r.resp.data(0).asTypeOf(Vec(stackSize, UInt(eccBits.W)))
    }
  } else {
  }

  val dataSelModules = Array.fill(stackSize) {
    Module(new DataSel(nrStacks, 2, bankBytes * 8, eccBits))
  }
  val data_grps = outData.grouped(stackSize).toList.transpose
  val ecc_grps = eccData.map(_.toList.transpose)
  val d_sel = sourceD_rreq.bankEn.asBools.grouped(stackSize).toList.transpose
  val c_sel = sourceC_req.bankEn.asBools.grouped(stackSize).toList.transpose
  for (i <- 0 until stackSize) {
    val dataSel = dataSelModules(i)
    dataSel.io.in := VecInit(data_grps(i))
    dataSel.io.ecc_in.map(_ := ecc_grps.get(i))
    dataSel.io.sel(0) := Cat(d_sel(i).reverse)
    dataSel.io.sel(1) := Cat(c_sel(i).reverse)
    dataSel.io.en(0) := io.sourceD_raddr.fire
    dataSel.io.en(1) := io.sourceC_raddr.fire
  }

  io.sourceD_rdata.data := Cat(dataSelModules.map(_.io.out(0)).reverse)
  io.sourceD_rdata.corrupt := Cat(dataSelModules.map(_.io.err_out(0))).orR
  io.sourceC_rdata.data := Cat(dataSelModules.map(_.io.out(1)).reverse)
  io.sourceC_rdata.corrupt := Cat(dataSelModules.map(_.io.err_out(1))).orR

  val d_addr_reg = RegNextN(io.sourceD_raddr.bits, sramLatency)
  val c_addr_reg = RegNextN(io.sourceC_raddr.bits, sramLatency)

  io.ecc.valid := io.sourceD_rdata.corrupt || io.sourceC_rdata.corrupt
  io.ecc.bits.errCode := EccInfo.ERR_DATA
  io.ecc.bits.addr := Mux(io.sourceD_rdata.corrupt,
    Cat(d_addr_reg.set, d_addr_reg.way, d_addr_reg.beat),
    Cat(c_addr_reg.set, c_addr_reg.way, c_addr_reg.beat)
  )

  val debug_stack_used = PopCount(bank_en.grouped(stackSize).toList.map(seq => Cat(seq).orR))

  for (i <- 1 to nrStacks) {
    XSPerfAccumulate(cacheParams, s"DS_${i}_stacks_used", debug_stack_used === i.U)
  }

}

class DataSel(inNum: Int, outNum: Int, width: Int, eccBits: Int)(implicit p: Parameters) extends HuanCunModule {

  val io = IO(new Bundle() {
    val ecc_in = if (eccBits > 0) Some(Input(Vec(inNum, UInt(eccBits.W)))) else None
    val in = Input(Vec(inNum, UInt(width.W)))
    val sel = Input(Vec(outNum, UInt(inNum.W))) // one-hot sel mask
    val en = Input(Vec(outNum, Bool()))
    val out = Output(Vec(outNum, UInt(width.W)))
    val err_out = Output(Vec(outNum, Bool()))
  })

  def dataCode: Code = Code.fromString(p(HCCacheParamsKey).dataECC)

  for (i <- 0 until outNum) {
    val en = RegNextN(io.en(i), sramLatency - 2)
    val sel_r = RegNextN(io.sel(i), sramLatency - 1)
    val odata = RegEnable(io.in, en)

    io.out(i) := RegEnable(Mux1H(sel_r, odata), RegNext(en, false.B))

    if (eccBits > 0) {
      val oeccs = RegEnable(io.ecc_in.get, en)
      val err = oeccs.zip(odata).map{
        case (e, d) => dataCode.decode(e ## d).error
      }
      io.err_out(i) := RegEnable(Mux1H(sel_r, err).orR, false.B, RegNext(en, false.B))
    } else {
      io.err_out(i) := false.B
    }
  }

}
