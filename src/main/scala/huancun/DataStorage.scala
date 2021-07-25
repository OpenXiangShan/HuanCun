package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3.{util, _}
import chisel3.util._
import huancun.utils.SRAMTemplate

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

  val bankedData = Seq.fill(nrBanks)(
    Module(
      new SRAMTemplate(
        UInt((8 * bankBytes).W),
        set = nrRows,
        way = 1,
        shouldReset = false,
        holdRead = false,
        singlePort = sramSinglePort
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
    addr.ready := accessVec(innerAddr(stackBits - 1, 0))

    out.wen := wen.B
    out.index := innerIndex
    // FillInterleaved: 0010 => 00000000 00000000 11111111 00000000
    out.bankSel := Mux(addr.valid, FillInterleaved(stackSize, stackSel), 0.U) // TODO: consider mask
    out.bankEn := out.bankSel & FillInterleaved(stackSize, accessVec) // TODO: consider noop req
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
      sourceD_wreq,
      sinkD_wreq,
      sourceD_rreq,
      sinkC_req
    ) // TODO: add more requests with priority carefully
  reqs.foldLeft(0.U(nrBanks.W)) {
    case (sum, req) =>
      req.bankSum := sum
      req.bankSel | sum
  }

  val outData = Wire(Vec(nrBanks, UInt((8 * bankBytes).W)))

  for (i <- 0 until nrBanks) {
    val en = reqs.map(_.bankEn(i)).reduce(_ || _)
    val selectedReq = PriorityMux(reqs.map(_.bankSel(i)), reqs)

    // Write
    bankedData(i).io.w.req.valid := en && selectedReq.wen
    bankedData(i).io.w.req.bits.apply(
      setIdx = selectedReq.index,
      data = selectedReq.data(i),
      waymask = 1.U
    )
    // Read
    bankedData(i).io.r.req.valid := en && !selectedReq.wen
    bankedData(i).io.r.req.bits.apply(setIdx = selectedReq.index)
    outData(i) := RegEnable(bankedData(i).io.r.resp.data(0), RegNext(en && !selectedReq.wen))
  }

  /* Pack out-data to channels */
  val sourceDlatch = RegNext(RegNext(sourceD_rreq.bankEn))
  val sourceClatch = RegNext(RegNext(sourceC_req.bankEn))

  val sourceDrdata = outData.zipWithIndex.map {
    case (r, i) => Mux(sourceDlatch(i), r, 0.U)
  }.grouped(stackSize).toList.transpose.map(s => s.reduce(_ | _))
  val sourceCrdata = outData.zipWithIndex.map {
    case (r, i) => Mux(sourceClatch(i), r, 0.U)
  }.grouped(stackSize).toList.transpose.map(s => s.reduce(_ | _))

  io.sourceD_rdata.data := Cat(sourceDrdata.reverse)
  io.sourceC_rdata.data := Cat(sourceCrdata.reverse)
}
