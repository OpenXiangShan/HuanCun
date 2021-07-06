package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun.utils.SRAMTemplate

class DataStorage(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val sourceD_raddr = Flipped(DecoupledIO(new DSAddress))
    val sourceD_rdata = Output(new DSData)
    val sourceD_waddr = Flipped(DecoupledIO(new DSAddress))
    val sourceD_wdata = Input(new DSData)
  })

  /* Define some internal parameters */
  val singlePort = true
  val nrPort = 4
  val bankBytes = 8
  val bankBits = 8 * bankBytes
  val rowBytes = nrPort * beatBytes
  val nrRows = sizeBytes / rowBytes
  val nrBanks = rowBytes / bankBytes

  // Suppose * as one bank
  // All banks can be grouped by nrPorts
  //     one row ==> ******** ******** ******** ********
  // If there's no conflict, one row can be accessed in parallel by nrPorts

  val bankedData = Seq.tabulate(nrBanks) {
    i =>
      Module(new SRAMTemplate(UInt(bankBits.W), set=nrRows, way=1,
        shouldReset=false, holdRead=false, singlePort=singlePort))
  }

  // Convert to internal request signals
  class DSrequest extends HuanCunBundle {
    val wen      = Bool()
    val index    = UInt((rowBytes*8).W)
    val bankSel  = UInt(nrBanks.W)
    val bankSum  = UInt(nrBanks.W)
    val bankEn   = UInt(nrBanks.W)
    val data     = Vec(nrBanks, UInt((8*bankBytes).W))
  }

  // Arbitrates r&w by bank according to priority

  val reqs = Seq(Wire(new DSrequest)) // TODO: replace with real requests

  val outData = Seq.fill(nrBanks)(UInt(bankBits.W))

  for (i <- 0 until nrBanks) {
    val en = reqs.map(_.bankEn(i)).reduce(_||_)
    val selectedReq = PriorityMux(reqs.map(_.bankSel(i)), reqs)

    // Write
    bankedData(i).io.w.req.valid := en && selectedReq.wen
    bankedData(i).io.w.req.bits.apply(
      setIdx = selectedReq.index,
      data = selectedReq.data,
      waymask = 1.U
    )

    // Read
    bankedData(i).io.r.req.valid := en && !selectedReq.wen
    bankedData(i).io.r.req.bits.apply(setIdx = selectedReq.index)
    outData(i) := RegEnable(bankedData(i).io.r.resp.data(0), RegNext(en && !selectedReq.wen))
  }

}
