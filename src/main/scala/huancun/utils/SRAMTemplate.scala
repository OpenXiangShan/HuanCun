/** *************************************************************************************
  * Copyright (c) 2020 Institute of Computing Technology, CAS
  * Copyright (c) 2020 University of Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * NutShell is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *             http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
  * FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
 */

// See LICENSE.SiFive for license details.

package huancun.utils

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.LFSR64
import huancun.mbist._
import huancun.utils.SRAMTemplate.uniqueId

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool, init: Option[T] = None): T = {
    val hold_data = if (init.isDefined) RegEnable(x, init.get, en) else RegEnable(x, en)
    Mux(en, x, hold_data)
  }
}

object DelayTwoCycle {
  def apply[T <: Data](x: T, en: Bool): T = {
    val en1 = RegNext(en)
    val data_reg = RegEnable(x, en1)
    val en2 = RegNext(en1)
    Mux(en2, data_reg, LFSR64().asTypeOf(data_reg))
  }
}

object ReadAndHold {
  def apply[T <: Data](x: Mem[T], addr:         UInt, en: Bool): T = HoldUnless(x.read(addr), en)
  def apply[T <: Data](x: SyncReadMem[T], addr: UInt, en: Bool): T = HoldUnless(x.read(addr, en), RegNext(en))
}

abstract class SRAM_Array(mbist:Boolean) extends RawModule {
  def init(clock: Clock, writeClock: Option[Clock]): Unit
  def read(addr: UInt): UInt
  def read(addr: UInt, enable: Bool): UInt = {
    var rdata = 0.U
    when (enable) {
      rdata = read(addr)
    }
    rdata
  }
  def write(addr: UInt, data: UInt): Unit
  def write(addr: UInt, data: UInt, mask: UInt): Unit
  val trim_fuse = IO(Input(UInt(11.W)))
  val sleep_fuse = IO(Input(UInt(2.W)))
  val bypsel = IO(Input(Bool()))
  val wdis_b = IO(Input(Bool()))
  val rdis_b = IO(Input(Bool()))
  val init_en = IO(Input(Bool()))
  val init_val = IO(Input(Bool()))
  val clkungate = IO(Input(Bool()))
  trim_fuse  := DontCare
  sleep_fuse := DontCare
  bypsel  := DontCare
  wdis_b := DontCare
  rdis_b  := DontCare
  init_en := DontCare
  init_val  := DontCare
  clkungate := DontCare
  if(mbist) {
    dontTouch(trim_fuse)
    dontTouch(sleep_fuse)
    dontTouch(bypsel)
    dontTouch(wdis_b)
    dontTouch(rdis_b)
    dontTouch(init_en)
    dontTouch(init_val)
    dontTouch(clkungate)
  }
}

class SRAM_Array_1P(depth: Int, width: Int, maskSegments: Int, mbist:Boolean) extends SRAM_Array(mbist) {
  val RW0 = IO(new Bundle() {
    val clk   = Input(Clock())
    val addr  = Input(UInt(log2Ceil(depth).W))
    val en    = Input(Bool())
    val wmode = Input(Bool())
    val wmask = if (maskSegments > 1) Some(Input(UInt(maskSegments.W))) else None
    val wdata = Input(UInt(width.W))
    val rdata = Output(UInt(width.W))
  })

  val ram = Seq.fill(maskSegments)(Mem(depth, UInt((width / maskSegments).W)))

  withClock(RW0.clk) {
    // read: rdata will keep stable until the next read enable.
    val RW0_ren = RW0.en && !RW0.wmode
    val RW0_ren_REG = RegNext(RW0_ren)
    val RW0_addr_REG = RegEnable(RW0.addr, RW0_ren)
    RW0.rdata := HoldUnless(VecInit(ram.map(_.read(RW0_addr_REG))).asUInt, RW0_ren_REG)
    // write with mask
    val RW0_wen = RW0.en && RW0.wmode
    val wdata = RW0.wdata.asTypeOf(Vec(maskSegments, UInt((width / maskSegments).W)))
    for (i <- 0 until maskSegments) {
      val wmask = if (RW0.wmask.isDefined) RW0.wmask.get(i) else true.B
      when (RW0_wen && wmask) {
        ram(i)(RW0.addr) := wdata(i)
      }
    }
  }

  def init(clock: Clock, writeClock: Option[Clock] = None): Unit = {
    dontTouch(RW0)
    RW0 := DontCare
    RW0.clk := clock
    RW0.en := false.B
  }
  def read(addr: UInt): UInt = {
    RW0.addr := addr
    RW0.en := true.B
    RW0.wmode := false.B
    RW0.rdata
  }
  def write(addr: UInt, data: UInt): Unit = {
    val mask = Fill(maskSegments, true.B)
    write(addr, data, mask)
  }
  def write(addr: UInt, data: UInt, mask: UInt): Unit = {
    RW0.addr := addr
    RW0.en := true.B
    RW0.wmode := true.B
    if (RW0.wmask.isDefined) {
      RW0.wmask.get := mask
    }
    RW0.wdata := data
  }
}

// MCP is used to distinguish SRAMs in Verilog. MCP SRAM is the same as normal SRAM.
class SRAM_Array_1P_MCP(depth: Int, width: Int, maskSegments: Int, mbist: Boolean)
  extends SRAM_Array_1P(depth, width, maskSegments, mbist)

class SRAM_Array_2P(depth: Int, width: Int, maskSegments: Int, mbist: Boolean)
  extends SRAM_Array(mbist)  {
  require(width % maskSegments == 0)

  val R0 = IO(new Bundle() {
    val clk   = Input(Clock())
    val addr  = Input(UInt(log2Ceil(depth).W))
    val en    = Input(Bool())
    val data  = Output(UInt(width.W))
  })

  val W0 = IO(new Bundle() {
    val clk   = Input(Clock())
    val addr  = Input(UInt(log2Ceil(depth).W))
    val en    = Input(Bool())
    val data  = Input(UInt(width.W))
    val mask  = if (maskSegments > 1) Some(Input(UInt(maskSegments.W))) else None
  })

  val ram = Seq.fill(maskSegments)(Mem(depth, UInt((width / maskSegments).W)))

  // read: rdata will keep stable until the next read enable.
  withClock(R0.clk) {
    val R0_ren = R0.en
    val RW0_ren_REG = RegNext(R0_ren)
    val RW0_addr_REG = RegEnable(R0.addr, R0_ren)
    R0.data := HoldUnless(VecInit(ram.map(_.read(RW0_addr_REG))).asUInt, RW0_ren_REG)
  }

  // write with mask
  withClock(W0.clk) {
    val RW0_wen = W0.en
    val wdata = W0.data.asTypeOf(Vec(maskSegments, UInt((width / maskSegments).W)))
    for (i <- 0 until maskSegments) {
      val wmask = if (W0.mask.isDefined) W0.mask.get(i) else true.B
      when (RW0_wen && wmask) {
        ram(i)(W0.addr) := wdata(i)
      }
    }
  }

  def init(clock: Clock, writeClock: Option[Clock]): Unit = {
    dontTouch(R0)
    dontTouch(W0)
    R0 := DontCare
    R0.clk := clock
    R0.en := false.B
    W0 := DontCare
    W0.clk := writeClock.getOrElse(clock)
    W0.en := false.B
  }
  def read(addr: UInt): UInt = {
    R0.addr := addr
    R0.en := true.B
    R0.data
  }
  def write(addr: UInt, data: UInt): Unit = {
    write(addr, data, ((1L << maskSegments) - 1).U)
  }
  def write(addr: UInt, data: UInt, mask: UInt): Unit = {
    W0.addr := addr
    W0.en := true.B
    if (W0.mask.isDefined) {
      W0.mask.get := mask
    }
    W0.data := data
  }
}

object SRAM_Array {
  def apply(clock: Clock, singlePort: Boolean, depth: Int, width: Int,
            maskSegments: Int = 1,
            MCP: Boolean = false,
            writeClock: Option[Clock] = None,
            mbist: Boolean
           ): SRAM_Array = {
    val array = (singlePort, MCP) match {
      case (true, true) => Module(new SRAM_Array_1P_MCP(depth, width, maskSegments, mbist))
      case (true, false) => Module(new SRAM_Array_1P(depth, width, maskSegments, mbist))
      case (false, _) => Module(new SRAM_Array_2P(depth, width, maskSegments, mbist))
    }
    array.init(clock, writeClock)
    array
  }
}

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(Vec(way, gen))
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    super.apply(setIdx)
    this.data := data
    this.waymask.foreach(_ := waymask)
    this
  }
  // this could only be used when waymask is onehot or nway is 1
  def apply(data: T, setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

object SRAMTemplate {
  private var uniqueId = 0

  def getWayNumForEachNodeAndNodeNum(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val dataNum1toNNode = mw / dw
    val numVec = (1 until dataNum1toNNode + 1)
    val validVec = numVec.map(num => (way % num == 0) && (way >= num))
    val validNum = numVec.zip(validVec).filter(_._2)
    val res = if(validNum.isEmpty) (1,way) else validNum.last
    (res._1, way / res._1)
  }
}

class SRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  clk_div_by_2: Boolean = false, mbist: Boolean = true,
  maxMbistDataWidth:Int = 256
) extends Module {

  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val isNto1 = gen.getWidth > maxMbistDataWidth
  /*************implement mbist interface node(multiple nodes for one way)********/
  val mbistNodeNumNto1 = way * ((gen.getWidth + maxMbistDataWidth - 1) / maxMbistDataWidth)
  val mbistNodeNumForEachWay = mbistNodeNumNto1 / way
  val maskWidthNto1 = 1
  val mbistDataWidthNto1 = (gen.getWidth + mbistNodeNumForEachWay - 1) / mbistNodeNumForEachWay

  val mbistDataWidthsForEachNode1toNWay = (0 until mbistNodeNumForEachWay).map({idx =>
    if(idx == mbistNodeNumForEachWay - 1 && (gen.getWidth % mbistDataWidthNto1) != 0)
      gen.getWidth - mbistDataWidthNto1 * (mbistNodeNumForEachWay - 1)
    else
      mbistDataWidthNto1
  })

  val mbistNodesNto1: Seq[SRAM2MBIST] = (0 until mbistNodeNumNto1).map({idx =>
    val params = SRAM2MBISTParams(set, mbistDataWidthsForEachNode1toNWay(idx % mbistNodeNumForEachWay),maskWidthNto1,singlePort)
    val bd = Wire(new SRAM2MBIST(params))
    if(isNto1) dontTouch(bd)
    bd := DontCare
    bd
  })
  val mbistGroupsNto1 = List.tabulate(way, mbistNodeNumForEachWay)((widx, nidx) => mbistNodesNto1(widx * mbistNodeNumForEachWay + nidx))
  val mbistWMaskNto1 = mbistGroupsNto1.map(_.map(_.wmask).reduce(_|_)).reverse.reduce(Cat(_,_))

  val mbistAddrNto1  = mbistNodesNto1.map(_.addr).reduce(_|_)
  val mbistAddrReadNto1  = mbistNodesNto1.map(_.addr_rd).reduce(_|_)
  val mbistWriteDataNto1 = VecInit(mbistGroupsNto1.map(_.map(_.wdata).reverse.reduce(Cat(_,_))))
  val mbistNodesDataWidthsInGroupsNto1 = mbistGroupsNto1.map(_.map(_.params.dataWidth))
  val mbistReadEnNto1 = mbistNodesNto1.map(_.re).reduce(_|_)
  val mbistWriteEnNto1 = mbistNodesNto1.map(_.we).reduce(_|_)
  val mbistFuncSelNto1 = mbistNodesNto1.map(_.ack).reduce(_|_)

  /*************implement mbist interface node(one node for multiple ways)********/

  val (wayNumForEachNode, mbistNodeNum1toN) = SRAMTemplate.getWayNumForEachNodeAndNodeNum(gen.getWidth, way, maxMbistDataWidth)

  val mbistDataWidth1toN = wayNumForEachNode * gen.getWidth
  val maskWidth1toN = wayNumForEachNode
  val mbistNodes1toN: Seq[SRAM2MBIST] = (0 until mbistNodeNum1toN).map({idx =>
    val params = SRAM2MBISTParams(set, mbistDataWidth1toN, maskWidth1toN,singlePort)
    val bd = Wire(new SRAM2MBIST(params))
    if(!isNto1) dontTouch(bd)
    bd := DontCare
    bd
  })
  val mbistWMask1toN = mbistNodes1toN.reverse.map(_.wmask).reduce(Cat(_,_))
  val mbistAddr1toN = mbistNodes1toN.map(_.addr).reduce(_|_)
  val mbistAddrRead1toN = mbistNodes1toN.map(_.addr_rd).reduce(_|_)
  val mbistWriteData1toN = VecInit(mbistNodes1toN.flatMap(node => {
    (0 until wayNumForEachNode).map(idx => node.wdata((idx + 1) * gen.getWidth - 1, idx * gen.getWidth))
  }))
  val mbistReadEn1toN = mbistNodes1toN.map(_.re).reduce(_|_)
  val mbistWriteEn1toN = mbistNodes1toN.map(_.we).reduce(_|_)
  val mbistFuncSel1toN = mbistNodes1toN.map(_.ack).reduce(_|_)


  /**************************************add nodes to global************************************/
  if(mbist) {
    if(isNto1) {
      mbistNodesNto1.zipWithIndex.foreach({
        case (node, idx) =>
          val sram_prefix = "sram_" + (uniqueId + idx) + "_"
          MBIST.addSRAM(node, sram_prefix, uniqueId + idx)
      })
    }
    else{
      mbistNodes1toN.zipWithIndex.foreach({
        case (node, idx) =>
          val sram_prefix = "sram_" + (uniqueId + idx) + "_"
          MBIST.addSRAM(node, sram_prefix, uniqueId + idx)
      })
    }
  }
  MBIST.noDedup(this)
  val addId = if(isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
  uniqueId += addId
  /*******************************select signals between two set*******************************/
  val mbistAddr           = if(isNto1) mbistAddrNto1 else mbistAddr1toN
  val mbistAddrRead       = if(isNto1) mbistAddrReadNto1 else mbistAddrRead1toN
  val mbistWriteData      = if(isNto1) mbistWriteDataNto1 else mbistWriteData1toN
  val mbistReadEn         = if(isNto1) mbistReadEnNto1 else mbistReadEn1toN
  val mbistWriteEn        = if(isNto1) mbistWriteEnNto1 else mbistWriteEn1toN
  val mbistWMask          = if(isNto1) mbistWMaskNto1 else mbistWMask1toN
  val mbistFuncSel         = if(isNto1) mbistFuncSelNto1 else mbistFuncSel1toN
  /********************************************************************************************/
  val wordType = UInt(gen.getWidth.W)
  val array = SRAM_Array(clock, singlePort, set, way * gen.getWidth, way, MCP = clk_div_by_2, mbist = mbist)

  val trim_fuse = if(isNto1) mbistNodesNto1.head.sram_trim_fuse else mbistNodes1toN.head.sram_trim_fuse
  val sleep_fuse = if(isNto1) mbistNodesNto1.head.sram_sleep_fuse else mbistNodes1toN.head.sram_sleep_fuse
  val bypsel = if(isNto1) mbistNodesNto1.head.bypsel else mbistNodes1toN.head.bypsel
  val wdis_b = if(isNto1) mbistNodesNto1.head.wdis_b else mbistNodes1toN.head.wdis_b
  val rdis_b = if(isNto1) mbistNodesNto1.head.rdis_b else mbistNodes1toN.head.rdis_b
  val init_en = if(isNto1) mbistNodesNto1.head.init_en else mbistNodes1toN.head.init_en
  val init_val = if(isNto1) mbistNodesNto1.head.init_val else mbistNodes1toN.head.init_val
  val clkungate = if(isNto1) mbistNodesNto1.head.clkungate else mbistNodes1toN.head.clkungate
  array.trim_fuse <> trim_fuse
  array.sleep_fuse <> sleep_fuse
  array.bypsel <> bypsel
  array.wdis_b <> wdis_b
  array.rdis_b <> rdis_b
  array.init_en <> init_en
  array.init_val <> init_val
  array.clkungate <> clkungate

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))

  val finalWriteSetIdx = if(mbist) Mux(mbistFuncSel, mbistAddr, setIdx) else setIdx
  val finalReadSetIdx = if(mbist) Mux(mbistFuncSel, mbistAddrRead, io.r.req.bits.setIdx) else io.r.req.bits.setIdx
  val finalWen = if(mbist) Mux(mbistFuncSel, mbistWriteEn, wen) else wen
  val finalRen = if(mbist) Mux(mbistFuncSel, mbistReadEn, ren) else ren
  val finalWmask = if(mbist) Mux(mbistFuncSel, mbistWMask, waymask) else waymask
  val finalWriteData = if(mbist) Mux(mbistFuncSel, mbistWriteData, wdata) else wdata

  val toSRAMRen = if (singlePort) (finalRen && !finalWen) else finalRen

  //  val raw_rdata = array.read(io.r.req.bits.setIdx, realRen).asTypeOf(Vec(way, wordType))
  //  when (wen) { array.write(setIdx, wdata.asUInt, waymask) }
  when (finalWen) { array.write(finalWriteSetIdx, finalWriteData.asUInt, finalWmask) }
  val raw_rdata = array.read(finalReadSetIdx, toSRAMRen).asTypeOf(Vec(way, wordType))

  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (
    if(clk_div_by_2){
      // DelayTwoCycle(mem_rdata, realRen)
      // Now we assume rdata will not change during two cycles
      mem_rdata
    } else if (holdRead) {
      HoldUnless(mem_rdata, RegNext(realRen))
    } else {
      mem_rdata
    }).map(_.asTypeOf(gen))

  if(clk_div_by_2){
    CustomAnnotations.annotateClkDivBy2(this)
  }
  if(!isPow2(set)){
    CustomAnnotations.annotateSpecialDepth(this)
  }

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  /*************************************mbist rdata output**************************************************/
  val mbistReadData = rdata.map(_.asTypeOf(UInt(gen.getWidth.W))).zip(mbistNodesDataWidthsInGroupsNto1).map({
    case (data,width) => {
      (0 until mbistNodeNumForEachWay).map({idx =>
        val start = if (idx == 0) 0 else width.take(idx).sum
        data(width(idx) + start - 1, start)
      })
    }
  })
  mbistGroupsNto1.map(_.map(_.rdata)).zip(mbistReadData).foreach({
    case (sinkList,sourceList) =>
      (sinkList zip sourceList).foreach({
        case (sink,source) =>
          sink := source
      })
  })

  val rdataToMbist = (0 until mbistNodeNum1toN).map(idx => {
    rdata.map(_.asTypeOf(UInt(gen.getWidth.W))).slice(idx * wayNumForEachNode, idx * wayNumForEachNode + wayNumForEachNode).reverse.reduce(Cat(_,_))
  })
  require(rdataToMbist.head.getWidth == mbistNodes1toN.head.rdata.getWidth)
  mbistNodes1toN.map(_.rdata).zip(rdataToMbist).foreach({
    case (out,in) => out := in
  })
  /*********************************************************************************************************/

}

class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
                                         shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.foreach { r =>
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
  }
}