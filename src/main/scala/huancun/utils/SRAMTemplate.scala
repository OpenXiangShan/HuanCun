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
import chisel3.util.experimental.BoringUtils
import freechips.rocketchip.tilelink.LFSR64
import huancun.mbist._
import huancun.utils.SRAMTemplate.{nodeId, wrapperId}

import scala.collection.mutable.ListBuffer
import scala.math.sqrt

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool, init: Option[T] = None): T = {
    val hold_data = if (init.isDefined) RegEnable(x, init.get, en) else RegEnable(x, en)
    Mux(en, x, hold_data)
  }
}

class SRAMMbistIO(selectedLen:Int) extends Bundle {
  val selectedOH = Input(UInt(selectedLen.W))
  val dft_ram_bypass = Input(Bool())
  val dft_ram_bp_clken = Input(Bool())
}

class BroadCastBundle() extends Bundle {
  val ram_hold = Input(Bool())
  val ram_bypass = Input(Bool())
  val ram_bp_clken = Input(Bool())
  val l3dataram_clk = Input(Bool())
  val l3dataramclk_bypass = Input(Bool())
  val cgen = Input(Bool())
}

abstract class SRAMArray(hasMbist: Boolean, sramName: Option[String] = None,selectedLen:Int) extends RawModule {
  val mbist = if (hasMbist) Some(IO(new SRAMMbistIO(selectedLen))) else None
  if (mbist.isDefined) {
    dontTouch(mbist.get)
  }

  override def desiredName: String = sramName.getOrElse(super.desiredName)

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
}

class SRAMArray1P(depth: Int, width: Int, maskSegments: Int, hasMbist: Boolean, sramName: Option[String] = None,selectedLen:Int)
  extends SRAMArray(hasMbist, sramName,selectedLen) {
  val RW0 = IO(new Bundle() {
    val clk   = Input(Clock())
    val addr  = Input(UInt(log2Ceil(depth).W))
    val en    = Input(Bool())
    val wmode = Input(Bool())
    val wmask = if (maskSegments > 1) Some(Input(UInt(maskSegments.W))) else None
    val wdata = Input(UInt(width.W))
    val rdata = Output(UInt(width.W))
  })

  withClock(RW0.clk) {
    val ram = Seq.fill(maskSegments)(Mem(depth, UInt((width / maskSegments).W)))
    // read: rdata will keep stable until the next read enable.
    val RW0_ren = RW0.en && !RW0.wmode
    val RW0_rdata = WireInit(VecInit(ram.map(_.read(RW0.addr))))
    RW0.rdata := RegEnable(RW0_rdata.asUInt, RW0_ren)
    // write with mask
    val RW0_wen = RW0.en && RW0.wmode
    val RW0_wdata = RW0.wdata.asTypeOf(Vec(maskSegments, UInt((width / maskSegments).W)))
    for (i <- 0 until maskSegments) {
      val RW0_wmask = if (RW0.wmask.isDefined) RW0.wmask.get(i) else true.B
      when (RW0_wen && RW0_wmask) {
        ram(i)(RW0.addr) := RW0_wdata(i)
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

class SRAMArray2P(depth: Int, width: Int, maskSegments: Int, hasMbist: Boolean, sramName: Option[String] = None,selectedLen:Int)
  extends SRAMArray(hasMbist, sramName,selectedLen)  {
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

  // read: rdata will keep stable until the next read enable.
  val ram = Seq.fill(maskSegments)(Mem(depth, UInt((width / maskSegments).W)))

  withClock(W0.clk) {
    val W0_data = W0.data.asTypeOf(Vec(maskSegments, UInt((width / maskSegments).W)))
    for (i <- 0 until maskSegments) {
      val W0_mask = if (W0.mask.isDefined) W0.mask.get(i) else true.B
      when(W0.en && W0_mask) {
        ram(i)(W0.addr) := W0_data(i)
      }
    }
  }

  withClock(R0.clk) {
    // RW0_conflict_data will be replaced by width'x in Verilog by scripts.
    val RW0_conflict_data = Wire(UInt((width / maskSegments).W))
    RW0_conflict_data := ((1L << (width / maskSegments)) - 1).U
    // DontTouch RW0_conflict_data to force Chisel not to optimize it out.
    dontTouch(RW0_conflict_data)
    val R0_data = VecInit((0 until maskSegments).map(i => {
      // To align with the real memory model, R0.data should be width'x when R0 and W0 have conflicts.
      val wmask = if (W0.mask.isDefined) W0.mask.get(i) else true.B
      val RW0_conflict_REG = RegEnable(W0.en && wmask && R0.addr === W0.addr, R0.en)
      val data_REG = RegEnable(ram(i).read(R0.addr), R0.en)
      // The read data naturally holds when not enabled.
      Mux(RW0_conflict_REG, RW0_conflict_data, data_REG)
    })).asUInt
    R0.data := R0_data
    // write with mask
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

object SRAMArray {
  private val instances = ListBuffer.empty[(Boolean, Int, Int, Int, Boolean, Boolean)]

  def apply(clock: Clock, singlePort: Boolean, depth: Int, width: Int,
            maskSegments: Int = 1,
            MCP: Boolean = false,
            writeClock: Option[Clock] = None,
            hasMbist: Boolean,
            selectedLen:Int
           ): (SRAMArray,String) = {
    val sram_key = (singlePort, depth, width, maskSegments, MCP, hasMbist)
    if (!instances.contains(sram_key)) {
      instances += sram_key
    }
    val mcpPrefix = if (MCP) "_multicycle" else ""
    val numPort = if (singlePort) 1 else 2
    val maskWidth = width / maskSegments
    val sramName = Some(s"sram_array_${numPort}p${depth}x${width}m$maskWidth$mcpPrefix")
    val array = if (singlePort) {
      Module(new SRAMArray1P(depth, width, maskSegments, hasMbist, sramName,selectedLen))
    } else {
      Module(new SRAMArray2P(depth, width, maskSegments, hasMbist, sramName,selectedLen))
    }
    array.init(clock, writeClock)
    (array,sramName.get)
  }
}

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt):SRAMBundleA = {
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

  def apply(valid: Bool, setIdx: UInt):SRAMReadBus[T] = {
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
  private var nodeId = 0
  private var wrapperId = 0
  private var domainId = 0
  private var firstToBeBroadCast = 0

  def getWayNumForEachNodeAndNodeNum_1toN(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val dataNum1toNNode = mw / dw
    val numVec = (1 until dataNum1toNNode + 1)
    val validVec = numVec.map(num => (way % num == 0) && (way >= num))
    val validNum = numVec.zip(validVec).filter(_._2)
    val res = if(validNum.isEmpty) (1,way) else validNum.last
    (res._1, way / res._1)
  }

  def getDivisor(in:Int):Seq[Int] = {
    val end = sqrt(in).toInt
    val divisors = Seq.tabulate(end)(_+1).map(idx => (in % idx == 0, Seq(idx,in/idx))).filter(_._1).flatMap(_._2).sorted
    divisors
  }

  def getNodeNumForEachWayAndNodeNum_Nto1(dw: Int, way: Int, mw: Int): (Int, Int) = {
    val divisors = getDivisor(dw)
    val validDivisors = divisors.filter(_<=mw)
    val goodNodeNumForEachWay = dw / validDivisors.max
    val defaultNodeNumForEachWay = ((dw + mw - 1) / mw)
    val finalNodeNumForEachWay = if(goodNodeNumForEachWay > 4 * defaultNodeNumForEachWay) defaultNodeNumForEachWay else goodNodeNumForEachWay
    (finalNodeNumForEachWay, way * finalNodeNumForEachWay)
  }

  def restartIndexing():Unit = domainId = 0

  def getDomainID():Int = domainId

  def increaseDomainID(add:Int):Unit = domainId += add

  def addBroadCastBundleSink(bd:BroadCastBundle):Unit = {
    for((sigName,wires) <- bd.elements){
      BoringUtils.addSink(wires,s"sram_${wrapperId}_${sigName}")
    }
  }
  def genBroadCastBundleTop():BroadCastBundle = {
    val connectors = Seq.fill(wrapperId - firstToBeBroadCast)(Wire(new BroadCastBundle))
    for((con,idx) <- connectors.zipWithIndex){
      con := DontCare
      dontTouch(con)
      for((sigName,wires) <- con.elements){
        BoringUtils.addSource(wires,s"sram_${idx + firstToBeBroadCast}_${sigName}")
      }
    }
    val res = Wire(new BroadCastBundle)
    dontTouch(res)
    connectors.foreach(_:=res)
    firstToBeBroadCast = firstToBeBroadCast + connectors.length
    res
  }
}

// WARNING: this SRAMTemplate assumes the SRAM lib itself supports holdRead.
class SRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1, singlePort: Boolean = false,
  shouldReset: Boolean = false, holdRead:Boolean = false,
  extraReset: Boolean = false, bypassWrite:Boolean = false,
  // multi-cycle path
  clk_div_by_2: Boolean = false,
  // mbist support
  hasMbist: Boolean = true, hasShareBus: Boolean = false,
  maxMbistDataWidth: Int = 256, parentName:String = s"Unknown",
  bitWrite:Boolean = false, foundry:String = "Unkown",
  sramInst:String = "STANDARD"
  ) extends Module {

  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None
  val broadCastSignals = Wire(new BroadCastBundle)
  val implementSinglePort = singlePort
  val isBypassWriteLegal = if(implementSinglePort) true else bypassWrite
  require(isBypassWriteLegal, "Dual port SRAM MUST implement bypass write!")

  broadCastSignals := DontCare
  dontTouch(broadCastSignals)
  if(hasMbist) SRAMTemplate.addBroadCastBundleSink(broadCastSignals)
  wrapperId += 1

  val clkGate = if(clk_div_by_2) Some(Module(new MBISTClockGateCell)) else None
  if(clk_div_by_2){
    clkGate.get.clock := clock
    clkGate.get.reset := reset
    clkGate.get.dft.cgen := broadCastSignals.cgen
    clkGate.get.dft.l3dataram_clk := broadCastSignals.l3dataram_clk
    clkGate.get.dft.l3dataramclk_bypass := broadCastSignals.l3dataramclk_bypass
  }
  val master_clock = if(clk_div_by_2) clkGate.get.out_clock else clock

  val isNto1 = gen.getWidth > maxMbistDataWidth

  /*************implement mbist interface node(multiple nodes for one way)********/

  val (mbistNodeNumForEachWay,mbistNodeNumNto1) = SRAMTemplate.getNodeNumForEachWayAndNodeNum_Nto1(gen.getWidth,way,maxMbistDataWidth)
  val maskWidthNto1 = 1
  val mbistDataWidthNto1 = (gen.getWidth + mbistNodeNumForEachWay - 1) / mbistNodeNumForEachWay
  /*************implement mbist interface node(one node for multiple ways)********/

  val (wayNumForEachNode, mbistNodeNum1toN) = SRAMTemplate.getWayNumForEachNodeAndNodeNum_1toN(gen.getWidth, way, maxMbistDataWidth)
  val mbistDataWidth1toN = wayNumForEachNode * gen.getWidth
  val maskWidth1toN = wayNumForEachNode
  /**************************************add nodes to global************************************/
  val myNodeNum = if (isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
  val myDataWidth = if (isNto1) mbistDataWidthNto1 else mbistDataWidth1toN
  val myMaskWidth = if (isNto1) maskWidthNto1 else maskWidth1toN
  val myArrayIds = Seq.tabulate(myNodeNum)(idx => SRAMTemplate.getDomainID() + idx)
  val (array,vname) = SRAMArray(master_clock, implementSinglePort, set, way * gen.getWidth, way, MCP = clk_div_by_2,
    hasMbist = hasMbist,selectedLen = if(hasMbist && hasShareBus) myNodeNum else 0)
  val myNodeParam = RAM2MBISTParams(set, myDataWidth,myMaskWidth,implementSinglePort,vname,parentName,myNodeNum,myArrayIds.max,bitWrite,foundry,sramInst)
  val sram_prefix = "sram_" + nodeId + "_"
  val myMbistBundle = Wire(new RAM2MBIST(myNodeParam))
  myMbistBundle := DontCare
  if(hasMbist && hasShareBus) {
    dontTouch(myMbistBundle)
  }

  /*******************************connection between mbist and sram*******************************/
  val mbistSelected       = RegNext(myMbistBundle.selectedOH.orR,0.U)
  val mbistArray          = RegEnable(myMbistBundle.array,0.U,myMbistBundle.selectedOH.orR)
  val mbistAddr           = myMbistBundle.addr
  val mbistAddrRead       = myMbistBundle.addr_rd
  val mbistWriteData      = Fill(myNodeNum,myMbistBundle.wdata)
  val mbistReadEn         = myMbistBundle.re
  val mbistWriteEn        = myMbistBundle.we
  val mbistWMask          = if (isNto1) Fill(way,myMbistBundle.wmask) else Fill(myNodeNum,myMbistBundle.wmask)
  val mbistFuncSel        = myMbistBundle.ack
  /********************************************************************************************/
  val wordType = UInt(gen.getWidth.W)

  if (hasMbist && hasShareBus) {
    MBIST.addRamNode(myMbistBundle, sram_prefix, myArrayIds)
    if(clk_div_by_2){
      clkGate.get.mbist.req := myMbistBundle.ack
      clkGate.get.mbist.writeen := myMbistBundle.we
      clkGate.get.mbist.readen := myMbistBundle.re
    }
    val addId = if (isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
    nodeId += addId
    SRAMTemplate.increaseDomainID(addId)
    array.mbist.get.selectedOH := myMbistBundle.selectedOH & (!broadCastSignals.ram_hold)
  }
  else{
    if(clk_div_by_2){
      clkGate.get.mbist.req := false.B
      clkGate.get.mbist.writeen := false.B
      clkGate.get.mbist.readen := false.B
    }
    array.mbist.get.selectedOH := DontCare
  }
  if(hasMbist) {
    MBIST.noDedup(this)
    array.mbist.get.dft_ram_bp_clken := broadCastSignals.ram_bp_clken
    array.mbist.get.dft_ram_bypass := broadCastSignals.ram_bypass
  }

  withClockAndReset(master_clock, reset) {
    val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))
    if (shouldReset) {
      val _resetState = RegInit(true.B)
      val (_resetSet, resetFinish) = Counter(_resetState, set)
      when(resetFinish) {
        _resetState := false.B
      }
      if (extra_reset.isDefined) {
        when(extra_reset.get) {
          _resetState := true.B
        }
      }

      resetState := _resetState
      resetSet := _resetSet
    }
    val needBypass = io.w.req.valid && io.r.req.valid && (io.w.req.bits.setIdx === io.r.req.bits.setIdx)
    val ren = if(implementSinglePort) io.r.req.valid else (!needBypass) & io.r.req.valid
    val wen = io.w.req.valid || resetState
    //  val realRen = (if (implementSinglePort) ren && !wen else ren)

    val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
    val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
    val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))

    val finalWriteSetIdx = if (hasMbist && hasShareBus) Mux(mbistFuncSel, mbistAddr, setIdx) else setIdx
    val finalReadSetIdx = if (hasMbist && hasShareBus) Mux(mbistFuncSel, mbistAddrRead, io.r.req.bits.setIdx) else io.r.req.bits.setIdx
    val finalWen = if (hasMbist && hasShareBus) Mux(mbistFuncSel, mbistWriteEn, wen) else wen
    val finalRen = if (hasMbist && hasShareBus) Mux(mbistFuncSel, mbistReadEn, ren) else ren
    val finalWmask = if (hasMbist && hasShareBus) Mux(mbistFuncSel, mbistWMask, waymask) else waymask
    val finalWriteData = if (hasMbist && hasShareBus) Mux(mbistFuncSel, mbistWriteData.asTypeOf(wdata), wdata) else wdata

    val toSRAMRen = if (implementSinglePort) (finalRen && !finalWen) else finalRen

    val raw_rdata = array.read(finalReadSetIdx, toSRAMRen).asTypeOf(Vec(way, wordType))
    when(finalWen) {
      array.write(finalWriteSetIdx, finalWriteData.asUInt, finalWmask)
    }

    // bypass for dual-port SRAMs
    if (!bypassWrite && !singlePort) {
      println(s"ERROR: 2-port SRAM $parentName does not enable bypassWrite. Please check it!!!\n")
      assert(!(ren && wen && io.r.req.bits.setIdx === io.w.req.bits.setIdx))
    }
    // force bypass write for implemented dual-port SRAMs
    val implementBypassWrite = !implementSinglePort && (bypassWrite || singlePort)

    def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt, isDoingMbist: Bool): UInt = {
      val need_check = RegNext(ren && wen)
      val waddr_reg = RegNext(waddr)
      val raddr_reg = RegNext(raddr)
      require(wmask.getWidth == way)
      val bypass = Mux(isDoingMbist,
        Fill(way, false.B),
        Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
      )
      bypass.asTypeOf(UInt(way.W))
    }

    val bypass_wdata = if (implementBypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
    else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
    val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx, mbistFuncSel)
    val mem_rdata = {
      if (implementSinglePort) raw_rdata
      else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
        case ((m, r), w) => Mux(m, w, r)
      })
    }

    // hold read data for SRAMs
    val rdata =
      if (clk_div_by_2) {
        mem_rdata
      } else if (holdRead) {
        HoldUnless(mem_rdata, RegNext(toSRAMRen))
      } else {
        mem_rdata
      }

    if (clk_div_by_2) {
      CustomAnnotations.annotateClkDivBy2(this)
    }
    if (!isPow2(set)) {
      CustomAnnotations.annotateSpecialDepth(this)
    }

    io.r.resp.data := rdata.map(_.asTypeOf(gen))
    io.r.req.ready := !resetState && (if (implementSinglePort) !wen else true.B)
    io.w.req.ready := true.B

    /** ***********************************mbist rdata output************************************************* */
    val nodeSelected = myArrayIds.map(_.U === mbistArray)
    val rdataInUIntHold = RegEnable(rdata.asUInt, 0.U, mbistSelected(0) && RegNext(toSRAMRen | needBypass, false.B))
    val rdataFitToNodes = Seq.tabulate(myNodeNum)(idx => {
      val highIdx = Seq(idx * myDataWidth + myDataWidth - 1, rdata.getWidth - 1).min
      rdataInUIntHold(highIdx, idx * myDataWidth)
    })
    myMbistBundle.rdata := ParallelMux(nodeSelected zip rdataFitToNodes)

    /** ****************************************************************************************************** */
  }
}

class FoldedSRAMTemplate[T <: Data]
(
  gen: T, set: Int, width: Int = 4, way: Int = 1,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  holdRead: Boolean = false, singlePort: Boolean = false, bypassWrite: Boolean = false,
  hasMbist:Boolean = true, hasShareBus:Boolean = false, parentName:String = "Unknown"
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None
  //   |<----- setIdx ----->|
  //   | ridx | width | way |

  require(width > 0 && isPow2(width))
  require(way > 0 && isPow2(way))
  require(set % width == 0)

  val nRows = set / width

  val array = Module(new SRAMTemplate(gen, set=nRows, way=width*way,
    shouldReset=shouldReset, extraReset=extraReset, holdRead=holdRead, singlePort=singlePort,
    hasMbist = hasMbist, hasShareBus = hasShareBus, parentName = parentName
  ))
  if (array.extra_reset.isDefined) {
    array.extra_reset.get := extra_reset.get
  }

  io.r.req.ready := array.io.r.req.ready
  io.w.req.ready := array.io.w.req.ready

  val raddr = io.r.req.bits.setIdx >> log2Ceil(width)
  val ridx = RegNext(if (width != 1) io.r.req.bits.setIdx(log2Ceil(width)-1, 0) else 0.U(1.W))
  val ren  = io.r.req.valid

  array.io.r.req.valid := ren
  array.io.r.req.bits.setIdx := raddr

  val rdata = array.io.r.resp.data
  for (w <- 0 until way) {
    val wayData = VecInit(rdata.indices.filter(_ % way == w).map(rdata(_)))
    val holdRidx = HoldUnless(ridx, RegNext(io.r.req.valid))
    val realRidx = if (holdRead) holdRidx else ridx
    io.r.resp.data(w) := Mux1H(UIntToOH(realRidx, width), wayData)
  }

  val wen = io.w.req.valid
  val wdata = VecInit(Seq.fill(width)(io.w.req.bits.data).flatten)
  val waddr = io.w.req.bits.setIdx >> log2Ceil(width)
  val widthIdx = if (width != 1) io.w.req.bits.setIdx(log2Ceil(width)-1, 0) else 0.U
  val wmask = (width, way) match {
    case (1, 1) => 1.U(1.W)
    case (x, 1) => UIntToOH(widthIdx)
    case _      => VecInit(Seq.tabulate(width*way)(n => (n / way).U === widthIdx && io.w.req.bits.waymask.get(n % way))).asUInt
  }
  require(wmask.getWidth == way*width)

  array.io.w.apply(wen, wdata, waddr, wmask)
}
