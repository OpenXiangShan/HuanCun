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

import scala.collection.mutable.ListBuffer
import scala.math.sqrt

object SramType extends Enumeration {
  val hd2prf = Value(0,"hd2prf")
  val hsuspsr = Value(1,"hsuspsr")
  val uhdusplr = Value(2,"uhdusplr")
  val hduspsr = Value(3,"hduspsr")
}

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool, init: Option[T] = None): T = {
    val hold_data = if (init.isDefined) RegEnable(x, init.get, en) else RegEnable(x, en)
    Mux(en, x, hold_data)
  }
}

class SRAMMbistIO(sramType:Int,selectedLen:Int,hasRepair:Boolean) extends Bundle {
  val hd2prf_trim_fuse = Input(UInt(if(sramType == SramType.hd2prf.id) 11.W else 0.W))
  val hd2prf_sleep_fuse = Input(UInt(if(sramType == SramType.hd2prf.id) 2.W else 0.W))
  val hsuspsr_trim_fuse = Input(UInt(if(sramType == SramType.hsuspsr.id) 20.W else 0.W))
  val hsuspsr_sleep_fuse = Input(UInt(if(sramType == SramType.hsuspsr.id) 2.W else 0.W))
  val uhdusplr_trim_fuse = Input(UInt(if(sramType == SramType.uhdusplr.id) 20.W else 0.W))
  val uhdusplr_sleep_fuse = Input(UInt(if(sramType == SramType.uhdusplr.id) 2.W else 0.W))
  val hduspsr_trim_fuse = Input(UInt(if(sramType == SramType.hduspsr.id) 20.W else 0.W))
  val hduspsr_sleep_fuse = Input(UInt(if(sramType == SramType.hduspsr.id) 2.W else 0.W))

  val selectedOH = Input(UInt(selectedLen.W))

  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
  val clkungate = Input(Bool())

  val IP_RESET_B = Input(Bool())
  val WRAPPER_RD_CLK_EN = Input(UInt((if(sramType == SramType.hd2prf.id) 1 else 0).W))
  val WRAPPER_WR_CLK_EN = Input(UInt((if(sramType == SramType.hd2prf.id) 1 else 0).W))
  val WRAPPER_CLK_EN = Input(UInt((if(sramType != SramType.hd2prf.id) 1 else 0).W))
  val OUTPUT_RESET = Input(Bool())

  val PWR_MGNT_IN = Input(UInt((if(sramType != SramType.hd2prf.id) 6 else 5).W))
  val PWR_MGNT_OUT = Output(UInt(1.W))

  val bisr_shift_en = Input(UInt((if(hasRepair) 1 else 0).W))
  val bisr_clock = Input(UInt((if(hasRepair) 1 else 0).W))
  val bisr_reset = Input(UInt((if(hasRepair) 1 else 0).W))
  val bisr_scan_in = Input(UInt((if(hasRepair) 1 else 0).W))
  val bisr_scan_out = Output(UInt((if(hasRepair) 1 else 0).W))
}

abstract class SRAMArray(hasMbist: Boolean, sramName: Option[String] = None,sramType:Int,hasRepair:Boolean,selectedLen:Int) extends RawModule {
  val mbist = if (hasMbist) Some(IO(new SRAMMbistIO(sramType,selectedLen,hasRepair))) else None
  if (mbist.isDefined) {
    mbist.get.PWR_MGNT_OUT := DontCare
    mbist.get.bisr_scan_out := DontCare
    dontTouch(mbist.get)
  }
  val repair = if (hasMbist && hasRepair) Some(IO(new RepairBundle)) else None
  if (repair.isDefined) {
    dontTouch(repair.get)
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

class SRAMArray1P(depth: Int, width: Int, maskSegments: Int, hasMbist: Boolean, sramName: Option[String] = None,sramType:Int,hasRepair:Boolean,selectedLen:Int)
  extends SRAMArray(hasMbist, sramName,sramType,hasRepair,selectedLen) {
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

class SRAMArray2P(depth: Int, width: Int, maskSegments: Int, hasMbist: Boolean, sramName: Option[String] = None,sramType:Int,hasRepair:Boolean,selectedLen:Int)
  extends SRAMArray(hasMbist, sramName,sramType,hasRepair,selectedLen)  {
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
  }

  // write with mask
  withClock(W0.clk) {
    val W0_data = W0.data.asTypeOf(Vec(maskSegments, UInt((width / maskSegments).W)))
    for (i <- 0 until maskSegments) {
      val W0_mask = if (W0.mask.isDefined) W0.mask.get(i) else true.B
      when (W0.en && W0_mask) {
        ram(i)(W0.addr) := W0_data(i)
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

object SRAMArray {
  private val instances = ListBuffer.empty[(Boolean, Int, Int, Int, Boolean, Boolean, Boolean)]

  def apply(clock: Clock, singlePort: Boolean, depth: Int, width: Int,
            maskSegments: Int = 1,
            MCP: Boolean = false,
            writeClock: Option[Clock] = None,
            hasMbist: Boolean,
            sramType:Int,
            hasRepair:Boolean,
            selectedLen:Int
           ): (SRAMArray,String) = {
    val sram_key = (singlePort, depth, width, maskSegments, MCP, hasMbist, hasRepair)
    if (!instances.contains(sram_key)) {
      instances += sram_key
    }
    val sram_index = instances.indexOf(sram_key)
    val mcpPrefix = if (MCP) "_multicycle" else ""
    val repair = if(hasRepair) "_repair" else ""
    val numPort = if (singlePort) 1 else 2
    val maskWidth = width / maskSegments
    val sramName = Some(s"sram_array_${numPort}p${depth}x${width}m$maskWidth$mcpPrefix$repair")
    val array = if (singlePort) {
      Module(new SRAMArray1P(depth, width, maskSegments, hasMbist, sramName,sramType,hasRepair,selectedLen))
    } else {
      Module(new SRAMArray2P(depth, width, maskSegments, hasMbist, sramName,sramType,hasRepair,selectedLen))
    }
    array.init(clock, writeClock)
    (array,sramName.get)
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
  private var mbistArrayRfId = 0
  private var mbistArraySramId = 0
  var sramRepairSerialSignals = Seq[String]()
  var sramNonRepairSerialSignals = Seq[String]()
  var rfRepairSerialSignals = Seq[String]()
  var rfNonRepairSerialSignals = Seq[String]()

  var bisrSerialSignals = Seq[String]()

  private val domainName = Seq(
    "_L3_slice0_", "_L3_slice1_", "_L3_slice2_", "_L3_slice3_",
    "_L2_", "_XSCore_"
  )

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

  def restartIndexing(isSRAM:Boolean):Unit={
    if(isSRAM)
      mbistArraySramId = 0
    else
      mbistArrayRfId = 0
  }

  def getID(isSRAM:Boolean):Int = {
    if(isSRAM)
      mbistArraySramId
    else
      mbistArrayRfId
  }

  def increaseID(add:Int,isSRAM:Boolean):Unit = {
    if(isSRAM)
      mbistArraySramId += add
    else
      mbistArrayRfId += add
  }

  def uniqueID:Int = uniqueId

  def getDomainName(parentName:String) = {
    val matchSeq = domainName.map(str=>(str,parentName.contains(str)))
    val res = matchSeq.filter(_._2).head._1.init.tail
    res
  }

  def doAddSink(srcSeq:Seq[String],signal:UInt,signalName:String):Seq[String]= {
    if(srcSeq.isEmpty){
      WiringUtils.addSink(signal,signalName)
      Seq(signalName)
    }else{
      WiringUtils.addSink(signal,srcSeq.last)
      srcSeq.init
    }
  }
  def doAddSource(srcSeq:Seq[String],signal:UInt,signalName:String):Seq[String]= {
    WiringUtils.addSource(signal,signalName)
    srcSeq :+ signalName
  }
  def addSinkPWRMGNTSignal(signal:UInt,signalName:String,isSRAM:Boolean,hasRepair:Boolean):Unit = {
    (isSRAM,hasRepair) match {
      case(false,false) => rfNonRepairSerialSignals = doAddSink(rfNonRepairSerialSignals,signal,signalName)
      case(false,true)  => rfRepairSerialSignals = doAddSink(rfRepairSerialSignals,signal,signalName)
      case(true,false)  => sramNonRepairSerialSignals = doAddSink(sramNonRepairSerialSignals,signal,signalName)
      case(true,true)   => sramRepairSerialSignals = doAddSink(sramRepairSerialSignals,signal,signalName)
    }
  }
  def addSourcePWRMGNTSignal(signal:UInt,signalName:String,isSRAM:Boolean,hasRepair:Boolean):Unit = {
    (isSRAM,hasRepair) match {
      case(false,false) => rfNonRepairSerialSignals = doAddSource(rfNonRepairSerialSignals,signal,signalName)
      case(false,true)  => rfRepairSerialSignals = doAddSource(rfRepairSerialSignals,signal,signalName)
      case(true,false)  => sramNonRepairSerialSignals = doAddSource(sramNonRepairSerialSignals,signal,signalName)
      case(true,true)   => sramRepairSerialSignals = doAddSource(sramRepairSerialSignals,signal,signalName)
    }
  }

  def addSinkBisrSignal(signal:UInt,signalName:String):Unit = {
    bisrSerialSignals = doAddSink(bisrSerialSignals,signal,signalName)
  }

  def addSourceBisrSignal(signal:UInt,signalName:String):Unit = {
    bisrSerialSignals = doAddSource(bisrSerialSignals,signal,signalName)
  }

  def getAndClearPWRMGNT(isSRAM:Boolean,hasRepair:Boolean):(String,String) = {
    (isSRAM,hasRepair) match {
      case(false,false) => {
        val res = (rfNonRepairSerialSignals.head,rfNonRepairSerialSignals.last)
        rfNonRepairSerialSignals = Seq()
        res
      }
      case(false,true)  => {
        val res = (rfRepairSerialSignals.head,rfRepairSerialSignals.last)
        rfRepairSerialSignals = Seq()
        res
      }
      case(true,false)  => {
        val res = (sramNonRepairSerialSignals.head,sramNonRepairSerialSignals.last)
        sramNonRepairSerialSignals = Seq()
        res
      }
      case(true,true)   => {
        val res = (sramRepairSerialSignals.head,sramRepairSerialSignals.last)
        sramRepairSerialSignals = Seq()
        res
      }
    }
  }

  def getAndClearBisr():(String,String) = {
    val res = (bisrSerialSignals.head,bisrSerialSignals.last)
    bisrSerialSignals = Seq()
    res
  }

  //(depth,width,mask)
  private val RfTable = Seq(
    (128,   232,  8),
    (128,   116,  4),
    (128,   1024, 2),
    (128,   256,  8),
    (128,   128,  4),
    (256,   24,   2),
    (2048,  4,    2),
    (256,   24,   4),
    (128,   50,   1),
    (128,   100,  2),
    (64,    236,  1),
    (64,    256,  1),
    (32,    548,  2),
    (128,   1380, 4),
    (256,   64,   1),
    (256,   208,  8),
    (128,   83,   1),
    (256,   13,   1),
    (128,   80,   10),
    (128,   230,  10),
    (128,   60,   10),
    (32,    148,  2)
  )
  private val hdTable = Seq(
    (256,   48,  1),
    (256,   8,   8),
    (256,   16,  16)
  )

  private val uhdTable = Seq(
    (2048,   64,  1),
    (2048,   32,  1)
  )
  def isRF(depth:Int,width:Int,mask:Int):Boolean = {
    RfTable.contains((depth,width,mask))
  }

  def getSramType(depth:Int,width:Int,mask:Int):Int = {
    val res = if(RfTable.contains((depth,width,mask)))
      SramType.hd2prf.id
    else if(hdTable.contains((depth,width,mask)))
      SramType.hduspsr.id
    else if(uhdTable.contains((depth,width,mask)))
      SramType.uhdusplr.id
    else
      SramType.hsuspsr.id
    res
  }

}

// WARNING: this SRAMTemplate assumes the SRAM lib itself supports holdRead.
class SRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1, singlePort: Boolean = false,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  bypassWrite: Boolean = false,
  // multi-cycle path
  clk_div_by_2: Boolean = false,
  // mbist support
  hasMbist: Boolean = true, maxMbistDataWidth: Int = 256,
  hasRepair:Boolean = false, parentName:String = s"Unknown",
  bitWrite:Boolean = false,
  foundry:String = "UNKNOWN",
  sramInst:String = "STANDARD"
  ) extends Module {

  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None

  val isNto1 = gen.getWidth > maxMbistDataWidth
  val isRF = if (true) SRAMTemplate.isRF(set,gen.getWidth * way,way) else !singlePort
  val myRamType = SRAMTemplate.getSramType(set,gen.getWidth * way,way)
  val implementSinglePort = if(isRF) false else singlePort

  /*************implement mbist interface node(multiple nodes for one way)********/

  val (mbistNodeNumForEachWay,mbistNodeNumNto1) = SRAMTemplate.getNodeNumForEachWayAndNodeNum_Nto1(gen.getWidth,way,maxMbistDataWidth)
  val maskWidthNto1 = 1
  val mbistDataWidthNto1 = (gen.getWidth + mbistNodeNumForEachWay - 1) / mbistNodeNumForEachWay
  /*************implement mbist interface node(one node for multiple ways)********/

  val (wayNumForEachNode, mbistNodeNum1toN) = SRAMTemplate.getWayNumForEachNodeAndNodeNum_1toN(gen.getWidth, way, maxMbistDataWidth)
  val mbistDataWidth1toN = wayNumForEachNode * gen.getWidth
  val maskWidth1toN = wayNumForEachNode
  /**************************************add nodes to global************************************/
  val sramClk = Wire(Clock())
  val myNodeNum = if (isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
  val myDataWidth = if (isNto1) mbistDataWidthNto1 else mbistDataWidth1toN
  val myMaskWidth = if (isNto1) maskWidthNto1 else maskWidth1toN
  val myArrayIds = Seq.tabulate(myNodeNum)(idx => SRAMTemplate.getID(!isRF) + idx)
  val (array,vname) = SRAMArray(sramClk, implementSinglePort, set, way * gen.getWidth, way, MCP = clk_div_by_2, hasMbist = hasMbist,sramType = myRamType,hasRepair = hasRepair,selectedLen = myNodeNum)
  val myNodeParam = RAM2MBISTParams(set, myDataWidth,myMaskWidth,implementSinglePort,vname,parentName,myRamType,myNodeNum,myArrayIds.max,bitWrite,foundry,sramInst)
  val sram_prefix = "sram_" + uniqueId + "_"
  val myMbistBundle = Wire(new RAM2MBIST(myNodeParam))
  dontTouch(myMbistBundle)
  myMbistBundle := DontCare
  if (hasMbist) {
    MBIST.addRamNode(myMbistBundle,sram_prefix,myArrayIds, !isRF,hasRepair)
    MBIST.noDedup(this)
  }

  val realSramClk = if(clk_div_by_2){
    val CG = Module(new MBISTClockGateCell)
    CG.clock := clock
    CG.reset := reset
    if(hasMbist){
      CG.fscan_clkungate := myMbistBundle.clkungate
      CG.mbist.req := myMbistBundle.ack
      CG.mbist.writeen := myMbistBundle.we
      CG.mbist.readen := myMbistBundle.re
    } else {
      CG.fscan_clkungate := false.B
      CG.mbist.req := false.B
      CG.mbist.writeen := false.B
      CG.mbist.readen := false.B
    }
    CG.out_clock
  } else clock
  sramClk := realSramClk

  val PWR_MGNT_IN0 = if(hasMbist) Some(Wire(UInt(1.W))) else None
  val PWR_MGNT_OUT = if(hasMbist) Some(Wire(UInt(1.W))) else None
  if(hasMbist) {
    PWR_MGNT_IN0.get := DontCare
    dontTouch(PWR_MGNT_IN0.get)
    dontTouch(PWR_MGNT_OUT.get)
    SRAMTemplate.addSinkPWRMGNTSignal(PWR_MGNT_IN0.get, s"sink_PWR_MGNT_${uniqueId}", !isRF, hasRepair)
    SRAMTemplate.addSourcePWRMGNTSignal(PWR_MGNT_OUT.get, s"source_PWR_MGNT_${uniqueId}", !isRF, hasRepair)
  }
  val bisr_scan_in = if(hasMbist && hasRepair) Some(Wire(UInt(1.W))) else None
  val bisr_scan_out = if(hasMbist && hasRepair) Some(Wire(UInt(1.W))) else None
  if(hasRepair && hasMbist){
    bisr_scan_in.get := DontCare
    dontTouch(bisr_scan_in.get)
    dontTouch(bisr_scan_out.get)
    SRAMTemplate.addSinkBisrSignal(bisr_scan_in.get, s"sink_bisr_scan_${uniqueId}")
    SRAMTemplate.addSourceBisrSignal(bisr_scan_out.get, s"source_bisr_scan_${uniqueId}")
  }

  if(hasMbist){
    val addId = if (isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
    uniqueId += addId
    SRAMTemplate.increaseID(addId, !isRF)
  }

  /*******************************connection between mbist and sram*******************************/
  val mbistSelected       = RegEnable(myMbistBundle.selectedOH.orR, 0.U, myMbistBundle.ack)
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

  if(hasMbist) {
    array.mbist.get.WRAPPER_RD_CLK_EN := myMbistBundle.WRAPPER_RD_CLK_EN
    array.mbist.get.WRAPPER_WR_CLK_EN := myMbistBundle.WRAPPER_WR_CLK_EN
    array.mbist.get.WRAPPER_CLK_EN := myMbistBundle.WRAPPER_CLK_EN
    array.mbist.get.IP_RESET_B := myMbistBundle.IP_RESET_B
    array.mbist.get.OUTPUT_RESET := myMbistBundle.OUTPUT_RESET

    array.mbist.get.hd2prf_trim_fuse := myMbistBundle.hd2prf_trim_fuse
    array.mbist.get.hd2prf_sleep_fuse := myMbistBundle.hd2prf_sleep_fuse
    array.mbist.get.hsuspsr_trim_fuse := myMbistBundle.hsuspsr_trim_fuse
    array.mbist.get.hsuspsr_sleep_fuse := myMbistBundle.hsuspsr_sleep_fuse
    array.mbist.get.uhdusplr_trim_fuse := myMbistBundle.uhdusplr_trim_fuse
    array.mbist.get.uhdusplr_sleep_fuse := myMbistBundle.uhdusplr_sleep_fuse
    array.mbist.get.hduspsr_trim_fuse := myMbistBundle.hduspsr_trim_fuse
    array.mbist.get.hduspsr_sleep_fuse := myMbistBundle.hduspsr_sleep_fuse

    array.mbist.get.selectedOH := myMbistBundle.selectedOH

    array.mbist.get.bypsel := myMbistBundle.bypsel
    array.mbist.get.wdis_b := myMbistBundle.wdis_b
    array.mbist.get.rdis_b := myMbistBundle.rdis_b
    array.mbist.get.init_en := myMbistBundle.init_en
    array.mbist.get.init_val := myMbistBundle.init_val
    array.mbist.get.clkungate := myMbistBundle.clkungate

    array.mbist.get.bisr_clock := myMbistBundle.bisr_clock
    array.mbist.get.bisr_reset := myMbistBundle.bisr_reset
    array.mbist.get.bisr_shift_en := myMbistBundle.bisr_shift_en
    array.mbist.get.bisr_scan_in := DontCare

    array.mbist.get.PWR_MGNT_IN := Cat(myMbistBundle.PWR_MGNT_IN,PWR_MGNT_IN0.get)
    PWR_MGNT_OUT.get := array.mbist.get.PWR_MGNT_OUT

    if (hasRepair) {
      val bd = Wire(new RepairBundle)
      Repair.addRepairNodeToGlobal(bd, parentName)
      array.repair.get <> bd
      bd := DontCare
      dontTouch(bd)
      array.mbist.get.bisr_scan_in := bisr_scan_in.get
      bisr_scan_out.get := array.mbist.get.bisr_scan_out
    }
  }
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }
    if (extra_reset.isDefined) {
      when (extra_reset.get) {
        _resetState := true.B
      }
    }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
//  val realRen = (if (implementSinglePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))

  val finalWriteSetIdx = if(hasMbist) Mux(mbistFuncSel, mbistAddr, setIdx) else setIdx
  val finalReadSetIdx = if(hasMbist) Mux(mbistFuncSel, mbistAddrRead, io.r.req.bits.setIdx) else io.r.req.bits.setIdx
  val finalWen = if(hasMbist) Mux(mbistFuncSel, mbistWriteEn, wen) else wen
  val finalRen = if(hasMbist) Mux(mbistFuncSel, mbistReadEn, ren) else ren
  val finalWmask = if(hasMbist) Mux(mbistFuncSel, mbistWMask, waymask) else waymask
  val finalWriteData = if(hasMbist) Mux(mbistFuncSel, mbistWriteData.asTypeOf(wdata), wdata) else wdata

  val toSRAMRen = if (implementSinglePort) (finalRen && !finalWen) else finalRen

  val raw_rdata = array.read(finalReadSetIdx, toSRAMRen).asTypeOf(Vec(way, wordType))
  when (finalWen) { array.write(finalWriteSetIdx, finalWriteData.asUInt, finalWmask) }

  // bypass for dual-port SRAMs
  if (!bypassWrite && !singlePort) {
    println(s"ERROR: 2-port SRAM $parentName does not enable bypassWrite. Please check it!!!\n")
    assert(!(ren && wen && io.r.req.bits.setIdx === io.w.req.bits.setIdx))
  }
  // force bypass write for implemented dual-port SRAMs
  val implementBypassWrite = !implementSinglePort && (bypassWrite || singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt, isDoingMbist:Bool) : UInt = {
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
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx,mbistFuncSel)
  val mem_rdata = {
    if (implementSinglePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  val rdata = mem_rdata

  if(clk_div_by_2){
    CustomAnnotations.annotateClkDivBy2(this)
  }
  if(!isPow2(set)){
    CustomAnnotations.annotateSpecialDepth(this)
  }

  io.r.resp.data := rdata.map(_.asTypeOf(gen))
  io.r.req.ready := !resetState && (if (implementSinglePort) !wen else true.B)
  io.w.req.ready := true.B

  /*************************************mbist rdata output**************************************************/
  val nodeSelected = myArrayIds.map(_.U === mbistArray)
  val rdata_en = mbistSelected(0) && RegNext(toSRAMRen, false.B)
  val rdataInUIntHold = if(clk_div_by_2){
    val rdata_en_delay1 = RegNext(rdata_en, false.B)
    RegEnable(rdata.asUInt, 0.U, rdata_en_delay1)
  } else {
    RegEnable(rdata.asUInt, 0.U, rdata_en)
  }
  val rdataFitToNodes = Seq.tabulate(myNodeNum)(idx => {
    val highIdx = Seq(idx * myDataWidth + myDataWidth - 1, rdata.getWidth - 1).min
    rdataInUIntHold(highIdx, idx * myDataWidth)
  })
  myMbistBundle.rdata := ParallelMux(nodeSelected zip rdataFitToNodes)
  /*********************************************************************************************************/
}

class FoldedSRAMTemplate[T <: Data]
(
  gen: T, set: Int, width: Int = 4, way: Int = 1,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  parentName:String
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
    shouldReset=shouldReset, extraReset=extraReset, singlePort=singlePort,parentName = parentName))
  if (array.extra_reset.isDefined) {
    array.extra_reset.get := extra_reset.get
  }

  io.r.req.ready := array.io.r.req.ready
  io.w.req.ready := array.io.w.req.ready

  val raddr = io.r.req.bits.setIdx >> log2Ceil(width)
  val ridx = if (width != 1) RegEnable(io.r.req.bits.setIdx(log2Ceil(width)-1, 0), io.r.req.valid) else 0.U(1.W)
  val ren  = io.r.req.valid

  array.io.r.req.valid := ren
  array.io.r.req.bits.setIdx := raddr

  val rdata = array.io.r.resp.data
  for (w <- 0 until way) {
    val wayData = VecInit(rdata.indices.filter(_ % way == w).map(rdata(_)))
    io.r.resp.data(w) := Mux1H(UIntToOH(ridx, width), wayData)
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
