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
import huancun.utils.SRAMTemplate.uniqueId

import scala.collection.mutable.ListBuffer
import scala.math.sqrt

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool, init: Option[T] = None): T = {
    val hold_data = if (init.isDefined) RegEnable(x, init.get, en) else RegEnable(x, en)
    Mux(en, x, hold_data)
  }
}

object ReadAndHold {
  def apply[T <: Data](x: Mem[T], addr:         UInt, en: Bool): T = HoldUnless(x.read(addr), en)
  def apply[T <: Data](x: SyncReadMem[T], addr: UInt, en: Bool): T = HoldUnless(x.read(addr, en), RegNext(en))
}

class SRAMMbistIO(isSRAM:Boolean) extends Bundle {
  val sram_trim_fuse = Input(UInt(if(isSRAM) 20.W else 0.W))
  val sram_sleep_fuse = Input(UInt(if(isSRAM) 2.W else 0.W))
  val rf_trim_fuse = Input(UInt(if(!isSRAM) 11.W else 0.W))
  val rf_sleep_fuse = Input(UInt(if(!isSRAM) 2.W else 0.W))

  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
  val clkungate = Input(Bool())

  val IP_RESET_B = Input(Bool())
  val WRAPPER_RD_CLK_EN = Input(UInt((if(!isSRAM) 1 else 0).W))
  val WRAPPER_WR_CLK_EN = Input(UInt((if(!isSRAM) 1 else 0).W))
  val WRAPPER_CLK_EN = Input(UInt((if(isSRAM) 1 else 0).W))
  val OUTPUT_RESET = Input(Bool())

  val PWR_MGNT_IN = Input(UInt((if(isSRAM) 5 else 4).W))
  val PWR_MGNT_IN0 = Input(UInt(1.W))
  val PWR_MGNT_OUT = Output(UInt(1.W))
}
class RepairIO(hasRepair:Boolean) extends Bundle{
  val rowRepair = Input(UInt(if(hasRepair) 26.W else 0.W))
  val colRepair = Input(UInt(if(hasRepair) 14.W else 0.W))
}

abstract class SRAMArray(hasMbist: Boolean, sramName: Option[String] = None,isSRAM:Boolean,hasRepair:Boolean) extends RawModule {
  val mbist = if (hasMbist) Some(IO(new SRAMMbistIO(isSRAM))) else None
  if (mbist.isDefined) {
    mbist.get.PWR_MGNT_OUT := DontCare
    dontTouch(mbist.get)
  }
  val repair = if (hasMbist) Some(IO(new RepairIO(hasRepair))) else None
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

class SRAMArray1P(depth: Int, width: Int, maskSegments: Int, hasMbist: Boolean, sramName: Option[String] = None,isSRAM:Boolean,hasRepair:Boolean)
  extends SRAMArray(hasMbist, sramName,isSRAM,hasRepair) {
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

class SRAMArray2P(depth: Int, width: Int, maskSegments: Int, hasMbist: Boolean, sramName: Option[String] = None,isSRAM:Boolean,hasRepair:Boolean)
  extends SRAMArray(hasMbist, sramName,isSRAM,hasRepair)  {
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

object SRAMArray {
  private val instances = ListBuffer.empty[(Boolean, Int, Int, Int, Boolean)]

  def apply(clock: Clock, singlePort: Boolean, depth: Int, width: Int,
            maskSegments: Int = 1,
            MCP: Boolean = false,
            writeClock: Option[Clock] = None,
            hasMbist: Boolean,
            isSRAM:Boolean,
            hasRepair:Boolean
           ): (SRAMArray,String) = {
    val sram_key = (singlePort, depth, width, maskSegments, hasMbist)
    if (!instances.contains(sram_key)) {
      instances += sram_key
    }
    val sram_index = instances.indexOf(sram_key)
    val mcpPrefix = if (MCP) "_multiCycle" else ""
    val repair = if(hasRepair) "_repair" else ""
    val numPort = if (singlePort) 1 else 2
    val maskWidth = width / maskSegments
    val sramName = Some(s"sram_array_${sram_index}_${numPort}p${depth}x${width}m$maskWidth$mcpPrefix$repair")
    val array = if (singlePort) {
      Module(new SRAMArray1P(depth, width, maskSegments, hasMbist, sramName,isSRAM,hasRepair))
    } else {
      Module(new SRAMArray2P(depth, width, maskSegments, hasMbist, sramName,isSRAM,hasRepair))
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

  def doAddSink(srcSeq:Seq[String],signal:UInt,signalName:String,info:String):Seq[String]= {
    if(srcSeq.isEmpty){
      BoringUtils.addSink(signal,signalName)
//      println(s"Addding head sink to ${info} Seq: ${signalName}")
      Seq(signalName)
    }else{
      BoringUtils.addSink(signal,srcSeq.last)
//      println(s"Connecting in ${info} Seq: ${srcSeq.last} to ${signalName}")
      srcSeq.init
    }
  }
  def addSinkPWRMGNTSignal(signal:UInt,signalName:String,isSRAM:Boolean,hasRepair:Boolean) = {
    (isSRAM,hasRepair) match {
      case(false,false) => rfNonRepairSerialSignals = doAddSink(rfNonRepairSerialSignals,signal,signalName,"RF_NonRepair")
      case(false,true)  => rfRepairSerialSignals = doAddSink(rfRepairSerialSignals,signal,signalName,"RF_Repair")
      case(true,false)  => sramNonRepairSerialSignals = doAddSink(sramNonRepairSerialSignals,signal,signalName,"Sram_NonRepair")
      case(true,true)   => sramRepairSerialSignals = doAddSink(sramRepairSerialSignals,signal,signalName,"Sram_Repair")
    }
  }
  def doAddSource(srcSeq:Seq[String],signal:UInt,signalName:String,info:String):Seq[String]= {
    BoringUtils.addSource(signal,signalName)
//    println(s"Addding source to ${info} Seq: ${signalName}")
    srcSeq :+ signalName
  }
  def addSourcePWRMGNTSignal(signal:UInt,signalName:String,isSRAM:Boolean,hasRepair:Boolean) = {
    (isSRAM,hasRepair) match {
      case(false,false) => rfNonRepairSerialSignals = doAddSource(rfNonRepairSerialSignals,signal,signalName,"RF_NonRepair")
      case(false,true)  => rfRepairSerialSignals = doAddSource(rfRepairSerialSignals,signal,signalName,"RF_Repair")
      case(true,false)  => sramNonRepairSerialSignals = doAddSource(sramNonRepairSerialSignals,signal,signalName,"Sram_NonRepair")
      case(true,true)   => sramRepairSerialSignals = doAddSource(sramRepairSerialSignals,signal,signalName,"Sram_Repair")
    }
  }
  def getAndClear(isSRAM:Boolean,hasRepair:Boolean):(String,String) = {
    (isSRAM,hasRepair) match {
      case(false,false) => {
        val res = (rfNonRepairSerialSignals.head,rfNonRepairSerialSignals.last)
//        println(s"rfNonRepair is cleared: ${rfNonRepairSerialSignals.toString}")
        rfNonRepairSerialSignals = Seq()
        res
      }
      case(false,true)  => {
        val res = (rfRepairSerialSignals.head,rfRepairSerialSignals.last)
//        println(s"rfRepair is cleared: ${rfRepairSerialSignals.toString}")
        rfRepairSerialSignals = Seq()
        res
      }
      case(true,false)  => {
        val res = (sramNonRepairSerialSignals.head,sramNonRepairSerialSignals.last)
//        println(s"sramNonRepair is cleared: ${sramNonRepairSerialSignals.toString}")
        sramNonRepairSerialSignals = Seq()
        res
      }
      case(true,true)   => {
        val res = (sramRepairSerialSignals.head,sramRepairSerialSignals.last)
//        println(s"sramRepair is cleared: ${sramRepairSerialSignals.toString}")
        sramRepairSerialSignals = Seq()
        res
      }
    }
  }
}

class SRAMTemplate[T <: Data] (
                                gen: T, set: Int, way: Int = 1, singlePort: Boolean = false,
                                shouldReset: Boolean = false, extraReset: Boolean = false,
                                holdRead: Boolean = false, bypassWrite: Boolean = false,
                                // multi-cycle path
                                clk_div_by_2: Boolean = false,
                                // mbist support
                                hasMbist: Boolean = true, maxMbistDataWidth: Int = 256,
                                hasRepair:Boolean = false, parentName:String = s"Unknown"
                              ) extends Module {

  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  val extra_reset = if (extraReset) Some(IO(Input(Bool()))) else None

  val isNto1 = gen.getWidth > maxMbistDataWidth
  val isRF = MBIST.isRF(set,gen.getWidth * way,way)
  val implementSinglePort = if(isRF) false else singlePort
  val (array,vname) = SRAMArray(clock, implementSinglePort, set, way * gen.getWidth, way, MCP = clk_div_by_2, hasMbist = hasMbist,isSRAM = !isRF,hasRepair = hasRepair)
  /*************implement mbist interface node(multiple nodes for one way)********/
  val (mbistNodeNumForEachWay,mbistNodeNumNto1) = SRAMTemplate.getNodeNumForEachWayAndNodeNum_Nto1(gen.getWidth,way,maxMbistDataWidth)
  val maskWidthNto1 = 1
  val mbistDataWidthNto1 = (gen.getWidth + mbistNodeNumForEachWay - 1) / mbistNodeNumForEachWay

  val mbistDataWidthsForEachNode1toNWay = (0 until mbistNodeNumForEachWay).map({idx =>
    if(idx == mbistNodeNumForEachWay - 1 && (gen.getWidth % mbistDataWidthNto1) != 0)
      gen.getWidth - mbistDataWidthNto1 * (mbistNodeNumForEachWay - 1)
    else
      mbistDataWidthNto1
  })

  val mbistNodesNto1: Seq[RAM2MBIST] = (0 until mbistNodeNumNto1).map({idx =>
    val params = RAM2MBISTParams(set, mbistDataWidthsForEachNode1toNWay(idx % mbistNodeNumForEachWay),maskWidthNto1,implementSinglePort,vname,parentName + s"node${idx}",isRF)
    val bd = Wire(new RAM2MBIST(params))
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

  val (wayNumForEachNode, mbistNodeNum1toN) = SRAMTemplate.getWayNumForEachNodeAndNodeNum_1toN(gen.getWidth, way, maxMbistDataWidth)

  val mbistDataWidth1toN = wayNumForEachNode * gen.getWidth
  val maskWidth1toN = wayNumForEachNode
  val mbistNodes1toN: Seq[RAM2MBIST] = (0 until mbistNodeNum1toN).map({idx =>
    val params = RAM2MBISTParams(set, mbistDataWidth1toN, maskWidth1toN,implementSinglePort,vname,parentName + s"node${idx}",isRF)
    val bd = Wire(new RAM2MBIST(params))
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
  if (hasMbist) {
    if (isNto1) {
      mbistNodesNto1.zipWithIndex.foreach({
        case (node, idx) =>
          val sram_prefix = "sram_" + (uniqueId + idx) + "_"
          MBIST.addRamNode(node, sram_prefix, SRAMTemplate.getID(!isRF) + idx, !isRF, hasRepair)
      })
    }
    else {
      mbistNodes1toN.zipWithIndex.foreach({
        case (node, idx) =>
          val sram_prefix = "sram_" + (uniqueId + idx) + "_"
          MBIST.addRamNode(node, sram_prefix, SRAMTemplate.getID(!isRF) + idx, !isRF, hasRepair)
      })
    }
    MBIST.noDedup(this)
  }

  val PWR_MGNT_IN0 = if(hasMbist) Some(Wire(UInt(1.W))) else None
  val PWR_MGNT_OUT = if(hasMbist) Some(Wire(UInt(1.W))) else None
  if(hasMbist) {
    PWR_MGNT_IN0.get := DontCare
    dontTouch(PWR_MGNT_IN0.get)
    dontTouch(PWR_MGNT_OUT.get)
    SRAMTemplate.addSinkPWRMGNTSignal(PWR_MGNT_IN0.get, s"sink_${uniqueId}", !isRF, hasRepair)
    SRAMTemplate.addSourcePWRMGNTSignal(PWR_MGNT_OUT.get, s"source_${uniqueId}", !isRF, hasRepair)
    val addId = if (isNto1) mbistNodeNumNto1 else mbistNodeNum1toN
    uniqueId += addId
    SRAMTemplate.increaseID(addId, !isRF)
  }

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


  val sram_trim_fuse = if(isNto1) mbistNodesNto1.head.sram_trim_fuse else mbistNodes1toN.head.sram_trim_fuse
  val sram_sleep_fuse = if(isNto1) mbistNodesNto1.head.sram_sleep_fuse else mbistNodes1toN.head.sram_sleep_fuse
  val rf_trim_fuse = if(isNto1) mbistNodesNto1.head.rf_trim_fuse else mbistNodes1toN.head.rf_trim_fuse
  val rf_sleep_fuse = if(isNto1) mbistNodesNto1.head.rf_sleep_fuse else mbistNodes1toN.head.rf_sleep_fuse
  val bypsel = if(isNto1) mbistNodesNto1.head.bypsel else mbistNodes1toN.head.bypsel
  val wdis_b = if(isNto1) mbistNodesNto1.head.wdis_b else mbistNodes1toN.head.wdis_b
  val rdis_b = if(isNto1) mbistNodesNto1.head.rdis_b else mbistNodes1toN.head.rdis_b
  val init_en = if(isNto1) mbistNodesNto1.head.init_en else mbistNodes1toN.head.init_en
  val init_val = if(isNto1) mbistNodesNto1.head.init_val else mbistNodes1toN.head.init_val
  val clkungate = if(isNto1) mbistNodesNto1.head.clkungate else mbistNodes1toN.head.clkungate

  val IP_RESET_B = if(isNto1) mbistNodesNto1.head.IP_RESET_B else mbistNodes1toN.head.IP_RESET_B
  val WRAPPER_RD_CLK_EN = if(isNto1) mbistNodesNto1.head.WRAPPER_RD_CLK_EN else mbistNodes1toN.head.WRAPPER_RD_CLK_EN
  val WRAPPER_WR_CLK_EN = if(isNto1) mbistNodesNto1.head.WRAPPER_WR_CLK_EN else mbistNodes1toN.head.WRAPPER_WR_CLK_EN
  val WRAPPER_CLK_EN = if(isNto1) mbistNodesNto1.head.WRAPPER_CLK_EN else mbistNodes1toN.head.WRAPPER_CLK_EN
  val OUTPUT_RESET = if(isNto1) mbistNodesNto1.head.OUTPUT_RESET else mbistNodes1toN.head.OUTPUT_RESET
  val PWR_MGNT_IN = if(isNto1) mbistNodesNto1.head.PWR_MGNT_IN else mbistNodes1toN.head.PWR_MGNT_IN

  if(hasMbist) {
    if (!isRF) {
      array.mbist.get.sram_trim_fuse <> sram_trim_fuse
      array.mbist.get.sram_sleep_fuse <> sram_sleep_fuse
      array.mbist.get.rf_trim_fuse := DontCare
      array.mbist.get.rf_sleep_fuse := DontCare
      array.mbist.get.WRAPPER_RD_CLK_EN <> WRAPPER_RD_CLK_EN
      array.mbist.get.WRAPPER_WR_CLK_EN <> WRAPPER_WR_CLK_EN
      array.mbist.get.WRAPPER_CLK_EN := DontCare
      array.mbist.get.PWR_MGNT_IN := PWR_MGNT_IN
    } else {
      array.mbist.get.sram_trim_fuse := DontCare
      array.mbist.get.sram_sleep_fuse := DontCare
      array.mbist.get.rf_trim_fuse <> rf_trim_fuse
      array.mbist.get.rf_sleep_fuse <> rf_sleep_fuse
      array.mbist.get.WRAPPER_RD_CLK_EN <> DontCare
      array.mbist.get.WRAPPER_WR_CLK_EN <> DontCare
      array.mbist.get.WRAPPER_CLK_EN <> WRAPPER_CLK_EN
      array.mbist.get.PWR_MGNT_IN := PWR_MGNT_IN
    }
    array.mbist.get.bypsel <> bypsel
    array.mbist.get.wdis_b <> wdis_b
    array.mbist.get.rdis_b <> rdis_b
    array.mbist.get.init_en <> init_en
    array.mbist.get.init_val <> init_val
    array.mbist.get.clkungate <> clkungate

    array.mbist.get.IP_RESET_B <> IP_RESET_B

    array.mbist.get.OUTPUT_RESET <> OUTPUT_RESET

    array.repair.get.colRepair := DontCare
    array.repair.get.rowRepair := DontCare

    array.mbist.get.PWR_MGNT_IN0 := PWR_MGNT_IN0.get
    PWR_MGNT_OUT.get := array.mbist.get.PWR_MGNT_OUT

    if (hasRepair) {
      val bd = Wire(new RepairBundle)
      Repair.addRepairNodeToGlobal(bd, parentName)
      array.repair.get.colRepair <> bd.colRepair
      array.repair.get.rowRepair <> bd.rowRepair
      bd := DontCare
      dontTouch(bd)
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
  val realRen = (if (implementSinglePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))

  val finalWriteSetIdx = if(hasMbist) Mux(mbistFuncSel, mbistAddr, setIdx) else setIdx
  val finalReadSetIdx = if(hasMbist) Mux(mbistFuncSel, mbistAddrRead, io.r.req.bits.setIdx) else io.r.req.bits.setIdx
  val finalWen = if(hasMbist) Mux(mbistFuncSel, mbistWriteEn, wen) else wen
  val finalRen = if(hasMbist) Mux(mbistFuncSel, mbistReadEn, ren) else ren
  val finalWmask = if(hasMbist) Mux(mbistFuncSel, mbistWMask, waymask) else waymask
  val finalWriteData = if(hasMbist) Mux(mbistFuncSel, mbistWriteData, wdata) else wdata

  val toSRAMRen = if (implementSinglePort) (finalRen && !finalWen) else finalRen

  //  val raw_rdata = array.read(io.r.req.bits.setIdx, realRen).asTypeOf(Vec(way, wordType))
  //  when (wen) { array.write(setIdx, wdata.asUInt, waymask) }
  when (finalWen) { array.write(finalWriteSetIdx, finalWriteData.asUInt, finalWmask) }
  val raw_rdata = array.read(finalReadSetIdx, toSRAMRen).asTypeOf(Vec(way, wordType))

  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !implementSinglePort)
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
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx,mbistFuncSel)
  val mem_rdata = {
    if (implementSinglePort) raw_rdata
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
  io.r.req.ready := !resetState && (if (implementSinglePort) !wen else true.B)
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

class FoldedSRAMTemplate[T <: Data]
(
  gen: T, set: Int, width: Int = 4, way: Int = 1,
  shouldReset: Boolean = false, extraReset: Boolean = false,
  holdRead: Boolean = false, singlePort: Boolean = false, bypassWrite: Boolean = false,
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
    shouldReset=shouldReset, extraReset=extraReset, holdRead=holdRead, singlePort=singlePort,parentName = parentName))
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
