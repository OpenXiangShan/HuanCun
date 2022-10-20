/** *************************************************************************************
  * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2022 Peng Cheng Laboratory
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

package huancun.mbist

import chisel3._
import chisel3.util._
import huancun.mbist.MBIST._
import huancun.mbist.MBISTPipeline.{generateCSV, uniqueId}
import huancun.utils.{ParallelMux, ParallelOR, SRAMTemplate}

import java.io.{File, IOException, PrintWriter}

class MbitsExtraInterface(val isSRAM:Boolean) extends Bundle{
  val IP_RESET_B = Input(Bool())
  val WRAPPER_RD_CLK_EN = Input(UInt((if(!isSRAM) 1 else 0).W))
  val WRAPPER_WR_CLK_EN = Input(UInt((if(!isSRAM) 1 else 0).W))
  val WRAPPER_CLK_EN = Input(UInt((if(isSRAM) 1 else 0).W))
  val OUTPUT_RESET = Input(Bool())
  val PWR_MGNT_IN = Input(UInt((if(isSRAM) 5 else 4).W))
}

class BISRInputInterface extends Bundle {
  val shift_en = Input(Bool())
  val clock = Input(Bool())
  val reset = Input(Bool())
  val scan_in = Input(Bool())
  val scan_out = Output(Bool())
}

class MbitsExtraFullInterface extends Bundle{
  val ext_in = Input(UInt(9.W))
  val ext_out = Output(UInt(1.W))

  def connectExtra(extra:MbitsExtraInterface):Unit = {
    if(extra.isSRAM){
      extra.PWR_MGNT_IN     := ext_in(5,1)
      extra.IP_RESET_B      := ext_in(6)
      extra.WRAPPER_CLK_EN  := ext_in(7)
      extra.OUTPUT_RESET    := ext_in(8)
    }else{
      extra.PWR_MGNT_IN         := ext_in(4,1)
      extra.IP_RESET_B          := ext_in(5)
      extra.WRAPPER_RD_CLK_EN   := ext_in(6)
      extra.WRAPPER_WR_CLK_EN   := ext_in(7)
      extra.OUTPUT_RESET        := ext_in(8)
    }
  }
  def connectPWR_MGNT(head:String,last:String):Unit = {

    val source = Wire(UInt(1.W))
    val sink = Wire(UInt(1.W))
    dontTouch(source)
    dontTouch(sink)
    sink := DontCare
    WiringUtils.addSource(source,head)
    WiringUtils.addSink(sink,last)
    source := ext_in(0)
    ext_out := sink
  }
}

class MbitsFscanInterface extends Bundle{
  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
  val clkungate = Input(Bool())
}
class MbitsFuseInterface(isSRAM:Boolean) extends Bundle{
  val trim_fuse = Input(UInt((if(isSRAM) 20 else 11).W))
  val sleep_fuse = Input(UInt(2.W))
}
class MbitsStandardInterface(val params:MBISTBusParams) extends Bundle{
  val isRF = params.isRF
  val array = Input(UInt(params.arrayWidth.W))
  val all, req= Input(Bool())
  val ack = Output(Bool())
  // write
  val writeen = Input(Bool())
  val be = Input(UInt(params.maskWidth.W))
  val addr = Input(UInt(params.addrWidth.W))
  val indata = Input(UInt(params.dataWidth.W))
  // read
  val readen = Input(Bool())
  val addr_rd = Input(UInt(if(isRF) params.addrWidth.W else 0.W)) // not used for single port srams
  val outdata = Output(UInt(params.dataWidth.W))
}

case class InterfaceInfo
(
  name: String,
  addrWidth: Int,
  dataWidth: Int,
  arrayWidth: Int,
  beWidth: Int
){
  override def toString = s"$name,$addrWidth,$dataWidth,$arrayWidth,$beWidth"
}

class MBISTInterface(params:Seq[MBISTBusParams],ids:Seq[Seq[Int]],name:String,isSRAM:Boolean,pipelineNum:Int,isRepair:Boolean = false) extends Module{
  require(params.nonEmpty)
  require(params.length == pipelineNum,s"Error @ ${name}:Params Number and pipelineNum must be the same!")
  val myMbistBusParams = MBIST.inferMBITSBusParamsFromParams(params)
  override val desiredName = name
  MBIST.noDedup(this)

  val toPipeline = IO(MixedVec(Seq.tabulate(pipelineNum)(idx => Flipped(new MBISTBus(params(idx))))))
  val mbist = IO(new MbitsStandardInterface(myMbistBusParams))
  val fscan_ram = IO(new MbitsFscanInterface)
  val hd2prf_fuse = IO(new MbitsFuseInterface(false))
  val hsuspsr_fuse = IO(new MbitsFuseInterface(true))
  val uhdusplr_fuse = IO(new MbitsFuseInterface(true))
  val hduspsr_fuse = IO(new MbitsFuseInterface(true))
  val extra = IO(Vec(pipelineNum,new MbitsExtraInterface(isSRAM)))
  val bisr = if(isRepair) Some(IO(new BISRInputInterface)) else None
  val scan_in_toPip = if(isRepair) Some(IO(Output(UInt(1.W)))) else None
  val scan_out_fromPip = if(isRepair) Some(IO(Input(UInt(1.W)))) else None

  val info = InterfaceInfo(name, myMbistBusParams.addrWidth, myMbistBusParams.dataWidth, myMbistBusParams.arrayWidth, myMbistBusParams.maskWidth)

  val gate = mbist.all | mbist.req
  val arrayReg = RegEnable(mbist.array,gate)
  val allReg = RegNext(mbist.all,0.U)
  val reqReg = RegNext(mbist.req,0.U)
  val weReg = RegEnable(mbist.writeen,gate)
  val beReg = RegEnable(mbist.be,gate)
  val addrReg = RegEnable(mbist.addr,gate)
  val inDataReg = RegEnable(mbist.indata,gate)
  val reReg = RegEnable(mbist.readen,gate)
  val addrRdReg = RegEnable(mbist.addr_rd,gate)
  val hit = if(params.length > 1) ids.map(item => ParallelOR(item.map(_.U === arrayReg))) else Seq(true.B)
  val outDataVec = toPipeline.map(_.mbist_outdata)
  mbist.outdata := RegEnable(ParallelMux(hit zip outDataVec),gate)
  val ackVec = toPipeline.map(_.mbist_ack)
  mbist.ack := RegNext(ParallelMux(hit zip ackVec),0.U)


  toPipeline.zip(extra).foreach({
    case(toPipeline,extra) =>
      toPipeline.mbist_array := arrayReg
      toPipeline.mbist_all := allReg
      toPipeline.mbist_req := reqReg

      toPipeline.mbist_writeen := weReg
      toPipeline.mbist_be := beReg
      toPipeline.mbist_addr := addrReg
      toPipeline.mbist_indata := inDataReg

      toPipeline.mbist_readen := reReg

      toPipeline.bypsel := fscan_ram.bypsel
      toPipeline.wdis_b := fscan_ram.wdis_b
      toPipeline.rdis_b := fscan_ram.rdis_b
      toPipeline.init_en := fscan_ram.init_en
      toPipeline.init_val := fscan_ram.init_val
      toPipeline.clkungate := fscan_ram.clkungate

      toPipeline.IP_RESET_B := extra.IP_RESET_B
      toPipeline.PWR_MGNT_IN := extra.PWR_MGNT_IN
      toPipeline.OUTPUT_RESET := extra.OUTPUT_RESET

      toPipeline.hd2prf_trim_fuse := hd2prf_fuse.trim_fuse
      toPipeline.hd2prf_sleep_fuse := hd2prf_fuse.sleep_fuse
      toPipeline.hsuspsr_trim_fuse := hsuspsr_fuse.trim_fuse
      toPipeline.hsuspsr_sleep_fuse := hsuspsr_fuse.sleep_fuse
      toPipeline.uhdusplr_trim_fuse := uhdusplr_fuse.trim_fuse
      toPipeline.uhdusplr_sleep_fuse := uhdusplr_fuse.sleep_fuse
      toPipeline.hduspsr_trim_fuse := hduspsr_fuse.trim_fuse
      toPipeline.hduspsr_sleep_fuse := hduspsr_fuse.sleep_fuse

      if(isSRAM){
        toPipeline.mbist_addr_rd := addrReg
        toPipeline.WRAPPER_RD_CLK_EN := DontCare
        toPipeline.WRAPPER_WR_CLK_EN := DontCare
        toPipeline.WRAPPER_CLK_EN := extra.WRAPPER_CLK_EN
      }else{
        toPipeline.mbist_addr_rd := addrRdReg
        toPipeline.WRAPPER_RD_CLK_EN := extra.WRAPPER_RD_CLK_EN
        toPipeline.WRAPPER_WR_CLK_EN := extra.WRAPPER_WR_CLK_EN
        toPipeline.WRAPPER_CLK_EN := DontCare
      }

      if(isRepair){
        toPipeline.bisr_shift_en := bisr.get.shift_en
        toPipeline.bisr_clock := bisr.get.clock
        toPipeline.bisr_reset := bisr.get.reset
        scan_in_toPip.get := bisr.get.scan_in
        bisr.get.scan_out := scan_out_fromPip.get
      }
      else{
        toPipeline.bisr_shift_en := DontCare
        toPipeline.bisr_clock := DontCare
        toPipeline.bisr_reset := DontCare
      }
  })
}

object MBISTPipeline {
  private var uniqueId = 0
  protected[mbist] def generateCSV
  (
    intfInfo:InterfaceInfo,
    node:PipelineBaseNode,
    infoName:String,
    isSRAM: Boolean,
  ): Unit ={
    val file = new File(f"build/$infoName.csv")
    if(!file.exists()){
      try{
        file.createNewFile()
      }catch {
        case ex:IOException=>
          println("error")
      }
    }

    val fileHandle = new PrintWriter(f"build/$infoName.csv")
    val intfHeads = "\"INTF Name\", \"INTF Addr\", \"INTF Data\", \"INTF Array\", \"INTF Be\"\n"
    fileHandle.print(intfHeads)
    fileHandle.print(intfInfo.toString + '\n')
    val sramHeads = if(isSRAM) {
      "\"SRAM Name\",\"SRAM Type\",\"SRAM array\",\"pipeline depth\",\"bitWrite\",\"foundry\",\"SRAM Inst\"\n"
    } else {
      "\"RF Name\",\"RF Type\",\"SRAM array\",\"pipeline depth\",\"bitWrite\",\"foundry\",\"SRAM Inst\"\n"
    }
    fileHandle.print(sramHeads)
    node.ramParamsBelongToThis.zip(node.array_id).zip(node.array_depth).foreach({
      case ((p,id),depth) =>
        fileHandle.print(p.hierarchyName + ",")
        fileHandle.print(p.vname + ".v,")
        fileHandle.print(id.toString + ",")
        fileHandle.print((depth * 2 + 2).toString + ",")
        fileHandle.print(if(p.bitWrite) "true," else "false,")
        fileHandle.print(p.foundry + ",")
        fileHandle.print(p.sramInst)
        fileHandle.print("\n")
    })
    fileHandle.close()
  }
  def placePipelines(level:Int,infoName:String = uniqueId.toString):
  (Option[MBISTPipeline],Option[MBISTPipeline],Option[MBISTPipeline],Option[MBISTPipeline]) = {

    val doSramChildrenExist = checkSramChildrenExistence(level)
    val doRfChildrenExist = checkRfChildrenExistence(level)
    val doSramRepairChildrenExist = checkSramRepairChildrenExistence(level)
    val doRfRepairChildrenExist = checkRfRepairChildrenExistence(level)
    val sramInfo = "MBIST_SRAM_" + infoName
    val rfInfo = "MBIST_RF_" + infoName
    val sramRepairInfo = "MBIST_SRAM_Repair_" + infoName
    val rfRepairInfo = "MBIST_RF_Repair_" + infoName
    val sramPipeline = if(doSramChildrenExist) Some(Module(new MBISTPipeline(level,sramInfo,true,false))) else None
    val rfPipeline = if(doRfChildrenExist) Some(Module(new MBISTPipeline(level,rfInfo,false,false))) else None
    val sramRepairPipeline = if(doSramRepairChildrenExist) Some(Module(new MBISTPipeline(level,sramRepairInfo,true,true))) else None
    val rfRepairPipeline = if(doRfRepairChildrenExist) Some(Module(new MBISTPipeline(level,rfRepairInfo,false,true))) else None
    (sramPipeline,rfPipeline,sramRepairPipeline,rfRepairPipeline)
  }
}

class MBISTPipeline(level: Int,infoName:String = s"MBISTPipeline_${uniqueId}",val isSRAM:Boolean = true, val isRepair:Boolean = false) extends Module {

  override val desiredName = infoName
  val prefix = "MBISTPipeline_" + uniqueId + "_"
  uniqueId += 1
  val node = MBIST.addController(prefix, level,isSRAM,isRepair)
  val bd = node.bd

  def genCSV(intf:InterfaceInfo):Unit = {
    generateCSV(intf,node,infoName,isSRAM)
  }

  if(MBIST.isMaxLevel(level)) {
    //Within every mbist domain, sram arrays are indexed from 0
    SRAMTemplate.restartIndexing(isSRAM)
  }

  val PWR_MGNT = if(MBIST.isMaxLevel(level)) Some(SRAMTemplate.getAndClearPWRMGNT(isSRAM,isRepair)) else None

  val io = IO(new Bundle() {
    val mbist = if(MBIST.isMaxLevel(level)) Some(new MBISTBus(bd.params)) else None
    val scan_in = if(MBIST.isMaxLevel(level) && isRepair) Some(Input(UInt(1.W))) else None
    val scan_out = if(MBIST.isMaxLevel(level) && isRepair) Some(Output(UInt(1.W))) else None
  })
  if(MBIST.isMaxLevel(level) && isRepair){
    val BISR_SCAN = SRAMTemplate.getAndClearBisr()
    val scan_in = Wire(UInt(1.W))
    val scan_out = Wire(UInt(1.W))
    scan_out := DontCare
    WiringUtils.addSource(scan_in,BISR_SCAN._1)
    WiringUtils.addSink(scan_out,BISR_SCAN._2)
    io.scan_out.get := scan_out
    scan_in := io.scan_in.get
  }

  if(io.mbist.isDefined) {
    io.mbist.get <> bd
  }

  val arrayHit = ParallelOR(node.array_id.map(_.U === bd.mbist_array))
  val activated = bd.mbist_all | (bd.mbist_req & arrayHit)

  val pipelineNodes   = node.children.filter(_.isInstanceOf[PipelineBaseNode]).map(_.asInstanceOf[PipelineBaseNode])
  val pipelineNodesAck= if(pipelineNodes.nonEmpty) ParallelOR(pipelineNodes.map(_.bd.mbist_ack)) else true.B
  val activatedReg    =   RegNext(activated)

  val arrayReg        =   RegEnable(bd.mbist_array,0.U,activated)
  val reqReg          =   RegNext(bd.mbist_req,0.U)
  val allReg          =   RegEnable(bd.mbist_all,0.U,activated)
  bd.mbist_ack        :=  reqReg & pipelineNodesAck

  val wenReg          =   RegEnable(bd.mbist_writeen,0.U,activated)
  val beReg           =   RegEnable(bd.mbist_be,0.U,activated)
  val addrReg         =   RegEnable(bd.mbist_addr,0.U,activated)
  val dataInReg       =   RegEnable(bd.mbist_indata,0.U,activated)

  val readEnReg       =   RegEnable(bd.mbist_readen,0.U,activated)
  val addrRdReg       =   RegEnable(bd.mbist_addr_rd,0.U,activated)

  val nodeSelected    =   node.children.map(_.array_id).map(ids => ids.map(_.U === arrayReg | allReg(0).asBool))


  val dataOut         =   Wire(Vec(node.children.length,UInt(node.bd.params.dataWidth.W)))
  val pipelineDataOut =   RegEnable(ParallelOR(node.children.zip(dataOut).filter(_._1.isInstanceOf[PipelineBaseNode]).map(_._2) :+ 0.U),activated)
  val sramDataOut     =   ParallelOR(node.children.zip(dataOut).filter(_._1.isInstanceOf[RAMBaseNode]).map(_._2) :+ 0.U)

  bd.mbist_outdata    :=  sramDataOut | pipelineDataOut

  node.children.foreach(_.bd.hd2prf_trim_fuse := node.bd.hd2prf_trim_fuse)
  node.children.foreach(_.bd.hd2prf_sleep_fuse := node.bd.hd2prf_sleep_fuse)
  node.children.foreach(_.bd.hsuspsr_trim_fuse := node.bd.hsuspsr_trim_fuse)
  node.children.foreach(_.bd.hsuspsr_sleep_fuse := node.bd.hsuspsr_sleep_fuse)
  node.children.foreach(_.bd.uhdusplr_trim_fuse := node.bd.uhdusplr_trim_fuse)
  node.children.foreach(_.bd.uhdusplr_sleep_fuse := node.bd.uhdusplr_sleep_fuse)
  node.children.foreach(_.bd.hduspsr_trim_fuse := node.bd.hduspsr_trim_fuse)
  node.children.foreach(_.bd.hduspsr_sleep_fuse := node.bd.hduspsr_sleep_fuse)
  node.children.foreach(_.bd.bypsel := node.bd.bypsel)
  node.children.foreach(_.bd.wdis_b := node.bd.wdis_b)
  node.children.foreach(_.bd.rdis_b := node.bd.rdis_b)
  node.children.foreach(_.bd.init_en := node.bd.init_en)
  node.children.foreach(_.bd.init_val := node.bd.init_val)
  node.children.foreach(_.bd.clkungate := node.bd.clkungate)
  node.children.foreach(_.bd.IP_RESET_B := node.bd.IP_RESET_B)
  node.children.foreach(_.bd.WRAPPER_RD_CLK_EN := node.bd.WRAPPER_RD_CLK_EN)
  node.children.foreach(_.bd.WRAPPER_WR_CLK_EN := node.bd.WRAPPER_WR_CLK_EN)
  node.children.foreach(_.bd.WRAPPER_CLK_EN := node.bd.WRAPPER_CLK_EN)
  node.children.foreach(_.bd.PWR_MGNT_IN := node.bd.PWR_MGNT_IN)
  node.children.foreach(_.bd.OUTPUT_RESET := node.bd.OUTPUT_RESET)
  node.children.foreach(_.bd.bisr_shift_en := node.bd.bisr_shift_en)
  node.children.foreach(_.bd.bisr_clock := node.bd.bisr_clock)
  node.children.foreach(_.bd.bisr_reset := node.bd.bisr_reset)

  node.children.zip(nodeSelected).zip(dataOut).foreach({
    case ((child:RAMBaseNode,selectedVec),dout) => {
      val selected = ParallelOR(selectedVec)
      child.bd.addr           := Mux(selected,addrReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.addr_rd        := Mux(selected,addrRdReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.wdata          := dataInReg(child.bd.params.dataWidth-1,0)
      child.bd.re             := Mux(selected,readEnReg,0.U)
      child.bd.we             := Mux(selected,wenReg,0.U)
      child.bd.wmask          := beReg(child.bd.params.maskWidth-1,0)
      child.bd.ack            := reqReg
      child.bd.selectedOH     := Mux(reqReg(0).asBool,selectedVec.map(_.asUInt).reverse.reduce(Cat(_,_)),~0.U(child.bd.selectedOH.getWidth.W))
      child.bd.array          := arrayReg
      dout                    := Mux(selected,child.bd.rdata,0.U)
    }
    case ((child:PipelineBaseNode,selectedVec),dout) => {
      val selected = ParallelOR(selectedVec)
      child.bd.mbist_array   := Mux(selected,arrayReg(child.bd.params.arrayWidth-1,0),0.U)
      child.bd.mbist_req     := reqReg
      child.bd.mbist_all     := Mux(selected,allReg,0.U)
      child.bd.mbist_writeen := Mux(selected,wenReg,0.U)
      child.bd.mbist_be      := beReg(child.bd.params.maskWidth-1,0)
      child.bd.mbist_addr    := Mux(selected,addrReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.mbist_indata  := dataInReg(child.bd.params.dataWidth-1,0)
      child.bd.mbist_readen  := Mux(selected,readEnReg,0.U)
      child.bd.mbist_addr_rd := Mux(selected,addrRdReg(child.bd.params.addrWidth-1,0),0.U)
      dout                   := Mux(selected,child.bd.mbist_outdata,0.U)
    }
  })
  MBIST.noDedup(this)
}
