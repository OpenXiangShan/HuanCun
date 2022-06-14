package huancun.mbist

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import huancun.mbist.MBIST._
import huancun.mbist.MBISTPipeline.{generateXLS, uniqueId}
import huancun.utils.SRAMTemplate

import java.io.{File, IOException, PrintWriter}

class MbitsExtraInterface(val isSRAM:Boolean) extends Bundle{
  val IP_RESET_B = Input(Bool())
  val WRAPPER_RD_CLK_EN = Input(UInt((if(!isSRAM) 1 else 0).W))
  val WRAPPER_WR_CLK_EN = Input(UInt((if(!isSRAM) 1 else 0).W))
  val WRAPPER_CLK_EN = Input(UInt((if(isSRAM) 1 else 0).W))
  val OUTPUT_RESET = Input(Bool())
  val PWR_MGNT_IN = Input(UInt((if(isSRAM) 5 else 4).W))
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
    BoringUtils.addSource(source,head)
    BoringUtils.addSink(sink,last)
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

class MBISTInterface(params:Seq[MBISTBusParams],name:String,isSRAM:Boolean,pipelineNum:Int) extends RawModule{
  require(params.length == pipelineNum,s"Error @ ${name}:Params Number and pipelineNum must be the same!")
  val myMbistBusParams = MBIST.inferMBITSBusParamsFromParams(params)
  override val desiredName = name

  val toPipeline = IO(MixedVec(Seq.tabulate(pipelineNum)(idx => Flipped(new MBISTBus(params(idx))))))
  val mbist = IO(new MbitsStandardInterface(myMbistBusParams))
  val fscan_ram = IO(new MbitsFscanInterface)
  val hd2prf_fuse = IO(new MbitsFuseInterface(false))
  val hsuspsr_fuse = IO(new MbitsFuseInterface(true))
  val uhdusplr_fuse = IO(new MbitsFuseInterface(true))
  val hduspsr_fuse = IO(new MbitsFuseInterface(true))
  val extra = IO(Vec(pipelineNum,new MbitsExtraInterface(isSRAM)))
  val clock = IO(Input(Clock()))

  dontTouch(clock)
  dontTouch(mbist)

  mbist.ack := toPipeline.map(_.mbist_ack).reduce(_|_)
  mbist.outdata := toPipeline.map(_.mbist_outdata).reduce(_|_)

  toPipeline.zip(extra).foreach({
    case(toPipeline,extra) =>
      toPipeline.mbist_array := mbist.array
      toPipeline.mbist_all := mbist.all
      toPipeline.mbist_req := mbist.req

      toPipeline.mbist_writeen := mbist.writeen
      toPipeline.mbist_be := mbist.be
      toPipeline.mbist_addr := mbist.addr
      toPipeline.mbist_indata := mbist.indata

      toPipeline.mbist_readen := mbist.readen

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
        toPipeline.mbist_addr_rd := mbist.addr
        toPipeline.WRAPPER_RD_CLK_EN := DontCare
        toPipeline.WRAPPER_WR_CLK_EN := DontCare
        toPipeline.WRAPPER_CLK_EN := extra.WRAPPER_CLK_EN
      }else{
        toPipeline.mbist_addr_rd := mbist.addr_rd
        toPipeline.WRAPPER_RD_CLK_EN := extra.WRAPPER_RD_CLK_EN
        toPipeline.WRAPPER_WR_CLK_EN := extra.WRAPPER_WR_CLK_EN
        toPipeline.WRAPPER_CLK_EN := DontCare
    }
  })
}

object MBISTPipeline {
  private var uniqueId = 0
  protected[mbist] def generateXLS(node:PipelineBaseNode,infoName:String,isSRAM:Boolean): Unit ={
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
    val heads = if(isSRAM) {
      "\"SRAM Name\",\"SRAM Type\",\"SRAM array\",\"data width\",\"addr width\",\"be width\",\"single port\",\"pipeline depth\""
    } else {
      "\"RF Name\",\"RF Type\",\"SRAM array\",\"data width\",\"addr width\",\"be width\",\"single port\",\"pipeline depth\""
    }
    fileHandle.println(heads)
    node.ramParamsBelongToThis.zip(node.array_id).zip(node.array_depth).foreach({
      case ((p,id),depth) =>
        fileHandle.print(p.hierarchyName + ",")
        fileHandle.print(p.vname + ",")
        fileHandle.print(id.toString + ",")
        fileHandle.print(p.dataWidth.toString + ",")
        fileHandle.print(p.addrWidth.toString + ",")
        fileHandle.print(p.maskWidth.toString + ",")
        fileHandle.print((if(p.singlePort) "true" else "false")  + ",")
        fileHandle.print((depth * 2 + 1).toString)
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

  if(MBIST.isMaxLevel(level)) {
    generateXLS(node,infoName,isSRAM)
    //Within every mbist domain, sram arrays are indexed from 0
    SRAMTemplate.restartIndexing(isSRAM)
  }

  val PWR_MGNT = if(MBIST.isMaxLevel(level)) Some(SRAMTemplate.getAndClear(isSRAM,isRepair)) else None
  val io = IO(new Bundle() {
    val mbist = if(MBIST.isMaxLevel(level)) Some(new MBISTBus(bd.params)) else None
  })

  if(io.mbist.isDefined) {
    io.mbist.get <> bd
  }

  val arrayHit = node.array_id.map(_.U === bd.mbist_array).map(_.asUInt).reduce(Cat(_,_)).orR
  val activated = bd.mbist_all | (bd.mbist_req & arrayHit)

  val pipelineNodes   = node.children.filter(_.isInstanceOf[PipelineBaseNode]).map(_.asInstanceOf[PipelineBaseNode])
  val pipelineNodesAck= if(pipelineNodes.nonEmpty) pipelineNodes.map(_.bd.mbist_ack).reduce(_|_) else true.B
  val activatedReg    =   RegNext(activated)

  val arrayReg        =   RegEnable(bd.mbist_array,0.U,activated)
  val reqReg          =   RegNext(bd.mbist_req)
  val allReg          =   RegEnable(bd.mbist_all,0.U,activated)
  bd.mbist_ack        :=  reqReg & pipelineNodesAck
  dontTouch(bd.mbist_ack)

  val wenReg          =   RegEnable(bd.mbist_writeen,0.U,activated)
  val beReg           =   RegEnable(bd.mbist_be,0.U,activated)
  val addrReg         =   RegEnable(bd.mbist_addr,0.U,activated)
  val dataInReg       =   RegEnable(bd.mbist_indata,0.U,activated)

  val readEnReg       =   RegEnable(bd.mbist_readen,0.U,activated)
  val addrRdReg       =   RegEnable(bd.mbist_addr_rd,0.U,activated)
  val dataOut         =   Wire(Vec(node.children.length,UInt(node.bd.params.dataWidth.W)))
  val dataOutSelected =   Wire(UInt(node.bd.params.dataWidth.W))
  val dataOutReg      =   RegEnable(dataOutSelected,activated)
  bd.mbist_outdata         :=  dataOutReg


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

  val nodeSelected = node.children.map(_.array_id).map(_.map(_.U === arrayReg).map(_.asUInt | allReg).reduce(Cat(_,_)).orR)
  dataOutSelected := Mux1H(nodeSelected,dataOut)

  node.children.zip(nodeSelected).zip(dataOut).foreach({
    case ((child:RAMBaseNode,selected),dout) => {
      child.bd.addr           := Mux(selected,addrReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.addr_rd        := Mux(selected,addrRdReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.wdata          := Mux(selected,dataInReg(child.bd.params.dataWidth-1,0),0.U)
      child.bd.re             := Mux(selected,readEnReg,0.U)
      child.bd.we             := Mux(selected,wenReg,0.U)
      child.bd.wmask          := Mux(selected,beReg(child.bd.params.maskWidth-1,0),0.U)
      child.bd.ack            := reqReg
      dout             := child.bd.rdata
    }
    case ((child:PipelineBaseNode,selected),dout) => {
      child.bd.mbist_array   := Mux(selected,arrayReg(child.bd.params.arrayWidth-1,0),0.U)
      child.bd.mbist_req     := reqReg
      child.bd.mbist_all     := Mux(selected,allReg,0.U)
      child.bd.mbist_writeen := Mux(selected,wenReg,0.U)
      child.bd.mbist_be      := Mux(selected,beReg(child.bd.params.maskWidth-1,0),0.U)
      child.bd.mbist_addr    := Mux(selected,addrReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.mbist_indata  := Mux(selected,dataInReg(child.bd.params.dataWidth-1,0),0.U)
      child.bd.mbist_readen  := Mux(selected,readEnReg,0.U)
      child.bd.mbist_addr_rd := Mux(selected,addrRdReg(child.bd.params.addrWidth-1,0),0.U)
      dout             := child.bd.mbist_outdata
    }
  })
  MBIST.noDedup(this)
}
