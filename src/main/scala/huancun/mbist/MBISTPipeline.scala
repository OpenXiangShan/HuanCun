package huancun.mbist

import chisel3._
import chisel3.util._
import huancun.mbist.MBIST._
import huancun.mbist.MBISTPipeline.{generateXLS, uniqueId}
import org.apache.poi.hssf.usermodel._
import huancun.utils.SRAMTemplate

import java.io.{File, FileOutputStream, IOException}

class MbitsFscanInterface extends Bundle{
  val bypsel = Input(Bool())
  val wdis_b = Input(Bool())
  val rdis_b = Input(Bool())
  val init_en = Input(Bool())
  val init_val = Input(Bool())
  val clkungate = Input(Bool())
}
class MbitsStaticInterface extends Bundle{
  val sram_trim_fuse = Input(UInt(11.W))
  val sram_sleep_fuse = Input(UInt(2.W))
  val rf_trim_fuse = Input(UInt(11.W))
  val rf_sleep_fuse = Input(UInt(2.W))
}
class MbitsStandardInterface(val params:MBISTBusParams) extends Bundle{
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
  val addr_rd = Input(UInt(params.addrWidth.W)) // not used for single port srams
  val outdata = Output(UInt(params.dataWidth.W))
}

class MBISTInterface(val params:MBISTBusParams,name:String) extends RawModule{
  override val desiredName = name
  val toPipeline = IO(Flipped(new MBISTBus(params)))
  val mbist = IO(new MbitsStandardInterface(params))
  val fscan_ram = IO(new MbitsFscanInterface)
  val static = IO(new MbitsStaticInterface)

  toPipeline.mbist_array := mbist.array
  toPipeline.mbist_all := mbist.all
  toPipeline.mbist_req := mbist.req
  mbist.ack := toPipeline.mbist_ack

  toPipeline.mbist_writeen := mbist.writeen
  toPipeline.mbist_be := mbist.be
  toPipeline.mbist_addr := mbist.addr
  toPipeline.mbist_indata := mbist.indata

  toPipeline.mbist_readen := mbist.readen
  toPipeline.mbist_addr_rd := mbist.addr_rd
  mbist.outdata := toPipeline.mbist_outdata

  toPipeline.sram_trim_fuse := static.sram_trim_fuse
  toPipeline.sram_sleep_fuse := static.sram_sleep_fuse
  toPipeline.rf_trim_fuse := static.rf_trim_fuse
  toPipeline.rf_sleep_fuse := static.rf_sleep_fuse

  toPipeline.bypsel := fscan_ram.bypsel
  toPipeline.wdis_b := fscan_ram.wdis_b
  toPipeline.rdis_b := fscan_ram.rdis_b
  toPipeline.init_en := fscan_ram.init_en
  toPipeline.init_val := fscan_ram.init_val
  toPipeline.clkungate := fscan_ram.clkungate
}

object MBISTPipeline {
  private var uniqueId = 0
  def generateXLS(node:PipelineNode,infoName:String): Unit ={
    val file = new File(f"build/$infoName.xls")
    if(!file.exists()){
      try{
        file.createNewFile()
      }catch {
        case ex:IOException=>
          println("error")
      }
    }
    val hssf = new HSSFWorkbook()
    val sheet = hssf.createSheet()
    val firstRow = sheet.createRow(0)
    val heads = Seq[String]("SRAM array","data width","be width","single port","pipeline depth")
    for(i <- heads.indices){
      val cell = firstRow.createCell(i)
      cell.setCellValue(new HSSFRichTextString(heads(i)))
    }
    node.sramParamsBelongToThis.zip(node.array_id).zip(node.array_depth).zipWithIndex.foreach({
      case (((p,id),depth),idx) =>
        val row = sheet.createRow(idx + 1)
        val cells = heads.indices.map(row.createCell)
        cells(0).setCellValue(id.toString)
        cells(1).setCellValue(p.dataWidth.toString)
        cells(2).setCellValue(p.maskWidth.toString)
        cells(3).setCellValue(if(p.singlePort) "true" else "false" )
        cells(4).setCellValue(depth.toString)
    })
    heads.indices.foreach(idx => sheet.setColumnWidth(idx,(heads(idx).length * 7 + 12) / 7 * 256))
    val out = new FileOutputStream(f"build/$infoName.xls")
    hssf.write(out)
    out.close()
    hssf.close()
  }
}

class MBISTPipeline(level: Int,infoName:String = s"MBISTPipeline_${uniqueId}") extends Module {

  override val desiredName = infoName
  val prefix = "MBISTPipeline_" + uniqueId + "_"
  uniqueId += 1
  val node = MBIST.addController(prefix, level)
  val bd = node.bd

  //  println("=====")
  //
  //  println("parent: " + prefix.init)
  //
  //  for(c <- node.children){
  //    println("children: " + c.prefix.init)
  //  }
  //  println("*****")

  if(MBIST.isMaxLevel(level)) {
    generateXLS(node,infoName)
    //Within every mbist domain, sram arrays are indexed from 0
    SRAMTemplate.restartIndexing()
  }
  val io = IO(new Bundle() {
    val mbist = if(MBIST.isMaxLevel(level)) Some(new MBISTBus(bd.params)) else None
  })

  if(io.mbist.isDefined) {
    io.mbist.get <> bd
  }

  val arrayHit = node.array_id.map(_.U === bd.mbist_array).map(_.asUInt).reduce(Cat(_,_)).orR
  val activated = bd.mbist_all | (bd.mbist_req & arrayHit)

  val pipelineNodes   = node.children.filter(_.isInstanceOf[PipelineNode]).map(_.asInstanceOf[PipelineNode])
  val pipelineNodesAck= if(pipelineNodes.nonEmpty) pipelineNodes.map(_.bd.mbist_ack).reduce(_|_) else true.B
  val activatedReg    =   RegNext(activated)

  val arrayReg        =   RegEnable(bd.mbist_array,0.U,activated)
  val reqReg          =   RegEnable(bd.mbist_req,0.U,activated)
  val allReg          =   RegEnable(bd.mbist_all,0.U,activated)
  bd.mbist_ack              :=  activatedReg & pipelineNodesAck
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


  node.children.foreach(_.bd.sram_trim_fuse := node.bd.sram_trim_fuse)
  node.children.foreach(_.bd.sram_sleep_fuse := node.bd.sram_sleep_fuse)
  node.children.foreach(_.bd.rf_trim_fuse := node.bd.rf_trim_fuse)
  node.children.foreach(_.bd.rf_sleep_fuse := node.bd.rf_sleep_fuse)
  node.children.foreach(_.bd.bypsel := node.bd.bypsel)
  node.children.foreach(_.bd.wdis_b := node.bd.wdis_b)
  node.children.foreach(_.bd.rdis_b := node.bd.rdis_b)
  node.children.foreach(_.bd.init_en := node.bd.init_en)
  node.children.foreach(_.bd.init_val := node.bd.init_val)
  node.children.foreach(_.bd.clkungate := node.bd.clkungate)

  val nodeSelected = node.children.map(_.array_id).map(_.map(_.U === arrayReg).map(_.asUInt | allReg).reduce(Cat(_,_)).orR)
  dataOutSelected := Mux1H(nodeSelected,dataOut)

  node.children.zip(nodeSelected).zip(dataOut).foreach({
    case ((child:SRAMNode,selected),dout) => {
      child.bd.addr           := Mux(selected,addrReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.addr_rd        := Mux(selected,addrRdReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.wdata          := Mux(selected,dataInReg(child.bd.params.dataWidth-1,0),0.U)
      child.bd.re             := Mux(selected,readEnReg,0.U)
      child.bd.we             := Mux(selected,wenReg,0.U)
      child.bd.wmask          := Mux(selected,beReg(child.bd.params.maskWidth-1,0),0.U)
      child.bd.ack            := Mux(selected,activatedReg,false.B)
      dout             := child.bd.rdata
    }
    case ((child:PipelineNode,selected),dout) => {
      child.bd.mbist_array   := Mux(selected,arrayReg(child.bd.params.arrayWidth-1,0),0.U)
      child.bd.mbist_req     := Mux(selected,reqReg,0.U)
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
