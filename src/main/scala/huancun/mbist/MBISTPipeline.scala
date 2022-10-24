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

class MBISTInterface(params:Seq[MBISTBusParams],ids:Seq[Seq[Int]],name:String,pipelineNum:Int) extends Module{
  require(params.nonEmpty)
  require(params.length == pipelineNum,s"Error @ ${name}:Params Number and pipelineNum must be the same!")
  val myMbistBusParams = MBIST.inferMBITSBusParamsFromParams(params)
  override val desiredName = name
  MBIST.noDedup(this)

  val toPipeline = IO(MixedVec(Seq.tabulate(pipelineNum)(idx => Flipped(new MBISTBus(params(idx))))))
  val mbist = IO(new MbitsStandardInterface(myMbistBusParams))

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


  toPipeline.foreach({
    case toPipeline =>
      toPipeline.mbist_array := arrayReg
      toPipeline.mbist_all := allReg
      toPipeline.mbist_req := reqReg

      toPipeline.mbist_writeen := weReg
      toPipeline.mbist_be := beReg
      toPipeline.mbist_addr := addrReg
      toPipeline.mbist_indata := inDataReg

      toPipeline.mbist_readen := reReg
      toPipeline.mbist_addr_rd := addrRdReg
  })
}

object MBISTPipeline {
  private var uniqueId = 0
  protected[mbist] def generateCSV
  (
    intfInfo:InterfaceInfo,
    node:PipelineBaseNode,
    infoName:String,
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
    val sramHeads = "\"SRAM Name\",\"SRAM Type\",\"SRAM array\",\"pipeline depth\",\"bitWrite\",\"foundry\",\"SRAM Inst\"\n"
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
}

class MBISTPipeline(level: Int,moduleName:String = s"MBISTPipeline_${uniqueId}") extends Module {

  override val desiredName = moduleName
  val prefix = "MBISTPipeline_" + uniqueId + "_"
  uniqueId += 1
  val node = MBIST.addController(prefix, level)
  val bd = node.bd

  def genCSV(intf:InterfaceInfo, csvName:String):Unit = {
    println(s"Generating ${csvName}.csv")
    generateCSV(intf,node,csvName)
  }

  if(MBIST.isMaxLevel(level)) {
    //Within every mbist domain, sram arrays are indexed from 0
    SRAMTemplate.restartIndexing()
  }


  val io = IO(new Bundle() {
    val mbist = if(MBIST.isMaxLevel(level)) Some(new MBISTBus(bd.params)) else None
  })

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

  val renReg       =   RegEnable(bd.mbist_readen,0.U,activated)
  val addrRdReg       =   RegEnable(bd.mbist_addr_rd,0.U,activated)

  val nodeSelected    =   node.children.map(_.array_id).map(ids => ids.map(_.U === arrayReg | allReg(0).asBool))


  val dataOut         =   Wire(Vec(node.children.length,UInt(node.bd.params.dataWidth.W)))
  val pipelineDataOut =   RegEnable(ParallelOR(node.children.zip(dataOut).filter(_._1.isInstanceOf[PipelineBaseNode]).map(_._2) :+ 0.U),activated)
  val sramDataOut     =   ParallelOR(node.children.zip(dataOut).filter(_._1.isInstanceOf[RAMBaseNode]).map(_._2) :+ 0.U)

  bd.mbist_outdata    :=  sramDataOut | pipelineDataOut

  node.children.zip(nodeSelected).zip(dataOut).foreach({
    case ((child:RAMBaseNode,selectedVec),dout) => {
      val selected = ParallelOR(selectedVec)
      child.bd.addr           := Mux(selected,addrReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.addr_rd        := Mux(selected,addrRdReg(child.bd.params.addrWidth-1,0),0.U)
      child.bd.wdata          := dataInReg(child.bd.params.dataWidth-1,0)
      child.bd.re             := Mux(selected,renReg,0.U)
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
      child.bd.mbist_readen  := Mux(selected,renReg,0.U)
      child.bd.mbist_addr_rd := Mux(selected,addrRdReg(child.bd.params.addrWidth-1,0),0.U)
      dout                   := Mux(selected,child.bd.mbist_outdata,0.U)
    }
  })
  MBIST.noDedup(this)
}
