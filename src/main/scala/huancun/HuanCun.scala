/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

// See LICENSE.SiFive for license details.

package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{BundleFieldBase, UIntToOH1}
import huancun.mbist._
import huancun.mbist.MBISTPipeline.placePipelines
import huancun.prefetch._
import utils.{DFTResetGen, FastArbiter, Pipeline, ResetGen}

import scala.collection.mutable

trait HasHuanCunParameters {
  val p: Parameters
  val cacheParams = p(HCCacheParamsKey)
  val prefetchOpt = cacheParams.prefetch
  val hasPrefetchBit = prefetchOpt.nonEmpty && prefetchOpt.get.hasPrefetchBit
  val hasAliasBits = if(cacheParams.clientCaches.isEmpty) false
    else cacheParams.clientCaches.head.needResolveAlias

  val blockBytes = cacheParams.blockBytes
  val beatBytes = cacheParams.channelBytes.d.get
  val beatSize = blockBytes / beatBytes

  val mshrs = cacheParams.mshrs
  val mshrsAll = cacheParams.mshrs + 2
  val mshrBits = log2Up(mshrsAll)
  val blocks = cacheParams.ways * cacheParams.sets
  val sizeBytes = blocks * blockBytes
  val dirReadPorts = cacheParams.dirReadPorts

  val wayBits = log2Ceil(cacheParams.ways)
  val setBits = log2Ceil(cacheParams.sets)
  val offsetBits = log2Ceil(blockBytes)
  val beatBits = offsetBits - log2Ceil(beatBytes)
  val pageOffsetBits = log2Ceil(cacheParams.pageBytes)
  val clientMaxWays = cacheParams.clientCaches.map(_.ways).fold(0)(math.max)
  val maxWays = math.max(clientMaxWays, cacheParams.ways)

  val stateBits = MetaData.stateBits

  val aliasBitsOpt = if(cacheParams.clientCaches.isEmpty) None
    else cacheParams.clientCaches.head.aliasBitsOpt

  val bufBlocks = mshrs / 2
  val bufIdxBits = log2Ceil(bufBlocks)

  val alwaysReleaseData = cacheParams.alwaysReleaseData

  // req -> sram ports 1 cycle
  // sram 1 or 2 cycles
  // sram ports -> channels 1 cycle
  val sramLatency = 1 + 1 + (if(cacheParams.sramClkDivBy2) 2 else 1)

  val numCSRPCntHc    = 5
  val numPCntHcMSHR   = 7
  val numPCntHcDir    = 11
  val numPCntHcReqb   = 6
  val numPCntHcProb   = 1
  val numPCntHc       = numPCntHcMSHR + numPCntHcDir + numPCntHcReqb + numPCntHcProb
  val print_hcperfcounter  = false

  lazy val edgeIn = p(EdgeInKey)
  lazy val edgeOut = p(EdgeOutKey)
  lazy val bankBits = p(BankBitsKey)

  lazy val clientBits = edgeIn.client.clients.count(_.supports.probe)
  lazy val sourceIdBits = edgeIn.bundle.sourceBits
  lazy val msgSizeBits = edgeIn.bundle.sizeBits

  // width params with bank idx (used in prefetcher / ctrl unit)
  lazy val fullAddressBits = edgeOut.bundle.addressBits
  lazy val fullTagBits = fullAddressBits - setBits - offsetBits
  // width params without bank idx (used in slice)
  lazy val addressBits = fullAddressBits - bankBits
  lazy val tagBits = fullTagBits - bankBits

  lazy val outerSinkBits = edgeOut.bundle.sinkBits

  val block_granularity = if (!cacheParams.inclusive && cacheParams.clientCaches.nonEmpty) {
    cacheParams.clientCaches.head.blockGranularity
  } else setBits

  def getClientBitOH(sourceId: UInt): UInt = {
    if (clientBits == 0) {
      0.U
    } else {
      Cat(
        edgeIn.client.clients
          .filter(_.supports.probe)
          .map(c => {
            c.sourceId.contains(sourceId).asInstanceOf[Bool]
          })
          .reverse
      )
    }
  }

  def getSourceId(client: UInt): UInt = {
    if (clientBits == 0) {
      0.U
    } else {
      Mux1H(
        client,
        edgeIn.client.clients
          .filter(_.supports.probe)
          .map(c => c.sourceId.start.U)
      )
    }
  }

  def parseFullAddress(x: UInt): (UInt, UInt, UInt) = {
    val offset = x // TODO: check address mapping
    val set = offset >> offsetBits
    val tag = set >> setBits
    (tag(fullTagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
  }

  def parseAddress(x: UInt): (UInt, UInt, UInt) = {
    val offset = x
    val set = offset >> (offsetBits + bankBits)
    val tag = set >> setBits
    (tag(tagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
  }

  def getPPN(x: UInt): UInt = {
    x(x.getWidth - 1, pageOffsetBits)
  }

  def startBeat(offset: UInt): UInt = {
    (offset >> log2Up(beatBytes)).asUInt()
  }

  def totalBeats(size: UInt): UInt = {
    (UIntToOH1(size, log2Up(blockBytes)) >> log2Ceil(beatBytes)).asUInt()
  }

}

trait DontCareInnerLogic { this: Module =>
  override def IO[T <: Data](iodef: T): T = {
    val p = chisel3.experimental.IO.apply(iodef)
    p <> DontCare
    p
  }
}

abstract class HuanCunBundle(implicit val p: Parameters) extends Bundle with HasHuanCunParameters

abstract class HuanCunModule(implicit val p: Parameters) extends MultiIOModule with HasHuanCunParameters

class HuanCun(parentName:String = "Unknown")(implicit p: Parameters) extends LazyModule with HasHuanCunParameters {

  val xfer = TransferSizes(blockBytes, blockBytes)
  val atom = TransferSizes(1, cacheParams.channelBytes.d.get)
  val access = TransferSizes(1, blockBytes)

  val clientPortParams = TLMasterPortParameters.v2(
    Seq(
      TLMasterParameters.v2(
        name = cacheParams.name,
        supports = TLSlaveToMasterTransferSizes(
          probe = xfer
        ),
        sourceId = IdRange(0, mshrsAll)
      )
    ),
    channelBytes = cacheParams.channelBytes,
    minLatency = 1,
    echoFields = cacheParams.echoField,
    requestFields = cacheParams.reqField,
    responseKeys = cacheParams.respKey
  )

  val node = TLAdapterNode(
    clientFn = { _ => clientPortParams },
    managerFn = { m =>
      TLSlavePortParameters.v1(
        m.managers.map { m =>
          m.v2copy(
            regionType = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
            supports = TLMasterToSlaveTransferSizes(
              acquireB = xfer,
              acquireT = if (m.supportsAcquireT) xfer else TransferSizes.none,
              arithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
              logical = if (m.supportsAcquireT) atom else TransferSizes.none,
              get = access,
              putFull = if (m.supportsAcquireT) access else TransferSizes.none,
              putPartial = if (m.supportsAcquireT) access else TransferSizes.none,
              hint = access
            ),
            fifoId = None
          )
        },
        beatBytes = 32,
        minLatency = 2,
        responseFields = cacheParams.respField,
        requestKeys = cacheParams.reqKey,
        endSinkId = mshrsAll
      )
    }
  )

  val ctrl_unit = cacheParams.ctrl.map(_ => LazyModule(new CtrlUnit(node)))
  val ctlnode = ctrl_unit.map(_.ctlnode)
  val rst_nodes = ctrl_unit.map(_.core_reset_nodes)
  val intnode = ctrl_unit.map(_.intnode)

  val pf_recv_node: Option[BundleBridgeSink[Valid[UInt]]] = prefetchOpt match {
    case Some(_: PrefetchReceiverParams) =>
      Some(BundleBridgeSink(Some(() => ValidIO(UInt(64.W)))))
    case _ => None
  }

  lazy val module = new LazyModuleImp(this) {
    val banks = node.in.size
    val io = IO(new Bundle {
      val perfEvents = Vec(banks, Vec(numPCntHc, Output(UInt(6.W))))
      val ecc_error = Valid(UInt(64.W))
      val dfx_reset = Input(new DFTResetGen)
    })

    val sizeBytes = cacheParams.toCacheParams.capacity.toDouble

    def sizeBytesToStr(sizeBytes: Double): String = sizeBytes match {
      case _ if sizeBytes >= 1024 * 1024 => (sizeBytes / 1024 / 1024) + "MB"
      case _ if sizeBytes >= 1024 => (sizeBytes / 1024) + "KB"
      case _ => "B"
    }

    val sizeStr = sizeBytesToStr(sizeBytes)
    val bankBits = if (banks == 1) 0 else log2Up(banks)
    val inclusion = if (cacheParams.inclusive) "Inclusive" else "Non-inclusive"
    val prefetch = "prefetch: " + cacheParams.prefetch.nonEmpty
    println(s"====== ${inclusion} ${cacheParams.name} ($sizeStr * $banks-bank) $prefetch ======")
    println(s"bankBits: ${bankBits}")
    println(s"sets:${cacheParams.sets} ways:${cacheParams.ways} blockBytes:${cacheParams.blockBytes}")
    if (!cacheParams.inclusive) {
      val clientParam = cacheParams.clientCaches.head
      println(s"[client] size:${sizeBytesToStr(clientParam.capacity.toDouble)}")
      println(s"[client] sets:${clientParam.sets} ways:${clientParam.ways} blockBytes:${clientParam.blockBytes}")
    }
    println(s"blockGranularityBits: ${block_granularity}")
    def print_bundle_fields(fs: Seq[BundleFieldBase], prefix: String) = {
      if (fs.nonEmpty) {
        println(fs.map { f => s"$prefix/${f.key.name}: (${f.data.getWidth}-bit)" }.mkString("\n"))
      }
    }

    print_bundle_fields(node.in.head._2.bundle.requestFields, "usr")
    print_bundle_fields(node.in.head._2.bundle.echoFields, "echo")

    val pftParams: Parameters = p.alterPartial {
      case EdgeInKey => node.in.head._2
      case EdgeOutKey => node.out.head._2
      case BankBitsKey => bankBits
    }

    def arbTasks[T <: Bundle](out: DecoupledIO[T], in: Seq[DecoupledIO[T]], name: Option[String] = None) = {
      val arbiter = Module(new FastArbiter[T](chiselTypeOf(out.bits), in.size))
      if (name.nonEmpty) {
        arbiter.suggestName(s"${name.get}_arb")
      }
      for ((arb, req) <- arbiter.io.in.zip(in)) {
        arb <> req
      }
      out <> arbiter.io.out
    }

    val prefetcher = prefetchOpt.map(_ => Module(new Prefetcher(parentName + "prefetcher_")(pftParams)))
    val prefetchTrains = prefetchOpt.map(_ => Wire(Vec(banks, DecoupledIO(new PrefetchTrain()(pftParams)))))
    val prefetchResps = prefetchOpt.map(_ => Wire(Vec(banks, DecoupledIO(new PrefetchResp()(pftParams)))))
    val prefetchReqsReady = WireInit(VecInit(Seq.fill(banks)(false.B)))
    prefetchOpt.foreach {
      _ =>
        arbTasks(prefetcher.get.io.train, prefetchTrains.get, Some("prefetch_train"))
        prefetcher.get.io.req.ready := Cat(prefetchReqsReady).orR
        arbTasks(prefetcher.get.io.resp, prefetchResps.get, Some("prefetch_resp"))
    }
    pf_recv_node match {
      case Some(x) => prefetcher.get.io.recv_addr := x.in.head._1
      case None => prefetcher.foreach(_.io.recv_addr := DontCare)
    }

    def bank_eq(set: UInt, bankId: Int, bankBits: Int): Bool = {
      if (bankBits == 0) true.B else set(bankBits - 1, 0) === bankId.U
    }

    def restoreAddress(x: UInt, idx: Int) = {
      restoreAddressUInt(x, idx.U)
    }

    def restoreAddressUInt(x: UInt, idx: UInt) = {
      if (bankBits == 0) {
        x
      } else {
        val high = x >> offsetBits
        val low = x(offsetBits - 1, 0)
        Cat(high, idx(bankBits - 1, 0), low)
      }
    }

    val slicesWithItsMBISTPipeline = node.in.zip(node.out).zipWithIndex.map {
      case (((in, edgeIn), (out, edgeOut)), i) =>
        require(in.params.dataBits == out.params.dataBits)
        val rst = ResetGen(2, Some(io.dfx_reset))
        val slice = withReset(rst) {
          Module(new Slice(parentName + s"slice${i}_")(p.alterPartial {
            case EdgeInKey => edgeIn
            case EdgeOutKey => edgeOut
            case BankBitsKey => bankBits
          }))
        }
        slice.io.in <> in
        in.b.bits.address := restoreAddress(slice.io.in.b.bits.address, i)
        out <> slice.io.out
        out.a.bits.address := restoreAddress(slice.io.out.a.bits.address, i)
        out.c.bits.address := restoreAddress(slice.io.out.c.bits.address, i)

        slice.io.prefetch.zip(prefetcher).foreach {
          case (s, p) =>
            s.req.valid := p.io.req.valid && bank_eq(p.io.req.bits.set, i, bankBits)
            s.req.bits := p.io.req.bits
            prefetchReqsReady(i) := s.req.ready && bank_eq(p.io.req.bits.set, i, bankBits)
            val train = Pipeline(s.train)
            val resp = Pipeline(s.resp)
            prefetchTrains.get(i) <> train
            prefetchResps.get(i) <> resp
            // restore to full address
            if (bankBits != 0) {
              val train_full_addr = Cat(
                train.bits.tag, train.bits.set, i.U(bankBits.W), 0.U(offsetBits.W)
              )
              val (train_tag, train_set, _) = s.parseFullAddress(train_full_addr)
              val resp_full_addr = Cat(
                resp.bits.tag, resp.bits.set, i.U(bankBits.W), 0.U(offsetBits.W)
              )
              val (resp_tag, resp_set, _) = s.parseFullAddress(resp_full_addr)
              prefetchTrains.get(i).bits.tag := train_tag
              prefetchTrains.get(i).bits.set := train_set
              prefetchResps.get(i).bits.tag := resp_tag
              prefetchResps.get(i).bits.set := resp_set
            }
        }
        io.perfEvents(i) := slice.perfinfo
        val mbistLevel = if (cacheParams.level == 3) Int.MaxValue else 3
        val mbistName = if (cacheParams.level == 3) "L3S" else "L2S"
        val (sliceMbistPipelineSram, sliceMbistPipelineRf, sliceMbistPipelineSramRepair, sliceMbistPipelineRfRepair) = placePipelines(level = mbistLevel, infoName = s"${mbistName}${i}")
        (slice, (sliceMbistPipelineSram, sliceMbistPipelineRf, sliceMbistPipelineSramRepair, sliceMbistPipelineRfRepair))
    }
    val l2MbistPipeline = Seq(if (cacheParams.level != 3) Some(placePipelines(level = Int.MaxValue, infoName = "L2")) else None)
    val slices = slicesWithItsMBISTPipeline.map(_._1)
    val sliceMbistPipelines = if (cacheParams.level == 3) slicesWithItsMBISTPipeline.map(_._2) else Seq(l2MbistPipeline.head.get)


    val ecc_arb = Module(new Arbiter(new EccInfo, slices.size))
    val slices_ecc = slices.zipWithIndex.map {
      case (s, i) => Pipeline(s.io.ctl_ecc, depth = 2, pipe = false, name = Some(s"ecc_buffer_$i"))
    }
    ecc_arb.io.in <> VecInit(slices_ecc)
    io.ecc_error.valid := ecc_arb.io.out.fire()
    io.ecc_error.bits := restoreAddressUInt(ecc_arb.io.out.bits.addr, ecc_arb.io.chosen)
    ctrl_unit.foreach { c =>
      val ctl_reqs = slices.zipWithIndex.map {
        case (s, i) => Pipeline.pipeTo(s.io.ctl_req, depth = 2, pipe = false, name = Some(s"req_buffer_$i"))
      }
      val ctl_resps = slices.zipWithIndex.map {
        case (s, i) => Pipeline(s.io.ctl_resp, depth = 2, pipe = false, name = Some(s"resp_buffer_$i"))
      }
      val bank_match = slices.map(_ => Wire(Bool()))
      c.module.io_req.ready := Mux1H(bank_match, ctl_reqs.map(_.ready))
      for ((s, i) <- ctl_reqs.zipWithIndex) {
        bank_match(i) := bank_eq(c.module.io_req.bits.set, i, bankBits)
        s.valid := c.module.io_req.valid && bank_match(i)
        s.bits := c.module.io_req.bits
      }
      val arb = Module(new Arbiter(new CtrlResp, slices.size))
      arb.io.in <> ctl_resps
      c.module.io_resp <> arb.io.out
      c.module.io_ecc <> ecc_arb.io.out
      c.module.io_ecc.bits.addr := io.ecc_error.bits
    }
    if (ctrl_unit.isEmpty) {
      slices.foreach(_.io.ctl_req <> DontCare)
      slices.foreach(_.io.ctl_req.valid := false.B)
      slices.foreach(_.io.ctl_resp.ready := false.B)
      ecc_arb.io.out.ready := true.B
    }
    node.edges.in.headOption.foreach { n =>
      n.client.clients.zipWithIndex.foreach {
        case (c, i) =>
          println(s"\t${i} <= ${c.name}")
      }
    }

    /** **************************************Under L2 configuration*******************************************************/
    val mbist_sram = if (cacheParams.level == 2 && sliceMbistPipelines.head._1.isDefined) Some(IO(sliceMbistPipelines.head._1.get.io.mbist.get.cloneType)) else None
    val mbist_rf = if (cacheParams.level == 2 && sliceMbistPipelines.head._2.isDefined) Some(IO(sliceMbistPipelines.head._2.get.io.mbist.get.cloneType)) else None
    if(mbist_sram.isDefined) sliceMbistPipelines.head._1.get.io.mbist.get <> mbist_sram.get
    if(mbist_rf.isDefined) sliceMbistPipelines.head._2.get.io.mbist.get <> mbist_rf.get

    /** **************************************Under L3 configuration*******************************************************/

    val mbist_extra_banks,mbist_extra_dirs = if (cacheParams.level == 3) Some(IO(Vec(sliceMbistPipelines.length,new MbitsExtraFullInterface))) else None
    val fscan_clkungate = if (cacheParams.level == 3) Some(IO(Input(Bool()))) else None
    val xsx_ultiscan = if (cacheParams.level == 3) Some(IO(new FSCANInputInterface)) else None
    val xsl2_ultiscan = if (cacheParams.level == 3) Some(IO(new FSCANInputInterface)) else None
    val hd2prf_in = if (cacheParams.level == 3) Some(IO(new MbitsFuseInterface(isSRAM = false))) else None
    val hsuspsr_in = if (cacheParams.level == 3) Some(IO(new MbitsFuseInterface(isSRAM = true))) else None
    val uhdusplr_in = if (cacheParams.level == 3) Some(IO(new MbitsFuseInterface(isSRAM = true))) else None
    val hduspsr_in = if (cacheParams.level == 3) Some(IO(new MbitsFuseInterface(isSRAM = true))) else None
    val bisr = if (cacheParams.level == 3) Some(IO(Vec(sliceMbistPipelines.length,new BISRInputInterface))) else None
    val mbist_jtag = if (cacheParams.level == 3) Some(IO(Vec(sliceMbistPipelines.length,new JTAGInterface))) else None
    val bisr_mem_chain_select = if (cacheParams.level == 3) Some(IO(Input(UInt(1.W)))) else None

    if (cacheParams.level == 3){
      val dirPipeline = sliceMbistPipelines.map(_._1.get)
      val bankPipeline = sliceMbistPipelines.map(_._3.get)

      val mbistInterfaces = bankPipeline.indices.zip(dirPipeline).zip(bankPipeline).map({
        case((idx,dirPl),bankPl) => {
          val intfName = f"MBIST_SRAM_L3_Slice_intf"
          val intf = Module(new MBISTInterface(
            Seq(dirPl.io.mbist.get.params,bankPl.io.mbist.get.params),
            Seq(dirPl.node.array_id,bankPl.node.array_id),
            intfName,
            true,
            2,
            true
          ))
          dirPl.genCSV(intf.info)
          bankPl.genCSV(intf.info)
          bankPl.io.scan_in.get := intf.scan_in_toPip.get
          intf.scan_out_fromPip.get := bankPl.io.scan_out.get
          intf.toPipeline(0) <> dirPl.io.mbist.get
          intf.toPipeline(1) <> bankPl.io.mbist.get
          intf.extra(0) := DontCare
          intf.extra(1) := DontCare
          intf.clock := clock
          mbist_extra_dirs.get(idx).connectExtra(intf.extra(0))
          mbist_extra_banks.get(idx).connectExtra(intf.extra(1))
          mbist_extra_dirs.get(idx).connectPWR_MGNT(dirPl.PWR_MGNT.get._1,dirPl.PWR_MGNT.get._2)
          mbist_extra_banks.get(idx).connectPWR_MGNT(bankPl.PWR_MGNT.get._1,bankPl.PWR_MGNT.get._2)
          intf
        }
      })
      val repairNodesList = Repair.globalRepairNode
      val repairNodesForEveyController = mbistInterfaces.indices.map({ idx =>
          repairNodesList.filter(_.prefix.contains(s"slice${idx}"))
      })
      val mbistControllers = mbistInterfaces.zip(repairNodesForEveyController).zipWithIndex.map({
        case ((intf,nodes),idx) =>
          val prefix = "L3"
          val ctrl = Module(new MBISTController(
            Seq(intf.mbist.params),
            Seq("L3_SRAM"),
            Seq(prefix),
            Some(nodes)
          ))
          dontTouch(ctrl.io)
          ctrl.repairPort.get.foreach(dontTouch(_))
          MBISTController.connectRepair(ctrl.repairPort.get,nodes)
          ctrl.mbist.head <> intf.mbist
          ctrl.fscan_ram.head <> intf.fscan_ram
          ctrl.io.hd2prf_out <> intf.hd2prf_fuse
          ctrl.io.hsuspsr_out <> intf.hsuspsr_fuse
          ctrl.io.uhdusplr_out <> intf.uhdusplr_fuse
          ctrl.io.hduspsr_out <> intf.hduspsr_fuse
          ctrl.io.fscan_clkungate := fscan_clkungate.get
          ctrl.io.clock := clock
          ctrl.io.hd2prf_in := hd2prf_in.get
          ctrl.io.hsuspsr_in := hsuspsr_in.get
          ctrl.io.uhdusplr_in := uhdusplr_in.get
          ctrl.io.hduspsr_in := hduspsr_in.get
          ctrl.io.xsx_fscan_in<> xsx_ultiscan.get
          ctrl.io.xsl2_fscan_in <> xsl2_ultiscan.get
          ctrl.io.L3_bisr.get <> intf.bisr.get
          ctrl.io.bisr_mem_chain_select.get := bisr_mem_chain_select.get
          ctrl
      })
      mbistControllers.zip(bisr.get).zip(mbist_jtag.get).foreach({
        case((ctrl,bisrPort),jtagPort) =>
          ctrl.io.bisr.get <> bisrPort
          ctrl.io.mbist_ijtag <> jtagPort
      })
    }
  }
}
