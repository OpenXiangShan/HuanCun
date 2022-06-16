/**
 * @author zeal4u
 */
package huancun.prefetch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import huancun.HasHuanCunParameters
import huancun.utils.{SRAMTemplate, SetAssocReplacer, XSPerfPrint, ReplacementPolicy, Min, Max}


case class SMSParameters(
                        ftSets: Int = 4,
                        ftWays: Int = 16,
                        atSets: Int = 8,
                        atWays: Int = 16,
                        ptSets: Int = 32,
                        ptWays: Int = 16,
//                        ptThresholdL1: Int = 75,
//                        ptThresholdL2: Int = 25,
                        psSets: Int = 4,
                        psWays: Int = 16,
                        // L2 caches can hardly get PC
                        // pcBits: Int = 0,
                        lineBits: Int = 5
                        ) extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 16

}

trait HasSMSParams extends HasHuanCunParameters {
  val smsParams = prefetchOpt.get.asInstanceOf[SMSParameters]
  val printFlag = false

  val ftSets = smsParams.ftSets
  val ftWays = smsParams.ftWays
  val atSets = smsParams.atSets
  val atWays = smsParams.atWays
  val ptSets = smsParams.ptSets
  val ptWays = smsParams.ptWays
//  val ptThresholdL1 = smsParams.ptThresholdL1
//  val ptThresholdL2 = smsParams.ptThresholdL2
  val psSets = smsParams.psSets
  val psWays = smsParams.psWays
  val lineBits = smsParams.lineBits

  val blockBits = fullAddressBits - offsetBits
  val regionBits = blockBits - lineBits
  val ftTagBits = regionBits - log2Ceil(ftSets)
  val atTagBits = regionBits - log2Ceil(atSets)
  val psTagBits = regionBits - log2Ceil(psSets)
  val ptLongTagBits = blockBits - log2Ceil(ptSets)
  val bitVectorSize = 1 << lineBits
}

abstract class SMSModule(implicit p:Parameters) extends PrefetchModule with HasSMSParams
{
  def getOffset(block: UInt) = block(lineBits-1, 0)
  def getRegion(block: UInt) = block(regionBits+lineBits-1, lineBits)
}

abstract class SMSBundle(implicit p:Parameters) extends PrefetchBundle with HasSMSParams

class FT2AT(implicit p:Parameters) extends SMSBundle {
  val block = UInt((fullAddressBits-log2Ceil(blockBytes)).W)
  // trigger offset
  val offset = UInt(lineBits.W)
}

class FilterTableIO(implicit  p:Parameters) extends SMSBundle {
  val train   = Flipped(Decoupled(new PrefetchTrain()))
  val resp    = Decoupled(new FT2AT())
  val queryPT = Decoupled(new PrefetchTrain())
}

class FilterTableEntry(implicit p:Parameters) extends SMSBundle {
  val tag = UInt(ftTagBits.W)
  val offset = UInt(lineBits.W)
}

/**
  * filter accesses that do not reappear.
  */
class FilterTable(implicit p:Parameters) extends SMSModule {
  def getTag(region: UInt) = region(log2Ceil(ftSets)+ftTagBits-1, log2Ceil(ftSets))
  def getIndex(region: UInt) = region(log2Ceil(ftSets)-1, 0)

  val io = IO(new FilterTableIO())
  io.train.ready := true.B

  val block = RegNext(getBlock(io.train.bits.addr))
  val offset = getOffset(block)
  val region = getRegion(block)

  val mem = Module(new SRAMTemplate(new FilterTableEntry(), set=ftSets, way=ftWays, singlePort=false, bypassWrite=true))
  val lrus = new SetAssocReplacer(ftSets, ftWays, "lru")
  val valids = RegInit(VecInit(Seq.fill(ftSets)(VecInit(Seq.fill(ftWays)(false.B)))))

  val hits = VecInit(Seq.fill(ftWays)(false.B))
  val hit = hits.reduce(_||_)

  val index = getIndex(region)
  val tag = getTag(getRegion(block))

  mem.io.r.req.valid := io.train.fire
  mem.io.r.req.bits.setIdx := getIndex(getRegion(getBlock(io.train.bits.addr)))
  val dataVec = mem.io.r.resp.data
  val victimWay = WireInit(0.U(log2Ceil(ftWays).W))

  // zeal4u: filter access if it has the same offset
  val fireStage1 = RegNext(mem.io.r.req.fire)

  for (i <- 0 until ftWays) {
    hits(i) := fireStage1 & valids(index)(i) & dataVec(i).tag === tag
  }

  io.queryPT.bits := RegNext(io.train.bits)
  // get the target way
  when (hit) {
    io.queryPT.valid := false.B
    victimWay := OHToUInt(hits)
  }.otherwise {
    // all valid
    when (valids(index).asUInt === ((1<<ftWays)-1).U) {
      victimWay := lrus.way(index)
    } otherwise {
      victimWay := PriorityEncoder(~valids(index).asUInt)
    }
    lrus.access(index, victimWay)
    io.queryPT.valid := fireStage1
  }

  // allocate new entry
  val writeMask = UIntToOH(victimWay)
  val dataVecWrite = WireInit(dataVec)
  dataVecWrite(victimWay).tag := getTag(getRegion(block))
  dataVecWrite(victimWay).offset := getOffset(block)
  mem.io.w.req.valid := fireStage1 && !hit
  mem.io.w.req.bits.apply(
    setIdx = index,
    data = dataVecWrite,
    waymask = writeMask.asUInt
  )
  valids(index)(victimWay) := fireStage1 && !hit
  XSPerfPrint(printFlag, fireStage1 && !hit, p"SMS FT -> Write Data, write mask:${Binary(writeMask)}, " +
    p"way:${victimWay}, tag:${Hexadecimal(dataVecWrite(victimWay).tag)}, " +
    p"offset:${dataVecWrite(victimWay).offset}\n")

  val findPattern = hit && dataVec(victimWay).offset =/= offset
  when (findPattern) {
    // invalid the record
    valids(index)(victimWay) := false.B

    io.resp.valid := true.B
    io.resp.bits.block := block
    io.resp.bits.offset := dataVec(victimWay).offset
    XSPerfPrint(printFlag,p"SMS FT -> Get a pattern, offset1:${getOffset(block)}, " +
      p"offset2:${dataVec(victimWay).offset}, " +
      p"region:0x${Hexadecimal(getRegion(block))}\n")
  } otherwise {
    io.resp.valid := false.B
    io.resp.bits := DontCare
  }

  XSPerfPrint(printFlag, io.train.fire,
    p"SMS FT -> New Access, " +
    p"block:0x${Hexadecimal(getBlock(io.train.bits.addr))}\n")
}

class AT2PT(implicit p:Parameters) extends SMSBundle {
  val bitVector = UInt(bitVectorSize.W)
  val block = UInt((fullAddressBits-log2Ceil(blockBytes)).W)
}

class AccumulateTableIO(implicit p:Parameters) extends SMSBundle {
  val train      = Flipped(Decoupled(new PrefetchTrain())) // update existing patterns
  val reqFT      = Flipped(Decoupled(new FT2AT())) //  a new pattern from FT
  val evict      = Flipped(Decoupled(new EvictionInfo()))
  val evictResp = Decoupled(new AT2PT())
  // indicate whether the record exists
  val qeuryFT    = Decoupled(new PrefetchTrain())
}

class AccumulateTableEntry(implicit p:Parameters) extends SMSBundle {
  val tag = UInt(atTagBits.W)
  val bitVector = Vec(bitVectorSize, Bool())
  val offset = UInt(lineBits.W)
}

class AccumulateTable(implicit p:Parameters) extends SMSModule {
  def getTag(region: UInt) = region(log2Ceil(atSets)+atTagBits-1, log2Ceil(atSets))
  def getIndex(region: UInt) = region(log2Ceil(atSets)-1, 0)
  def rotateOffset(startOffset:UInt, offset:UInt) = Mux(offset >= startOffset, offset-startOffset, bitVectorSize.U-startOffset+offset)

  val io = IO(new AccumulateTableIO())
  io.evict.ready := true.B
  io.reqFT.ready := true.B
  io.train.ready := true.B

  val reqFTQueue = Module(new Queue(new FT2AT, 1))
  val trainQueue = Module(new Queue(new PrefetchTrain, 6))

  reqFTQueue.io.enq <> io.reqFT
  trainQueue.io.enq <> io.train

  val train = trainQueue.io.deq
  val reqFT = reqFTQueue.io.deq
  val reqFire = reqFT.fire || train.fire || io.evict.fire
  val index = Mux(io.evict.fire, getIndex(getRegion(getBlock(io.evict.bits.addr))),
              Mux(train.fire, getIndex(getRegion(getBlock(train.bits.addr))),
              Mux(reqFT.fire, getIndex(getRegion(reqFT.bits.block)), 0.U)))

  val table = Module(new SRAMTemplate(new AccumulateTableEntry(), set=atSets, way=atWays, singlePort=false, bypassWrite=true))
  val valids = RegInit(VecInit(Seq.fill(atSets)(VecInit(Seq.fill(atWays)(false.B)))))
  val lrus = new SetAssocReplacer(atSets, atWays, "lru")
  val victimWay = WireInit((0.U(log2Ceil(atWays).W)))
  val hits = VecInit(Seq.fill(atWays)(false.B))
  val hit = hits.reduce(_||_)
  val tag = RegNext(Mux(io.evict.fire, getTag(getRegion(getBlock(io.evict.bits.addr))),
                    Mux(reqFT.fire, getTag(getRegion(reqFT.bits.block)),
                    Mux(train.fire, getTag(getRegion(getBlock(train.bits.addr))), 0.U))))

  table.io.r.req.valid := reqFire
  table.io.r.req.bits.setIdx := index
  val dataVec = table.io.r.resp.data
  val dataVecWrite = WireInit(dataVec)
  val regIndex = RegNext(index)

  table.io.w.req.valid := RegNext(reqFT.fire) || RegNext(train.fire) && hit
  table.io.w.req.bits.apply(
    setIdx = regIndex,
    data = dataVecWrite,
    waymask = UIntToOH(victimWay).asUInt
  )

  for (i <- 0 until atWays) {
    hits(i) := RegNext(reqFire) && valids(regIndex)(i) && dataVec(i).tag === tag
  }

  when (hit) {
    victimWay := OHToUInt(hits)
  } .elsewhen (valids(regIndex).asUInt === ((1<<atWays)-1).U) {
    victimWay := lrus.way(regIndex)
  } .otherwise {
    victimWay := PriorityEncoder(~valids(regIndex).asUInt)
  }

  io.qeuryFT.valid := !hit && train.fire
  io.qeuryFT.bits  := train.bits

  reqFT.ready := !io.evict.fire
  train.ready := !io.evict.fire && !reqFT.fire

  io.evictResp.valid := false.B
  io.evictResp.bits := DontCare

  when (RegNext(io.evict.fire)){
    when (hit) {
      val hitWay = OHToUInt(hits)
      io.evictResp.valid := true.B
      io.evictResp.bits.bitVector := dataVec(hitWay).bitVector.asUInt
      io.evictResp.bits.block := Cat(dataVec(hitWay).tag, regIndex, dataVec(hitWay).offset)
      // invalid it
      valids(regIndex)(hitWay) := false.B
      XSPerfPrint(printFlag,p"SMS AT -> Cache eviction hits, block:0x${Hexadecimal(Cat(dataVec(hitWay).tag, regIndex, dataVec(hitWay).offset))}, " +
        p"bit vector:${dataVec(hitWay).bitVector}, " +
        p"hit:${hits}\n")
    }
    XSPerfPrint(printFlag,p"SMS AT -> Cache eviction, addr:0x${Hexadecimal(io.evict.bits.addr)}\n")
  }.elsewhen(RegNext(reqFT.fire)){
    val startOffset = RegNext(reqFT.bits.offset)
    val offset = RegNext(getOffset(reqFT.bits.block))
    when (hit) {
      val rotatedOffset1 = rotateOffset(dataVec(victimWay).offset, startOffset)
      val rotatedOffset2 = rotateOffset(dataVec(victimWay).offset, offset)
      dataVecWrite(victimWay).bitVector(rotatedOffset1) := true.B
      dataVecWrite(victimWay).bitVector(rotatedOffset2) := true.B
      XSPerfPrint(printFlag, p"SMS AT -> Error! Attempt to insert existing record, set:${index}, way:${victimWay}, tag:${dataVecWrite(victimWay).tag}, " +
                             p"Insert rotated offset:${rotatedOffset1}, ${rotatedOffset2}\n")
    }.otherwise {
      val bitVector = VecInit(Seq.fill(bitVectorSize)(false.B))

      bitVector(0):= true.B
      bitVector(rotateOffset(startOffset, offset)) := true.B

      dataVecWrite(victimWay).tag := tag
      dataVecWrite(victimWay).offset := startOffset
      dataVecWrite(victimWay).bitVector := bitVector
      XSPerfPrint(printFlag, p"SMS AT -> New Pattern, index:${regIndex}, way:${victimWay}, " +
                             p"start offset:${startOffset}, offset:${getOffset(reqFT.bits.block)}\n")
    }

    valids(regIndex)(victimWay) := true.B
    lrus.access(regIndex, victimWay)
  }.elsewhen (RegNext(train.fire)){
    when (hit) {
      val offset = getOffset(getBlock(train.bits.addr))
      val rotatedOffset = rotateOffset(dataVec(OHToUInt(hits)).offset, offset)

      dataVecWrite(OHToUInt(hits)).bitVector(rotatedOffset) := true.B
      XSPerfPrint(printFlag,p"SMS AT -> New train, addr:0x${Hexadecimal(train.bits.addr)}, " +
        p"offset:${offset}, start offset:${dataVec(OHToUInt(hits)).offset} rotate offset:${rotatedOffset}, " +
        p"tag:0x${Hexadecimal(tag)}, index:${regIndex}, hit:${hits}, "+
        p"vector:${dataVecWrite(OHToUInt(hits)).bitVector}\n")
    }
  }
}

class PT2PS(implicit p:Parameters) extends SMSBundle {
  val block = UInt((fullAddressBits-log2Ceil(blockBytes)).W)
  val prefetchPattern = Vec(bitVectorSize, UInt(2.W))
}

class PatternHistoryTableIO(implicit p:Parameters) extends SMSBundle {
  val predictReq  = Flipped(Decoupled(new PrefetchTrain))
  val predictResp = Decoupled(new PT2PS())
  val reqAT = Flipped(Decoupled(new AT2PT()))
}

class PatternHistoryTableEntry(implicit p:Parameters) extends SMSBundle {
  val tag = UInt(ptLongTagBits.W)
  val pattern = Vec(bitVectorSize, Bool())
}

class PatternHistoryTable(implicit p:Parameters) extends SMSModule {
  def getIndex(key: UInt) = key(log2Ceil(ptSets)-1, 0)
  def getLongTag(key: UInt) = key(log2Ceil(ptSets)+ptLongTagBits-1, log2Ceil(ptSets))

  val io = IO(new PatternHistoryTableIO())
  io.reqAT.ready := true.B
  io.predictReq.ready := !io.reqAT.fire

  val table = Module(new SRAMTemplate(new PatternHistoryTableEntry(), set=ptSets, way=ptWays, singlePort=false, bypassWrite=true))
  val lrus = new SetAssocReplacer(ptSets, ptWays, "lru")
  val valids = RegInit(VecInit(Seq.fill(ptSets)(VecInit(Seq.fill(ptWays)(false.B)))))

  val key = Mux(io.reqAT.fire, io.reqAT.bits.block,
            Mux(io.predictReq.fire, getBlock(io.predictReq.bits.addr), 0.U))

  table.io.r.req.valid := io.predictReq.fire || io.reqAT.fire
  table.io.r.req.bits.setIdx := getIndex(key)
  val dataVec = table.io.r.resp.data
  val dataVecWrite = WireInit(dataVec)
  val regIndex = RegNext(getIndex(key))
  val longTag = RegNext(getLongTag(key))
  val victimWay = WireInit(0.U(log2Ceil(ptWays).W))
  val hits = Wire(Vec(ptWays, Bool()))
  val hit = hits.reduce(_||_)
  // val short_hit_count_vector = VecInit(Seq.fill(bitVectorSize)(0.U(log2Ceil(ptWays).W)))
  // val count_matrix = VecInit(Seq.fill(bitVectorSize)(VecInit(Seq.fill(ptWays)(false.B))))

  table.io.w.req.valid := RegNext(io.reqAT.fire)
  table.io.w.req.bits.apply(
    setIdx = regIndex,
    data = dataVecWrite,
    waymask = UIntToOH(victimWay).asUInt
  )

  for (i <- 0 until ptWays) {
    hits(i) := RegNext(io.predictReq.fire) && valids(regIndex)(i) && dataVec(i).tag === longTag
  }

  when (RegNext(io.reqAT.fire)) {
    when (hit) {
      victimWay := OHToUInt(hits)
    }.elsewhen(valids(regIndex).asUInt === ((1<<ptWays) - 1).U) {
      victimWay := lrus.way(regIndex)
    }.otherwise{
      victimWay := PriorityEncoder(~valids(regIndex).asUInt)
    }
    lrus.access(regIndex, victimWay)
    dataVecWrite(victimWay).pattern := io.reqAT.bits.bitVector.asBools
    dataVecWrite(victimWay).tag := longTag
    valids(regIndex)(victimWay) := true.B
  }

  io.predictResp.valid := hit
  io.predictResp.bits.prefetchPattern := dataVec(OHToUInt(hits)).pattern
  io.predictResp.bits.block := getBlock(io.predictReq.bits.addr)

  XSPerfPrint(printFlag, io.reqAT.fire, p"SMS PT -> New Train, block:0x${Hexadecimal(io.reqAT.bits.block)}, " +
    p"bit vector:${io.reqAT.bits.bitVector}\n")
  XSPerfPrint(printFlag,io.predictReq.fire, p"SMS PT -> New Request, " +
    p"addr:0x${Hexadecimal(io.predictReq.bits.addr)}, " +
    p"long tag:0x${Hexadecimal(longTag)}\n")
  XSPerfPrint(printFlag, hit,
    p"hits:${hits}, prefetch pattern:${dataVec(OHToUInt(hits)).pattern}\n")
}


class PrefetchStreamerIO(implicit p:Parameters) extends SMSBundle {
  val reqPT = Flipped(Decoupled(new PT2PS()))
  val prefetchReq = Flipped(Decoupled(new PrefetchTrain()))
  val prefetchResp = Decoupled(new PrefetchReq())
}

class PrefetchStreamerEntry(implicit p:Parameters) extends SMSBundle {
  val tag = UInt(psTagBits.W)
  val prefetchPattern = Vec(bitVectorSize, Bool())
}

class PrefetchStreamer(implicit p:Parameters) extends SMSModule {
  def getIndex(key: UInt) = key(log2Ceil(psSets)-1, 0)
  def getTag(key:UInt) = key(log2Ceil(psSets)+psTagBits-1, log2Ceil(psSets))

  def getTargetAddr(curAddr: UInt, offset: UInt): UInt = {
    val curBlock  = getBlock(curAddr)
    val curOffset = getOffset(curBlock)
    // zeal4u: allow overflowing for circular offset
    val newOffset = curOffset + offset

    Cat(getRegion(curBlock), newOffset, 0.U(offsetBits.W))
  }

  val io = IO(new PrefetchStreamerIO())

  val table = Module(new SRAMTemplate(new PrefetchStreamerEntry(), set=psSets, way=psWays, singlePort=false, bypassWrite=true))
  val lrus = new SetAssocReplacer(psSets, psWays, "lru")
  val valids = RegInit(VecInit(Seq.fill(psSets)(VecInit(Seq.fill(psWays)(false.B)))))

  val key = Mux(io.reqPT.fire, getRegion(io.reqPT.bits.block), 
            Mux(io.prefetchReq.fire, getRegion(getBlock(io.prefetchReq.bits.addr)), 0.U))
  val tag = getTag(key)
  val regTag = RegNext(tag)
  val index = getIndex(key)
  val regIndex = RegNext(index)
  table.io.r.req.valid := io.reqPT.fire || io.prefetchReq.fire
  table.io.r.req.bits.setIdx := index
  val dataVec = table.io.r.resp.data
  val dataVecWrite = WireInit(dataVec)

  val req = RegNext(io.prefetchReq.bits)
  val hits = VecInit(Seq.fill(psWays)(false.B))
  val hit = hits.reduce(_||_)
  val victimWay = WireInit(0.U(log2Ceil(psWays).W))

  val draining = RegInit(false.B)
  val drainReq = RegInit(0.U.asTypeOf(new PrefetchTrain))

  val keepDraining = draining || draining && RegNext(io.prefetchReq.fire) && getRegion(getBlock(req.addr)) === getRegion(getBlock(drainReq.addr))
  val drainDataVec = RegEnable(dataVecWrite, !keepDraining && hit)
  val drainHitWay = RegEnable(OHToUInt(hits), !keepDraining && hit)

  for (i <- 0 until psWays) {
    hits(i) := RegNext(io.prefetchReq.fire) && valids(regIndex)(i) & regTag === dataVec(i).tag
  }

  io.prefetchResp.valid := false.B
  io.prefetchResp.bits := DontCare

  table.io.w.req.valid := false.B
  table.io.w.req.bits := DontCare

  io.reqPT.ready := true.B
  io.prefetchReq.ready := !io.reqPT.fire
  when (keepDraining) {
    val prefetchPattern = drainDataVec(drainHitWay).prefetchPattern
    val firstEntry = PriorityEncoder(prefetchPattern)
    val targetAddr = getTargetAddr(drainReq.addr, firstEntry)

    io.prefetchResp.valid := prefetchPattern.asUInt =/= 0.U
    io.prefetchResp.bits.tag := parseFullAddress(targetAddr)._1
    io.prefetchResp.bits.set := parseFullAddress(targetAddr)._2
    io.prefetchResp.bits.needT := drainReq.needT
    io.prefetchResp.bits.source:= drainReq.source
    prefetchPattern(firstEntry) := false.B

    draining := prefetchPattern.asUInt =/= 0.U
    XSPerfPrint(printFlag, p"SMS PS -> Draining, Generate prefetch, block:${getBlock(targetAddr)}, addr:${targetAddr}, " +
      p"pattern:${prefetchPattern}\n")
  }.elsewhen (hit) {
    val firstEntry = PriorityEncoder(dataVec(OHToUInt(hits)).prefetchPattern)
    val targetAddr = getTargetAddr(req.addr, firstEntry)
    io.prefetchResp.valid := dataVec(OHToUInt(hits)).prefetchPattern.asUInt =/= 0.U
    io.prefetchResp.bits.tag := parseFullAddress(targetAddr)._1
    io.prefetchResp.bits.set := parseFullAddress(targetAddr)._2
    io.prefetchResp.bits.needT := req.needT
    io.prefetchResp.bits.source:= req.source
    dataVecWrite(OHToUInt(hits)).prefetchPattern(firstEntry) := false.B
    when(draining) {
      table.io.w.req.valid := true.B
      table.io.w.req.bits.apply(
        setIdx = getIndex(getRegion(getBlock(drainReq.addr))),
        data = drainDataVec,
        waymask = UIntToOH(drainHitWay).asUInt
      )
    }.otherwise {
      draining := dataVecWrite(OHToUInt(hits)).prefetchPattern.asUInt =/= 0.U
      valids(getIndex(getRegion(getBlock(drainReq.addr))))(drainHitWay) := false.B
    }

    drainReq := req
    XSPerfPrint(printFlag, p"SMS PS -> Generate prefetch, block:${getBlock(targetAddr)}, addr:${targetAddr}\n")
  }.elsewhen (RegNext(io.reqPT.fire)) {
    when(hit) {
      victimWay := OHToUInt(hits)
    }.elsewhen(valids(regIndex).asUInt === ((1<<psWays) - 1).U) {
      victimWay := lrus.way(regIndex)
    }.otherwise {
      victimWay := PriorityEncoder(~valids(regIndex).asUInt)
    }

    dataVecWrite(victimWay).tag := regTag
    dataVecWrite(victimWay).prefetchPattern := RegNext(io.reqPT.bits.prefetchPattern)
    valids(regIndex)(victimWay) := true.B

    table.io.w.req.valid := true.B
    table.io.w.req.bits.apply(
      setIdx = regIndex,
      data = dataVecWrite,
      waymask = UIntToOH(victimWay).asUInt
    )
    lrus.access(regIndex, victimWay)
    XSPerfPrint(printFlag, p"SMS PS -> New Train, block:0x${Hexadecimal(RegNext(io.reqPT.bits.block))}, " +
                p"index:${regIndex}, tag:${regTag}" +
                p"bit vector:${RegNext(io.reqPT.bits.prefetchPattern)}\n")
  }

  XSPerfPrint(printFlag, io.prefetchReq.fire, p"SMS PS -> Trigger Prefetch, key:0x${Hexadecimal(key)}, " + 
              p"index:${index}, tag:${tag}\n") 
}

class SpatialMemoryStreaming(implicit p: Parameters) extends SMSModule {
  val io = IO(new PrefetchIO)

  val filterTable = Module(new FilterTable())
  val accumulateTable = Module(new AccumulateTable())
  val patternTable = Module(new PatternHistoryTable())
  val prefetchStreamer = Module(new PrefetchStreamer())

  accumulateTable.io.evict <> io.evict
  accumulateTable.io.train <> io.train
  accumulateTable.io.reqFT <> filterTable.io.resp

  filterTable.io.train <> accumulateTable.io.qeuryFT

  patternTable.io.predictReq <> filterTable.io.queryPT
  patternTable.io.reqAT <> accumulateTable.io.evictResp

  prefetchStreamer.io.reqPT <> patternTable.io.predictResp
  prefetchStreamer.io.prefetchReq <> io.train
  io.req <> prefetchStreamer.io.prefetchResp

  io.resp.ready := true.B
  io.train.ready := true.B
  io.evict.ready := true.B
}
