/**
  * @author zeal4u
  */

package huancun.prefetch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import huancun.HasHuanCunParameters
import huancun.utils.{SRAMTemplate, SetAssocReplacer, XSPerfPrint, ReplacementPolicy, Min, Max}

import java.awt.BufferCapabilities.FlipContents

case class MatryoshkaParameters(
                                 htSets:         Int = 128,
                                 dmaSize:        Int = 16,
                                 dmaConfBits:    Int = 6,
                                 dssSets:        Int = 16,
                                 dssWays:        Int = 8,
                                 dssConfBits:    Int = 9,
                                 scoreBits:      Int = 10,
                                 lineBits:       Int = 9,
                                 deltaNum:       Int = 4,
                                 shortHitWeight: Int = 2,
                                 longHitWeight:  Int = 4,
                               ) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val inflightEntries: Int = 16
}

trait HasMatryoshkaParams extends HasHuanCunParameters {
  val mParams = prefetchOpt.get.asInstanceOf[MatryoshkaParameters]

  val printFlag   = true

  val htSets      = mParams.htSets
  val dmaSize     = mParams.dmaSize
  val dssSets     = mParams.dssSets
  val dssWays     = mParams.dssWays
  val lineBits    = mParams.lineBits
  val deltaNum    = mParams.deltaNum
  val dmaConfBits = mParams.dmaConfBits
  val dssConfBits = mParams.dssConfBits
  val scoreBits   = mParams.scoreBits
  val shortHitWeight = mParams.shortHitWeight
  val longHitWeight = mParams.longHitWeight

  val bottomBits   = pageOffsetBits - lineBits
  val regionBits   = fullAddressBits - pageOffsetBits
  val htTagBits    = regionBits - log2Ceil(htSets)
  val deltaBits    = lineBits + 1
  val deltaSeqBits = deltaBits * deltaNum
  val maxDSSConf   = (1 << dssConfBits) - 1
  val maxDMAConf   = (1 << dmaConfBits) - 1
}

abstract class MatryoshkaModule(implicit p:Parameters) extends PrefetchModule with HasMatryoshkaParams
{
  def getOffset(block: UInt) = block(lineBits-1, 0)
  def getRegion(block: UInt) = block(regionBits+lineBits-1, lineBits)
  def getDelta(curOffset:UInt, lastOffset:UInt) : UInt = {
    Mux(curOffset >= lastOffset, curOffset -& lastOffset, (1.U << (lineBits)) | (lastOffset-&curOffset))
  }

  def getOriginDelta (delta:UInt): SInt = {
    Mux(delta(deltaBits-1), -(((1 << lineBits) - 1).U & delta).asSInt(), delta.asSInt())
  }

  override def getBlock(addr: UInt) = addr(fullAddressBits - 1, bottomBits)
  override def getBlockAddr(addr: UInt) = Cat(addr, 0.U(bottomBits.W))
}
abstract class MatryoshkaBundle(implicit p:Parameters) extends PrefetchBundle with HasMatryoshkaParams

class MPTOut(implicit p:Parameters) extends MatryoshkaBundle {
  val deltaSeq = UInt(deltaSeqBits.W)
}

class MatryoshkaHistoryTableIO(implicit p:Parameters) extends MatryoshkaBundle {
  val train = Flipped(Decoupled(new PrefetchTrain))
  val resp = Decoupled(new MPTOut())
}

class MatryoshkaHistoryTableEntry(implicit p:Parameters) extends MatryoshkaBundle {
  val tag        = UInt(htTagBits.W)
  val lastOffset = UInt(lineBits.W)
  val lastSeq    = UInt((deltaSeqBits-deltaBits).W)
}

class MatryoshkaHistoryTable(implicit p:Parameters) extends MatryoshkaModule {
  def getIndex(addr:UInt) = getPPN(addr)(log2Ceil(htSets)-1, 0)
  def getTag(addr:UInt) = getPPN(addr)(htTagBits+log2Ceil(htSets)-1, log2Ceil(htSets))

  val io = IO(new MatryoshkaHistoryTableIO())
  io.train.ready := true.B

  val table = Module(new SRAMTemplate(new MatryoshkaHistoryTableEntry(), set=htSets, way=1, singlePort=false, bypassWrite=true))
  val valids = RegInit(VecInit(Seq.fill(htSets)(false.B)))
  val index = getIndex(io.train.bits.addr)
  val train = RegNext(io.train.bits)

  table.io.r.req.valid := io.train.fire //&& valids(index)
  table.io.r.req.bits.setIdx := index

  val fireS1 = RegNext(io.train.fire)
  val storedData = table.io.r.resp.data(0)
  val tag = getTag(train.addr)
  val indexS1 = getIndex(train.addr)
  val hit = storedData.tag === tag

  val dataWrite = WireInit(storedData)
  val curOffset = getOffset(getBlock(train.addr))
  val delta = getDelta(curOffset, storedData.lastOffset)
  val newSeq = Cat(storedData.lastSeq, delta)

  dataWrite.lastOffset := curOffset
  dataWrite.lastSeq := newSeq(deltaSeqBits - deltaBits - 1, 0)

  when (fireS1 && hit) {
    table.io.w.req.valid := true.B
    valids(indexS1) := true.B
  }.otherwise {
    // do not replace if it is valid, give a record one chance
    table.io.w.req.valid := fireS1 && !valids(indexS1)
    dataWrite.tag := tag
    valids(indexS1) := ~valids(indexS1)
  }

  table.io.w.req.bits.setIdx := indexS1
  table.io.w.req.bits.data(0) := dataWrite

  io.resp.valid := newSeq(deltaBits*3-1, deltaBits*2) =/= 0.U && newSeq(deltaBits*2-1, deltaBits) =/= 0.U && newSeq(deltaBits-1, 0) =/= 0.U && hit && valids(indexS1)
  io.resp.bits.deltaSeq := newSeq
  XSPerfPrint(printFlag, io.train.fire, p"HT -> New Train, Addr:0x${Hexadecimal(io.train.bits.addr)}, "+
    p"index:${index}\n")
  XSPerfPrint(printFlag, fireS1, p"HT -> Tag:0x${Hexadecimal(tag)}, Hit:${hit}, Valid:${valids(indexS1)}, Current Offset:${curOffset}, Delta:${delta}, "+
    p"d0:${newSeq(deltaBits-1, 0)}, d1:${newSeq(deltaBits*2-1, deltaBits)}, d2:${newSeq(deltaBits*3-1, deltaBits*2)}, d3:${newSeq(deltaBits*4-1, deltaBits*3)}\n")
}

class DeltaSequenceTableIO(implicit p:Parameters) extends MatryoshkaBundle {
  val reqHT = Flipped(Decoupled(new MPTOut()))
  val targetDelta = Decoupled(UInt(deltaBits.W))
}

class DSSEntry(implicit p:Parameters) extends MatryoshkaBundle {
  val deltaSeq = UInt((deltaSeqBits-deltaBits).W)
  val conf = UInt(dssConfBits.W)
}

class DeltaSequenceTable(implicit p:Parameters) extends MatryoshkaModule {
  val io = IO(new DeltaSequenceTableIO())
  io.reqHT.ready := true.B

  // zeal4u: DMA
  val dmaIndex = RegInit(VecInit(Seq.fill(dmaSize)(0.U(deltaBits.W))))
  val dmaConf  = RegInit(VecInit(Seq.fill(dmaSize)(0.U(dmaConfBits.W))))

  /**
    * structure of current delta sequence
    * |---------------------------------------------|
    * | delta 0 | delta 1 | index delta | cur delta |
    * |---------------------------------------------|
    *
    * index delta is stored in DMA
    * delta 0, delta 1 and cur delta are stored in DSS
    */

  val reqFireS1 = RegNext(io.reqHT.fire)
  val curDeltaSeq = io.reqHT.bits.deltaSeq
  val curDelta = curDeltaSeq(deltaBits-1, 0)
  val prefetchSeq = Reg(UInt((deltaSeqBits-deltaBits).W))
  val indexDelta = Mux(io.reqHT.fire, curDeltaSeq(deltaBits*2-1, deltaBits), prefetchSeq(deltaBits-1, 0))
  val dmaHits = VecInit(Seq.fill(dmaSize)(false.B))
  val dmaHit = dmaHits.reduce(_||_)
  val dmaHitIndex = OHToUInt(dmaHits)

  val dmaIndexS1 = RegInit(0.U(log2Ceil(dmaSize).W))

  for (i <- 0 until dmaSize) {
    dmaHits(i) := dmaIndex(i) =/= 0.U && dmaIndex(i) === indexDelta
  }

  //zeal4u: update DMA
  when (io.reqHT.fire) {
    when(dmaHit) {
      dmaConf(dmaHitIndex) := Mux(dmaConf(dmaHitIndex) === maxDMAConf.U, dmaConf(dmaHitIndex), dmaConf(dmaHitIndex) + 1.U)
      // zeal4u: when a counter is saturated, half all their values.
      when(dmaConf(dmaHitIndex) === maxDMAConf.U) {
        for (i <- 0 until dmaSize) {
          dmaConf(i) := dmaConf(i) >> 1
        }
      }
      dmaIndexS1 := dmaHitIndex
      XSPerfPrint(printFlag, p"DMA -> Update, Index:${dmaHitIndex}, Delta:${indexDelta}, Old Conf:${dmaConf(dmaHitIndex)}\n")
    }.otherwise {
      val minOh = Min(dmaConf, ((1 << dmaSize) - 1).U, dmaConfBits)
      val minIndex = OHToUInt(minOh)
      dmaConf(minIndex) := 1.U
      dmaIndex(minIndex) := indexDelta
      dmaIndexS1 := minIndex
      XSPerfPrint(printFlag, p"DMA -> Replace, Index:${minIndex}, Delta:${indexDelta}, Old Delta:${dmaIndex(minIndex)}\n")
    }
  }

  XSPerfPrint(printFlag, io.reqHT.fire, p"DMA -> New Train, Hit:${dmaHit}, Current Delta Seq[d0:${curDeltaSeq(deltaBits-1, 0)}, d1:${curDeltaSeq(deltaBits*2-1, deltaBits)}, " +
    p"d2:${curDeltaSeq(deltaBits*3-1, deltaBits*2)}, d3:${curDeltaSeq(deltaBits*4-1, deltaBits*3)}]\n")

  // zeal4u: DSS
  val dss = Module(new SRAMTemplate(new DSSEntry(), dssSets, dssWays, singlePort=false, bypassWrite=true))
  val dssValids = RegInit(VecInit(Seq.fill(dssSets)(VecInit(Seq.fill(dssWays)(false.B)))))
  val dssHits = VecInit(Seq.fill(dssWays)(false.B))
  val dssHit = dssHits.reduce(_||_)
  val dssHitWay = OHToUInt(dssHits)
  val dmaHitS1 = RegNext(dmaHit)

  // update DSS
  val dssDataVec = dss.io.r.resp.data
  val dssDataVecWrite = WireInit(dssDataVec)

  val cmpDeltaSeqS1 = RegNext(Cat(curDeltaSeq(deltaSeqBits-1, 2*deltaBits), curDelta))
  when (RegNext(io.reqHT.fire && dmaHit)) {
    for (i <- 0 until dssWays) {
      dssHits(i) := cmpDeltaSeqS1 === dssDataVec(i).deltaSeq
    }
  }
  // there must be a update after a read
  when (dmaHitS1 && dssHit) {
    dss.io.w.req.valid := true.B
    dssDataVecWrite(dssHitWay).conf := Mux(dssDataVec(dssHitWay).conf === maxDSSConf.U, dssDataVec(dssHitWay).conf, dssDataVec(dssHitWay).conf + 1.U)
    dss.io.w.req.bits.apply(
      setIdx = dmaIndexS1,
      data = dssDataVecWrite,
      waymask = dssHits.asUInt
    )
    dssValids(dmaIndexS1)(dssHitWay) := true.B
    XSPerfPrint(printFlag, p"DSS -> Update, Index:${dmaIndexS1}, Hit Way:${dssHitWay}, Seq:0x${Hexadecimal(dssDataVecWrite(dssHitWay).deltaSeq)}, Conf:${dssDataVecWrite(dssHitWay).conf}\n")
  }.elsewhen (reqFireS1) {
    val confs = dssDataVec.map(_.conf)
    val victimWay = Wire(UInt(log2Ceil(dssWays).W))
    when (dssValids(dmaIndexS1).asUInt =/= ((1<<dssWays) -1).U) {
      victimWay := PriorityEncoder(~dssValids(dmaIndexS1).asUInt)
    }.otherwise {
      val minOh = Min(confs, dssValids(dmaIndexS1).asUInt, dssConfBits)
      victimWay := OHToUInt(minOh)
    }

    dssDataVecWrite(victimWay).conf := 1.U
    dssDataVecWrite(victimWay).deltaSeq := cmpDeltaSeqS1

    dss.io.w.req.valid := true.B
    dss.io.w.req.bits.apply(
      setIdx = dmaIndexS1,
      data = dssDataVecWrite,
      waymask = UIntToOH(victimWay).asUInt
    )
    dssValids(dmaIndexS1)(victimWay) := true.B

    when (!dmaHitS1) {
      dssValids(dmaIndexS1).zipWithIndex.foreach{case (v,i) => when(i.U =/= victimWay) {v := false.B}}
      XSPerfPrint(printFlag, p"DSS -> Invalid Set, Index:${dmaIndexS1}\n")
    }
    XSPerfPrint(printFlag, p"DSS -> Replace, Index:${dmaIndexS1}, Victim Way:${victimWay}, Seq:0x${Hexadecimal(cmpDeltaSeqS1)}\n")
  }.otherwise {
    dss.io.w.req.valid := false.B
    dss.io.w.req.bits := DontCare
  }

  // for recursive lookahead mechanism
  val predictDelta = WireInit(0.U(deltaBits.W))
  /**
    *   prefetch sequence :    d0 d1 d2
    *   long tag:              d0 d1
    *   short tag:                d1
    *   index delta:                 d2
    */
  def getLongTag(seq:UInt) = seq(deltaBits*3-1, deltaBits)
  def getShortTag(seq:UInt) = seq(deltaBits*2-1, deltaBits)

  val longTag = getLongTag(prefetchSeq)
  val shortTag = getShortTag(prefetchSeq)

  val dmaPredictHit = dmaHit && !io.reqHT.fire
  val dmaPredictHitS2 = RegNext(dmaPredictHit)
  val longHitsS2 = VecInit(Seq.fill(dssWays)(false.B))
  val shortHitsS2 = VecInit(Seq.fill(dssWays)(false.B))
  val targetDeltasS2 = VecInit(Seq.fill(dssWays)(0.U(deltaBits.W)))
  val confS2 = VecInit(Seq.fill(dssWays)(0.U(dssConfBits.W)))

  XSPerfPrint(printFlag, dmaPredictHit, p"DMA -> DMA Lookahead, Prefetch Seq[d0:${prefetchSeq(deltaBits-1, 0)}, d1:${prefetchSeq(deltaBits*2-1, deltaBits)}, " +
    p"d2:${prefetchSeq(deltaBits*3-1, deltaBits*2)}], Long Tag:0x${Hexadecimal(longTag)}, Short Tag:0x${Hexadecimal(shortTag)}\n")

  // read dss for prefetching
  when (dmaPredictHitS2) {
    for (i <- 0 until dssWays) {
      longHitsS2(i) := dssValids(dmaIndexS1)(i) && getLongTag(dssDataVec(i).deltaSeq) === longTag
      shortHitsS2(i) := dssValids(dmaIndexS1)(i) && getShortTag(dssDataVec(i).deltaSeq) === shortTag
      targetDeltasS2(i) := dssDataVec(i).deltaSeq(deltaBits-1, 0)
      confS2(i) := dssDataVec(i).conf
    }
  }

  io.targetDelta.valid := false.B
  io.targetDelta.bits := DontCare
  // compute scores and compare them with thresholds
  when (shortHitsS2.reduce(_||_)) {
    val finalScores = RegInit(VecInit(Seq.fill(dssWays)(0.U(scoreBits.W))))
    var sumScore = WireInit(0.U((scoreBits+1).W))
    val currentScores = (0 until dssWays).map(j => Mux(longHitsS2(j), longHitWeight.U, Mux(shortHitsS2(j), shortHitWeight.U, 0.U)) * confS2(j))
    val validDeltas = (0 until dssWays).map(i => (i until dssWays).map(j => if (i == j) true.B else targetDeltasS2(i) === targetDeltasS2(j)).reduce(_&&_))
    for (i <- 0 until dssWays) {
      sumScore = sumScore + currentScores(i)
      var eachScore = WireInit(0.U(scoreBits.W))
      for (j <- 0 until dssWays) {
        eachScore = eachScore + Mux(targetDeltasS2(i) === targetDeltasS2(j), currentScores(j), 0.U)
      }
      finalScores(i) := Mux(validDeltas(i), eachScore, 0.U)
    }

    val candArbiter = Module(new Arbiter(UInt(deltaBits.W), dssWays))
    candArbiter.io.out.ready := true.B
    // zeal4u: delta that has a conf over half of the sum can be prefetched
    for (i <- 0 until dssWays) {
      candArbiter.io.in(i).valid := (finalScores(i) << 1) > sumScore
      candArbiter.io.in(i).bits :=  targetDeltasS2(i)
    }
    predictDelta := candArbiter.io.out.bits
    io.targetDelta.valid := candArbiter.io.out.fire
    io.targetDelta.bits := candArbiter.io.out.bits
    XSPerfPrint(printFlag, p"DSS -> Lookahead, Candidate Deltas:${targetDeltasS2}\n")
    XSPerfPrint(printFlag, p"DSS -> Lookahead, Final Scores:${finalScores}\n")
    XSPerfPrint(printFlag, p"DSS -> Lookahead, Predict Delta:${predictDelta}, Final Score:${finalScores(candArbiter.io.chosen)}\n")
    XSPerfPrint(printFlag, p"DSS -> Lookahead, Long Hit:${longHitsS2}\n")
    XSPerfPrint(printFlag, p"DSS -> Lookahead, Short Hit:${shortHitsS2}\n")
  }

  prefetchSeq := Mux(io.reqHT.fire, io.reqHT.bits.deltaSeq, Cat(prefetchSeq(deltaBits*2-1, 0), predictDelta))
  dss.io.r.req.valid := dmaHit
  dss.io.r.req.bits.setIdx := dmaHitIndex
}

class Matryoshka (implicit p:Parameters) extends MatryoshkaModule {
  val io = IO(new PrefetchIO)
  io.resp.ready := true.B
  io.evict.ready := true.B

  val historyTable = Module(new MatryoshkaHistoryTable())
  val patternTable = Module(new DeltaSequenceTable())

  historyTable.io.train <> io.train
  patternTable.io.reqHT <> historyTable.io.resp

  patternTable.io.targetDelta.ready := true.B

  val train = Reg(new PrefetchTrain)
  val trainWrite = WireInit(train)
  val targetDelta = getOriginDelta(patternTable.io.targetDelta.bits)
  val targetAddr = getBlockAddr((getBlock(train.addr).asSInt() + targetDelta).asUInt())

  trainWrite.tag := parseFullAddress(targetAddr)._1
  trainWrite.set := parseFullAddress(targetAddr)._2

  train := Mux(io.train.fire, io.train.bits,
           Mux(patternTable.io.targetDelta.fire, trainWrite, train))

  io.req.valid := patternTable.io.targetDelta.fire
  io.req.bits <> trainWrite
}