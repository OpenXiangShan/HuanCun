package huancun.prefetch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import huancun.HasHuanCunParameters
import huancun.utils.{XSPerfPrint,ReplacementPolicy, Min, Max}


case class SPPParameters(
                        sigTableSets: Int = 1,
                        sigTableWays: Int = 256,
                        sigTableTagBits: Int = 16,
                        sigShift: Int = 3,
                        sigBits: Int = 12,
                        sigOffsetBits: Int = 6,
                        patternTableSets: Int = 512,
                        patternTableWays: Int = 1,
                        patternTableDeltaPerEntry: Int = 4,
                        sigCounterBits: Int = 4,
                        deltaCounterBits: Int = 4,
                        quotientBits: Int = 10,
                        remainderBits: Int = 6,
                        fillThreshold: Int = 90,
                        pfThreshold: Int = 25,
                        globalCounterBits: Int = 10,
                        globalHistoryRegisterEntries: Int = 8,
                        prefetchDegree: Int = 8,
                        trainQueueSize: Int = 1
                        ) extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 16
}

trait HasSPPParams extends HasHuanCunParameters {
  val sppParams = prefetchOpt.get.asInstanceOf[SPPParameters]
  val printFlag = false

  val sigTableSets = sppParams.sigTableSets
  val sigTableWays = sppParams.sigTableWays
  val sigTableTagBits = sppParams.sigTableTagBits
  val sigShift = sppParams.sigShift
  val sigBits = sppParams.sigBits
  val sigOffsetBits = sppParams.sigOffsetBits
  val patternTableSets = sppParams.patternTableSets
  val patternTableWays = sppParams.patternTableWays
  val patternTableDeltaPerEntry = sppParams.patternTableDeltaPerEntry
  val sigCounterBits = sppParams.sigCounterBits
  val deltaCounterBits = sppParams.deltaCounterBits
  val quotientBits = sppParams.quotientBits
  val remainderBits = sppParams.remainderBits
  val fillThreshold = sppParams.fillThreshold
  val pfThreshold = sppParams.pfThreshold
  val globalCounterBits = sppParams.globalCounterBits
  val globalHistoryRegisterEntries = sppParams.globalHistoryRegisterEntries
  val prefetchDegree = sppParams.prefetchDegree
  val trainQueueSize = sppParams.trainQueueSize

  val sigMask = (1 << sigBits) - 1
  val sigTableTagMask = (1 << sigTableTagBits) - 1
  val sigCounterMax = (1 << sigCounterBits) - 1
  val deltaCounterMax = (1 << deltaCounterBits) - 1
  val filterSets = (1 << quotientBits)
  val globalCounterMax = (1 << globalCounterBits) - 1
  val sigDeltaBits = sigOffsetBits + 1
  val ptEntriesMask = (1 << patternTableDeltaPerEntry) - 1
}

abstract class SPPModule(implicit p: Parameters) extends PrefetchModule with HasSPPParams 
{
  def getBlock(addr: UInt) = addr(fullAddressBits - 1, offsetBits)
  def getBlockAddr(addr: UInt) = Cat(addr, 0.U(offsetBits.W))
}

abstract class SPPBundle(implicit p: Parameters) extends PrefetchBundle with HasSPPParams
{
  def getBlock(addr: UInt) = addr(fullAddressBits - 1, offsetBits)
  def getBlockAddr(addr: UInt) = Cat(addr, 0.U(offsetBits.W))
}

class SignatureTableEntry(implicit p:Parameters) extends SPPBundle {
  val tag = UInt(sigTableTagBits.W)
  val lastOffset = UInt(sigOffsetBits.W)
  val signature = UInt(sigBits.W)

  // override def cloneType = new SignatureTableEntry().asInstanceOf[this.type]
}

class ST2PT(implicit p:Parameters) extends SPPBundle {
  val lastSignature = UInt(sigBits.W)
  val curDelta = UInt(sigDeltaBits.W)

  // override def cloneType = new ST2PT().asInstanceOf[this.type]
}

class SignatureTableIO(implicit p:Parameters) extends SPPBundle {
  val updateReq = Flipped(Decoupled(new Bundle() {
    val block = UInt(blockBytes.W)
  }))
  val updateResp = Decoupled(new ST2PT())
}

class SignatureTable(implicit p:Parameters) extends SPPModule {
  val io = IO(new SignatureTableIO())

  def getPage(block:UInt) = block(block.getWidth-1, log2Up(blockBytes))
  def getSetIdx(page:UInt) = if (sigTableSets > 1) page(log2Up(sigTableSets)-1,0) else 0.U
  def getTag(page:UInt) = if (sigTableSets > 1) page(log2Up(sigTableSets)+sigTableTagBits, log2Up(sigTableSets))
                          else page(sigTableTagBits, 0)
  def getOffset(block:UInt) = block(sigOffsetBits-1,0)
  def getDelta(curOffset:UInt, lastOffset:UInt) = Mux(curOffset >= lastOffset,
                                                      curOffset -& lastOffset, (1.U << (sigOffsetBits)) | (lastOffset-&curOffset))
  def updateSignature (sig:UInt, delta:UInt): UInt = {
    (sig << sigShift) | delta
  }

  if (sigTableSets == 1) {
    val mem = Reg(Vec(sigTableWays, new SignatureTableEntry()))
    val lru = ReplacementPolicy.fromString("lru", sigTableWays)

    val block = io.updateReq.bits.block
    val hits = VecInit(Seq.fill(sigTableWays) {false.B})
    val readArb = Module(new Arbiter(new SignatureTableEntry(), sigTableWays))
    val readResult = readArb.io.out.bits


    io.updateReq.ready := true.B
    readArb.io.out.ready := true.B

    io.updateResp.valid := readArb.io.out.valid
    io.updateResp.bits.lastSignature := readArb.io.out.bits.signature
    io.updateResp.bits.curDelta := getDelta(getOffset(block), readArb.io.out.bits.lastOffset)
    XSPerfPrint(printFlag, io.updateResp.valid, p"SPP ST -> Return Last Signature:0x${Hexadecimal(io.updateResp.bits.lastSignature)}," +
                p"Current Delta:0x${Hexadecimal(io.updateResp.bits.curDelta)}\n")

    for (i <- 0 until sigTableWays) {
      hits(i) := io.updateReq.fire() && mem(i).tag === getTag(getPage(block))
      readArb.io.in(i).valid := io.updateReq.fire() && mem(i).tag === getTag(getPage(block))
      readArb.io.in(i).bits := mem(i)
    }

    when(hits.reduce(_ || _)) {
      val idx = PriorityEncoder(hits)
      mem(idx).signature := updateSignature(readResult.signature, getDelta(getOffset(block), readResult.lastOffset))
      mem(idx).lastOffset := getOffset(block)
      lru.access(idx)
      XSPerfPrint(printFlag, p"SPP ST -> Record Hit, Tag:0x${Hexadecimal(mem(idx).tag)}, " +
        p"Update Signature from 0x${Hexadecimal(mem(idx).signature)} to " +
        p"0x${Hexadecimal(updateSignature(readResult.signature, getDelta(getOffset(block), readResult.lastOffset)))}, \n" +
        p"Update Offset from 0x${Hexadecimal(mem(idx).lastOffset)} to 0x${Hexadecimal(getOffset(block))}\n")
    }.otherwise {
      val idx = lru.way
      lru.miss
      mem(idx).signature := 0.U
      mem(idx).lastOffset := getOffset(block)
      mem(idx).tag := getTag(getPage(block))
      XSPerfPrint(printFlag, p"SPP ST -> Record Miss, Reset Tag 0x${Hexadecimal(getTag(getPage(block)))}, \n" +
        p"Reset Signature from 0x${Hexadecimal(mem(idx).signature)} to 0x0, " +
        p"Reset Offset to 0x${Hexadecimal(getOffset(block))}\n")
    }
  }
}

class PatternTableEntry(implicit p:Parameters) extends SPPBundle {
  val deltas = Vec(patternTableDeltaPerEntry, UInt(sigDeltaBits.W))
  val deltaCounters = Vec(patternTableDeltaPerEntry, UInt(deltaCounterBits.W))
  val sigCounter = UInt(sigCounterBits.W)

  // override def cloneType = new PatternTableEntry().asInstanceOf[this.type]
}

class PatternTableIO(implicit p:Parameters) extends SPPBundle {
  val req = Flipped(Decoupled(new ST2PT()))

  val resp = Decoupled(new Bundle() {
    val lookaheadDelta = SInt(sigDeltaBits.W)
  })
}

class PatternTable(implicit p:Parameters) extends SPPModule {
  val io = IO(new PatternTableIO)

  def updateSignature (sig:UInt, delta:UInt): UInt = {
    (sig << sigShift) | delta
  }

  def getOriginDelta (delta:UInt): SInt = {
    Mux(delta(sigDeltaBits-1), -(((1 << sigOffsetBits) - 1).U & delta).asSInt(), delta.asSInt())
  }

  val table = Mem(patternTableSets, new PatternTableEntry())
  val req = Reg(new ST2PT())

  io.req.ready := true.B // what about querying ?

  val nextSig = Reg(UInt(sigBits.W))
  val score = Reg(UInt(8.W))
  io.resp.valid := false.B
  io.resp.bits.lookaheadDelta := 0.S

  val prefetchCounter = Counter(prefetchDegree+1)
  val doPrefetching = RegInit(false.B)

  // update the metadata at any stage when a request coming
  when (io.req.fire()) {
    XSPerfPrint(printFlag, p"Pattern Table -> Access, Sig:0x${Hexadecimal(io.req.bits.lastSignature)}, Delta:${io.req.bits.curDelta}\n")
    // for update
    val entry = table.read(io.req.bits.lastSignature)
    val minWay = Wire(Vec(patternTableDeltaPerEntry, Bool()))
    val hits   = Wire(Vec(patternTableDeltaPerEntry, Bool()))
    minWay := Min(entry.deltaCounters, ptEntriesMask.U, deltaCounterBits)
    for (i <- 0 until patternTableDeltaPerEntry) {
      hits(i) := entry.deltas(i) === io.req.bits.curDelta
    }

    val entryUpdateWire = Wire(new PatternTableEntry())
    entryUpdateWire := entry
    when(hits.reduce(_||_)){
      val counter = entryUpdateWire.deltaCounters(PriorityEncoder(hits))
      val counter1 = entry.deltaCounters(PriorityEncoder(hits))
      counter := Mux(counter1 === deltaCounterMax.U, counter1, counter1 + 1.U)
      entryUpdateWire.sigCounter := entry.sigCounter + 1.U
    }.otherwise {
      entryUpdateWire.sigCounter := entry.sigCounter - entry.deltaCounters(PriorityEncoder(minWay)) + 1.U
      entryUpdateWire.deltas(PriorityEncoder(minWay)) := io.req.bits.curDelta
      entryUpdateWire.deltaCounters(PriorityEncoder(minWay)) := 1.U
    }
    table.write(io.req.bits.lastSignature, entryUpdateWire)

    XSPerfPrint(printFlag, p"Pattern Table -> Old Record, Deltas:${entry.deltas.toPrintable}, "+
                p"Delta Counters:${entry.deltaCounters}, Sig Counter:${entry.sigCounter}\n")
    XSPerfPrint(printFlag, p"Pattern Table -> Update, Deltas:${entryUpdateWire.deltas.toPrintable}, "+
                p"Delta Counters:${entryUpdateWire.deltaCounters}, Sig Counter:${entryUpdateWire.sigCounter}\n")

    // for query and generate prefetches if hit
    // new request will interrupt the generation process
    prefetchCounter.reset()
    doPrefetching := true.B
    nextSig := updateSignature(io.req.bits.lastSignature, io.req.bits.curDelta)
    score := 100.U
  }.elsewhen(doPrefetching) {
    val entry = table.read(nextSig)
    val maxWay = Max(entry.deltaCounters, ptEntriesMask.U, deltaCounterBits)
    val delta = Mux1H(maxWay, entry.deltas)
    val cur_score = score * entry.deltaCounters(PriorityEncoder(maxWay)) / entry.sigCounter
    nextSig := updateSignature(nextSig, delta)
    when (entry.sigCounter === 0.U
        || cur_score < pfThreshold.U
        || prefetchCounter.value === (prefetchCounter.n-1).U) {
      XSPerfPrint(printFlag, p"Pattern Table -> Lookahead Over\n")
      doPrefetching := false.B
    }.otherwise {
      prefetchCounter.inc()
      io.resp.valid := true.B
      // get the target of the highest score due to the limit of microarchitecture
      io.resp.bits.lookaheadDelta := getOriginDelta(delta)
      XSPerfPrint(printFlag, p"Pattern Table -> Return, Delta:${getOriginDelta(delta)}\n")
    }
    XSPerfPrint(printFlag, p"Pattern Table -> Cur Sig:0x${Hexadecimal(nextSig)}, Next Sig:0x${Hexadecimal(updateSignature(nextSig, delta))}\n")
  }
}

class SignaturePathPrefetcher(implicit p: Parameters) extends SPPModule {
  val io = IO(new PrefetchIO)

  val signatureTable = Module(new SignatureTable())
  val patternTable = Module(new PatternTable())

  val train = Reg(new PrefetchTrain)
  val next_train = Wire(new PrefetchTrain)
  next_train := train

  signatureTable.io.updateReq.valid := io.train.fire()
  signatureTable.io.updateReq.bits.block := getBlock(io.train.bits.addr)

  patternTable.io.req <> signatureTable.io.updateResp
  patternTable.io.resp.ready := true.B

  val predictDelta = patternTable.io.resp.bits.lookaheadDelta
  val predictAddr = getBlockAddr((getBlock(train.addr).asSInt() + predictDelta).asUInt())
  next_train.tag := parseFullAddress(predictAddr)._1
  next_train.set := parseFullAddress(predictAddr)._2

  train := Mux(io.train.fire(), io.train.bits, next_train)
  
  io.req.valid := patternTable.io.resp.fire()
  io.req.bits.tag := parseFullAddress(predictAddr)._1
  io.req.bits.set := parseFullAddress(predictAddr)._2
  io.req.bits.needT := train.needT
  io.req.bits.source := train.source

  io.resp.ready := true.B
  io.train.ready := true.B
  io.evict.ready := true.B
  XSPerfPrint(true, io.evict.fire(), p"SPP -> Recieve Eviction Info, Address: ${Hexadecimal(io.evict.bits.addr)}\n")
  XSPerfPrint(printFlag, io.req.fire(), p"SPP-> Predicted Address: ${Hexadecimal(io.req.bits.addr)}\n")
}

