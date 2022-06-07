package huancun.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{Random, SeqToAugmentedSeq}
import chipsalliance.rocketchip.config.Parameters
import huancun.HasHuanCunParameters
import huancun.utils.{XSPerfPrint, Min}
// import xiangshan.utils.XSDebug

// case object SandboxParamsKey extends Field[SandboxParameters]

case class SandboxParameters(
                              roundRobinMax: Int = 256,
                              // cacheName: String, // distinguish between different prefetchers
                              offsetList: Seq[Int] = Seq(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -4, -5, -6, -7, -8),
                              unusedOffsetList: Seq[Int] = Seq(9, 10, 11, 12, 13, 14, 15, 16, -9, -10, -11, -12, -13, -14, -15, -16),
                              // offsetBits: Int = 6,
                              prefetchThreshold: Int = 3,
                              bloomSize: Int = 2048,
                              bloomQueryPortN: Int = 4,
                              prefetchQueueSize: Seq[Int] = Seq(4, 2, 2),
                              prefetchDegree: Int = 3,
                              offsetUpdateSize: Int = 4
                            ) extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 16
}

trait HasSandboxParams extends HasHuanCunParameters {
  val sandboxParams = prefetchOpt.get.asInstanceOf[SandboxParameters]
  val printFlag = false

  val bloomSize = sandboxParams.bloomSize
  val bloomQueryPortN = sandboxParams.bloomQueryPortN
  val roundRobinMax = sandboxParams.roundRobinMax
  val offsetList = sandboxParams.offsetList
  val unusedOffsetList =  sandboxParams.unusedOffsetList
  // override val offsetBits = sandboxParams.offsetBits
  val prefetchThreshold = sandboxParams.prefetchThreshold
  val prefetchQueueSize = sandboxParams.prefetchQueueSize
  val prefetchDegree = sandboxParams.prefetchDegree
  val offsetUpdateSize = sandboxParams.offsetUpdateSize

  val accuracyCounterMax = 4 * roundRobinMax
  val threshold1 = roundRobinMax
  val threshold2 = 2 * roundRobinMax
  val threshold3 = 3 * roundRobinMax
  val accuracyCounterWidth = log2Up(accuracyCounterMax)
  val candidatePrefetcherNWidth = log2Up(offsetList.length)
}

abstract class SPModule(implicit p: Parameters) extends PrefetchModule with HasSandboxParams {
  def getBlock(addr: UInt) = addr(fullAddressBits - 1, offsetBits)
  def getBlockAddr(addr: UInt) = Cat(addr, 0.U(offsetBits.W))
}

abstract class SPBundle(implicit p: Parameters) extends PrefetchBundle with HasSandboxParams {
  def getBlock(addr: UInt) = addr(fullAddressBits - 1, offsetBits)
  def getBlockAddr(addr: UInt) = Cat(addr, 0.U(offsetBits.W))
}

class BloomFilterBundle(implicit p: Parameters) extends SPBundle {
  val update: Bool = Input(Bool())
  val query: Bool = Input(Bool())
  val reset: Bool = Input(Bool())
  val queryData: Vec[UInt] = Input(Vec(bloomQueryPortN, UInt(fullAddressBits.W)))
  val updateData : UInt = Input(UInt(fullAddressBits.W))

  val hit: Vec[Bool] = Output(Vec(bloomQueryPortN, Bool()))
}

class BloomFilter(implicit p: Parameters) extends SPModule {
  val io = IO(new BloomFilterBundle)


  def hashBloom(key: UInt) = {
    val mask = bloomSize - 1
    val firstHalf = key
    val secHalf = (key >> 12)
    // assert((firstHalf ^ secHalf) < bloomSize.U, "Sandbox bloom hash error!")
    (firstHalf ^ secHalf) & mask.U
  }

  // assert(!(io.update || io.query || io.reset) || (!(io.update && io.query && io.reset) && (io.update ^ io.query ^ io.reset)), "Sandbox bloom filter error")

  val bloomReg = RegInit(VecInit(Seq.fill(bloomSize)(false.B)))

  for (i <- 0 until bloomQueryPortN) {
    io.hit(i) := io.query && bloomReg(hashBloom(io.queryData(i)))
  }

  when(io.reset) {
    for (i <- 0 until bloomSize) {
      bloomReg(i) := false.B
    }
  }.elsewhen(io.update) {
      bloomReg(hashBloom(io.updateData)) := true.B
  }
}

/**
 * 预取和训练同时进行
 *
 * @param p
 */
class SandboxPrefetcher(implicit p: Parameters) extends SPModule {
  val io = IO(new PrefetchIO)

  io.resp.ready := true.B
  io.train.ready := true.B
  io.evict.ready := true.B
  XSPerfPrint(true, io.evict.fire(), p"SP -> Recieve Eviction Info, Address: ${Hexadecimal(io.evict.bits.addr)}\n")


  val bloomFilter = Module(new BloomFilter)

  // core registers for learning
  val usedOffsets = RegInit(VecInit(offsetList.indices map { i => offsetList(i).S(offsetBits.W) }))
  val unusedOffsets = RegInit(VecInit(unusedOffsetList.indices map { i => unusedOffsetList(i).S(offsetBits.W) }))
  val offsetUpdateStages = Counter(offsetUpdateSize)

  val histAccuracies = Reg(Vec(offsetList.length, UInt(accuracyCounterWidth.W)))
  val mask = RegInit(VecInit(Seq.fill(offsetList.length) {true.B}))
  val readyToPrefetch1 = RegInit(VecInit(offsetList.indices map { i => false.B }))
  val readyToPrefetch2 = RegInit(VecInit(offsetList.indices map { i => false.B }))
  val readyToPrefetch3 = RegInit(VecInit(offsetList.indices map { i => false.B }))

  val readyToPrefetchs = Seq(readyToPrefetch1, readyToPrefetch2, readyToPrefetch3)
  /**
   * *****************Learning phase****************************
   */

  val s0TrainValid = RegNext(io.train.fire())
  val s0TrainAccess = RegNext(io.train.bits)

  val currentCandidate = Counter(offsetList.length)
  val roundRobinCount = Counter(roundRobinMax)
  val curAccuracy = RegInit(0.U(accuracyCounterWidth.W))
  val curOffset = usedOffsets(currentCandidate.value)

  val curBlock = getBlock(s0TrainAccess.addr)
  // be careful with minus
  // for stream detector

  bloomFilter.io.query := s0TrainValid
  for (i <- 0 until bloomQueryPortN) {
    bloomFilter.io.queryData(i) := (curBlock.asSInt() - i.S * curOffset).asUInt()
  }
  when (s0TrainValid) {
    roundRobinCount.inc()
    XSPerfPrint(printFlag, p"SP -> [Training] S0, RoundRobin current value:${roundRobinCount.value}\n")
  }

  // query and update at the same time
  val nextBlock_ = (curBlock.asSInt() + curOffset).asUInt()
  // cross page is forbiden
  val nextBlock = Mux(getPPN(getBlockAddr(nextBlock_)) === getPPN(s0TrainAccess.addr), nextBlock_, Cat(curBlock(7, 6), nextBlock_(5, 0)))
  bloomFilter.io.update := s0TrainValid
  bloomFilter.io.updateData := nextBlock
  XSPerfPrint(printFlag, p"SP -> [Training] S0, Train start, query and update bloom :${s0TrainValid}, address:${s0TrainAccess.addr}, current block:${curBlock}, next block:${nextBlock}\n")

  val newAccuracy_ = curAccuracy + PopCount(bloomFilter.io.hit)
  val newAccuracy = Mux(newAccuracy_ < curAccuracy, curAccuracy, newAccuracy_)
  curAccuracy := newAccuracy
  XSPerfPrint(printFlag, p"SP -> [Training] S0, Current offset:${curOffset}, accuracy:${curAccuracy}, new accuracy:${newAccuracy}\n")

  // training finish for all the candidates
  val s1_update_offsets_list = RegInit(false.B)

  when(RegNext(roundRobinCount.value) =/= (roundRobinCount.n-1).U && roundRobinCount.value === (roundRobinCount.n-1).U) {
    readyToPrefetch1(currentCandidate.value) := newAccuracy >= threshold1.U
    readyToPrefetch2(currentCandidate.value) := newAccuracy >= threshold2.U
    readyToPrefetch3(currentCandidate.value) := newAccuracy >= threshold3.U
    histAccuracies(currentCandidate.value) := newAccuracy
    curAccuracy := 0.U

    // check next candidate
    currentCandidate.inc()

    // one round for all offsets finished
    when (currentCandidate.value === (currentCandidate.n - 1).U) {
      mask := VecInit(Seq.fill(offsetList.length) {true.B})
      s1_update_offsets_list := true.B
    }

    bloomFilter.io.reset  := true.B
    XSPerfPrint(printFlag, p"SP -> [Training] Round for an offset over, offset:${curOffset}, accuracy:${newAccuracy}\n")
  }.otherwise {
    bloomFilter.io.reset  := false.B
  }

  // XSDebug(p"***************************Start Updating Offsets****************************\n")
  when (s1_update_offsets_list) {
    val minFlags = Min(histAccuracies, mask.asUInt(), accuracyCounterWidth)
    val updateIndex = PriorityEncoder(minFlags)
    val replaceIndex = Random(unusedOffsets.length)
    mask(updateIndex) := false.B
    histAccuracies(updateIndex) := 0.U
    usedOffsets(updateIndex) := unusedOffsets(replaceIndex)
    unusedOffsets(replaceIndex) := usedOffsets(updateIndex)

    offsetUpdateStages.inc()
    when (offsetUpdateStages.value === (offsetUpdateStages.n - 1).U) {
      s1_update_offsets_list := false.B
    }
    when (updateIndex === currentCandidate.value) {
      curAccuracy := 0.U;
    }
    XSPerfPrint(printFlag, p"SP -> [Training] S1, Update offset list, step:${offsetUpdateStages.value}, old offset:${usedOffsets(updateIndex)}, "+
          "accuracy:${histAccuracies(updateIndex)}, new offset:${unusedOffsets(replaceIndex)}\n")
  }
  // *************************************************************

  /**
   * ****************Prefetching Phase****************************
   */

  val prefetchQueues = prefetchQueueSize.map { m =>
      Module(new Queue(new PrefetchReq, m))
  }

  val prefetchArbs = prefetchQueueSize.map { _ =>
      Module(new Arbiter(new PrefetchReq, offsetList.length))
  }

  val prefetchCountDown = Counter(3)
  val s1prefetchAccess = Reg(new PrefetchTrain)
  val s1PrefetchValid = RegInit(false.B)

  val prefetchBloomFilter = Module(new BloomFilter)
  val prefetchCounter = Counter(roundRobinMax)
  val selectedOffset = RegInit(VecInit(Seq.fill(usedOffsets.length)(false.B)))

  when (s0TrainValid) {
    prefetchCountDown.reset()
    s1PrefetchValid := true.B
    s1prefetchAccess := s0TrainAccess
    selectedOffset := VecInit(Seq.fill(usedOffsets.length)(false.B))
  }.elsewhen(prefetchCountDown.value === (prefetchCountDown.n - 1).U) {
    s1PrefetchValid := false.B
  }.otherwise {
    prefetchCountDown.inc()
  }
  XSPerfPrint(printFlag, p"SP -> [Prefetching] count down:${prefetchCountDown.value}, prefetch valid:${s1PrefetchValid}\n")

  def DecidePrefetch(arbiter: Arbiter[PrefetchReq], readyToPrefetch: Seq[Bool], prefetchQ: Queue[PrefetchReq], confLevel: UInt): Unit = {
    // XSDebug(p"${arbiter} decides prefetch.\n")
    for (i <- offsetList.indices) {
      // arbiter.io.in(i).bits.write := s1prefetchAccess.write
      val testNewaddr = getBlockAddr((getBlock(s1prefetchAccess.addr).asSInt() + confLevel * usedOffsets(i)).asUInt())
      val crossPage = getPPN(testNewaddr) =/= getPPN(s1prefetchAccess.addr) // unequal tags
      assert(testNewaddr =/= 0.U, "Sandbox got a zero address!")
      arbiter.io.in(i).valid := s1PrefetchValid && readyToPrefetch(i) && !crossPage && !selectedOffset(i)
      arbiter.io.in(i).bits.needT := s1prefetchAccess.needT
      arbiter.io.in(i).bits.source := s1prefetchAccess.source
      arbiter.io.in(i).bits.tag := parseFullAddress(testNewaddr)._1
      arbiter.io.in(i).bits.set := parseFullAddress(testNewaddr)._2
      when (s1PrefetchValid && readyToPrefetch(i) && !selectedOffset(i)) {
        XSPerfPrint(printFlag, p"SP -> [Prefetching] Generate prefetches, offset:${usedOffsets(i)}, accuracy:${histAccuracies(i)}, addr:${testNewaddr}, cross page:${crossPage}\n")
      }
    }
    arbiter.io.out.ready := prefetchQ.io.enq.ready
    prefetchQ.io.enq.valid := arbiter.io.out.valid
    prefetchQ.io.enq.bits := arbiter.io.out.bits
    selectedOffset(arbiter.io.chosen) := arbiter.io.out.valid
  }

  for (i <- 0 until prefetchQueueSize.length) {
    DecidePrefetch(prefetchArbs(i), readyToPrefetchs(i), prefetchQueues(i), (i+1).U)
  }

  for (i <- (0 until (prefetchQueueSize.length-1)).reverse) {
    prefetchQueues(i+1).io.enq <> prefetchQueues(i).io.deq
  }

  prefetchBloomFilter.io.query := false.B
  prefetchBloomFilter.io.update := false.B
  prefetchBloomFilter.io.reset := false.B
  prefetchBloomFilter.io.queryData := DontCare
  prefetchBloomFilter.io.updateData := DontCare

  when (prefetchQueues(prefetchQueues.length-1).io.deq.valid) {
    prefetchBloomFilter.io.query := prefetchQueues(prefetchQueues.length-1).io.deq.valid
    prefetchBloomFilter.io.queryData(0) := prefetchQueues(prefetchQueues.length-1).io.deq.bits.addr 
    prefetchBloomFilter.io.update := prefetchQueues(prefetchQueues.length-1).io.deq.valid 
    prefetchBloomFilter.io.updateData := prefetchQueues(prefetchQueues.length-1).io.deq.bits.addr
  }

  val reqValid = prefetchQueues(prefetchQueues.length-1).io.deq.valid && !prefetchBloomFilter.io.hit.reduce(_||_)
  when (prefetchCounter.value === (prefetchCounter.n-1).U && RegNext(prefetchCounter.value) =/= (prefetchCounter.n-1).U){
    prefetchBloomFilter.reset := true.B
  }.elsewhen (reqValid) {
    prefetchCounter.inc()
  }
  prefetchQueues(prefetchQueues.length-1).io.deq.ready := true.B
  io.req.valid := reqValid 
  io.req.bits <> prefetchQueues(prefetchQueues.length-1).io.deq.bits
}