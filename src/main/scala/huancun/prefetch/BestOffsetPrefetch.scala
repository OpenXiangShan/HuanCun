package huancun.prefetch

import huancun.utils.SRAMTemplate
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import huancun.HasHuanCunParameters
import utility.MemReqSource

case class BOPParameters(
  rrTableEntries: Int = 256,
  rrTagBits:      Int = 12,
  scoreBits:      Int = 5,
  roundMax:       Int = 50,
  badScore:       Int = 1,
  offsetList: Seq[Int] = Seq(
    -32, -30, -27, -25, -24, -20, -18, -16, -15,
    -12, -10,  -9,  -8,  -6,  -5,  -4,  -3,  -2,  -1,
      1,   2,   3,   4,   5,   6,   8,   9,  10,
     12,  15,  16,  18,  20,  24,  25,  27,  30//,
    /*32,  36,
     40,  45,  48,  50,  54,  60,  64,  72,  75,  80,
     81,  90,  96, 100, 108, 120, 125, 128, 135, 144,
    150, 160, 162, 180, 192, 200, 216, 225, 240, 243,
    250, 256*/
  ))
    extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 16
}

trait HasBOPParams extends HasHuanCunParameters {
  val bopParams = prefetchOpt.get.asInstanceOf[BOPParameters]
  // Best offset
  val defaultMinAddrBits = offsetBits + log2Up(bopParams.rrTableEntries) + bopParams.rrTagBits
  val defaultConfig = fullAddressBits >= defaultMinAddrBits

  val rrTableEntries = if (defaultConfig) bopParams.rrTableEntries else 2
  val rrIdxBits = log2Up(rrTableEntries)
  val rrTagBits = if (defaultConfig) bopParams.rrTagBits else (fullAddressBits - offsetBits - rrIdxBits)
  val scoreBits = bopParams.scoreBits
  val roundMax = bopParams.roundMax
  val badScore = bopParams.badScore
  val offsetList = bopParams.offsetList
  val inflightEntries = bopParams.inflightEntries

  val scores = offsetList.length
  val offsetWidth = log2Up(-offsetList(0)) + 1 // -32 <= offset <= 31
  val roundBits = log2Up(roundMax)
  val scoreMax = (1 << scoreBits) - 1
  val scoreTableIdxBits = log2Up(scores)
  // val prefetchIdWidth = log2Up(inflightEntries)

  def signedExtend(x: UInt, width: Int): UInt = {
    if (x.getWidth >= width) {
      x
    } else {
      Cat(Fill(width - x.getWidth, x.head(1)), x)
    }
  }
}

abstract class BOPBundle(implicit val p: Parameters) extends Bundle with HasBOPParams
abstract class BOPModule(implicit val p: Parameters) extends Module with HasBOPParams

class ScoreTableEntry(implicit p: Parameters) extends BOPBundle {
  // val offset = UInt(offsetWidth.W)
  val score = UInt(scoreBits.W)

  def apply(score: UInt) = {
    val entry = Wire(this)
    // entry.offset := offset
    entry.score := score
    entry
  }
}

class TestOffsetReq(implicit p: Parameters) extends BOPBundle {
  // find whether (X-d) is in recent request table
  val addr = UInt(fullAddressBits.W)
  val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
}

class TestOffsetResp(implicit p: Parameters) extends BOPBundle {
  // val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
  val hit = Bool()
}

class TestOffsetBundle(implicit p: Parameters) extends BOPBundle {
  val req = DecoupledIO(new TestOffsetReq)
  val resp = Flipped(DecoupledIO(new TestOffsetResp))
}

class RecentRequestTable(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle {
    val w = Flipped(DecoupledIO(UInt(fullAddressBits.W)))
    val r = Flipped(new TestOffsetBundle)
  })

  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(addr: UInt) = addr(fullAddressBits - 1, offsetBits)
  def hash1(addr:    UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
  def hash2(addr:    UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
  def idx(addr:      UInt) = hash1(addr) ^ hash2(addr)
  def tag(addr:      UInt) = lineAddr(addr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
  def rrTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(rrTagBits.W)
  }

  val rrTable = Module(
    new SRAMTemplate(rrTableEntry(), set = rrTableEntries, way = 1, shouldReset = true, singlePort = true)
  )

  val wAddr = io.w.bits
  rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data(0).valid := true.B
  rrTable.io.w.req.bits.data(0).tag := tag(wAddr)

  val rAddr = io.r.req.bits.addr - signedExtend((io.r.req.bits.testOffset << offsetBits), fullAddressBits)
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.r.req.fire
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  assert(!RegNext(io.w.fire && io.r.req.fire), "single port SRAM should not read and write at the same time")

  io.w.ready := rrTable.io.w.req.ready && !io.r.req.valid
  io.r.req.ready := true.B
  io.r.resp.valid := RegNext(rrTable.io.r.req.fire)
  io.r.resp.bits.ptr := RegNext(io.r.req.bits.ptr)
  io.r.resp.bits.hit := rData.valid && rData.tag === RegNext(tag(rAddr))

}

class OffsetScoreTable(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(UInt(fullAddressBits.W)))
    val prefetchOffset = Output(UInt(offsetWidth.W))
    val test = new TestOffsetBundle
  })

  val prefetchOffset = RegInit(2.U(offsetWidth.W))
  // score table
  // val st = RegInit(VecInit(offsetList.map(off => (new ScoreTableEntry).apply(off.U, 0.U))))
  val st = RegInit(VecInit(Seq.fill(scores)((new ScoreTableEntry).apply(0.U))))
  val offList = WireInit(VecInit(offsetList.map(off => off.S(offsetWidth.W).asUInt)))
  val ptr = RegInit(0.U(scoreTableIdxBits.W))
  val round = RegInit(0.U(roundBits.W))

  val bestOffset = RegInit(2.U(offsetWidth.W)) // the entry with the highest score while traversing
  val bestScore = RegInit(badScore.U(scoreBits.W))
  val testOffset = offList(ptr)
  // def winner(e1: ScoreTableEntry, e2: ScoreTableEntry): ScoreTableEntry = {
  //   val w = Wire(new ScoreTableEntry)
  //   w := Mux(e1.score > e2.score, e1, e2)
  //   w
  // }

  val s_idle :: s_learn :: Nil = Enum(2)
  val state = RegInit(s_idle)

  // 1. At the start of a learning phase
  // All the scores are reset to 0.
  // At the end of every learning phase, the prefetch offset is updated as the one with the highest score.
  when(state === s_idle) {
    st.foreach(_.score := 0.U)
    ptr := 0.U
    round := 0.U
    bestScore := badScore.U
    prefetchOffset := bestOffset
    state := s_learn
  }

  // 2. During a learning phase
  // On every eligible L2 read access (miss or prefetched hit), we test an offset d_i from the list.
  // If X-d_i hits in the RR table, the score of offset d_i is incremented. During a round, each offset
  // in the list is test once. When all the offsets in the list have been tested, the current round is
  // finished, and a new round begins from offset d_1 again.
  // The current learning phase finishes at the end of a round when:
  // (1) one of the score equals SCOREMAX, or
  // (2) the number of rounds equals ROUNDMAX.
  when(state === s_learn) {
    when(io.test.req.fire) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)
    }

    // (2) the number of rounds equals ROUNDMAX.
    when(round >= roundMax.U) {
      state := s_idle
    }

    when(io.test.resp.fire && io.test.resp.bits.hit) {
      val oldScore = st(io.test.resp.bits.ptr).score
      val newScore = oldScore + 1.U
      val offset = offList(io.test.resp.bits.ptr)
      st(io.test.resp.bits.ptr).score := newScore
      // bestOffset := winner((new ScoreTableEntry).apply(offset, newScore), bestOffset)
      val renewOffset = newScore > bestScore
      bestOffset := Mux(renewOffset, offset, bestOffset)
      bestScore := Mux(renewOffset, newScore, bestScore)
      // (1) one of the score equals SCOREMAX
      when(newScore >= scoreMax.U) {
        state := s_idle
      }
    }
  }

  io.req.ready := state === s_learn
  io.prefetchOffset := prefetchOffset
  io.test.req.valid := state === s_learn && io.req.valid
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr
  io.test.resp.ready := true.B

}

class BestOffsetPrefetch(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
  })

  val rrTable = Module(new RecentRequestTable)
  val scoreTable = Module(new OffsetScoreTable)

  val prefetchOffset = scoreTable.io.prefetchOffset
  val oldAddr = io.train.bits.addr
  val newAddr = oldAddr + signedExtend((prefetchOffset << offsetBits), fullAddressBits)

  rrTable.io.r <> scoreTable.io.test
  rrTable.io.w.valid := io.resp.valid
  rrTable.io.w.bits := Cat(Cat(io.resp.bits.tag, io.resp.bits.set) - signedExtend(prefetchOffset, setBits + fullTagBits), 0.U(offsetBits.W))
  scoreTable.io.req.valid := io.train.valid
  scoreTable.io.req.bits := oldAddr

  val req = Reg(new PrefetchReq)
  val req_valid = RegInit(false.B)
  val crossPage = getPPN(newAddr) =/= getPPN(oldAddr) // unequal tags
  when(io.req.fire) {
    req_valid := false.B
  }
  when(scoreTable.io.req.fire) {
    req.tag := parseFullAddress(newAddr)._1
    req.set := parseFullAddress(newAddr)._2
    req.needT := io.train.bits.needT
    req.source := io.train.bits.source
    req_valid := !crossPage // stop prefetch when prefetch req crosses pages
  }

  io.req.valid := req_valid
  io.req.bits := req
  io.req.bits.pfSource := MemReqSource.Prefetch2L2BOP.id.U
  io.train.ready := scoreTable.io.req.ready && (!req_valid || io.req.ready)
  io.resp.ready := rrTable.io.w.ready
}
