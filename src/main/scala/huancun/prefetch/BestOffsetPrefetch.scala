package huancun.prefetch

import huancun.utils.{SRAMTemplate}
import chipsalliance.rocketchip.config.{Parameters, Field}
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

case object BOPParamsKey extends Field[BOPParameters]

case class BOPParameters (
  rrTableEntries: Int  = 256,
  rrTagBits: Int       = 12,
  scoreBits: Int       = 5,
  roundMax: Int        = 50,
  badScore: Int        = 1,
  offsetList: Seq[Int] = Seq(
      1,   2,   3,   4,   5,   6,   8,   9,  10,  12,
     15,  16/*,  18,  20,  24,  25,  27,  30,  32,  36,
     40,  45,  48,  50,  54,  60,  64,  72,  75,  80,
     81,  90,  96, 100, 108, 120, 125, 128, 135, 144,
    150, 160, 162, 180, 192, 200, 216, 225, 240, 243,
    250, 256*/
  ),
  inflightEntries: Int = 4 // max num of inflight prefetch reqs
)

class ScoreTableEntry(implicit p: Parameters) extends PrefetchBundle {
  val offset = UInt(offsetWidth.W)
  val score = UInt(scoreBits.W)

  def apply(offset: UInt, score: UInt) = {
    val entry = Wire(this)
    entry.offset := offset
    entry.score := score
    entry
  }
}

class TestOffsetReq(implicit p: Parameters) extends PrefetchBundle {
  // find whether (X-d) is in recent request table
  val addr = UInt(addressBits.W)
  val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
}

class TestOffsetResp(implicit p: Parameters) extends PrefetchBundle {
  // val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
  val hit = Bool()
}

class TestOffsetBundle(implicit p: Parameters) extends PrefetchBundle {
  val req = DecoupledIO(new TestOffsetReq)
  val resp = Flipped(DecoupledIO(new TestOffsetResp))
}

class RecentRequestTable(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val w = Flipped(DecoupledIO(UInt(addressBits.W)))
    val r = Flipped(new TestOffsetBundle)
  })

  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(addr: UInt) = addr(addressBits - 1, offsetBits)
  def hash1(addr: UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
  def hash2(addr: UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
  def idx(addr: UInt) = hash1(addr) ^ hash2(addr)
  def tag(addr: UInt) = lineAddr(addr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
  def rrTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(rrTagBits.W)
  }

  val rrTable = Module(new SRAMTemplate(rrTableEntry(), set = rrTableEntries, way = 1, shouldReset = true, singlePort = true))

  val wAddr = io.w.bits
  rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data(0).valid := true.B
  rrTable.io.w.req.bits.data(0).tag := tag(wAddr)

  val rAddr = io.r.req.bits.addr - (io.r.req.bits.testOffset << offsetBits)
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.r.req.fire()
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  assert(!RegNext(io.w.fire() && io.r.req.fire()), "single port SRAM should not read and write at the same time")

  io.w.ready := rrTable.io.w.req.ready && io.r.req.valid
  io.r.req.ready := true.B
  io.r.resp.valid := RegNext(rrTable.io.r.req.fire())
  io.r.resp.bits.ptr := RegNext(io.r.req.bits.ptr)
  io.r.resp.bits.hit := rData.valid && rData.tag === RegNext(tag(rAddr))

}

class OffsetScoreTable(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(UInt(addressBits.W)))
    val prefetchOffset = Output(UInt(offsetWidth.W))
    val test = new TestOffsetBundle
  })

  val prefetchOffset = RegInit(2.U(offsetWidth.W))
  // score table
  val st = RegInit(VecInit(offsetList.map(off => (new ScoreTableEntry).apply(off.U, 0.U))))
  val ptr = RegInit(0.U(scoreTableIdxBits.W))
  val round = RegInit(0.U(roundBits.W))

  val bestOffset = RegInit((new ScoreTableEntry).apply(2.U, 0.U)) // the entry with the highest score while traversing
  val testOffset = WireInit(st(ptr).offset)
  def winner(e1: ScoreTableEntry, e2: ScoreTableEntry): ScoreTableEntry = {
    val w = Wire(new ScoreTableEntry)
    w := Mux(e1.score > e2.score, e1, e2)
    w
  }

  val s_idle :: s_learn :: Nil = Enum(2)
  val state = RegInit(s_idle)

  // 1. At the start of a learning phase
  // All the scores are reset to 0.
  // At the end of every learning phase, the prefetch offset is updated as the one with the highest score.
  when (state === s_idle) {
    st.foreach(_.score := 0.U)
    ptr := 0.U
    round := 0.U
    bestOffset.score := badScore.U
    prefetchOffset := bestOffset.offset
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
  when (state === s_learn) {
    when (io.test.req.fire()) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)
    }

    // (2) the number of rounds equals ROUNDMAX.
    when (round >= roundMax.U) {
      state := s_idle
    }

    when (io.test.resp.fire() && io.test.resp.bits.hit) {
      val oldEntry = st(io.test.resp.bits.ptr)
      val oldScore = oldEntry.score
      val newScore = oldScore + 1.U
      val offset = oldEntry.offset
      st(io.test.resp.bits.ptr).score := newScore
      bestOffset := winner((new ScoreTableEntry).apply(offset, newScore), bestOffset)
      // (1) one of the score equals SCOREMAX
      when (newScore >= scoreMax.U) {
        state := s_idle
      }
    }
  }

  io.req.ready := true.B
  io.prefetchOffset := prefetchOffset
  io.test.req.valid := state === s_learn && io.req.fire()
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr
  io.test.resp.ready := true.B

}