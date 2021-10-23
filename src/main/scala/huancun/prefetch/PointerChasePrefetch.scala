package huancun.prefetch

import chipsalliance.rocketchip.config.{Parameters, Field}
import chisel3._
import chisel3.util._
import huancun._

case object PCParamsKey extends Field[PCParameters]

case class PCParameters (
  sets: Int = 64 * 1024, // 64K
  ways: Int = 4
) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val inflightEntries: Int = 16
  override val needCommitInfo: Boolean = true
}

trait HasPCParams extends HasHuanCunParameters {
  val pcParams = prefetchOpt.get.asInstanceOf[PCParameters]

  // pointer cache
  val pcSets = pcParams.sets
  val pcWays = pcParams.ways

  val wordOffBits = log2Up(64 / 8) // TODO: parameterize this
  val pcIdxBits = log2Up(pcSets)
  val pcTagBits = 10 // partial tag
  val diffAddrBits = pcIdxBits + pcTagBits
  require((diffAddrBits + wordOffBits) <= vaddrBits)
  // The pointer cache store only if the address of the pointer and the address of the object
  // it points to fall within the range of the heap.
  val heapTagBits = 6 // the N most significant bits of vaddr

  val replacer = Some("setplru")

  def get_pc_idx(vaddr: UInt) = vaddr(wordOffBits + pcIdxBits - 1, wordOffBits)
  def get_pc_partial_tag(vaddr: UInt) = vaddr(wordOffBits + pcIdxBits + pcTagBits - 1, wordOffBits + pcIdxBits)
  def get_diff_addr(vaddr: UInt) = Cat(get_pc_partial_tag(vaddr), get_pc_idx(vaddr))
}

abstract class PCBundle(implicit val p: Parameters) extends Bundle with HasPCParams
abstract class PCModule(implicit val p: Parameters) extends Module with HasPCParams

class MultiInOneOutQueue[T <: Data](val gen: T, val entries: Int, val enqPorts: Int)(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle() {
    val enq = Flipped(Vec(enqPorts, ValidIO(gen)))
    val deq = DecoupledIO(gen)
  })
  val queue = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val idxWidth = log2Up(entries)
  val head = RegInit(0.U(idxWidth.W))
  val tail = RegInit(0.U(idxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last

  val emptyCnt = PopCount(~(valids.asUInt)) // how many empty slots are there?
  val enqValids = io.enq.map(_.valid)
  val enqCnt = PopCount(Cat(enqValids)) // how many enq?
  val enqValid = Cat(enqValids).orR

  when (io.deq.ready) {
    when (!empty) {
      valids(head) := false.B
      head := head + 1.U
    }.elsewhen (enqValid) {
      head := head + 1.U
    }
  }

  io.enq.zipWithIndex.foreach {
    case (enq, i) =>
      when (enq.valid) {
        val innerIdx = PopCount(Cat(enqValids) & ~(((1 << (enqPorts - i)) - 1).U(enqPorts.W)))
        val idx = tail + innerIdx
        queue(idx) := enq.bits
        valids(idx) := innerIdx =/= 0.U || !empty || !io.deq.ready
      }
  }
  tail := tail + enqCnt
  when (enqCnt > emptyCnt) {
    head := tail + enqCnt
  }

  io.deq.valid := !empty || enqValid
  io.deq.bits := Mux(empty, PriorityMux(enqValids, io.enq.map(_.bits)), queue(head))
}

class PointerCachePipelineReq(implicit p: Parameters) extends PCBundle {
  val loadCmt, storeCmt, train = Bool() // where this req comes from
  // if this is a load/store commit
  val vaddr = UInt(vaddrBits.W)
  val data = UInt(64.W)
  // if this is a train req to trigger prefetch
  val needT = Bool()
}

class PointerCachePipeline(implicit p: Parameters) extends PCModule {
  val io = IO(new Bundle() {
    val req = Flipped(Decoupled(new PointerCachePipelineReq()))
    val tag_read = DecoupledIO(new TagReadReq)
    val tag_resp = Input(new TagReadResp)
    val tag_write = Decoupled(new TagWriteReq)
    val data_read = DecoupledIO(new DataReadReq)
    val data_resp = Input(new DataReadResp)
    val data_write = DecoupledIO(new DataWriteReq)
    val prefetch_req = ValidIO(new Bundle() {
      val vaddr = UInt(vaddrBits.W)
      val needT = Bool()
    })
  })
  val s1_ready = Wire(Bool())

  val s0_valid = io.req.valid
  val s0_req = io.req.bits
  val s0_can_go = s1_ready && io.tag_read.ready
  val s0_fire = s0_valid && s0_can_go

  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(s0_req, s0_fire)
  val s1_tag_resp = Mux(RegNext(s0_fire), io.tag_resp, RegNext(s1_tag_resp))
  val s1_tag_eq_way = s1_tag_resp.tags.map(_ === get_pc_partial_tag(s1_req.vaddr))
  val s1_tag_match_way = s1_tag_eq_way.zip(s1_tag_resp.valids).map((eq, v) => eq && v)
  val s1_hit = Cat(s1_tag_match_way).orR
  val s1_need_r_data = s1_req.train && s1_hit
  val s1_need_w_tag = s1_req.loadCmt && !s1_hit
  val s1_need_w_data = s1_req.loadCmt || s1_req.storeCmt && s1_hit
  assert(RegNext(!s1_valid || !(s1_need_r_data && s1_need_w_data)))
  val s1_can_go = (!s1_need_r_data || io.data_read.ready) &&
    (!s1_need_w_data || io.data_write.ready) &&
    (!s1_need_w_tag || io.tag_write.ready)
  val s1_fire = s1_valid && s1_can_go
  s1_ready := !s1_valid || s1_fire
  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }

  // replacement algorithm
  val repl = new SetAssocReplacer(pcSets, pcWays, "plru")
  val s1_repl_way_en = UIntToOH(repl.way(get_pc_idx(s1_req.vaddr)))
  val s1_way_en = Mux(s1_hit, VecInit(s1_tag_match_way).asUInt, s1_repl_way_en)

  io.req.ready := s0_can_go

  io.tag_read.valid := s0_valid && s1_ready
  io.tag_read.bits.idx := get_pc_idx(s0_req.vaddr)
  io.tag_read.bits.way_en := (-1).asSInt.asUInt

  io.tag_write.valid := s1_valid && s1_need_w_tag &&
    (!s1_need_w_data || io.data_write.ready)
  io.tag_write.bits.idx := get_pc_idx(s1_req.vaddr)
  io.tag_write.bits.way_en := s1_way_en
  io.tag_write.bits.tag := get_pc_partial_tag(s1_req.vaddr)

  io.data_read.valid := s1_valid && s1_need_r_data
  io.date_read.bits.idx := get_pc_idx(s1_req.vaddr)
  io.data_read.bits.way_en := s1_way_en

  io.data_write.valid := s1_valid && s1_need_w_data &&
    (!s1_need_w_tag || io.tag_write.ready)
  io.data_write.bits.idx := get_pc_idx(s1_req.vaddr)
  io.data_write.bits.way_en := s1_way_en
  io.data_write.bits.diffAddr := get_diff_addr(s1_req.data)

//  io.prefetch_req.valid
}

class PointerChasePrefetch(implicit p: Parameters) extends PCModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val update = Flipped(new PrefetchUpdate)
  })
  /*  1. Handle the update reqs sent from DCache
   *  These update reqs will firstly be filtered to obtain pointer loads or
   *  possible stores that might hit on pointer loads.
   *  Than these reqs will be enqueued, waiting for access to pointer cache.
   */
  def isPointerAddr(vaddr: UInt, data: UInt): Bool = {
    require(vaddr.getWidth == vaddrBits)
    require(data.getWidth >= vaddr.getWidth)
    data(vaddrBits - 1, 0).head(heapTagBits) === vaddr.head(heapTagBits)
  }
  def pointerFilter(cmt: Valid[CommitInfo]): Valid[CommitInfo] = {
    val res = Wire(Valid(new CommitInfo))
    res.bits := cmt.bits
    res.valid := cmt.valid && isPointerAddr(cmt.bits.vaddr, cmt.bits.data)
  }
  val pointerLoads = io.update.commit.ld.map(cmt => pointerFilter(cmt))
  val pointerStores = io.update.commit.st.map(cmt => pointerFilter(cmt))
  val pointerLdQueue = Module(new MultiInOneOutQueue(new CommitInfo, entries = 32, enqPorts = commitWidth))
  val pointerStQueue = Module(new MultiInOneOutQueue(new CommitInfo, entries = 32, enqPorts = commitWidth))
  require(pointerLdQueue.io.enq.size == pointerLoads.size)
  pointerLdQueue.io.enq := VecInit(pointerLoads)
  pointerStQueue.io.enq := VecInit(pointerStores)

  /*  2. Receive DCache load miss to trigger prefetch  */
  val trainReqQueue = Module(new PrefetchQueue(new PrefetchTrain, entries = 16))
  trainReqQueue.io.enq <> io.train

  /*  3. Choose one req from the 3 queues above to access pointer cache  */
  val arb = Module(new RRArbiter(new CommitInfo, 3))
  arb.io.in(0) <> pointerLdQueue.io.deq
  arb.io.in(1) <> pointerStQueue.io.deq
  val trainReq = Wire(DecoupledIO(new CommitInfo))
  trainReq.valid := trainReqQueue.io.deq.valid
  trainReq.bits := DontCare
  trainReq.bits.vaddr := trainReqQueue.io.deq.bits.vaddr
  io.deq.ready := trainReq.ready
  arb.io.in(2) <> trainReq

}