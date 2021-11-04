package huancun.prefetch

import chipsalliance.rocketchip.config.{Parameters, Field}
import chisel3._
import chisel3.util._
import huancun._
import huancun.utils._

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

  val pcIdxBits = log2Up(pcSets)
  val pcTagBits = 10 // partial tag
  val diffAddrBits = pcIdxBits + pcTagBits
  require((diffAddrBits + offsetBits) <= vaddrBits)
  // The pointer cache store only if the address of the pointer and the address of the object
  // it points to fall within the range of the heap.
  val heapTagBits = 6 + 7 // the N most significant bits of vaddr
  val aboveTagBits = vaddrBits - pcTagBits - pcIdxBits - offsetBits

  val replacer = Some("setplru")

  def get_pc_idx(vaddr: UInt) = vaddr(offsetBits + pcIdxBits - 1, offsetBits)
  def get_pc_partial_tag(vaddr: UInt) = vaddr(offsetBits + pcIdxBits + pcTagBits - 1, offsetBits + pcIdxBits)
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

  if (cacheParams.enablePerf) {
    XSPerfAccumulate(cacheParams, "enq", PopCount(io.enq.map(_.valid)))
    XSPerfAccumulate(cacheParams, "deq", io.deq.fire())
    XSPerfHistogram(cacheParams, "valid_entries", PopCount(valids), true.B, 0, entries, 1)
  }
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
    val access_pc = Flipped(new PointerCacheIO)
    val prefetch_req = ValidIO(new Bundle() {
      val vaddr = UInt(vaddrBits.W)
      val needT = Bool()
    })
  })
  val s1_ready = Wire(Bool())

  val s0_valid = io.req.valid
  val s0_req = io.req.bits
  val s0_can_go = s1_ready && io.access_pc.tag_read.ready
  val s0_fire = s0_valid && s0_can_go

  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(s0_req, s0_fire)
  val s1_tag_resp = Wire(io.access_pc.tag_resp.cloneType)
  s1_tag_resp := Mux(RegNext(s0_fire), io.access_pc.tag_resp, RegNext(s1_tag_resp))
  val s1_tag_eq_way = s1_tag_resp.tags.map(_ === get_pc_partial_tag(s1_req.vaddr))
  val s1_tag_match_way = s1_tag_eq_way.zip(s1_tag_resp.valids).map { case (eq, v) => eq && v }
  val s1_hit = Cat(s1_tag_match_way).orR
  val s1_need_r_data = s1_req.train && s1_hit
  val s1_need_w_tag = s1_req.loadCmt && !s1_hit
  val s1_need_w_data = s1_req.loadCmt || s1_req.storeCmt && s1_hit
  assert(RegNext(!s1_valid || !(s1_need_r_data && s1_need_w_data)))
  val s1_can_go = (!s1_need_r_data || io.access_pc.data_read.ready) &&
    (!s1_need_w_data || io.access_pc.data_write.ready) &&
    (!s1_need_w_tag || io.access_pc.tag_write.ready)
  val s1_fire = s1_valid && s1_can_go
  s1_ready := !s1_valid || s1_fire
  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }

  // replacement algorithm
  val repl = new SetAssocReplacer(pcSets, pcWays, "plru")
  val s1_repl_way_en = UIntToOH(repl.way(get_pc_idx(s1_req.vaddr)))
  val s1_way_en = Mux(s1_hit, VecInit(s1_tag_match_way).asUInt, s1_repl_way_en)
  val repl_access = Wire(ValidIO(UInt(log2Up(pcWays).W)))
  repl_access.valid := s1_fire && (s1_req.loadCmt || s1_hit && (s1_req.storeCmt || s1_req.train))
  repl_access.bits := OHToUInt(s1_way_en)
  repl.access(Seq(get_pc_idx(s1_req.vaddr)), Seq(repl_access))

  io.req.ready := s0_can_go

  io.access_pc.tag_read.valid := s0_valid && s1_ready
  io.access_pc.tag_read.bits.idx := get_pc_idx(s0_req.vaddr)
  io.access_pc.tag_read.bits.way_en := ~0.U(pcWays.W)

  io.access_pc.tag_write.valid := s1_valid && s1_need_w_tag &&
    (!s1_need_w_data || io.access_pc.data_write.ready)
  io.access_pc.tag_write.bits.idx := get_pc_idx(s1_req.vaddr)
  io.access_pc.tag_write.bits.way_en := s1_way_en
  io.access_pc.tag_write.bits.tag := get_pc_partial_tag(s1_req.vaddr)

  io.access_pc.data_read.valid := s1_valid && s1_need_r_data
  io.access_pc.data_read.bits.idx := get_pc_idx(s1_req.vaddr)
  io.access_pc.data_read.bits.way_en := s1_way_en

  io.access_pc.data_write.valid := s1_valid && s1_need_w_data &&
    (!s1_need_w_tag || io.access_pc.tag_write.ready)
  io.access_pc.data_write.bits.idx := get_pc_idx(s1_req.vaddr)
  io.access_pc.data_write.bits.way_en := s1_way_en
  io.access_pc.data_write.bits.diffAddr := get_diff_addr(s1_req.data)

  io.prefetch_req.valid := RegNext(s1_valid && s1_need_r_data && io.access_pc.data_read.ready)
  io.prefetch_req.bits.vaddr := Cat(
    RegNext(s1_req.vaddr.head(aboveTagBits)),
    io.access_pc.data_resp.diffAddr,
    0.U(offsetBits.W)
  )
  io.prefetch_req.bits.needT := RegNext(s1_req.needT)

  if (cacheParams.enablePerf) {
    XSPerfAccumulate(cacheParams, "req", io.req.fire())
    XSPerfAccumulate(cacheParams, "req_ld_commit", io.req.fire() && io.req.bits.loadCmt)
    XSPerfAccumulate(cacheParams, "req_st_commit", io.req.fire() && io.req.bits.storeCmt)
    XSPerfAccumulate(cacheParams, "req_prefetch_train", io.req.fire() && io.req.bits.train)
    XSPerfAccumulate(cacheParams, "tag_read_nack", io.access_pc.tag_read.valid && !io.access_pc.tag_read.ready)
    XSPerfAccumulate(cacheParams, "data_read_nack", io.access_pc.data_read.valid && !io.access_pc.data_read.ready)
    for (i <- 0 until pcWays) {
      XSPerfAccumulate(cacheParams, "tag_write_way_" + Integer.toString(i, 10),
        io.access_pc.tag_write.valid && s1_way_en(i).asBool)
    }
  }
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
    res
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
  val loadCmtPort = 0
  val storeCmtPort = 1
  val prefetchTrainPort = 2
  arb.io.in(loadCmtPort) <> pointerLdQueue.io.deq
  arb.io.in(storeCmtPort) <> pointerStQueue.io.deq
  val trainReq = Wire(DecoupledIO(new CommitInfo))
  trainReq.valid := trainReqQueue.io.deq.valid
  trainReq.bits := DontCare
  trainReq.bits.vaddr := trainReqQueue.io.deq.bits.vaddr
  trainReqQueue.io.deq.ready := trainReq.ready
  arb.io.in(prefetchTrainPort) <> trainReq

  /*  4. Access pointer cache  */
  val pipe = Module(new PointerCachePipeline)
  val pc = Module(new PointerCache)
  pc.io <> pipe.io.access_pc
  pipe.io.req.valid := arb.io.out.valid
  pipe.io.req.bits.loadCmt := arb.io.chosen === loadCmtPort.U
  pipe.io.req.bits.storeCmt := arb.io.chosen === storeCmtPort.U
  pipe.io.req.bits.train := arb.io.chosen === prefetchTrainPort.U
  pipe.io.req.bits.vaddr := arb.io.out.bits.vaddr
  pipe.io.req.bits.data := arb.io.out.bits.data
  pipe.io.req.bits.needT := trainReqQueue.io.deq.bits.needT
  arb.io.out.ready := pipe.io.req.ready

  /*  5. Translate vaddr to paddr  */
  val prefetch_vaddr = Wire(ValidIO(UInt(vaddrBits.W)))
  val prefetch_paddr = Wire(ValidIO(UInt(addressBits.W)))
  prefetch_paddr := DontCare
  ExcitingUtils.addSource(prefetch_vaddr.valid, "POINT_CHASE_TLB_REQ_VALID")
  ExcitingUtils.addSource(prefetch_vaddr.bits, "POINT_CHASE_TLB_REQ_VADDR")
  ExcitingUtils.addSink(prefetch_paddr.valid, "POINT_CHASE_TLB_RESP_VALID")
  ExcitingUtils.addSink(prefetch_paddr.bits, "POINT_CHASE_TLB_RESP_PADDR")
  prefetch_vaddr.valid := pipe.io.prefetch_req.valid
  prefetch_vaddr.bits := pipe.io.prefetch_req.bits.vaddr

  io.req.valid := prefetch_paddr.valid
  io.req.bits := DontCare
  val (tag, set, _) = parseAddress(prefetch_paddr.bits)
  io.req.bits.tag := tag
  io.req.bits.set := set
  io.req.bits.needT := RegNext(pipe.io.prefetch_req.bits.needT)
  io.req.bits.source := 0.U // TODO
  io.req.bits.alias.foreach(_ := RegNext(pipe.io.prefetch_req.bits.vaddr(pageOffsetBits + aliasBitsOpt.get - 1, pageOffsetBits)))

  if (cacheParams.enablePerf) {
    XSPerfAccumulate(cacheParams, "ld_commit", PopCount(io.update.commit.ld.map(_.valid)))
    XSPerfAccumulate(cacheParams, "pointer_ld_commit", PopCount(pointerLoads.map(_.valid)))
    XSPerfAccumulate(cacheParams, "st_commit", PopCount(io.update.commit.st.map(_.valid)))
    XSPerfAccumulate(cacheParams, "pointer_st_commit", PopCount(pointerStores.map(_.valid)))
    XSPerfAccumulate(cacheParams, "l1d_miss", io.train.fire())
    XSPerfAccumulate(cacheParams, "pipe_output_pft_req", pipe.io.prefetch_req.valid)
    XSPerfAccumulate(cacheParams, "after_tlb_pft_req", /* RegNext(pipe.io.prefetch_req.valid) && */prefetch_paddr.valid)
  }
}