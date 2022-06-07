package huancun.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import huancun._
import huancun.utils.Pipeline

class PrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)

  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchResp(implicit p: Parameters) extends PrefetchBundle {
  // val id = UInt(sourceIdBits.W)
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchTrain(implicit p: Parameters) extends PrefetchBundle {
  // val addr = UInt(addressBits.W)
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  // prefetch only when L2 receives a miss or prefetched hit req
  // val miss = Bool()
  // val prefetched = Bool()
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

// zeal4u:
class EvictionInfo(implicit p: Parameters) extends PrefetchBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)

  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  val train = Flipped(DecoupledIO(new PrefetchTrain))
  val req = DecoupledIO(new PrefetchReq)
  val resp = Flipped(DecoupledIO(new PrefetchResp))
  val evict = Flipped(DecoupledIO(new EvictionInfo))
}

class PrefetchQueue(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(new PrefetchReq))
    val deq = DecoupledIO(new PrefetchReq)
  })
  /*  Here we implement a queue that
   *  1. is pipelined  2. flows
   *  3. always has the latest reqs, which means the queue is always ready for enq and deserting the eldest ones
   */
  val queue = RegInit(VecInit(Seq.fill(inflightEntries)(0.U.asTypeOf(new PrefetchReq))))
  val valids = RegInit(VecInit(Seq.fill(inflightEntries)(false.B)))
  val idxWidth = log2Up(inflightEntries)
  val head = RegInit(0.U(idxWidth.W))
  val tail = RegInit(0.U(idxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last

  when(!empty && io.deq.ready) {
    valids(head) := false.B
    head := head + 1.U
  }

  when(io.enq.valid) {
    queue(tail) := io.enq.bits
    valids(tail) := !empty || !io.deq.ready // true.B
    tail := tail + (!empty || !io.deq.ready).asUInt
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
  }

  io.enq.ready := true.B
  io.deq.valid := !empty || io.enq.valid
  io.deq.bits := Mux(empty, io.enq.bits, queue(head))
}

class Prefetcher(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO)

  prefetchOpt.get match {
    case bop: BOPParameters =>
      val pft = Module(new BestOffsetPrefetch)
      val pftQueue = Module(new PrefetchQueue)
      val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
      pft.io.train <> io.train
      pft.io.resp <> io.resp
      pftQueue.io.enq <> pft.io.req
      pipe.io.in <> pftQueue.io.deq
      io.req <> pipe.io.out
    case sp: SandboxParameters =>
      val pft = Module(new SandboxPrefetcher)
      val pftQueue = Module(new PrefetchQueue)
      val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
      pft.io.train <> io.train
      pft.io.resp <> io.resp
      pft.io.resp <> io.resp
      pft.io.evict <> io.evict
      pftQueue.io.enq <> pft.io.req
      pipe.io.in <> pftQueue.io.deq
      io.req <> pipe.io.out
    case spp: SPPParameters =>
      val pft = Module(new SignaturePathPrefetcher)
      val pftQueue = Module(new PrefetchQueue)
      val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
      pft.io.train <> io.train
      pft.io.resp <> io.resp
      pft.io.evict <> io.evict
      pftQueue.io.enq <> pft.io.req
      pipe.io.in <> pftQueue.io.deq
      io.req <> pipe.io.out
    case sms: SMSParameters =>
      val pft = Module(new SpatialMemoryStreaming)
      val pftQueue = Module(new PrefetchQueue)
      val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
      pft.io.train <> io.train
      pft.io.resp <> io.resp
      pft.io.evict <> io.evict
      pftQueue.io.enq <> pft.io.req
      pipe.io.in <> pftQueue.io.deq
      io.req <> pipe.io.out
    case _ => assert(cond = false, "Unknown prefetcher")
  }
}
