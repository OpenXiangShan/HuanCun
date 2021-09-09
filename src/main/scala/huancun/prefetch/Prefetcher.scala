package huancun.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import huancun._

class PrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  // val addr = UInt(addressBits.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val alias = if (hasAliasBits) Some(UInt(aliasBitsOpt.get.W)) else None
  // val id = UInt(sourceIdBits.W)
}

class PrefetchResp(implicit p: Parameters) extends PrefetchBundle {
  // val id = UInt(sourceIdBits.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchTrain(implicit p: Parameters) extends PrefetchBundle {
  // val addr = UInt(addressBits.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val alias = if (hasAliasBits) Some(UInt(aliasBitsOpt.get.W)) else None
  // prefetch only when L2 receives a miss or prefetched hit req
  // val miss = Bool()
  // val prefetched = Bool()
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  val train = Flipped(DecoupledIO(new PrefetchTrain))
  val req = DecoupledIO(new PrefetchReq)
  val resp = Flipped(DecoupledIO(new PrefetchResp))
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
    valids(tail) := !empty // true.B
    tail := tail + (!empty).asUInt
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
  }

  io.enq.ready := true.B
  io.deq.valid := !empty || io.enq.valid
  io.deq.bits := Mux(empty, io.enq.bits, queue(head))
}

class Prefetcher(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new MSHRRequest)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
  })

  prefetchOpt.get match {
    case bop: BOPParameters =>
      val pft = Module(new BestOffsetPrefetch)
      val pftQueue = Module(new PrefetchQueue)
      pft.io.train <> io.train
      pft.io.resp <> io.resp
      pftQueue.io.enq <> pft.io.req
      pftQueue.io.deq.ready := io.req.ready
      io.req.valid := pftQueue.io.deq.valid
      io.req.bits := DontCare
      io.req.bits.opcode := TLMessages.Hint
      io.req.bits.param := Mux(pftQueue.io.deq.bits.needT, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ)
      io.req.bits.size := log2Up(blockBytes).U
      io.req.bits.source := pftQueue.io.deq.bits.source
      io.req.bits.set := pftQueue.io.deq.bits.set
      io.req.bits.tag := pftQueue.io.deq.bits.tag
      io.req.bits.off := 0.U
      io.req.bits.channel := "b001".U
      io.req.bits.needHint.foreach(_ := false.B)
      io.req.bits.alias.foreach(_ := pftQueue.io.deq.bits.alias.get)
    case _ => assert(cond = false, "Unknown prefetcher")
  }
}
