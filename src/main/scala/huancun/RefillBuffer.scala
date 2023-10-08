package huancun

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._

class SourceDBufferRead(implicit p: Parameters) extends HuanCunBundle {
  val valid = Input(Bool())
  val beat = Input(UInt(beatBits.W))
  val id = Input(UInt(bufIdxBits.W))
  val ready = Output(Bool())
  val buffer_data = Output(new DSData)
  val last = Input(Bool())
}

class SinkDBufferWrite(implicit p: Parameters) extends HuanCunBundle {
  val valid = Input(Bool())
  val beat = Input(UInt(beatBits.W))
  val data = Input(new DSData)
  val ready = Output(Bool())
  val id = Output(UInt(bufIdxBits.W))
}

/**
  *   RefillBuffer is used to reduce outer grant -> inner grant latency
  *   refill data can be bypassed to inner cache without go through SRAM
  */
class RefillBuffer(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val r = new SourceDBufferRead()
    val w = new SinkDBufferWrite()
  })

  val buffer = Mem(bufBlocks, Vec(beatSize, new DSData()))
  val valids = RegInit(VecInit(Seq.fill(bufBlocks){
    VecInit(Seq.fill(beatSize){false.B})
  }))

  val (r, w) = (io.r, io.w)
  val rlast = r.last
  val wlast = w.beat.andR
  val wfirst = w.beat === 0.U

  r.buffer_data := buffer(r.id)(r.beat)
  r.ready := valids(r.id)(r.beat)

  when(r.valid && r.beat === 0.U){
    assert(r.ready, "[%d] first beat must hit!", r.id)
  }

  when(r.valid && r.ready && rlast){ // last beat
    // assert(valids(r.id).asUInt.andR, "[%d] attempt to invalidate a invalid entry", r.id)
    valids(r.id).foreach(_ := false.B)
  }

  val validMask = VecInit(valids.map(vec => vec.asUInt.orR)).asUInt
  val freeIdx = PriorityEncoder(~validMask)

  w.ready := Mux(wfirst, RegNext(!validMask.andR, true.B), true.B)
  w.id := Mux(wfirst,
    RegNext(freeIdx, 0.U),
    RegEnable(w.id, w.valid && w.ready && wfirst)
  )

  when(w.valid && w.ready){
    assert(!valids(w.id)(w.beat), "[%d] attempt to write a valid entry", w.id)
    valids(w.id)(w.beat) := true.B
    buffer(w.id)(w.beat) := w.data
  }

}
