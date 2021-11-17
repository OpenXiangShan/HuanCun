package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

class SourceDBufferRead(implicit p: Parameters) extends HuanCunBundle {
  val r_valid = Input(Bool())
  val r_beat = Input(UInt(beatBits.W))
  val r_id = Input(UInt(log2Up(mshrs).W))
  val buffer_hit = Output(Bool())
  val buffer_data = Output(new DSData)
}

class SinkDBufferWrite(implicit p: Parameters) extends HuanCunBundle {
  val w_beat = UInt(beatBits.W)
  val w_id = UInt(log2Up(mshrs).W)
  val w_data = new DSData
}

/**
  *   RefillBuffer is used to reduce outer grant -> inner grant latency
  *   refill data can be bypassed to inner cache without go through SRAM
  */
class RefillBuffer(flow: Boolean = false)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val sourceDr = new SourceDBufferRead
    val sinkDw = Flipped(DecoupledIO(new SinkDBufferWrite))
  })

  val sourceDr = io.sourceDr
  val sinkDw = io.sinkDw

  /*
     mshr0   beat0  beat1 ... beatN
     mshr1   beat0  beat1 ... beatN
                  ...
     mshrN   beat0  beat1 ... beatN
   */
  val buffers = Mem(mshrs, Vec(beatSize, new DSData))
  val valids = RegInit(VecInit(Seq.fill(mshrs){
    VecInit(Seq.fill(beatSize){false.B})
  }))

  val flow_hit = if(flow) {
    sinkDw.valid && sinkDw.bits.w_id === sourceDr.r_id && sinkDw.bits.w_beat === sourceDr.r_beat
  } else false.B

  val buffer_hit = valids(sourceDr.r_id)(sourceDr.r_beat)

  sourceDr.buffer_hit := buffer_hit || flow_hit
  sourceDr.buffer_data := Mux(flow_hit,
    sinkDw.bits.w_data,
    buffers(sourceDr.r_id)(sourceDr.r_beat)
  )

  sinkDw.ready := !valids(sinkDw.bits.w_id)(sinkDw.bits.w_beat)

  when(sinkDw.fire()){
    valids(sinkDw.bits.w_id)(sinkDw.bits.w_beat) := true.B
    buffers(sinkDw.bits.w_id)(sinkDw.bits.w_beat) := sinkDw.bits.w_data
  }
  when(sourceDr.r_valid){
    when(valids(sourceDr.r_id)(sourceDr.r_beat) || flow_hit){
      valids(sourceDr.r_id)(sourceDr.r_beat) := false.B
    }
  }

}
