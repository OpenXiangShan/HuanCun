package huancun
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SourceC(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val c = DecoupledIO(new TLBundleC(edge.bundle))
    val bs_raddr = DecoupledIO(new DSAddress)
    val bs_rdata = Input(new DSData)
    val task = Flipped(DecoupledIO(new SourceCReq))
  })
  // TODO: handle ProbeAck & ProbeAckData
  val queue_size = 6
  val queue_flow = true

  val bs_busy = RegInit(false.B)
  val back_pressure = RegInit(false.B)
  val queue = Module(new Queue(chiselTypeOf(io.c.bits), entries = queue_size, flow = queue_flow))
  back_pressure := queue.io.count >= (queue_size - 2).U

  // Handle task
  val task_latch = RegEnable(io.task.bits, !bs_busy && io.task.valid)
  val task = Mux(!bs_busy, io.task.bits, task_latch)
  when(io.task.valid && !back_pressure && io.task.bits.dirty) { bs_busy := true.B }
  when(io.bs_raddr.fire()) { bs_busy := false.B }
  io.task.ready := !bs_busy && !back_pressure

  // Read Datastorage
  val beat = RegInit(0.U(beatBits.W))
  when(io.bs_raddr.fire()) {
    beat := beat + 1.U
  }
  val has_data = (io.task.valid && io.task.bits.dirty && !back_pressure) || bs_busy
  io.bs_raddr.valid := has_data
  io.bs_raddr.bits.way := task.way
  io.bs_raddr.bits.set := task.set
  io.bs_raddr.bits.beat := beat
  io.bs_raddr.bits.write := false.B

  // Stage 0 => Stage 1
  val s1_valid = RegInit(false.B)
  val s1_task = RegInit(io.task.bits)
  val s1_beat = RegInit(0.U(beatBits.W))
  val task_handled = Mux(has_data, io.bs_raddr.ready, io.task.fire())
  when(task_handled) {
    s1_valid := true.B
    s1_task := task
    s1_beat := beat
  }

  // Stage 1 => Stage 2
  val s2_valid = RegInit(false.B)
  val s2_task = RegInit(io.task.bits)
  val s2_beat = RegInit(0.U(beatBits.W))
  when(s1_valid) {
    s2_valid := true.B
    s2_task := s1_task
    s2_beat := s1_beat
  }

  queue.io.enq.valid := s2_valid
  queue.io.enq.bits.opcode := s2_task.opcode
  queue.io.enq.bits.param := s2_task.param
  queue.io.enq.bits.size := offsetBits.U
  queue.io.enq.bits.source := s2_task.source
  queue.io.enq.bits.address := Cat(s2_task.tag, s2_task.set, 0.U(offsetBits))
  queue.io.enq.bits.data := io.bs_rdata.data
  queue.io.enq.bits.corrupt := false.B

  io.c <> queue.io.deq
}
