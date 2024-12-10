package huancun.noninclusive

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{TLMessages, TLPermissions}
import huancun.{HuanCunModule, MSHRRequest, MetaData}
import huancun.utils.XSPerfAccumulate
import utility.MemReqSource

class ProbeHelper(entries: Int = 5, enqDelay: Int = 1)(implicit p: Parameters)
  extends HuanCunModule with HasClientInfo
{
  val io = IO(new Bundle() {
    val dirResult = Flipped(Valid(new DirResult()))
    val probe = DecoupledIO(new MSHRRequest)
    val full = Output(Bool())
  })

  val queue = Module(new Queue(new MSHRRequest, entries = entries, pipe = false, flow = false))

  io.full := queue.io.count >= (entries - enqDelay).U

  val dir = io.dirResult.bits
  val req_client = OHToUInt(getClientBitOH(dir.sourceId))
  val req = Wire(new MSHRRequest)

  // addr without bankIdx
  val addr = Cat(dir.clients.tag, dir.set(clientSetBits - 1, 0))
  val tgt_tag = addr.head(tagBits)
  val tgt_set = addr.tail(tagBits).head(setBits)

  req.fromProbeHelper := true.B
  req.opcode := TLMessages.Probe
  req.param := TLPermissions.toN
  req.channel := "b010".U
  req.size := log2Up(blockBytes).U
  req.source := dir.sourceId
  req.tag := tgt_tag
  req.set := tgt_set
  req.off := 0.U
  req.mask := DontCare
  req.bufIdx := DontCare
  req.needHint.foreach(_ := false.B)
  req.isPrefetch.foreach(_ := false.B)
  req.isBop.foreach(_ := false.B)
  req.alias.foreach(_ := 0.U)
  req.preferCache := true.B
  req.dirty := false.B // ignored
  req.isHit := true.B // ignored
  req.needProbeAckData.foreach(_ := false.B)
  req.fromCmoHelper := false.B
  req.reqSource := MemReqSource.NoWhere.id.U

  val client_dir = dir.clients.states(req_client)
  val dir_conflict = !dir.clients.tag_match && Cat(
    dir.clients.states.map(s => !s.hit && s.state =/= MetaData.INVALID)
  ).orR
  val formA = dir.replacerInfo.channel === 1.U
  val req_acquire = formA && (dir.replacerInfo.opcode === TLMessages.AcquirePerm ||
    dir.replacerInfo.opcode === TLMessages.AcquireBlock)
  queue.io.enq.valid := req_acquire && io.dirResult.valid && dir_conflict
  queue.io.enq.bits := req
  when(queue.io.enq.valid){ assert(queue.io.enq.ready) }

  io.probe <> queue.io.deq

  XSPerfAccumulate(cacheParams, "client_dir_conflict", queue.io.enq.fire)
  //val perfinfo = IO(new Bundle(){
  //  val perfEvents = Output(new PerfEventsBundle(numPCntHcReqb))
  //})
  val perfinfo = IO(Output(Vec(numPCntHcProb, (UInt(6.W)))))
  val perfEvents = Seq(
    ("client_dir_conflict        ", queue.io.enq.fire             ),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.zip(perfEvents).zipWithIndex) {
    perf_out := RegNext(perf)
  }
}
