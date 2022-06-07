/**
 * @author zeal4u
 */
package huancun.prefetch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import huancun.HasHuanCunParameters
import huancun.utils.{SRAMTemplate, SetAssocReplacer, XSPerfPrint, ReplacementPolicy, Min, Max}


case class SMSParameters(
                        ftSets: Int = 4,
                        ftWays: Int = 16,
                        atSets: Int = 8,
                        atWays: Int = 16,
                        ptSets: Int = 32,
                        ptWays: Int = 16,
//                        ptThresholdL1: Int = 75,
//                        ptThresholdL2: Int = 25,
                        psSets: Int = 4,
                        psWays: Int = 16,
                        // L2 caches can hardly get PC
                        // pcBits: Int = 0,
                        addrBits: Int = 16,
                        lineBits: Int = 5
                        ) extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 16

}

trait HasSMSParams extends HasHuanCunParameters {
  val smsParams = prefetchOpt.get.asInstanceOf[SMSParameters]
  val printFlag = false

  val ftSets = smsParams.ftSets
  val ftWays = smsParams.ftWays
  val atSets = smsParams.atSets
  val atWays = smsParams.atWays
  val ptSets = smsParams.ptSets
  val ptWays = smsParams.ptWays
  val ptThresholdL1 = smsParams.ptThresholdL1
  val ptThresholdL2 = smsParams.ptThresholdL2
  val psSets = smsParams.psSets
  val psWays = smsParams.psWays
  val addrBits = smsParams.addrBits
  val lineBits = smsParams.lineBits

  val blockBits = fullAddressBits - offsetBits
  val regionBits = blockBits - lineBits
  val ftTagBits = regionBits - log2Ceil(ftSets)
  val atTagBits = regionBits - log2Ceil(atSets)
  val psTagBits = regionBits - log2Ceil(psSets)
  val ptLongTagBits = blockBits - log2Ceil(ptSets)
  val bitVectorSize = 1 << lineBits
}

abstract class SMSModule(implicit p:Parameters) extends PrefetchModule with HasSMSParams
{
  def getBlock(addr: UInt) = addr(fullAddressBits - 1, offsetBits)
  def getBlockAddr(addr: UInt) = Cat(addr, 0.U(offsetBits.W))
  def getOffset(block: UInt) = block(lineBits-1, 0)
  def getRegion(block: UInt) = block(regionBits+lineBits-1, lineBits)
}

abstract class SMSBundle(implicit p:Parameters) extends PrefetchBundle with HasSMSParams

class FT2AT(implicit p:Parameters) extends SMSBundle {
  val block = UInt((fullAddressBits-log2Ceil(blockBytes)).W)
  // trigger offset
  val offset = UInt(lineBits.W)
}

class FilterTableIO(implicit  p:Parameters) extends SMSBundle {
  val train   = Flipped(Decoupled(new PrefetchTrain()))
  val resp    = Decoupled(new FT2AT())
  val queryPT = Decoupled(new PrefetchTrain())
}

class FilterTableEntry(implicit p:Parameters) extends SMSBundle {
  val tag = UInt(ftTagBits.W)
  val offset = UInt(lineBits.W)
}

/**
  * filter accesses that do not reappear.
  */
class FilterTable(implicit p:Parameters) extends SMSModule {
  def getTag(region: UInt) = region(log2Ceil(ftSets)+ftTagBits-1, log2Ceil(ftSets))
  def getIndex(region: UInt) = region(log2Ceil(ftSets)-1, 0)

  val io = IO(new FilterTableIO())
  io.train.ready := true.B

  val block = RegNext(getBlock(io.train.bits.addr))
  val offset = getOffset(block)
  val region = getRegion(block)

  val mem = Module(new SRAMTemplate(new FilterTableEntry(), set=ftSets, way=ftWays, singlePort=false, bypassWrite=true))
  val lrus = new SetAssocReplacer(ftSets, ftWays, "lru")
  val valids = RegInit(VecInit(Seq.fill(ftSets)(VecInit(Seq.fill(ftWays)(false.B)))))

  val hits = VecInit(Seq.fill(ftWays)(false.B))
  val hit = hits.reduce(_||_)

  val index = getIndex(region)
  val tag = getTag(getRegion(block))

  mem.io.r.req.valid := io.train.fire
  mem.io.r.req.bits.setIdx := getIndex(getRegion(getBlock(io.train.bits.addr)))
  val data_vec = mem.io.r.resp.data
  val victim_way = WireInit(0.U(log2Ceil(ftWays).W))

  // zeal4u: filter access if it has the same offset
  val fire_stage1 = RegNext(mem.io.r.req.fire)

  for (i <- 0 until ftWays) {
    hits(i) := fire_stage1 & valids(index)(i) & data_vec(i).tag === tag
  }

  io.queryPT.bits := RegNext(io.train.bits)
  // get the target way
  when (hit) {
    io.queryPT.valid := false.B
    victim_way := OHToUInt(hits)
  }.otherwise {
    // all valid
    when (valids(index).asUInt === (ftWays-1).U) {
      victim_way := lrus.way(index)
    } otherwise {
      victim_way := PriorityEncoder(~valids(index).asUInt)
    }
    lrus.access(index, victim_way)
    io.queryPT.valid := fire_stage1
  }

  // allocate new entry
  val write_mask = UIntToOH(victim_way)
  val data_vec_write = WireInit(data_vec)
  data_vec_write(victim_way).tag := getTag(getRegion(block))
  data_vec_write(victim_way).offset := getOffset(block)
  mem.io.w.req.valid := fire_stage1 && !hit
  mem.io.w.req.bits.apply(
    setIdx = index, 
    data = data_vec_write,
    waymask = write_mask.asUInt
  )
  valids(index)(victim_way) := fire_stage1 && !hit
  XSPerfPrint(printFlag, fire_stage1 && !hit, p"SMS FT -> Write Data, write mask:${Binary(write_mask)}, " +
    p"way:${victim_way}, tag:${Hexadecimal(data_vec_write(victim_way).tag)}, " +
    p"offset:${data_vec_write(victim_way).offset}\n")

  val find_pattern = hit && data_vec(victim_way).offset =/= offset
  when (find_pattern) {
    // invalid the record
    valids(index)(victim_way) := false.B

    io.resp.valid := true.B
    io.resp.bits.block := block
    io.resp.bits.offset := data_vec(victim_way).offset
    XSPerfPrint(printFlag,p"SMS FT -> Get a pattern, offset1:${getOffset(block)}, " +
      p"offset2:${data_vec(victim_way).offset}, " +
      p"region:0x${Hexadecimal(getRegion(block))}\n")
  } otherwise {
    io.resp.valid := false.B
    io.resp.bits := DontCare
  }

  XSPerfPrint(printFlag, io.train.fire,
    p"SMS FT -> New Access, " +
    p"block:0x${Hexadecimal(getBlock(io.train.bits.addr))}\n")
}

class AT2PT(implicit p:Parameters) extends SMSBundle {
  val bit_vector = UInt(bitVectorSize.W)
  val block = UInt((fullAddressBits-log2Ceil(blockBytes)).W)
}

class AccumulateTableIO(implicit p:Parameters) extends SMSBundle {
  val train      = Flipped(Decoupled(new PrefetchTrain())) // update existing patterns
  val reqFT      = Flipped(Decoupled(new FT2AT())) //  a new pattern from FT
  val evict      = Flipped(Decoupled(new EvictionInfo()))
  val evict_resp = Decoupled(new AT2PT())
  // indicate whether the record exists
  val qeuryFT    = Decoupled(new PrefetchTrain())
}

class AccumulateTableEntry(implicit p:Parameters) extends SMSBundle {
  val tag = UInt(atTagBits.W)
  val bit_vector = Vec(bitVectorSize, Bool())
  val offset = UInt(lineBits.W)
}

class AccumulateTable(implicit p:Parameters) extends SMSModule {
  def getTag(region: UInt) = region(log2Ceil(atSets)+atTagBits-1, log2Ceil(atSets))
  def getIndex(region: UInt) = region(log2Ceil(atSets)-1, 0)
  def rotateOffset(startOffset:UInt, offset:UInt) = Mux(offset >= startOffset, offset-startOffset, bitVectorSize.U-startOffset+offset)

  val io = IO(new AccumulateTableIO())
  io.evict.ready := true.B
  io.reqFT.ready := true.B
  io.train.ready := true.B

  val reqFTQueue = Module(new Queue(new FT2AT, 1))
  val trainQueue = Module(new Queue(new PrefetchTrain, 6))

  reqFTQueue.io.enq <> io.reqFT
  trainQueue.io.enq <> io.train

  val train = trainQueue.io.deq
  val reqFT = reqFTQueue.io.deq
  val req_fire = reqFT.fire || train.fire || io.evict.fire
  val index = Mux(io.evict.fire, getIndex(getRegion(getBlock(io.evict.bits.addr))), 
              Mux(train.fire, getIndex(getRegion(getBlock(train.bits.addr))),
              Mux(reqFT.fire, getIndex(getRegion(reqFT.bits.block)), 0.U)))

  val table = Module(new SRAMTemplate(new AccumulateTableEntry(), set=atSets, way=atWays, singlePort=false, bypassWrite=true))
  val valids = RegInit(VecInit(Seq.fill(atSets)(VecInit(Seq.fill(atWays)(false.B)))))
  val lrus = new SetAssocReplacer(atSets, atWays, "lru")
  val victim_way = WireInit((0.U(log2Ceil(atWays).W)))
  val hits = VecInit(Seq.fill(atWays)(false.B))
  val hit = hits.reduce(_||_)
  val tag = RegNext(Mux(io.evict.fire, getTag(getRegion(getBlock(io.evict.bits.addr))), 
                    Mux(reqFT.fire, getTag(getRegion(reqFT.bits.block)), 
                    Mux(train.fire, getTag(getRegion(getBlock(train.bits.addr))), 0.U))))

  table.io.r.req.valid := req_fire
  table.io.r.req.bits.setIdx := index
  val data_vec = table.io.r.resp.data
  val data_vec_write = WireInit(data_vec)
  val reg_index = RegNext(index)

  table.io.w.req.valid := RegNext(reqFT.fire) || RegNext(train.fire) && hit
  table.io.w.req.bits.apply(
    setIdx = reg_index,
    data = data_vec_write,
    waymask = UIntToOH(victim_way).asUInt
  )

  for (i <- 0 until atWays) {
    hits(i) := RegNext(req_fire) && valids(reg_index)(i) && data_vec(i).tag === tag
  }

  when (hit) {
    victim_way := OHToUInt(hits)
  } .elsewhen (valids(reg_index).asUInt === (atWays-1).U) {
    victim_way := lrus.way(reg_index)
  } .otherwise {
    victim_way := PriorityEncoder(~valids(reg_index).asUInt)
  }

  io.qeuryFT.valid := !hit && train.fire
  io.qeuryFT.bits  := train.bits

  reqFT.ready := !io.evict.fire
  train.ready := !io.evict.fire && !reqFT.fire

  io.evict_resp.valid := false.B
  io.evict_resp.bits := DontCare

  when (RegNext(io.evict.fire)){
    when (hit) {
      val hit_way = OHToUInt(hits)
      io.evict_resp.valid := true.B
      io.evict_resp.bits.bit_vector := data_vec(hit_way).bit_vector.asUInt
      io.evict_resp.bits.block := Cat(data_vec(hit_way).tag, reg_index, data_vec(hit_way).offset)
      // invalid it
      valids(reg_index)(hit_way) := false.B
      XSPerfPrint(printFlag,p"SMS AT -> Cache eviction hits, block:0x${Hexadecimal(Cat(data_vec(hit_way).tag, reg_index, data_vec(hit_way).offset))}, " +
        p"bit vector:${data_vec(hit_way).bit_vector}, " +
        p"hit:${hits}\n")
    }
    XSPerfPrint(printFlag,p"SMS AT -> Cache eviction, addr:0x${Hexadecimal(io.evict.bits.addr)}\n")
  }.elsewhen(RegNext(reqFT.fire)){
    val start_offset = RegNext(reqFT.bits.offset)
    val offset = RegNext(getOffset(reqFT.bits.block))
    when (hit) {
      val rotated_offset1 = rotateOffset(data_vec(victim_way).offset, start_offset)
      val rotated_offset2 = rotateOffset(data_vec(victim_way).offset, offset)
      data_vec_write(victim_way).bit_vector(rotated_offset1) := true.B
      data_vec_write(victim_way).bit_vector(rotated_offset2) := true.B
      XSPerfPrint(printFlag, p"SMS AT -> Error! Attempt to insert existing record, set:${index}, way:${victim_way}, tag:${data_vec_write(victim_way).tag}, " +
                             p"Insert rotated offset:${rotated_offset1}, ${rotated_offset2}\n")
    }.otherwise {
      val bit_vector = VecInit(Seq.fill(bitVectorSize)(false.B))

      bit_vector(0):= true.B
      bit_vector(rotateOffset(start_offset, offset)) := true.B
 
      data_vec_write(victim_way).tag := tag
      data_vec_write(victim_way).offset := start_offset
      data_vec_write(victim_way).bit_vector := bit_vector
      XSPerfPrint(printFlag, p"SMS AT -> New Pattern, index:${reg_index}, way:${victim_way}, " +
                             p"start offset:${start_offset}, offset:${getOffset(reqFT.bits.block)}\n")
    }

    valids(reg_index)(victim_way) := true.B
    lrus.access(reg_index, victim_way)
  }.elsewhen (RegNext(train.fire)){
    when (hit) {
      val offset = getOffset(getBlock(train.bits.addr))
      val rotated_offset = rotateOffset(data_vec(OHToUInt(hits)).offset, offset)

      data_vec_write(OHToUInt(hits)).bit_vector(rotated_offset) := true.B
      XSPerfPrint(printFlag,p"SMS AT -> New train, addr:0x${Hexadecimal(train.bits.addr)}, " +
        p"offset:${offset}, start offset:${data_vec(OHToUInt(hits)).offset} rotate offset:${rotated_offset}, " +
        p"tag:0x${Hexadecimal(tag)}, index:${reg_index}, hit:${hits}, "+
        p"vector:${data_vec_write(OHToUInt(hits)).bit_vector}\n")
    }
  }
}

class PT2PS(implicit p:Parameters) extends SMSBundle {
  val block = UInt((fullAddressBits-log2Ceil(blockBytes)).W)
  val prefetch_pattern = Vec(bitVectorSize, UInt(2.W))
}

class PatternHistoryTableIO(implicit p:Parameters) extends SMSBundle {
  val predict_req  = Flipped(Decoupled(new PrefetchTrain))
  val predict_resp = Decoupled(new PT2PS())
  val reqAT = Flipped(Decoupled(new AT2PT()))
}

class PatternHistoryTableEntry(implicit p:Parameters) extends SMSBundle {
  val tag = UInt(ptLongTagBits.W)
  val pattern = Vec(bitVectorSize, Bool())
}

class PatternHistoryTable(implicit p:Parameters) extends SMSModule {
  def getIndex(key: UInt) = key(log2Ceil(ptSets)-1, 0)
  def getLongTag(key: UInt) = key(log2Ceil(ptSets)+ptLongTagBits-1, log2Ceil(ptSets))

  val io = IO(new PatternHistoryTableIO())
  io.reqAT.ready := true.B
  io.predict_req.ready := !io.reqAT.fire

  val table = Module(new SRAMTemplate(new PatternHistoryTableEntry(), set=ptSets, way=ptWays, singlePort=false, bypassWrite=true))
  val lrus = new SetAssocReplacer(ptSets, ptWays, "lru")
  val valids = RegInit(VecInit(Seq.fill(ptSets)(VecInit(Seq.fill(ptWays)(false.B)))))

  val key = Mux(io.reqAT.fire, io.reqAT.bits.block,
            Mux(io.predict_req.fire, getBlock(io.predict_req.bits.addr), 0.U))

  table.io.r.req.valid := io.predict_req.fire || io.reqAT.fire
  table.io.r.req.bits.setIdx := getIndex(key)
  val data_vec = table.io.r.resp.data
  val data_vec_write = WireInit(data_vec)
  val reg_index = RegNext(getIndex(key))
  val long_tag = RegNext(getLongTag(key))
  val victim_way = WireInit(0.U(log2Ceil(ptWays).W))
  val hits = Wire(Vec(ptWays, Bool()))
  val hit = hits.reduce(_||_)
  // val short_hit_count_vector = VecInit(Seq.fill(bitVectorSize)(0.U(log2Ceil(ptWays).W)))
  // val count_matrix = VecInit(Seq.fill(bitVectorSize)(VecInit(Seq.fill(ptWays)(false.B))))

  table.io.w.req.valid := RegNext(io.reqAT.fire)
  table.io.w.req.bits.apply(
    setIdx = reg_index,
    data = data_vec_write,
    waymask = UIntToOH(victim_way).asUInt
  )

  for (i <- 0 until ptWays) {
    hits(i) := RegNext(io.predict_req.fire) && valids(reg_index)(i) && data_vec(i).tag === long_tag
  }

  when (RegNext(io.reqAT.fire)) {
    when (hit) {
      victim_way := OHToUInt(hits)
    }.elsewhen(valids(reg_index).asUInt === (ptWays - 1).U) {
      victim_way := lrus.way(reg_index)
    }.otherwise{
      victim_way := PriorityEncoder(~valids(reg_index).asUInt)
    }
    lrus.access(reg_index, victim_way)
    data_vec_write(victim_way).pattern := io.reqAT.bits.bit_vector.asBools
    data_vec_write(victim_way).tag := long_tag
    valids(reg_index)(victim_way) := true.B
  }

  io.predict_resp.valid := hit
  io.predict_resp.bits.prefetch_pattern := data_vec(OHToUInt(hits)).pattern
  io.predict_resp.bits.block := getBlock(io.predict_req.bits.addr)

  XSPerfPrint(printFlag, io.reqAT.fire, p"SMS PT -> New Train, block:0x${Hexadecimal(io.reqAT.bits.block)}, " +
    p"bit vector:${io.reqAT.bits.bit_vector}\n")
  XSPerfPrint(printFlag,io.predict_req.fire, p"SMS PT -> New Request, " +
    p"addr:0x${Hexadecimal(io.predict_req.bits.addr)}, " +
    p"long tag:0x${Hexadecimal(long_tag)}\n")
  XSPerfPrint(printFlag, hit,
    p"hits:${hits}, prefetch pattern:${data_vec(OHToUInt(hits)).pattern}\n")
}


class PrefetchStreamerIO(implicit p:Parameters) extends SMSBundle {
  val reqPT = Flipped(Decoupled(new PT2PS()))
  val prefetch_req = Flipped(Decoupled(new PrefetchTrain()))
  val prefetch_resp = Decoupled(new PrefetchReq())
}

class PrefetchStreamerEntry(implicit p:Parameters) extends SMSBundle {
  val tag = UInt(psTagBits.W)
  val prefetch_pattern = Vec(bitVectorSize, Bool())
}

class PrefetchStreamer(implicit p:Parameters) extends SMSModule {
  def getIndex(key: UInt) = key(log2Ceil(psSets)-1, 0)
  def getTag(key:UInt) = key(log2Ceil(psSets)+psTagBits-1, log2Ceil(psSets))
  val io = IO(new PrefetchStreamerIO())

  val table = Module(new SRAMTemplate(new PrefetchStreamerEntry(), set=psSets, way=psWays, singlePort=false, bypassWrite=true))
  val lrus = new SetAssocReplacer(psSets, psWays, "lru")
  val valids = RegInit(VecInit(Seq.fill(psSets)(VecInit(Seq.fill(psWays)(false.B)))))

  val key = Mux(io.reqPT.fire, getRegion(io.reqPT.bits.block), 
            Mux(io.prefetch_req.fire, getRegion(getBlock(io.prefetch_req.bits.addr)), 0.U))
  val tag = getTag(key)
  val reg_tag = RegNext(tag)
  val index = getIndex(key)
  val reg_index = RegNext(index)
  table.io.r.req.valid := io.reqPT.fire || io.prefetch_req.fire
  table.io.r.req.bits.setIdx := index
  val data_vec = table.io.r.resp.data
  val data_vec_write = WireInit(data_vec)

  val req = RegNext(io.prefetch_req.bits)
  val hits = VecInit(Seq.fill(psWays)(false.B))
  val hit = hits.reduce(_||_)
  val victim_way = WireInit(0.U(log2Ceil(psWays).W))

  val draining = RegInit(false.B)
  val drain_req = RegInit(0.U.asTypeOf(new PrefetchTrain))

  val keep_draining = draining || draining && RegNext(io.prefetch_req.fire) && getRegion(getBlock(req.addr)) === getRegion(getBlock(drain_req.addr))
  val drain_data_vec = RegEnable(data_vec_write, !keep_draining && hit)
  val drain_hit_way = RegEnable(OHToUInt(hits), !keep_draining && hit) 

  for (i <- 0 until psWays) {
    hits(i) := RegNext(io.prefetch_req.fire) && valids(reg_index)(i) & reg_tag === data_vec(i).tag
  }

  io.prefetch_resp.valid := false.B
  io.prefetch_resp.bits := DontCare

  table.io.w.req.valid := false.B
  table.io.w.req.bits := DontCare

  io.reqPT.ready := true.B
  io.prefetch_req.ready := !io.reqPT.fire
  when (keep_draining) {
    val prefetch_pattern = drain_data_vec(drain_hit_way).prefetch_pattern
    val first_entry = PriorityEncoder(prefetch_pattern)
    val target_addr = getBlockAddr(getBlock(drain_req.addr) + first_entry)

    io.prefetch_resp.valid := prefetch_pattern.asUInt =/= 0.U
    io.prefetch_resp.bits.tag := parseFullAddress(target_addr)._1
    io.prefetch_resp.bits.set := parseFullAddress(target_addr)._2
    io.prefetch_resp.bits.needT := drain_req.needT
    io.prefetch_resp.bits.source:= drain_req.source
    prefetch_pattern(first_entry) := false.B
    
    draining := prefetch_pattern.asUInt =/= 0.U
    XSPerfPrint(printFlag, p"SMS PS -> Draining, Generate prefetch, block:${getBlock(target_addr)}, addr:${target_addr}, " +
      p"pattern:${prefetch_pattern}\n")
  }.elsewhen (hit) {
    val first_entry = PriorityEncoder(data_vec(OHToUInt(hits)).prefetch_pattern)
    val target_addr = getBlockAddr(getBlock(req.addr) + first_entry)
    io.prefetch_resp.valid := data_vec(OHToUInt(hits)).prefetch_pattern.asUInt =/= 0.U
    io.prefetch_resp.bits.tag := parseFullAddress(target_addr)._1
    io.prefetch_resp.bits.set := parseFullAddress(target_addr)._2
    io.prefetch_resp.bits.needT := req.needT
    io.prefetch_resp.bits.source:= req.source
    data_vec_write(OHToUInt(hits)).prefetch_pattern(first_entry) := false.B
    when(draining) {
      table.io.w.req.valid := true.B
      table.io.w.req.bits.apply(
        setIdx = getIndex(getRegion(getBlock(drain_req.addr))),
        data = drain_data_vec,
        waymask = UIntToOH(drain_hit_way).asUInt
      )
    }.otherwise {
      draining := data_vec_write(OHToUInt(hits)).prefetch_pattern.asUInt =/= 0.U 
      valids(getIndex(getRegion(getBlock(drain_req.addr))))(drain_hit_way) := false.B
    }

    drain_req := req
    XSPerfPrint(printFlag, p"SMS PS -> Generate prefetch, block:${getBlock(target_addr)}, addr:${target_addr}\n")
  }.elsewhen (RegNext(io.reqPT.fire)) {
    when(hit) {
      victim_way := OHToUInt(hits)
    }.elsewhen(valids(reg_index).asUInt === (psWays - 1).U) {
      victim_way := lrus.way(reg_index)
    }.otherwise {
      victim_way := PriorityEncoder(~valids(reg_index).asUInt)
    }

    data_vec_write(victim_way).tag := reg_tag
    data_vec_write(victim_way).prefetch_pattern := RegNext(io.reqPT.bits.prefetch_pattern)
    valids(reg_index)(victim_way) := true.B

    table.io.w.req.valid := true.B
    table.io.w.req.bits.apply(
      setIdx = reg_index,
      data = data_vec_write,
      waymask = UIntToOH(victim_way).asUInt
    )
    lrus.access(reg_index, victim_way)
    XSPerfPrint(printFlag, p"SMS PS -> New Train, block:0x${Hexadecimal(RegNext(io.reqPT.bits.block))}, " +
                p"index:${reg_index}, tag:${reg_tag}" +
                p"bit vector:${RegNext(io.reqPT.bits.prefetch_pattern)}\n")
  }

  XSPerfPrint(printFlag, io.prefetch_req.fire, p"SMS PS -> Trigger Prefetch, key:0x${Hexadecimal(key)}, " + 
              p"index:${index}, tag:${tag}\n") 
}

class SpatialMemoryStreaming(implicit p: Parameters) extends SMSModule {
  val io = IO(new PrefetchIO)

  val filterTable = Module(new FilterTable())
  val accumulateTable = Module(new AccumulateTable())
  val patternTable = Module(new PatternHistoryTable())
  val prefetchStreamer = Module(new PrefetchStreamer())

  val s1_valid = RegNext(io.train.valid)
  val s2_valid = RegNext(s1_valid)

  val s1_train = RegNext(io.train.bits)
  val s2_train = RegNext(s1_train)

  accumulateTable.io.evict <> io.evict
  accumulateTable.io.train <> io.train
  accumulateTable.io.reqFT <> filterTable.io.resp

  filterTable.io.train <> accumulateTable.io.qeuryFT

  patternTable.io.predict_req <> filterTable.io.queryPT
  patternTable.io.reqAT <> accumulateTable.io.evict_resp

  prefetchStreamer.io.reqPT <> patternTable.io.predict_resp
  prefetchStreamer.io.prefetch_req <> io.train
  io.req <> prefetchStreamer.io.prefetch_resp

  io.resp.ready := true.B
  io.train.ready := true.B
  io.evict.ready := true.B
}
