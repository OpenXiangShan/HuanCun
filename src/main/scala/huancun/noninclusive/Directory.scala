package huancun.noninclusive

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages
import huancun.MetaData._
import huancun._
import huancun.debug.{DirectoryLogger, TypeId}
import huancun.utils._

trait HasClientInfo { this: HasHuanCunParameters =>
  // assume all clients have same params
  // TODO: check client params to ensure they are same
  val clientCacheParams = cacheParams.clientCaches.head
  val aliasBits = aliasBitsOpt.getOrElse(0)

  val clientSets = clientCacheParams.sets
  val clientWays = clientCacheParams.ways
  val clientSetBits = log2Ceil(clientSets)
  val clientWayBits = log2Ceil(clientWays)
  val clientTagBits = addressBits - clientSetBits - offsetBits
}

class SelfDirEntry(implicit p: Parameters) extends HuanCunBundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clientStates = Vec(clientBits, UInt(stateBits.W))
  val prefetch = if (hasPrefetchBit) Some(Bool()) else None // whether the block is prefetched
}

class ClientDirEntry(implicit p: Parameters) extends HuanCunBundle {
  val state = UInt(stateBits.W)
  val alias = aliasBitsOpt.map(bits => UInt(bits.W))
}

class SelfDirResult(implicit p: Parameters) extends SelfDirEntry {
  val hit = Bool()
  val way = UInt(wayBits.W)
  val tag = UInt(tagBits.W)
}

class ClientDirResult(implicit p: Parameters) extends ClientDirEntry with HasClientInfo {
  val hit = Bool()
  val way = UInt(clientWayBits.W)
  val tag = UInt(clientTagBits.W)

  def parseTag(lineAddr: UInt): UInt = {
    lineAddr(clientSetBits + clientTagBits - 1, clientSetBits)
  }
}

class DirResult(implicit p: Parameters) extends BaseDirResult {
  val self = new SelfDirResult
  val clients = Vec(clientBits, new ClientDirResult)
  val sourceId = UInt(sourceIdBits.W)
  val set = UInt(setBits.W)
  val replacerInfo = new ReplacerInfo
}

class SelfTagWrite(implicit p: Parameters) extends BaseTagWrite {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val tag = UInt(tagBits.W)
}

class ClientTagWrite(implicit p: Parameters) extends HuanCunBundle with HasClientInfo {
  val set = UInt(clientSetBits.W)
  val way = UInt(clientWayBits.W) // log2
  val tag = UInt(clientTagBits.W)

  def apply(lineAddr: UInt, way: UInt) = {
    this.set := lineAddr(clientSetBits - 1, 0)
    this.way := way
    this.tag := lineAddr(clientSetBits + clientTagBits - 1, clientSetBits)
  }
}

class SelfDirWrite(implicit p: Parameters) extends BaseDirWrite {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val data = new SelfDirEntry
}

class ClientDirWrite(implicit p: Parameters) extends HuanCunBundle with HasClientInfo {
  val set = UInt(clientSetBits.W)
  val way = UInt(clientWayBits.W)
  val data = new ClientDirEntry()

  def apply(lineAddr: UInt, way: UInt, data: ClientDirEntry) = {
    this.set := lineAddr(clientSetBits - 1, 0)
    this.way := way
    this.data := data
  }
}

trait NonInclusiveCacheReplacerUpdate { this: HasUpdate =>
  override def doUpdate(info: ReplacerInfo): Bool = {
    val release_update = info.channel(2) && info.opcode === TLMessages.ReleaseData
    val prefetch_update = info.channel(0) && info.opcode === TLMessages.Hint
    release_update | prefetch_update
  }
}

class DirectoryIO(implicit p: Parameters) extends BaseDirectoryIO[DirResult, SelfDirWrite, SelfTagWrite] {
  val reads = Vec(dirReadPorts, Flipped(DecoupledIO(new DirRead)))
  val results = Vec(dirReadPorts, ValidIO(new DirResult))
  val dirWReqs = Vec(mshrsAll, Flipped(DecoupledIO(new SelfDirWrite)))
  val tagWReq = Flipped(DecoupledIO(new SelfTagWrite))
  val clientDirWReqs = Vec(clientBits, Vec(mshrsAll, Flipped(DecoupledIO(new ClientDirWrite))))
  val clientTagWreq = Vec(clientBits, Flipped(DecoupledIO(new ClientTagWrite)))
}

class Directory(implicit p: Parameters)
    extends BaseDirectory[DirResult, SelfDirWrite, SelfTagWrite]
    with HasClientInfo {
  val io = IO(new DirectoryIO())

  val stamp = GTimer()
  for (i <- 0 until mshrsAll) {
    val selfDirW = io.dirWReqs(i)
    DirectoryLogger(cacheParams.name, TypeId.self_dir)(
      selfDirW.bits.set,
      selfDirW.bits.way,
      0.U,
      selfDirW.bits.data,
      stamp,
      selfDirW.fire(),
      this.clock,
      this.reset
    )
  }
  DirectoryLogger(cacheParams.name, TypeId.self_tag)(
    io.tagWReq.bits.set,
    io.tagWReq.bits.way,
    io.tagWReq.bits.tag,
    0.U,
    stamp,
    io.tagWReq.fire(),
    this.clock,
    this.reset
  )

  for ((cDir, cTag) <- io.clientDirWReqs.zip(io.clientTagWreq)) {
    for (i <- 0 until mshrsAll) {
      val dirW = cDir(i)
      DirectoryLogger(cacheParams.name, TypeId.client_dir)(
        dirW.bits.set,
        dirW.bits.way,
        0.U,
        dirW.bits.data,
        stamp,
        dirW.fire(),
        this.clock,
        this.reset
      )
    }
    DirectoryLogger(cacheParams.name, TypeId.client_tag)(
      cTag.bits.set,
      cTag.bits.way,
      cTag.bits.tag,
      0.U,
      stamp,
      cTag.fire(),
      this.clock,
      this.reset
    )
  }

  def clientHitFn(dir: ClientDirEntry): Bool = dir.state =/= MetaData.INVALID
  def client_invalid_way_fn(metaVec: Seq[ClientDirEntry], repl: UInt): (Bool, UInt) = {
    val invalid_vec = metaVec.map(_.state === MetaData.INVALID)
    val has_invalid_way = Cat(invalid_vec).orR()
    val way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(clientWayBits.W)))
    (has_invalid_way, way)
  }
  val clientDirs = (0 until clientBits).map { _ =>
    val clientDir = Module(
      new SubDirectory[ClientDirEntry](
        rports = dirReadPorts,
        wports = mshrsAll,
        sets = clientSets,
        ways = clientWays,
        tagBits = clientTagBits,
        dir_init_fn = () => {
          val init = Wire(new ClientDirEntry)
          init.state := MetaData.INVALID
          init.alias.foreach( _ := DontCare)
          init
        },
        dir_hit_fn = clientHitFn,
        invalid_way_sel = client_invalid_way_fn,
        replacement = "random"
      )
    )
    clientDir
  }

  def selfHitFn(dir: SelfDirEntry): Bool = dir.state =/= MetaData.INVALID
  def self_invalid_way_sel(metaVec: Seq[SelfDirEntry], repl: UInt): (Bool, UInt) = {
    // 1.try to find a invalid way
    val invalid_vec = metaVec.map(_.state === MetaData.INVALID)
    val has_invalid_way = Cat(invalid_vec).orR()
    val invalid_way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(wayBits.W)))
    // 2.if there is no invalid way, then try to find a TRUNK to replace
    // (we are non-inclusive, if we are trunk, there must be a TIP in our client)
    val trunk_vec = metaVec.map(_.state === MetaData.TRUNK)
    val has_trunk_way = Cat(trunk_vec).orR()
    val trunk_way = ParallelPriorityMux(trunk_vec.zipWithIndex.map(x => x._1 -> x._2.U(wayBits.W)))
    val repl_way_is_trunk = VecInit(metaVec)(repl).state === MetaData.TRUNK
    (
      has_invalid_way || has_trunk_way,
      Mux(has_invalid_way, invalid_way, Mux(repl_way_is_trunk, repl, trunk_way))
      )
  }
  val selfDir = Module(
    new SubDirectoryDoUpdate[SelfDirEntry](
      rports = dirReadPorts,
      wports = mshrsAll,
      sets = cacheParams.sets,
      ways = cacheParams.ways,
      tagBits = tagBits,
      dir_init_fn = () => {
        val init = Wire(new SelfDirEntry())
        init := DontCare
        init.state := MetaData.INVALID
        init
      },
      dir_hit_fn = selfHitFn,
      self_invalid_way_sel,
      replacement = "lru"
    ) with NonInclusiveCacheReplacerUpdate
  )

  def addrConnect(lset: UInt, ltag: UInt, rset: UInt, rtag: UInt) = {
    assert(lset.getWidth + ltag.getWidth == rset.getWidth + rtag.getWidth)
    val addr = Cat(rtag, rset)
    lset := addr.tail(ltag.getWidth)
    ltag := addr.head(ltag.getWidth)
  }

  for (i <- 0 until dirReadPorts) {
    val rports = clientDirs.map(_.io.reads(i)) :+ selfDir.io.reads(i)
    val req = io.reads(i)
    rports.foreach { p =>
      p.valid := req.valid
      addrConnect(p.bits.set, p.bits.tag, req.bits.set, req.bits.tag)
      p.bits.replacerInfo := req.bits.replacerInfo
    }
    req.ready := Cat(rports.map(_.ready)).andR()
    val reqIdOHReg = RegEnable(req.bits.idOH, req.fire())
    val sourceIdReg = RegEnable(req.bits.source, req.fire())
    val setReg = RegEnable(req.bits.set, req.fire())
    val replacerInfoReg = RegEnable(req.bits.replacerInfo, req.fire())
    val resp = io.results(i)
    val clientResps = clientDirs.map(_.io.resps(i))
    val selfResp = selfDir.io.resps(i)
    resp.valid := selfResp.valid
    val valids = Cat(clientResps.map(_.valid) :+ selfResp.valid)
    assert(valids.andR() || !valids.orR(), "valids must be all 1s or 0s")
    resp.bits.idOH := reqIdOHReg
    resp.bits.sourceId := sourceIdReg
    resp.bits.set := setReg
    resp.bits.replacerInfo := replacerInfoReg
    resp.bits.self.hit := selfResp.bits.hit
    resp.bits.self.way := selfResp.bits.way
    resp.bits.self.tag := selfResp.bits.tag
    resp.bits.self.dirty := selfResp.bits.dir.dirty
    resp.bits.self.state := selfResp.bits.dir.state
    resp.bits.self.clientStates := selfResp.bits.dir.clientStates
    resp.bits.self.prefetch.foreach(p => p := selfResp.bits.dir.prefetch.get)
    resp.bits.clients.zip(clientResps).foreach {
      case (resp, clientResp) =>
        resp.hit := clientResp.bits.hit
        resp.way := clientResp.bits.way
        resp.tag := clientResp.bits.tag
        resp.state := clientResp.bits.dir.state
        resp.alias.foreach(_ := clientResp.bits.dir.alias.get)
    }
  }

  // Self Tag Write
  selfDir.io.tag_w.valid := io.tagWReq.valid
  selfDir.io.tag_w.bits.tag := io.tagWReq.bits.tag
  selfDir.io.tag_w.bits.set := io.tagWReq.bits.set
  selfDir.io.tag_w.bits.way := io.tagWReq.bits.way
  io.tagWReq.ready := selfDir.io.tag_w.ready
  // Clients Tag Write
  for ((req, wport) <- io.clientTagWreq.zip(clientDirs.map(_.io.tag_w))) {
    wport.valid := req.valid
    wport.bits.tag := req.bits.tag
    wport.bits.set := req.bits.set
    wport.bits.way := req.bits.way
    req.ready := wport.ready
  }

  // Self Dir Write
  for ((req, wport) <- io.dirWReqs.zip(selfDir.io.dir_w)) {
    wport.valid := req.valid
    wport.bits.set := req.bits.set
    wport.bits.way := req.bits.way
    wport.bits.dir := req.bits.data
    req.ready := wport.ready
  }
  // Clients Dir Write
  for ((reqs, client) <- io.clientDirWReqs.zip(clientDirs)) {
    for ((req, wport) <- reqs.zip(client.io.dir_w)) {
      wport.valid := req.valid
      wport.bits.set := req.bits.set
      wport.bits.way := req.bits.way
      wport.bits.dir := req.bits.data
      req.ready := wport.ready
    }
  }

  assert(dirReadPorts == 1)
  val resp = io.results.head
  val req = io.reads.head
  XSPerfAccumulate(cacheParams, "selfdir_A_req", req.bits.replacerInfo.channel(0) && resp.valid)
  XSPerfAccumulate(cacheParams, "selfdir_A_hit", req.bits.replacerInfo.channel(0) && resp.valid && resp.bits.self.hit)
  XSPerfAccumulate(cacheParams, "selfdir_B_req", req.bits.replacerInfo.channel(1) && resp.valid)
  XSPerfAccumulate(cacheParams, "selfdir_B_hit", req.bits.replacerInfo.channel(1) && resp.valid && resp.bits.self.hit)
  XSPerfAccumulate(cacheParams, "selfdir_C_req", req.bits.replacerInfo.channel(2) && resp.valid)
  XSPerfAccumulate(cacheParams, "selfdir_C_hit", req.bits.replacerInfo.channel(2) && resp.valid && resp.bits.self.hit)

  XSPerfAccumulate(cacheParams, "selfdir_dirty", resp.valid && resp.bits.self.dirty)
  XSPerfAccumulate(cacheParams, "selfdir_TIP", resp.valid && resp.bits.self.state === TIP)
  XSPerfAccumulate(cacheParams, "selfdir_BRANCH", resp.valid && resp.bits.self.state === BRANCH)
  XSPerfAccumulate(cacheParams, "selfdir_TRUNK", resp.valid && resp.bits.self.state === TRUNK)
  XSPerfAccumulate(cacheParams, "selfdir_INVALID", resp.valid && resp.bits.self.state === INVALID)
}
