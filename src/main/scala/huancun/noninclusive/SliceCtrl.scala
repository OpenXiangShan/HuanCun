package huancun.noninclusive

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import huancun._
import utility.{MemReqSource, RegNextN}

class SliceCtrl()(implicit p: Parameters) extends HuanCunModule {

  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new CtrlReq))
    val resp = DecoupledIO(new CtrlResp)
    val s_dir_w = DecoupledIO(new SelfDirWrite())
    val c_dir_w = DecoupledIO(new ClientDirWrite())
    val s_tag_w = DecoupledIO(new SelfTagWrite())
    val c_tag_w = DecoupledIO(new ClientTagWrite())
    val bs_w_addr = DecoupledIO(new DSAddress())
    val bs_w_data = Output(new DSData())
    val dir_read = DecoupledIO(new DirRead())
    val dir_result = Flipped(ValidIO(new DirResult()))
    val bs_r_addr = DecoupledIO(new DSAddress())
    val bs_r_data = Input(new DSData())
    val cmo_req = DecoupledIO(new MSHRRequest())
  })

  val req_reg = Reg(new CtrlReq)
  val busy = RegInit(false.B)
  val done = RegInit(false.B)

  val full_address = Cat(
    io.req.bits.tag(fullTagBits - 1, 0),
    io.req.bits.set(setBits - 1, 0),
    0.U(offsetBits.W)
  )
  val (tag, set, _) = parseAddress(full_address)
  when(io.req.fire){
    req_reg := io.req.bits
    req_reg.set := set
    req_reg.tag := tag
  }

  val data_beats = Wire(Vec(beatSize, UInt((8 * beatBytes).W)))
  data_beats := req_reg.data.asTypeOf(data_beats)

  when(io.req.fire){
    busy := true.B
  }
  when(io.resp.fire){
    busy := false.B
    done := false.B
  }

  val s_wb_self_dir = RegInit(false.B)
  val s_wb_client_dir = RegInit(false.B)
  val s_wb_self_tag = RegInit(false.B)
  val s_wb_client_tag = RegInit(false.B)
  val s_data_write = RegInit(beatSize.U)
  val s_data_read = RegInit(beatSize.U)
  val s_dir_read = RegInit(false.B)
  val s_cmo = RegInit(false.B)

  when(io.req.fire){
    switch(io.req.bits.cmd){
      is(CacheCMD.CMD_R_S_DIR){
        s_dir_read := true.B
      }
      is(CacheCMD.CMD_R_C_DIR){
        s_dir_read := true.B
      }
      is(CacheCMD.CMD_R_S_TAG){
        s_dir_read := true.B
      }
      is(CacheCMD.CMD_R_C_TAG){
        s_dir_read := true.B
      }
      is(CacheCMD.CMD_R_DATA){
        s_data_read := 0.U
      }
      is(CacheCMD.CMD_W_S_DIR){
        s_wb_self_dir := true.B
      }
      is(CacheCMD.CMD_W_C_DIR){
        s_wb_client_dir := true.B
      }
      is(CacheCMD.CMD_W_S_TAG){
        s_wb_self_tag := true.B
      }
      is(CacheCMD.CMD_W_C_TAG){
        s_wb_client_tag := true.B
      }
      is(CacheCMD.CMD_W_DATA){
        s_data_write := 0.U
      }
      is(CacheCMD.CMD_CMO_INV){
        s_cmo := true.B
      }
      is(CacheCMD.CMD_CMO_CLEAN){
        s_cmo := true.B
      }
      is(CacheCMD.CMD_CMO_FLUSH){
        s_cmo := true.B
      }
    }
  }

  io.dir_read.valid := s_dir_read
  io.dir_read.bits.idOH := Cat(req_reg.way, "b11".U)
  io.dir_read.bits.tag := req_reg.tag
  io.dir_read.bits.set := req_reg.set
  io.dir_read.bits.way := req_reg.way
  io.dir_read.bits.wayMode := false.B // TODO: it seems incorrect
  io.dir_read.bits.replacerInfo := DontCare
  io.dir_read.bits.source := DontCare

  when(io.dir_result.fire){
    switch(req_reg.cmd){
      is(CacheCMD.CMD_R_S_DIR){
        req_reg.data(0) := io.dir_result.bits.self.asTypeOf(new SelfDirEntry()).asUInt
      }
      is(CacheCMD.CMD_R_C_DIR){
        req_reg.data(0) := io.dir_result.bits.clients.states.asUInt
      }
      is(CacheCMD.CMD_R_S_TAG){
        req_reg.data(0) := io.dir_result.bits.self.tag
      }
      is(CacheCMD.CMD_R_C_TAG){
        req_reg.data(0) := io.dir_result.bits.clients.tag
      }
    }
    s_dir_read := false.B
    done := true.B
  }


  io.s_dir_w.valid := s_wb_self_dir
  io.c_dir_w.valid := s_wb_client_dir
  io.s_tag_w.valid := s_wb_self_tag
  io.c_tag_w.valid := s_wb_client_tag

  io.s_dir_w.bits.set := req_reg.set
  io.s_dir_w.bits.way := req_reg.way
  io.s_dir_w.bits.data := req_reg.dir.asTypeOf(io.s_dir_w.bits.data)

  io.c_dir_w.bits.set := req_reg.set
  io.c_dir_w.bits.way := req_reg.way
  io.c_dir_w.bits.data := req_reg.dir.asTypeOf(io.c_dir_w.bits.data)

  io.s_tag_w.bits.set := req_reg.set
  io.s_tag_w.bits.way := req_reg.way
  io.s_tag_w.bits.tag := req_reg.tag

  io.c_tag_w.bits.set := req_reg.set
  io.c_tag_w.bits.way := req_reg.way
  io.c_tag_w.bits.tag := req_reg.tag

  io.bs_r_addr.valid := s_data_read =/= beatSize.U
  io.bs_r_addr.bits.way := req_reg.way
  io.bs_r_addr.bits.set := req_reg.set
  io.bs_r_addr.bits.beat := s_data_read
  io.bs_r_addr.bits.noop := false.B
  io.bs_r_addr.bits.write := false.B

  when(io.bs_r_addr.fire){
    s_data_read := s_data_read + 1.U
  }
  val data_wen = RegNextN(io.bs_r_addr.fire, n = sramLatency, initOpt = Some(false.B))
  val w_counter = RegInit(0.U(log2Ceil(beatSize).W))
  when(data_wen){
    w_counter := w_counter + 1.U
    for(i <- req_reg.data.indices){
      val beatId = i / (beatBytes / 8)
      val wdata = io.bs_r_data.data.asTypeOf(Vec(beatBytes / 8, UInt(64.W)))
      when(beatId.U === w_counter){
        req_reg.data(i) := wdata(i - beatId * beatBytes / 8)
      }
    }
    when(w_counter === (beatSize - 1).U){
      done := true.B
      w_counter := 0.U
    }
  }

  io.bs_w_addr.valid := s_data_write =/= beatSize.U
  io.bs_w_addr.bits.way := req_reg.way
  io.bs_w_addr.bits.set := req_reg.set
  io.bs_w_addr.bits.beat := s_data_write
  io.bs_w_addr.bits.write := true.B
  io.bs_w_addr.bits.noop := false.B
  io.bs_w_data.data := data_beats(s_data_write)
  io.bs_w_data.corrupt := false.B
  when(io.bs_w_addr.fire){
    s_data_write := s_data_write + 1.U
  }

  when(io.s_dir_w.fire){
    s_wb_self_dir := false.B
    done := true.B
  }
  when(io.c_dir_w.fire){
    s_wb_client_dir := false.B
    done := true.B
  }
  when(io.s_tag_w.fire){
    s_wb_self_tag := false.B
    done := true.B
  }
  when(io.c_tag_w.fire){
    s_wb_client_tag := false.B
    done := true.B
  }

  io.cmo_req.bits.channel := 4.U
  io.cmo_req.bits.opcode := 0.U  // DontCare
  io.cmo_req.bits.param := io.req.bits.cmd(1, 0)
  io.cmo_req.bits.size := log2Up(blockBytes).U
  io.cmo_req.bits.source := 0.U  // DontCare
  io.cmo_req.bits.set := io.req.bits.set
  io.cmo_req.bits.tag := io.req.bits.tag
  io.cmo_req.bits.off := 0.U  // DontCare
  io.cmo_req.bits.mask := 0.U  // DontCare
  io.cmo_req.bits.bufIdx := 0.U  // DontCare
  io.cmo_req.bits.needHint.foreach(_ := false.B)
  io.cmo_req.bits.isPrefetch.foreach(_ := false.B)
  io.cmo_req.bits.alias.foreach(_ := false.B)
  io.cmo_req.bits.preferCache := false.B
  io.cmo_req.bits.dirty := false.B
  io.cmo_req.bits.isHit := true.B
  io.cmo_req.bits.fromProbeHelper := false.B
  io.cmo_req.bits.fromCmoHelper := true.B
  io.cmo_req.bits.needProbeAckData.foreach(_ := false.B)
  io.cmo_req.bits.reqSource := MemReqSource.NoWhere.id.U
  io.cmo_req.bits.isBop.foreach(_ := false.B)

  io.cmo_req.valid := s_cmo
  when(io.cmo_req.fire){
    s_cmo := false.B
    done := true.B
  }

  io.req.ready := !busy
  io.resp.valid := done
  io.resp.bits.cmd := req_reg.cmd
  io.resp.bits.data := req_reg.data
}
