package huancun

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, BundleBridgeSource, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortParameters, IntSourcePortSimple}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup, RegWriteFn}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLRegisterNode}
import freechips.rocketchip.util.{SimpleRegIO, UIntToOH1}

class CtrlUnit(val node: TLAdapterNode)(implicit p: Parameters)
  extends LazyModule with HasHuanCunParameters
{

  val ctlnode = TLRegisterNode(
    address = Seq(AddressSet(cacheParams.ctrl.get.address, 0xffff)),
    device = new SimpleDevice("cache-controller", Nil),
    concurrency = 1,
    beatBytes = cacheParams.ctrl.get.beatBytes
  )
  val device = new SimpleDevice("L3CacheCtrl", Seq("xiangshan,cache_ctrl"))
  val intnode = IntSourceNode(IntSourcePortSimple(resources = device.int))
  val num_cores = cacheParams.ctrl.get.numCores
  val core_reset_nodes = (0 until num_cores) map(_ => BundleBridgeSource(() => Reset()))

  lazy val module = new CtrlUnitImp(this)
}

class CtrlUnitImp(wrapper: CtrlUnit) extends LazyModuleImp(wrapper) {

  val cacheParams = wrapper.p(HCCacheParamsKey)

  val io_req = IO(DecoupledIO(new CtrlReq()))
  val io_resp = IO(Flipped(DecoupledIO(new CtrlResp())))
  val io_ecc = IO(Flipped(DecoupledIO(new EccInfo())))

  val req = io_req
  val resp = io_resp
  val ecc = io_ecc

  val node = wrapper.node
  val ctlnode = wrapper.ctlnode

  val banksR = RegField.r(
    8, node.edges.in.size.U,
    RegFieldDesc("Banks", "Number of banks in the cache", reset=Some(node.edges.in.size))
  )
  val waysR = RegField.r(
    8, cacheParams.ways.U,
    RegFieldDesc("Ways", "Number of ways per bank", reset=Some(cacheParams.ways))
  )
  val lgSetsR = RegField.r(
    8, log2Ceil(cacheParams.sets).U,
    RegFieldDesc(
      "lgSets", "Base-2 logarithm of the sets per bank", reset=Some(log2Ceil(cacheParams.sets))
    )
  )
  val selfInfo = Seq(banksR, waysR, lgSetsR)

  val clientInfo = if(cacheParams.inclusive) Seq() else {
    val clientDirWays = RegField.r(
      8, cacheParams.clientCaches.head.ways.U,
      RegFieldDesc("ClientDirWays", "Number of client dir ways per bank",
        reset = Some(cacheParams.clientCaches.head.ways))
    )
    val clientDirLgSets = RegField.r(
      8, cacheParams.clientCaches.head.sets.U,
      RegFieldDesc("ClientDirLgSets", "Base-2 logarithm of the client dir sets per bank",
        reset = Some(cacheParams.clientCaches.head.ways))
    )
    Seq(clientDirWays, clientDirLgSets)
  }

  val ctl_tag = RegInit(0.U(64.W))
  val ctl_set = RegInit(0.U(64.W))
  val ctl_way = RegInit(0.U(64.W))
  val ctl_dir = RegInit(0.U(64.W))
  val ctl_data = Seq.fill(cacheParams.blockBytes / 8){ RegInit(0.U(64.W)) }
  val ctl_cmd = RegInit(0.U(64.W))

  val ecc_code = RegInit(0.U(64.W)) // assume non-zero as ECC error
  // for data error: ecc_addr = {set, way, beat}
  // for tag error: ecc_addr = physical address
  val ecc_addr = RegInit(0.U(64.W))

  val core_reset = Seq.fill(wrapper.num_cores){ RegInit(0.U(64.W)) }

  val reset_regs = core_reset.zipWithIndex.map{ case (r, i) =>
    RegField(64, r, RegWriteFn((valid, data) => {
      when(valid){ r := data(0) }
      true.B
    }), RegFieldDesc(s"CoreReset_$i", s"soft reset of core #[$i]"))
  }

  wrapper.core_reset_nodes.zip(core_reset).foreach{
    case (node, reg) => node.out.head._1 := reg(0)
  }

  val cmd_in_valid = RegInit(false.B)
  val cmd_in_ready = WireInit(false.B)
  val cmd_out_valid = RegInit(false.B)
  val cmd_out_ready = WireInit(false.B)

  when(cmd_out_ready){ cmd_out_valid := false.B }
  when(cmd_in_ready){ cmd_in_valid := false.B }

  val ctl_config_regs = (
    Seq(ctl_tag, ctl_set, ctl_way) ++
    ctl_data ++
    Seq(ctl_dir) ++
    Seq(ecc_code, ecc_addr)
  ).map(reg => RegField(64, reg, RegWriteFn(reg)))

  ctlnode.regmap(
    0x0000 -> RegFieldGroup(
      "Config", Some("Information about cache configuration"),
      selfInfo ++ clientInfo
    ),
    0x0100 -> RegFieldGroup(
      "Ctrl", None,
      ctl_config_regs
    ),
    0x0200 -> Seq(RegField.w(64, RegWriteFn((ivalid, oready, data) => {
      when(oready){ cmd_out_ready := true.B }
      when(ivalid){ cmd_in_valid := true.B }
      when(ivalid && !cmd_in_valid){
        ctl_cmd := data
      }
      (!cmd_in_valid, cmd_out_valid)
    }))),
    0x1000 -> RegFieldGroup(
      "CoreReset", desc = Some("core reset registers"),
      regs = reset_regs
    )
  )
  cmd_in_ready := req.ready
  when(resp.fire){
    cmd_out_valid := true.B
  }
  resp.ready := !cmd_out_valid
  ecc.ready := ecc_code === 0.U // Block multiple ecc req
  req.valid := cmd_in_valid
  req.bits.cmd := ctl_cmd
  req.bits.data.zip(ctl_data).foreach(x => x._1 := x._2)
  req.bits.tag := ctl_tag
  req.bits.set := ctl_set
  req.bits.way := ctl_way
  req.bits.dir := ctl_dir

  when(resp.fire) {
    switch(resp.bits.cmd){
      is(CacheCMD.CMD_R_S_TAG){
        ctl_tag := resp.bits.data(0)
      }
      is(CacheCMD.CMD_R_C_TAG){
        ctl_tag := resp.bits.data(0)
      }
      is(CacheCMD.CMD_R_DATA){
        ctl_data.zip(resp.bits.data).foreach(x => x._1 := x._2)
      }
      is(CacheCMD.CMD_R_S_DIR){
        ctl_dir := resp.bits.data(0)
      }
      is(CacheCMD.CMD_R_C_DIR){
        ctl_dir := resp.bits.data(0)
      }
    }
  }

  when(ecc.fire) {
    ecc_code := ecc.bits.errCode
    ecc_addr := ecc.bits.addr
  }
  wrapper.intnode.out.head._1.head := ecc_code =/= 0.U
}

class CtrlReq() extends Bundle {
  val cmd = UInt(8.W)
  val data = Vec(8, UInt(64.W))
  val set = UInt(64.W)
  val tag = UInt(64.W)
  val way = UInt(64.W)
  val dir = UInt(64.W)
}

class CtrlResp() extends Bundle {
  val cmd = UInt(8.W)
  val data = Vec(8, UInt(64.W))
}

class EccInfo() extends Bundle {
  val errCode = UInt(8.W)
  val addr = UInt(64.W)
}
object EccInfo {
  def ERR_NO = 0.U(8.W)
  def ERR_SELF_DIR = 1.U(8.W)
  def ERR_CLIENT_DIR = 2.U(8.W)
  def ERR_DATA = 3.U(8.W)
}

object CacheCMD {
  def CMD_R_S_TAG = 0.U(8.W)
  def CMD_R_C_TAG = 1.U(8.W)
  def CMD_R_DATA = 2.U(8.W)
  def CMD_R_S_DIR = 3.U(8.W)
  def CMD_R_C_DIR = 4.U(8.W)
  def CMD_W_S_TAG = 5.U(8.W)
  def CMD_W_C_TAG = 6.U(8.W)
  def CMD_W_DATA = 7.U(8.W)
  def CMD_W_S_DIR = 8.U(8.W)
  def CMD_W_C_DIR = 9.U(8.W)
  def CMD_CMO_INV = (0 + 16).U(8.W)
  def CMD_CMO_CLEAN = (1 + 16).U(8.W)
  def CMD_CMO_FLUSH = (2 + 16).U(8.W)
}
