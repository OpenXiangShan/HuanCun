package huancun

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup, RegWriteFn}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLRegisterNode}

class CtrlUnit(val node: TLAdapterNode)(implicit p: Parameters)
  extends LazyModule with HasHuanCunParameters
{

  val ctlnode = cacheParams.ctrl.map{c =>
    TLRegisterNode(
      address = Seq(AddressSet(c.address, 0xfff)),
      device = new SimpleDevice("cache-controller", Nil),
      concurrency = 1,
      beatBytes = c.beatBytes
    )
  }

  lazy val module = new CtrlUnitImp(this)
}

class CtrlUnitImp(wrapper: CtrlUnit) extends LazyModuleImp(wrapper) with HasHuanCunParameters {

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
    8, setBits.U,
    RegFieldDesc(
      "lgSets", "Base-2 logarithm of the sets per bank", reset=Some(setBits)
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
  val ctl_data = Seq.fill(cacheParams.blockBytes / 8){ RegInit(0.U(64.W)) }
  val ctl_waymask = RegInit(0.U(64.W))
  val ctl_ecc = RegInit(0.U(64.W))
  val ctl_bank = RegInit(0.U(64.W))
  val ctl_cmd = RegInit(0.U(64.W))

  val cmd_in_valid = RegInit(false.B)
  val cmd_in_ready = WireInit(false.B)
  val cmd_out_valid = RegInit(false.B)
  val cmd_out_ready = WireInit(false.B)

  when(cmd_out_ready){ cmd_out_valid := false.B }
  when(cmd_in_ready){ cmd_in_valid := false.B }

  val ctl_config_regs = (
    Seq(ctl_tag, ctl_set, ctl_way) ++
    ctl_data ++ Seq(ctl_waymask, ctl_ecc, ctl_bank)
    ).map(reg => RegField(64, reg, RegWriteFn(reg)))

  ctlnode.map{ c =>
    c.regmap(
      0x000 -> RegFieldGroup(
        "Config", Some("Information about cache configuration"),
        selfInfo ++ clientInfo
      ),
      0x100 -> RegFieldGroup(
        "Ctrl", None,
        ctl_config_regs
      ),
      0x200 -> Seq(RegField.w(64, RegWriteFn((ivalid, oready, data) => {
        when(oready){ cmd_out_ready := true.B }
        when(ivalid){ cmd_in_valid := true.B }
        when(ivalid && !cmd_in_valid){
          ctl_cmd := data
        }
        (!cmd_in_valid, cmd_out_valid)
      })))
    )
  }
}

object CacheCMD {
  def CMD_R_S_TAG = 0.U(8.W)
  def CMD_R_C_TAG = 1.U(8.W)
  def CMD_R_DATA = 2.U(8.W)
  def CMD_W_S_TAG = 3.U(8.W)
  def CMD_W_C_TAG = 4.U(8.W)
  def CMD_W_DATA = 5.U(8.W)
}
