package huancun.debug

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.TLHints._
import huancun._
import huancun.utils._
import huancun.MetaData._
import huancun.noninclusive.MSHR

class DebugMSHR()(implicit p: Parameters) extends noninclusive.MSHR {
  val cntCycle = RegInit(0.U(32.W))
  cntCycle := cntCycle + 1.U
  when (cntCycle === 8.U) {
    print_sw_flags()
    print_tasks()
    print_new_meta()
  }

  def print_sw_flags() = {
    printf("=== SW FLAGS ===\n")
    // print all s_ and w_ registers
    printf(p"s_acquire = $s_acquire\n")
    printf(p"s_probe = $s_probe\n")
    printf(p"s_release = $s_release\n")
    printf(p"s_probeack = $s_probeack\n")
    printf(p"s_execute = $s_execute\n")
    printf(p"s_grantack = $s_grantack\n")
    printf(p"s_wbselfdir = $s_wbselfdir\n")
    printf(p"s_wbselftag = $s_wbselftag\n")
    printf(p"s_wbclientsdir = $s_wbclientsdir\n")
    printf(p"s_wbclientstag = $s_wbclientstag\n")
    printf(p"s_transferput = $s_transferput\n")
    printf(p"s_writerelease = $s_writerelease\n")
    printf(p"s_writeprobe = $s_writeprobe\n")
    s_triggerprefetch.map(printf("s_triggerprefetch = %b\n",_))
    s_prefetchack.map(printf("s_prefetchack = %b\n",_))
    printf(p"w_probeackfirst = $w_probeackfirst\n")
    printf(p"w_probeacklast = $w_probeacklast\n")
    printf(p"w_probeack = $w_probeack\n")
    printf(p"w_grantfirst = $w_grantfirst\n")
    printf(p"w_grantlast = $w_grantlast\n")
    printf(p"w_grant = $w_grant\n")
    printf(p"w_releaseack = $w_releaseack\n")
    printf(p"w_grantack = $w_grantack\n")
    printf(p"w_putwritten = $w_putwritten\n")
  }

  def print_other_flags = {
    printf(p"promoteT_safe = $promoteT_safe\n")
    printf(p"gotT = $gotT\n")
    printf(p"probe_dirty = $probe_dirty\n")
    printf(p"probes_done = $probes_done\n")
    printf(p"bad_grant = $bad_grant\n")
    printf(p"need_block_downwards = $need_block_downwards\n")
    printf(p"inv_self_dir = $inv_self_dir\n")
    printf(p"nested_c_hit_reg = $nested_c_hit_reg\n")
    printf(p"gotDirty = $gotDirty\n")
    printf(p"acquire_flag = $acquire_flag\n")
    printf(p"a_do_release = $a_do_release\n")
    printf(p"a_do_probe = $a_do_probe\n")
  }

  def print_some_signals = {
    printf(p"transmit_from_other_client = $transmit_from_other_client\n")
    
    printf(p"releaseThrough = $releaseThrough\n")
    printf(p"releaseDrop = $releaseDrop\n")
    printf(p"releaseSave = $releaseSave\n")

    printf(p"probeAckDataThrough = $probeAckDataThrough\n")
    printf(p"probeAckDataDrop = $probeAckDataDrop\n")
    printf(p"probeAckDataSave = $probeAckDataSave\n")
  }

  def print_tasks() = {
    printf("=== ALL TASKS ===\n")

  }

  def print_new_meta() = {
    printf("=== NEW META ===\n")


  }

}