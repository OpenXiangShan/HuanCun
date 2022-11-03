package huancun.debug

// import scala.util.control._
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
  println("hello MSHR, now let's start our test!")

  when(io.alloc.valid) {
    print_req_info()
  }

  when(io.dirResult.valid) {
    print_dir_result()
  }

  when(meta_valid) {
    print_sw_flags()
    print_new_self_dir()
  }

  when(io.tasks.source_a.valid) {
    print_sourceA_tasks()
  }

  when(io.tasks.source_b.valid) {
    print_sourceB_tasks()
  }

  when(io.tasks.source_c.valid) {
    print_sourceC_tasks()
  }

  when(io.tasks.source_d.valid) {
    print_sourceD_tasks()
  }





  def print_req_info() = {
    val req = io.alloc.bits
    printf("================ REQ INFO ================\n")
    printf(p"channel          = ${req.channel}\n")
    printf(p"opcode           = ${req.opcode}\n")
    printf(p"param            = ${req.param}\n")
    printf(p"size             = ${req.size}\n")
    printf(p"source           = ${req.source}\n")
    printf(p"set              = ${req.set}\n")
    printf(p"tag              = ${req.tag}\n")
    printf(p"off              = ${req.off}\n")
    printf("mask = %b\n", req.mask)
    printf(p"bufIdx           = ${req.bufIdx}\n")
    printf(p"needHint         = ${req.needHint}\n")
    printf(p"isPrefetch       = ${req.isPrefetch}\n")
    printf(p"alias            = ${req.alias}\n")
    printf(p"preferCache      = ${req.preferCache}\n")
    printf(p"dirty            = ${req.dirty}\n")
    printf(p"fromProbeHelper  = ${req.fromProbeHelper}\n")
    printf(p"fromCmoHelper    = ${req.fromCmoHelper}\n")
    printf(p"needProbeAckData = ${req.needProbeAckData}\n\n")
  }

  def print_dir_result() = {
    val meta = io.dirResult.bits
    printf("================ DIR RESULT ================\n")
    printf(p"sourceId             = ${meta.sourceId}\n")
    printf(p"set                  = ${meta.set}\n")
    printf(p"replacerInfo_channel = ${meta.replacerInfo.channel}\n")
    printf(p"replacerInfo_opcode  = ${meta.replacerInfo.opcode}\n")

    printf(p"self_dir_hit         = ${meta.self.hit}\n")
    printf(p"self_dir_way         = ${meta.self.way}\n")
    printf(p"self_dir_tag         = ${meta.self.tag}\n")
    printf(p"self_dir_dirty       = ${meta.self.dirty}\n")
    printf(p"self_dir_state       = ${meta.self.state}\n")
    printf(p"self_dir_error       = ${meta.self.error}\n")
    meta.self.clientStates.zipWithIndex.foreach {
      case(s, i) => printf(p"self_clientstates_$i = $s\n")
    }
    meta.self.prefetch.map(printf("self_prefetch = %b\n",_))

    printf(p"clients_tag          = ${meta.clients.tag}\n")
    printf(p"clients_way          = ${meta.clients.way}\n")
    printf(p"clients_error        = ${meta.clients.error}\n")
    printf(p"clients_tag_match    = ${meta.clients.tag_match}\n")
    meta.clients.states.zipWithIndex.foreach {
      case (s, i) => printf(p"clientstates_state_$i = ${s.state}, alias = ${s.alias}, hit = ${s.hit}\n")
    }
    printf("\n")
  }


  def print_sw_flags() = {
    printf("================ SW FLAGS ================\n")
    // print all s_ and w_ registers
    printf(p"s_acquire          = $s_acquire\n")
    printf(p"s_probe            = $s_probe\n")
    printf(p"s_release          = $s_release\n")
    printf(p"s_probeack         = $s_probeack\n\n")
    printf(p"s_execute          = $s_execute\n")
    printf(p"s_grantack         = $s_grantack\n")
    printf(p"s_wbselfdir        = $s_wbselfdir\n")
    printf(p"s_wbselftag        = $s_wbselftag\n\n")
    printf(p"s_wbclientsdir     = $s_wbclientsdir\n")
    printf(p"s_wbclientstag     = $s_wbclientstag\n")
    printf(p"s_transferput      = $s_transferput\n")
    printf(p"s_writerelease     = $s_writerelease\n\n")
    printf(p"s_writeprobe       = $s_writeprobe\n")
    s_triggerprefetch.map(printf("s_triggerprefetch   = %b\n",_))
    s_prefetchack.map(printf("s_prefetchack     = %b\n",_))
    printf(p"w_probeackfirst    = $w_probeackfirst\n\n")
    printf(p"w_probeacklast     = $w_probeacklast\n")
    printf(p"w_probeack         = $w_probeack\n")
    printf(p"w_grantfirst       = $w_grantfirst\n")
    printf(p"w_grantlast        = $w_grantlast\n\n")
    printf(p"w_grant            = $w_grant\n")
    printf(p"w_releaseack       = $w_releaseack\n")
    printf(p"w_grantack         = $w_grantack\n")
    printf(p"w_putwritten       = $w_putwritten\n\n")
  }

  def print_sourceA_tasks() = {
    val oa = io.tasks.source_a.bits
    printf("================ SourceA TASKS ================\n")
    when(io.tasks.source_a.valid) {
      printf("#### SourceA valid ####\n")
      printf(p"source_a_source    = ${oa.source}\n")
      printf(p"source_a_tag       = ${oa.tag}\n")
      printf(p"source_a_set       = ${oa.set}\n")
      printf(p"source_a_off       = ${oa.off}\n")
      printf(p"source_a_mask      = ${oa.mask}\n")
      printf(p"source_a_opcode    = ${oa.opcode}\n")
      printf(p"source_a_param     = ${oa.param}\n")
      printf(p"source_a_bufIdx    = ${oa.bufIdx}\n")
      printf(p"source_a_size      = ${oa.size}\n")
      printf(p"source_a_needData  = ${oa.needData}\n")
      printf(p"source_a_putData   = ${oa.putData}\n\n")
    }
  }

  def print_sourceB_tasks() = {
    val ob = io.tasks.source_b.bits
    printf("================ SourceB TASKS ================\n")
    when(io.tasks.source_b.valid) {
      printf("#### SourceB valid ####\n")
      printf(p"source_b_tag       = ${ob.tag}\n")
      printf(p"source_b_set       = ${ob.set}\n")
      printf(p"source_b_param     = ${ob.param}\n")
      printf(p"source_b_clients   = ${ob.clients}\n")
      printf(p"source_b_needData  = ${ob.needData}\n\n")
    }
  }

  def print_sourceC_tasks() = {
    val oc = io.tasks.source_c.bits
    printf("================ SourceC TASKS ================\n")
    when(io.tasks.source_c.valid) {
      printf("#### SourceC valid ####\n")
      printf(p"souce_c_source     = ${oc.source}\n")
      printf(p"source_c_tag       = ${oc.tag}\n")
      printf(p"source_c_set       = ${oc.set}\n")
      printf(p"source_c_way       = ${oc.way}\n")
      printf(p"source_c_opcode    = ${oc.opcode}\n")
      printf(p"source_c_param     = ${oc.param}\n")
      printf(p"source_c_dirty     = ${oc.dirty}\n\n")
    }
  }

  def print_sourceD_tasks() = {
    val od = io.tasks.source_d.bits
    printf("================ SourceD TASKS ================\n")
    when(io.tasks.source_d.valid) {
      printf("#### SourceD valid ####\n")
      printf(p"source_d_size      = ${od.size}\n")
      printf(p"source_d_way       = ${od.way}\n")
      printf(p"source_d_off       = ${od.off}\n")
      printf(p"source_d_opcode    = ${od.opcode}\n")
      printf(p"source_d_param     = ${od.param}\n")
      printf(p"source_d_useBypass = ${od.useBypass}\n")
      printf(p"source_d_bufIdx    = ${od.bufIdx}\n")
      printf(p"source_d_denied    = ${od.denied}\n")
      printf(p"source_d_sinkId    = ${od.sinkId}\n")
      printf(p"source_d_bypassPut = ${od.bypassPut}\n")
      printf(p"source_d_dirty     = ${od.dirty}\n\n")
    }
  }

  def print_sourceE_tasks() = {
    val oe = io.tasks.source_e.bits
    printf("================ SourceE TASKS ================\n")
    when(io.tasks.source_e.valid) {
      printf("#### SourceE valid ####\n")
      printf(p"source_e_sink      = ${oe.sink}\n\n")
    }
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
    printf(p"a_do_probe = $a_do_probe\n\n")
  }

  def print_some_signals = {
    printf(p"transmit_from_other_client = $transmit_from_other_client\n")
    
    printf(p"releaseThrough = $releaseThrough\n")
    printf(p"releaseDrop = $releaseDrop\n")
    printf(p"releaseSave = $releaseSave\n")

    printf(p"probeAckDataThrough = $probeAckDataThrough\n")
    printf(p"probeAckDataDrop = $probeAckDataDrop\n")
    printf(p"probeAckDataSave = $probeAckDataSave\n\n")
  }

  def print_new_self_dir() = {
    val meta = new_self_meta
    printf("================ NEW META ================\n")
    printf(p"new_self_dirty    = ${meta.dirty}\n")
    printf(p"new_self_state    = ${meta.state}\n")
    meta.clientStates.zipWithIndex.foreach { case (m, i) =>
      printf(p"new_self_clientStates_$i   = $m\n")
    }
    new_clients_meta.zipWithIndex.foreach { case (m, i) =>
      printf(p"new_clients_meta_$i    = $m\n\n")
    }
  }
  def print_new_self_tag() = {
    printf("================ NEW SELF TAG ================\n")

  }

}