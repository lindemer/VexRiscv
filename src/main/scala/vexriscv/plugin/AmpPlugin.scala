/*
 * Copyright (c) 2021 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

package vexriscv.plugin

import vexriscv.{VexRiscv, _}
import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

case class AmpRegister(previous : AmpRegister, privilegeService : PrivilegeService)
  extends Area {

  def OFF = 0
  def TOR = 1
  def NA4 = 2
  def NAPOT = 3

  val state = new Area {
    val r, w, x = Reg(Bool)
    val l, d, m = RegInit(False)
    val a = Reg(UInt(2 bits)) init(0)
    val addr = Reg(UInt(32 bits))
  }

  // CSR writes connect to these signals rather than the internal state
  // registers. This makes locking and WARL possible.
  val csr = new Area {
    val r, w, x = Bool
    val l, d, m = Bool
    val a = UInt(2 bits)
    val addr = UInt(32 bits)
  }

  // Last valid assignment wins; nothing happens if a user-initiated write did 
  // not occur on this clock cycle.
  val machineMode = privilegeService.isMachine()
  when(machineMode | state.d) {
    csr.r    := state.r
    csr.w    := state.w
    csr.x    := state.x
    csr.l    := state.l
    csr.d    := state.d
    csr.m    := state.m
    csr.a    := state.a
    csr.addr := state.addr
  } otherwise {

    // TODO: Verify that this does not trigger a CSR write on the next cycle.
    csr.r    := False
    csr.w    := False
    csr.x    := False
    csr.l    := False
    csr.d    := False
    csr.m    := False
    csr.a    := U"2'0"
    csr.addr := U"32'0"
  }

  // Computed AMP region bounds
  val region = new Area {
    val valid, locked, delegated, modified = Bool
    val start, end = UInt(32 bits)
  }

  when(~state.l & (machineMode | state.d)) {
    state.r    := csr.r
    state.w    := csr.w
    state.x    := csr.x
    state.m    := csr.m
    state.a    := csr.a
    state.addr := csr.addr

    when(machineMode) {
      state.d := csr.d
      state.l := csr.l

      // TODO: Verify that this doesn't break the truth table.
      if (csr.l == True & csr.a == TOR) {
        previous.state.l := True
      }
    }
  }

  val shifted = state.addr |<< 2
  val mask = state.addr & ~(state.addr + 1)
  val masked = (state.addr & ~mask) |<< 2

  // AMP changes take effect two clock cycles after the initial CSR write (i.e.,
  // settings propagate from csr -> state -> region).
  region.locked := state.l
  region.delegated := state.d
  region.modified := state.m
  region.valid := True

  switch(csr.a) {
    is(TOR) {
      if (previous == null) region.start := 0
      else region.start := previous.region.end
      region.end := shifted
    }
    is(NA4) {
      region.start := shifted
      region.end := shifted + 4
    }
    is(NAPOT) {
      region.start := masked
      region.end := masked + ((mask + 1) |<< 3)
    }
    default {
      region.start := 0
      region.end := shifted
      region.valid := False
    }
  }
}

class AmpPlugin(regions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator {

  // Each pmpcfg# CSR configures four regions.
  assert((regions % 4) == 0)
   
  val amps = ArrayBuffer[AmpRegister]()
  val portsInfo = ArrayBuffer[ProtectedMemoryTranslatorPort]()

  override def newTranslationPort(priority : Int, args : Any): MemoryTranslatorBus = {
    val port = ProtectedMemoryTranslatorPort(MemoryTranslatorBus())
    portsInfo += port
    port.bus
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline.config._
    import pipeline._
    import Riscv._

    val csrService = pipeline.service(classOf[CsrInterface])
    val privilegeService = pipeline.service(classOf[PrivilegeService])

    val core = pipeline plug new Area {

      // Instantiate pmpaddr0 ... pmpaddr# CSRs.
      for (i <- 0 until regions) {
        if (i == 0) {
          amps += AmpRegister(null, privilegeService)
        } else {
          amps += AmpRegister(amps.last, privilegeService)
        }
        csrService.r(0x910 + i, amps(i).state.addr)
        csrService.w(0x910 + i, amps(i).csr.addr)
      }

      // Instantiate pmpcfg0 ... pmpcfg# CSRs.
      for (i <- 0 until (regions / 4)) {

        // TODO: Do this in a functional-programmy way.
        csrService.r(0x900 + i,
          31 -> amps((i * 4) + 3).state.l,
          30 -> amps((i * 4) + 3).state.d, 
          29 -> amps((i * 4) + 3).state.m, 
          27 -> amps((i * 4) + 3).state.a, 
          26 -> amps((i * 4) + 3).state.x,
          25 -> amps((i * 4) + 3).state.w, 
          24 -> amps((i * 4) + 3).state.r,
          23 -> amps((i * 4) + 2).state.l,
          22 -> amps((i * 4) + 2).state.d,
          21 -> amps((i * 4) + 2).state.m,
          19 -> amps((i * 4) + 2).state.a, 
          18 -> amps((i * 4) + 2).state.x,
          17 -> amps((i * 4) + 2).state.w, 
          16 -> amps((i * 4) + 2).state.r,
          15 -> amps((i * 4) + 1).state.l,  
          14 -> amps((i * 4) + 1).state.d,  
          13 -> amps((i * 4) + 1).state.m,  
          11 -> amps((i * 4) + 1).state.a, 
          10 -> amps((i * 4) + 1).state.x,
           9 -> amps((i * 4) + 1).state.w, 
           8 -> amps((i * 4) + 1).state.r,
           7 -> amps((i * 4)    ).state.l,
           6 -> amps((i * 4)    ).state.d,
           5 -> amps((i * 4)    ).state.m,
           3 -> amps((i * 4)    ).state.a, 
           2 -> amps((i * 4)    ).state.x,
           1 -> amps((i * 4)    ).state.w, 
           0 -> amps((i * 4)    ).state.r
        )
        csrService.w(0x900 + i,
          31 -> amps((i * 4) + 3).state.l,
          30 -> amps((i * 4) + 3).state.d, 
          29 -> amps((i * 4) + 3).state.m, 
          27 -> amps((i * 4) + 3).state.a, 
          26 -> amps((i * 4) + 3).state.x,
          25 -> amps((i * 4) + 3).state.w, 
          24 -> amps((i * 4) + 3).state.r,
          23 -> amps((i * 4) + 2).state.l,
          22 -> amps((i * 4) + 2).state.d,
          21 -> amps((i * 4) + 2).state.m,
          19 -> amps((i * 4) + 2).state.a, 
          18 -> amps((i * 4) + 2).state.x,
          17 -> amps((i * 4) + 2).state.w, 
          16 -> amps((i * 4) + 2).state.r,
          15 -> amps((i * 4) + 1).state.l,  
          14 -> amps((i * 4) + 1).state.d,  
          13 -> amps((i * 4) + 1).state.m,  
          11 -> amps((i * 4) + 1).state.a, 
          10 -> amps((i * 4) + 1).state.x,
           9 -> amps((i * 4) + 1).state.w, 
           8 -> amps((i * 4) + 1).state.r,
           7 -> amps((i * 4)    ).state.l,
           6 -> amps((i * 4)    ).state.d,
           5 -> amps((i * 4)    ).state.m,
           3 -> amps((i * 4)    ).state.a, 
           2 -> amps((i * 4)    ).state.x,
           1 -> amps((i * 4)    ).state.w, 
           0 -> amps((i * 4)    ).state.r
        )
      }

      // Connect memory ports to AMP logic.
      val ports = for ((port, portId) <- portsInfo.zipWithIndex) yield new Area {

        val address = port.bus.cmd.virtualAddress
        port.bus.rsp.physicalAddress := address

        val machineMode = privilegeService.isMachine()
        val machineMatch = amps.map(amp => amp.region.valid &
                                           amp.region.start <= address &
                                           amp.region.end > address &
                                          (amp.region.locked | ~machineMode))

        // TODO: _.state changes 1 cycle before _.region. This could lead to
        // inconsistencies.
        val machineRead = MuxOH(OHMasking.first(machineMatch), amps.map(_.state.r))
        val machineWrite = MuxOH(OHMasking.first(machineMatch), amps.map(_.state.w))
        val machineExecute = MuxOH(OHMasking.first(machineMatch), amps.map(_.state.x))
        val machineModified = MuxOH(OHMasking.first(machineMatch), amps.map(_.state.m))

        // M-mode has full access by default, others have none.
        when(CountOne(machineMatch) === 0) {

          port.bus.rsp.allowRead := machineMode
          port.bus.rsp.allowWrite := machineMode
          port.bus.rsp.allowExecute := machineMode
          port.bus.rsp.isPaging := False

        } otherwise {

          val userMode = privilegeService.isUser()
          val supervisorMatch = amps.map(amp => amp.region.valid &
                                                amp.region.start <= address &
                                                amp.region.end > address &
                                                ~machineMode)

          val supervisorRead = MuxOH(OHMasking.first(supervisorMatch), amps.map(_.csr.r))
          val supervisorWrite = MuxOH(OHMasking.first(supervisorMatch), amps.map(_.csr.w))
          val supervisorExecute = MuxOH(OHMasking.first(supervisorMatch), amps.map(_.csr.x))
          val supervisorModified = MuxOH(OHMasking.first(supervisorMatch), amps.map(_.csr.m))

          when(CountOne(supervisorMatch) === 0) {

            port.bus.rsp.allowRead := machineRead
            port.bus.rsp.allowWrite := machineWrite
            port.bus.rsp.allowExecute := machineExecute
            port.bus.rsp.isPaging := False

          } otherwise {
            
            port.bus.rsp.allowRead := machineRead & supervisorRead & (userMode ^ supervisorModified)
            port.bus.rsp.allowWrite := machineWrite & supervisorWrite & (userMode ^ supervisorModified)
            port.bus.rsp.allowExecute := machineExecute & supervisorExecute & (userMode ^ supervisorModified)
            port.bus.rsp.isPaging := True
          
          }
        }

        port.bus.rsp.isIoAccess := ioRange(port.bus.rsp.physicalAddress)
        port.bus.rsp.exception := False
        port.bus.rsp.refilling := False
        port.bus.busy := False

      }
    }
  }
}

