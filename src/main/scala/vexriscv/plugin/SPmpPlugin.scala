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

case class SPmp(previous : SPmp) extends Area {

  def OFF = 0
  def TOR = 1
  def NA4 = 2
  def NAPOT = 3

  val csr = new Area {
    val r, w, x = Reg(Bool)
    val l = Reg(Bool)
    val a = Reg(UInt(2 bits)) init(0)
    val addr = Reg(UInt(32 bits))
  }

  val region = new Area {
    val valid, locked = Bool
    val start, end = UInt(32 bits)
  }
  
  val shifted = csr.addr |<< 2
  val mask = csr.addr & ~(csr.addr + 1)
  val masked = (csr.addr & ~mask) |<< 2

  region.locked := csr.l
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

class SPmpPlugin(regions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator {

  assert((regions % 4) == 0)
     
  def pmpcfg0 = 0x3a0
  def pmpaddr0 = 0x3b0
  def spmpcfg0 = 0x900
  def spmpaddr0 = 0x910
  
  val pmps = ArrayBuffer[Pmp]()
  val spmps = ArrayBuffer[SPmp]()
  val ports = ArrayBuffer[ProtectedMemoryTranslatorPort]()

  override def newTranslationPort(priority : Int, args : Any): MemoryTranslatorBus = {
    val port = ProtectedMemoryTranslatorPort(MemoryTranslatorBus(new MemoryTranslatorBusParameter(0, 0)))
    ports += port
    port.bus
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline.config._
    import pipeline._
    import Riscv._

    val csrService = pipeline.service(classOf[CsrInterface])
    val privilegeService = pipeline.service(classOf[PrivilegeService])

    val core = pipeline plug new Area {

      for (region <- 0 until regions) {
        if (region == 0) {
          pmps += Pmp(null)
          spmps += SPmp(null)
        } else {
          pmps += Pmp(pmps.last)
          spmps += SPmp(spmps.last)
        }
        csrService.r(pmpaddr0 + region, pmps(region).state.addr)
        csrService.w(pmpaddr0 + region, pmps(region).csr.addr)
        csrService.rw(spmpaddr0 + region, spmps(region).csr.addr)
      }

      for (pmpNcfg <- Range(0, regions, 4)) {
        val pmpCsr = pmpcfg0 + pmpNcfg / 4
        val sPmpCsr = spmpcfg0 + pmpNcfg / 4
        for (offset <- 0 until 4) {
          csrService.r(pmpCsr, (offset * 8 + 0) -> pmps(pmpNcfg + offset).state.r)
          csrService.r(pmpCsr, (offset * 8 + 1) -> pmps(pmpNcfg + offset).state.w)
          csrService.r(pmpCsr, (offset * 8 + 2) -> pmps(pmpNcfg + offset).state.x)
          csrService.r(pmpCsr, (offset * 8 + 3) -> pmps(pmpNcfg + offset).state.a)
          csrService.r(pmpCsr, (offset * 8 + 7) -> pmps(pmpNcfg + offset).state.l)
          csrService.w(pmpCsr, (offset * 8 + 0) -> pmps(pmpNcfg + offset).csr.r)
          csrService.w(pmpCsr, (offset * 8 + 1) -> pmps(pmpNcfg + offset).csr.w)
          csrService.w(pmpCsr, (offset * 8 + 2) -> pmps(pmpNcfg + offset).csr.x)
          csrService.w(pmpCsr, (offset * 8 + 3) -> pmps(pmpNcfg + offset).csr.a)
          csrService.w(pmpCsr, (offset * 8 + 7) -> pmps(pmpNcfg + offset).csr.l)
          csrService.rw(sPmpCsr, (offset * 8 + 0) -> pmps(pmpNcfg + offset).csr.r)
          csrService.rw(sPmpCsr, (offset * 8 + 1) -> pmps(pmpNcfg + offset).csr.w)
          csrService.rw(sPmpCsr, (offset * 8 + 2) -> pmps(pmpNcfg + offset).csr.x)
          csrService.rw(sPmpCsr, (offset * 8 + 3) -> pmps(pmpNcfg + offset).csr.a)
          csrService.rw(sPmpCsr, (offset * 8 + 7) -> pmps(pmpNcfg + offset).csr.l)
        }
      }

      for (port <- ports) yield new Area {

        val address = port.bus.cmd(0).virtualAddress
        port.bus.rsp.physicalAddress := address

        val machineMode = privilegeService.isMachine()
        val machineHits = pmps.map(pmp => pmp.region.valid &
                                           pmp.region.start <= address &
                                           pmp.region.end > address &
                                          (pmp.region.locked | ~machineMode))

        val machineHit0 = OHMasking.first(machineHits)
        val machineRead = MuxOH(machineHit0, pmps.map(_.state.r))
        val machineWrite = MuxOH(machineHit0, pmps.map(_.state.w))
        val machineExecute = MuxOH(machineHit0, pmps.map(_.state.x))

        when(CountOne(machineHits) === 0) {

          port.bus.rsp.allowRead := machineMode
          port.bus.rsp.allowWrite := machineMode
          port.bus.rsp.allowExecute := machineMode
          port.bus.rsp.isPaging := False
        
        } otherwise {

          val userMode = privilegeService.isUser()
          val supervisorHits = spmps.map(spmp => spmp.region.valid &
                                                  spmp.region.start <= address &
                                                  spmp.region.end > address &
                                                 (spmp.region.locked | userMode) & 
                                                  ~machineMode)

          val supervisorHit0 = OHMasking.first(supervisorHits)
          val supervisorRead = MuxOH(supervisorHit0, spmps.map(_.csr.r))
          val supervisorWrite = MuxOH(supervisorHit0, spmps.map(_.csr.w))
          val supervisorExecute = MuxOH(supervisorHit0, spmps.map(_.csr.x))
          val supervisorLocked = MuxOH(supervisorHit0, spmps.map(_.csr.l))

          when(CountOne(supervisorHits) === 0) {

            port.bus.rsp.allowRead := machineRead
            port.bus.rsp.allowWrite := machineWrite
            port.bus.rsp.allowExecute := machineExecute
            port.bus.rsp.isPaging := False

          } otherwise {
            
            port.bus.rsp.allowRead := machineRead & supervisorRead & (userMode ^ supervisorLocked)
            port.bus.rsp.allowWrite := machineWrite & supervisorWrite & (userMode ^ supervisorLocked)
            port.bus.rsp.allowExecute := machineExecute & supervisorExecute & (userMode ^ supervisorLocked)
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

