/*
 * Copyright (c) 2020 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

package vexriscv.plugin

import vexriscv.{VexRiscv, _}
import vexriscv.{plugin, _}
import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

/* Each 32-bit pmpcfg# register contains four 8-bit configuration sections.
 * These section numbers contain flags which apply to regions defined by the
 * corresponding pmpaddr# register.
 *
 *    3                   2                   1
 *  1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |    pmp3cfg    |    pmp2cfg    |    pmp1cfg    |    pmp0cfg    | pmpcfg0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |    pmp7cfg    |    pmp6cfg    |    pmp5cfg    |    pmp4cfg    | pmpcfg2
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *     7       6       5       4       3       2       1       0
 * +-------+-------+-------+-------+-------+-------+-------+-------+
 * |   L   |       0       |       A       |   X   |   W   |   R   | pmp#cfg
 * +-------+-------+-------+-------+-------+-------+-------+-------+
 *
 *	  L: locks configuration until system reset (including M-mode)
 *	  0: hardwired to zero
 *	  A: 0 = OFF (null region / disabled)
 *	     1 = TOR (top of range)
 * 	     2 = NA4 (naturally aligned four-byte region)
 *	     3 = NAPOT (naturally aligned power-of-two region, > 7 bytes)
 *	  X: execute
 *	  W: write
 *	  R: read
 *
 * TOR: Each 32-bit pmpaddr# register defines the upper bound of the pmp region
 * right-shifted by two bits. The lower bound of the region is the previous
 * pmpaddr# register. In the case of pmpaddr0, the lower bound is address 0x0.
 *
 *    3                   2                   1
 *  1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |                        address[33:2]                          | pmpaddr#
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * NAPOT: Each 32-bit pmpaddr# register defines the region address and the size
 * of the pmp region. The number of concurrent 1s begging at the LSB indicates
 * the size of the region as a power of two (e.g. 0x...0 = 8-byte, 0x...1 =
 * 16-byte, 0x...11 = 32-byte, etc.).
 *
 *    3                   2                   1
 *  1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |                        address[33:2]                |0|1|1|1|1| pmpaddr#
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * NA4: This is essentially an edge case of NAPOT where the entire pmpaddr#
 * register defines a 4-byte wide region.
 */

case class Pmp(previous : Pmp) extends Area {

  def OFF = 0
  def TOR = 1
  def NA4 = 2
  def NAPOT = 3

  val state = new Area {
    val r, w, x = Reg(Bool)
    val l = RegInit(False)
    val a = Reg(UInt(2 bits)) init(0)
    val addr = Reg(UInt(32 bits))
  }

  // CSR writes connect to these signals rather than the internal state
  // registers. This makes locking and WARL possible.
  val csr = new Area {
    val r, w, x = Bool
    val l = Bool
    val a = UInt(2 bits)
    val addr = UInt(32 bits)
  }

  csr.r    := state.r
  csr.w    := state.w
  csr.x    := state.x
  csr.l    := state.l
  csr.a    := state.a
  csr.addr := state.addr

  // Computed PMP region bounds
  val region = new Area {
    val valid, locked = Bool
    val start, end = UInt(32 bits)
  }

  when(~state.l) {
    state.r    := csr.r
    state.w    := csr.w
    state.x    := csr.x
    state.l    := csr.l
    state.a    := csr.a
    state.addr := csr.addr

    if (csr.l == True & csr.a == TOR) {
      previous.state.l := True
    }
  }

  val shifted = state.addr |<< 2
  val mask = state.addr & ~(state.addr + 1)
  val napotStart = (state.addr & ~mask) |<< 2
  val napotEnd = napotStart + ((mask + 1) |<< 3)

  // PMP changes take effect two clock cycles after the initial CSR write (i.e.,
  // settings propagate from csr -> state -> region).
  region.locked := state.l
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
      region.start := napotStart
      region.end := napotEnd
    }
    default {
      region.start := 0
      region.end := shifted
      region.valid := False
    }
  }
}

case class ProtectedMemoryTranslatorPort(bus : MemoryTranslatorBus)

class PmpPlugin(regions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator {

  // Each pmpcfg# CSR configures four regions.
  assert((regions % 4) == 0)
  
  def pmpcfg0 = 0x3a0
  def pmpaddr0 = 0x3b0
  
  val pmps = ArrayBuffer[Pmp]()
  val dPorts = ArrayBuffer[ProtectedMemoryTranslatorPort]()
  val iPorts = ArrayBuffer[ProtectedMemoryTranslatorPort]()

  override def newTranslationPort(priority : Int, args : Any): MemoryTranslatorBus = {
    val port = ProtectedMemoryTranslatorPort(MemoryTranslatorBus())
    priority match {
      case MemoryTranslatorPort.PRIORITY_DATA => dPorts += port
      case MemoryTranslatorPort.PRIORITY_INSTRUCTION => iPorts += port
    }
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
      for (region <- 0 until regions) {
        if (region == 0) pmps += Pmp(null)
        else pmps += Pmp(pmps.last)
        csrService.r(pmpaddr0 + region, pmps(region).state.addr)
        csrService.w(pmpaddr0 + region, pmps(region).csr.addr)
      }

      // Instantiate pmpcfg0 ... pmpcfg# CSRs.
      for (pmpNcfg <- Range(0, regions, 4)) {
        val cfgCsr = pmpcfg0 + pmpNcfg / 4
        for (offset <- 0 until 4) {
          csrService.r(cfgCsr, (offset * 8 + 0) -> pmps(pmpNcfg + offset).state.r)
          csrService.r(cfgCsr, (offset * 8 + 1) -> pmps(pmpNcfg + offset).state.w)
          csrService.r(cfgCsr, (offset * 8 + 2) -> pmps(pmpNcfg + offset).state.x)
          csrService.r(cfgCsr, (offset * 8 + 3) -> pmps(pmpNcfg + offset).state.a)
          csrService.r(cfgCsr, (offset * 8 + 7) -> pmps(pmpNcfg + offset).state.l)
          csrService.w(cfgCsr, (offset * 8 + 0) -> pmps(pmpNcfg + offset).csr.r)
          csrService.w(cfgCsr, (offset * 8 + 1) -> pmps(pmpNcfg + offset).csr.w)
          csrService.w(cfgCsr, (offset * 8 + 2) -> pmps(pmpNcfg + offset).csr.x)
          csrService.w(cfgCsr, (offset * 8 + 3) -> pmps(pmpNcfg + offset).csr.a)
          csrService.w(cfgCsr, (offset * 8 + 7) -> pmps(pmpNcfg + offset).csr.l)
        }
      }

      def check(address : UInt) : ArrayBuffer[Bool] = {
        pmps.map(pmp => pmp.region.valid &
                        pmp.region.start <= address &
                        pmp.region.end > address &
                       (pmp.region.locked | ~privilegeService.isMachine()))
      }

      for (port <- (dPorts ++ iPorts)) yield new Area {
        port.bus.rsp.physicalAddress := port.bus.cmd.virtualAddress
        port.bus.rsp.isIoAccess := ioRange(port.bus.rsp.physicalAddress)
        port.bus.rsp.isPaging := False
        port.bus.rsp.exception := False
        port.bus.rsp.refilling := False
        port.bus.busy := False
      }

      // Only PMP R/W rules apply to data ports. X is not allowed.
      for (port <- dPorts) yield new Area {
        port.bus.rsp.allowExecute := False

        val hits = check(port.bus.cmd.virtualAddress)
        when(CountOne(hits) === 0) {
          port.bus.rsp.allowRead := privilegeService.isMachine()
          port.bus.rsp.allowWrite := privilegeService.isMachine()
        } otherwise {
          val firstHit = OHMasking.first(hits)
          port.bus.rsp.allowRead := MuxOH(firstHit, pmps.map(_.state.r))
          port.bus.rsp.allowWrite := MuxOH(firstHit, pmps.map(_.state.w))
        }
      }

      // Only PMP X rules apply to instruciton ports. R/W are not allowed.
      for (port <- iPorts) yield new Area {
        port.bus.rsp.allowRead := False
        port.bus.rsp.allowWrite := False

        val hits = check(port.bus.cmd.virtualAddress)
        when(CountOne(hits) === 0) {
          port.bus.rsp.allowExecute := privilegeService.isMachine()
        } otherwise {
          port.bus.rsp.allowExecute := MuxOH(OHMasking.first(hits), pmps.map(_.state.x))
        }
      }
    }
  }
}
