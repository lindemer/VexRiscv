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

class EPmpPlugin(regions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator {

  assert((regions % 4) == 0)
     
  def pmpcfg0 = 0x3a0
  def pmpaddr0 = 0x3b0
  
  val pmps = ArrayBuffer[Pmp]()
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

      // TODO Rule Locking Bypass 
      val mseccfg = new Area { val MML, MMWP, RLB = RegInit(False) }
      csrService.rw(0x390, 0 -> mseccfg.MML, 1 -> mseccfg.MMWP, 2 -> mseccfg.RLB)

      for (region <- 0 until regions) {
        if (region == 0) pmps += Pmp(null)
        else pmps += Pmp(pmps.last)
        csrService.r(pmpaddr0 + region, pmps(region).state.addr)
        csrService.w(pmpaddr0 + region, pmps(region).csr.addr)
      }

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

      for (port <- ports) yield new Area {

        val address = port.bus.cmd.virtualAddress
        port.bus.rsp.physicalAddress := address

        val m = privilegeService.isMachine()
        val hits = pmps.map(pmp => pmp.region.valid &
                                   pmp.region.start <= address &
                                   pmp.region.end > address &
                                  (pmp.region.locked | ~m | mseccfg.MML))

        when(CountOne(hits) === 0) {

          port.bus.rsp.allowRead := m & (~mseccfg.MML | ~mseccfg.MMWP)
          port.bus.rsp.allowWrite := m & (~mseccfg.MML | ~mseccfg.MMWP)
          port.bus.rsp.allowExecute := m & ~mseccfg.MML
          
        } otherwise {

          val firstHit = OHMasking.first(hits)
          val r = MuxOH(firstHit, pmps.map(_.state.r))
          val w = MuxOH(firstHit, pmps.map(_.state.w))
          val x = MuxOH(firstHit, pmps.map(_.state.x))
          val l = MuxOH(firstHit, pmps.map(_.state.l))

          when(~mseccfg.MML) {

            port.bus.rsp.allowRead := r 
            port.bus.rsp.allowWrite := w
            port.bus.rsp.allowExecute := x

          } otherwise {

            port.bus.rsp.allowRead := ((l ^ ~m) & r) | (~l & ~r & w) | ((l & w & x) & (r | m))
            port.bus.rsp.allowWrite := ((l ^ ~m) & w) | (~l & ~r & w & (x | m))
            port.bus.rsp.allowExecute := ((l ^ ~m) & x) | (l & ~r & w)

          }

        }

        port.bus.rsp.isIoAccess := ioRange(port.bus.rsp.physicalAddress)
        port.bus.rsp.isPaging := False
        port.bus.rsp.exception := False
        port.bus.rsp.refilling := False
        port.bus.busy := False

      }
    }
  }
}

