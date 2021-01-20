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

  // Each pmpcfg# CSR configures four regions.
  assert((regions % 4) == 0)
   
  val pmps = ArrayBuffer[PmpRegister]()
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

      // TODO Rule Locking Bypass 
      val mseccfg = new Area { val MML, MMWP, RLB = RegInit(False) }
      csrService.rw(0x390, 0 -> mseccfg.MML, 1 -> mseccfg.MMWP, 2 -> mseccfg.RLB)

      // Instantiate pmpaddr0 ... pmpaddr# CSRs.
      for (i <- 0 until regions) {
        if (i == 0) {
          pmps += PmpRegister(null)
        } else {
          pmps += PmpRegister(pmps.last)
        }
        csrService.r(0x3b0 + i, pmps(i).state.addr)
        csrService.w(0x3b0 + i, pmps(i).csr.addr)
      }

      // Instantiate pmpcfg0 ... pmpcfg# CSRs.
      for (i <- 0 until (regions / 4)) {
        csrService.r(0x3a0 + i,
          31 -> pmps((i * 4) + 3).state.l, 23 -> pmps((i * 4) + 2).state.l,
          15 -> pmps((i * 4) + 1).state.l,  7 -> pmps((i * 4)    ).state.l,
          27 -> pmps((i * 4) + 3).state.a, 26 -> pmps((i * 4) + 3).state.x,
          25 -> pmps((i * 4) + 3).state.w, 24 -> pmps((i * 4) + 3).state.r,
          19 -> pmps((i * 4) + 2).state.a, 18 -> pmps((i * 4) + 2).state.x,
          17 -> pmps((i * 4) + 2).state.w, 16 -> pmps((i * 4) + 2).state.r,
          11 -> pmps((i * 4) + 1).state.a, 10 -> pmps((i * 4) + 1).state.x,
           9 -> pmps((i * 4) + 1).state.w,  8 -> pmps((i * 4) + 1).state.r,
           3 -> pmps((i * 4)    ).state.a,  2 -> pmps((i * 4)    ).state.x,
           1 -> pmps((i * 4)    ).state.w,  0 -> pmps((i * 4)    ).state.r
        )
        csrService.w(0x3a0 + i,
          31 -> pmps((i * 4) + 3).csr.l, 23 -> pmps((i * 4) + 2).csr.l,
          15 -> pmps((i * 4) + 1).csr.l,  7 -> pmps((i * 4)    ).csr.l,
          27 -> pmps((i * 4) + 3).csr.a, 26 -> pmps((i * 4) + 3).csr.x,
          25 -> pmps((i * 4) + 3).csr.w, 24 -> pmps((i * 4) + 3).csr.r,
          19 -> pmps((i * 4) + 2).csr.a, 18 -> pmps((i * 4) + 2).csr.x,
          17 -> pmps((i * 4) + 2).csr.w, 16 -> pmps((i * 4) + 2).csr.r,
          11 -> pmps((i * 4) + 1).csr.a, 10 -> pmps((i * 4) + 1).csr.x,
           9 -> pmps((i * 4) + 1).csr.w,  8 -> pmps((i * 4) + 1).csr.r,
           3 -> pmps((i * 4)    ).csr.a,  2 -> pmps((i * 4)    ).csr.x,
           1 -> pmps((i * 4)    ).csr.w,  0 -> pmps((i * 4)    ).csr.r
        )
      }

      // Connect memory ports to PMP logic.
      val ports = for ((port, portId) <- portsInfo.zipWithIndex) yield new Area {

        val address = port.bus.cmd.virtualAddress
        port.bus.rsp.physicalAddress := address

        // Only the first matching PMP region applies.
        val m = privilegeService.isMachine()
        val hits = pmps.map(pmp => pmp.region.valid &
                                   pmp.region.start <= address &
                                   pmp.region.end > address &
                                  (pmp.state.l | ~m | mseccfg.MML))

        when(CountOne(hits) === 0) {

          port.bus.rsp.allowRead := m & (~mseccfg.MML | ~mseccfg.MMWP)
          port.bus.rsp.allowWrite := m & (~mseccfg.MML | ~mseccfg.MMWP)
          port.bus.rsp.allowExecute := m & ~mseccfg.MML
          
        } otherwise {

          val r = MuxOH(OHMasking.first(hits), pmps.map(_.state.r))
          val w = MuxOH(OHMasking.first(hits), pmps.map(_.state.w))
          val x = MuxOH(OHMasking.first(hits), pmps.map(_.state.x))
          val l = MuxOH(OHMasking.first(hits), pmps.map(_.state.l))

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

