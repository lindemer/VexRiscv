/*
 * Copyright (c) 2020 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

package vexriscv.plugin

import vexriscv.{VexRiscv, _}
import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

class SPmpPlugin(regions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator {

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

      // Instantiate pmpaddr0 ... pmpaddr# CSRs.
      for (i <- 0 until regions) {
        if (i == 0) {
          pmps += PmpRegister(null)
        } else {
          pmps += PmpRegister(pmps.last)
        }
        csrService.rw(0x3b0 + i, pmps(i).csr.addr)
      }

      // Instantiate pmpcfg0 ... pmpcfg# CSRs.
      for (i <- 0 until (regions / 4)) {
        csrService.rw(0x3a0 + i,
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
        val hits = pmps.map(pmp => pmp.region.valid && 
                                   pmp.region.start <= address && 
                                   pmp.region.end > address &&
                                  (pmp.region.l || ~privilegeService.isMachine()))

        // M-mode has full access by default, others have none.
        when(CountOne(hits) === 0) {
          port.bus.rsp.allowRead := privilegeService.isMachine()
          port.bus.rsp.allowWrite := privilegeService.isMachine()
          port.bus.rsp.allowExecute := privilegeService.isMachine()
        } otherwise {
          port.bus.rsp.allowRead := MuxOH(OHMasking.first(hits), pmps.map(_.region.r))
          port.bus.rsp.allowWrite := MuxOH(OHMasking.first(hits), pmps.map(_.region.w))
          port.bus.rsp.allowExecute := MuxOH(OHMasking.first(hits), pmps.map(_.region.x))
        }

        port.bus.rsp.isIoAccess := ioRange(port.bus.rsp.physicalAddress)
        port.bus.rsp.exception := False
        port.bus.rsp.refilling := False
        port.bus.busy := False

      }
    }
  }
}

