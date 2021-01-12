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

case class SPmpRegister(previous : SPmpRegister) extends Area {

  def OFF = 0
  def TOR = 1
  def NA4 = 2
  def NAPOT = 3

  // Software-accessible CSR interface
  val csr = new Area {
    val r, w, x = Reg(Bool)
    val u, l = RegInit(False)
    val a = Reg(UInt(2 bits)) init(0)
    val addr = Reg(UInt(32 bits))
  }

  // TODO: Make this a function instead of another set of registers.
  val region = new Area {
    val valid = RegInit(False)
    val start, end = Reg(UInt(32 bits))
  }
  
  val shifted = csr.addr |<< 2
  region.valid := True

  switch(csr.a) {

    is(TOR) {
      if (previous == null) {
        region.start := 0
      } else {
        region.start := previous.region.end
      }
      region.end := shifted
    }

    is(NA4) {
      region.start := shifted
      region.end := shifted + 4
    }

    is(NAPOT) {
      val mask = csr.addr & ~(csr.addr + 1)
      val masked = (csr.addr & ~mask) |<< 2
      region.start := masked
      region.end := masked + ((mask + 1) |<< 3)
    }

    default {
      region.end := shifted
      region.valid := False
    }

  }
}

class SPmpPlugin(regions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator {

  // Each pmpcfg# CSR configures four regions.
  assert((regions % 4) == 0)
   
  val pmps = ArrayBuffer[PmpRegister]()
  val spmps = ArrayBuffer[SPmpRegister]()
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
          spmps += SPmpRegister(null)
        } else {
          pmps += PmpRegister(pmps.last)
          spmps += SPmpRegister(spmps.last)
        }
        csrService.rw(0x3b0 + i, pmps(i).csr.addr)
        csrService.rw(0x3c0 + i, spmps(i).csr.addr)
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
        csrService.rw(0x3a4 + i,
          31 -> spmps((i * 4) + 3).csr.l, 23 -> spmps((i * 4) + 2).csr.l,
          15 -> spmps((i * 4) + 1).csr.l,  7 -> spmps((i * 4)    ).csr.l,
          30 -> spmps((i * 4) + 3).csr.u, 22 -> spmps((i * 4) + 2).csr.u,
          14 -> spmps((i * 4) + 1).csr.u,  6 -> spmps((i * 4)    ).csr.u,
          27 -> spmps((i * 4) + 3).csr.a, 26 -> spmps((i * 4) + 3).csr.x,
          25 -> spmps((i * 4) + 3).csr.w, 24 -> spmps((i * 4) + 3).csr.r,
          19 -> spmps((i * 4) + 2).csr.a, 18 -> spmps((i * 4) + 2).csr.x,
          17 -> spmps((i * 4) + 2).csr.w, 16 -> spmps((i * 4) + 2).csr.r,
          11 -> spmps((i * 4) + 1).csr.a, 10 -> spmps((i * 4) + 1).csr.x,
           9 -> spmps((i * 4) + 1).csr.w,  8 -> spmps((i * 4) + 1).csr.r,
           3 -> spmps((i * 4)    ).csr.a,  2 -> spmps((i * 4)    ).csr.x,
           1 -> spmps((i * 4)    ).csr.w,  0 -> spmps((i * 4)    ).csr.r
        )
      }

      // Connect memory ports to PMP logic.
      val ports = for ((port, portId) <- portsInfo.zipWithIndex) yield new Area {

        val address = port.bus.cmd.virtualAddress
        port.bus.rsp.physicalAddress := address

        // Only the first matching PMP region applies.
        val m = privilegeService.isMachine()
        val mMatch = pmps.map(pmp => pmp.region.valid &
                                     pmp.region.start <= address &
                                     pmp.region.end > address &
                                    (pmp.region.l | ~m))

        val mR = MuxOH(OHMasking.first(mMatch), pmps.map(_.region.r))
        val mW = MuxOH(OHMasking.first(mMatch), pmps.map(_.region.w))
        val mX = MuxOH(OHMasking.first(mMatch), pmps.map(_.region.x))

        // M-mode has full access by default, others have none.
        when(CountOne(mMatch) === 0) {

          port.bus.rsp.allowRead := m
          port.bus.rsp.allowWrite := m
          port.bus.rsp.allowExecute := m
        
        } otherwise {

          val s = privilegeService.isSupervisor()
          val sMatch = spmps.map(spmp => spmp.region.valid &
                                         spmp.region.start <= address &
                                         spmp.region.end > address &
                                        (spmp.csr.l | ~s))

          val sR = MuxOH(OHMasking.first(sMatch), spmps.map(_.csr.r))
          val sW = MuxOH(OHMasking.first(sMatch), spmps.map(_.csr.w))
          val sX = MuxOH(OHMasking.first(sMatch), spmps.map(_.csr.x))

          when((m | s) & CountOne(sMatch) === 0) {

            port.bus.rsp.allowRead := mR
            port.bus.rsp.allowWrite := mW
            port.bus.rsp.allowExecute := mX

          } otherwise {
            
            port.bus.rsp.allowRead := mR & sR
            port.bus.rsp.allowWrite := mW & sW
            port.bus.rsp.allowExecute := mX & sX
          
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

