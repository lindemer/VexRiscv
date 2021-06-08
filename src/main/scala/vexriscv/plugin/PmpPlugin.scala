/*
 * Copyright (c) 2021 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

package vexriscv.plugin

import vexriscv.{VexRiscv, _}
import vexriscv.plugin.MemoryTranslatorPort.{_}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import scala.collection.script.Index

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
 * 
 * N.B. THIS IMPLEMENTATION ONLY SUPPORTS NAPOT ADDRESSING. REGIONS ARE NOT
 * ORDERED BY PRIORITY. A PERMISSION IS GRANTED TO AN ACCESS IF ANY MATCHING
 * PMP REGION HAS THAT PERMISSION ENABLED.
 */

trait Pmp {
  def OFF = 0
  def TOR = 1
  def NA4 = 2
  def NAPOT = 3

  def xlen = 32
  def rBit = 0
  def wBit = 1
  def xBit = 2
  def aBits = 4 downto 3
  def lBit = 7
}

class PmpSetter(cutoff : Int) extends Component with Pmp {
  val io = new Bundle {
    val addr = in UInt(xlen bits)
    val base, mask = out UInt(xlen - cutoff bits)
  }

  val ones = io.addr & ~(io.addr + 1)
  io.base := io.addr(xlen - 3 downto cutoff - 2) ^ ones(xlen - 3 downto cutoff - 2)
  io.mask := ~ones(xlen - 2 downto cutoff - 1)
}

case class ProtectedMemoryTranslatorPort(bus : MemoryTranslatorBus)

class PmpPlugin(regions : Int, granularity : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator with Pmp {
  assert(regions % 4 == 0 & regions <= 16)
  assert(granularity >= 8)

  var setter : PmpSetter = null
  var dPort, iPort : ProtectedMemoryTranslatorPort = null
  val cutoff = log2Up(granularity) - 1
  
  override def newTranslationPort(priority : Int, args : Any): MemoryTranslatorBus = {
    val port = ProtectedMemoryTranslatorPort(MemoryTranslatorBus(new MemoryTranslatorBusParameter(0, 0)))
    priority match {
      case PRIORITY_INSTRUCTION => iPort = port
      case PRIORITY_DATA => dPort = port
    }
    port.bus
  }

  override def setup(pipeline: VexRiscv): Unit = {
    setter = new PmpSetter(cutoff)
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline.config._
    import pipeline._
    import Riscv._

    val csrService = pipeline.service(classOf[CsrInterface])
    val privilegeService = pipeline.service(classOf[PrivilegeService])

    val pmp, mpu = pipeline plug new Area {
      val addr = Mem(UInt(xlen bits), regions)
      val cfg = Vector.fill(regions)(Reg(Bits(8 bits)) init(0))
      val base, mask = Vector.fill(regions)(Reg(UInt(xlen - cutoff bits)))
    }

    def mMode : Bool = privilegeService.isMachine()
    def sMode : Bool = privilegeService.isSupervisor()
    def uMode : Bool = privilegeService.isUser()

    execute plug new Area {
      import execute._

      val fsmPending = RegInit(False) clearWhen(!arbitration.isStuck)
      val fsmComplete = False
      val hazardFree = csrService.isHazardFree()

      val csr = new Area {
        val address = input(INSTRUCTION)(csrRange)
        val ___Ncfg = address(log2Up(regions) - 1 downto 0).asUInt
        val ___cfgN = ___Ncfg(log2Up(regions) - 3 downto 0)
        val isPmpCfg = input(INSTRUCTION)(31 downto 24) === 0x3a
        val isPmpAddr = input(INSTRUCTION)(31 downto 24) === 0x3b
        val isMpuCfg = input(INSTRUCTION)(31 downto 24) === 0x90
        val isMpuAddr = input(INSTRUCTION)(31 downto 24) === 0x91
      }

      val save = new Area {
        val ___Ncfg = Reg(UInt(log2Up(regions) bits))
        val ___cfgN = Reg(UInt(log2Up(regions) - 2 bits))
        val isPmpCfg, isPmpAddr = RegInit(False)
        val isMpuCfg, isMpuAddr = RegInit(False)
        val writeData = Reg(Bits(xlen bits))
      }

      csrService.duringAnyRead {
        when (mMode) {
          when (csr.isPmpCfg) {
            csrService.allowCsr()
            csrService.readData() :=
              pmp.cfg(csr.___cfgN @@ U(3, 2 bits)) ##
              pmp.cfg(csr.___cfgN @@ U(2, 2 bits)) ##
              pmp.cfg(csr.___cfgN @@ U(1, 2 bits)) ##
              pmp.cfg(csr.___cfgN @@ U(0, 2 bits))
          }
          when (csr.isPmpAddr) {
            csrService.allowCsr()
            csrService.readData() := pmp.addr(csr.___Ncfg).asBits
          }
        }
        when (mMode | sMode) {
          when (csr.isMpuCfg) {
            csrService.allowCsr()
            csrService.readData() :=
              mpu.cfg(csr.___cfgN @@ U(3, 2 bits)) ##
              mpu.cfg(csr.___cfgN @@ U(2, 2 bits)) ##
              mpu.cfg(csr.___cfgN @@ U(1, 2 bits)) ##
              mpu.cfg(csr.___cfgN @@ U(0, 2 bits))
          }
          when (csr.isMpuAddr) {
            csrService.allowCsr()
            csrService.readData() := mpu.addr(csr.___Ncfg).asBits
          }
        }
      }

      csrService.duringAnyWrite {
        when (((csr.isPmpCfg | csr.isPmpAddr) & mMode) |
              ((csr.isMpuCfg | csr.isMpuAddr) & (mMode | sMode))) {
          csrService.allowCsr()
          arbitration.haltItself := !fsmComplete
          when (!fsmPending && hazardFree) {
            fsmPending := True
            save.writeData := csrService.writeData()
            save.___Ncfg := csr.___Ncfg
            save.___cfgN := csr.___cfgN
            save.isPmpCfg := csr.isPmpCfg
            save.isPmpAddr := csr.isPmpAddr
            save.isMpuCfg := csr.isMpuCfg
            save.isMpuAddr := csr.isMpuAddr
          }
        }
      }

      val fsm = new StateMachine {
        val fsmEnable = RegInit(False)
        val fsmCounter = Reg(UInt(log2Up(regions) bits)) init(0)

        val stateIdle : State = new State with EntryPoint {
          onEntry {
            fsmPending := False
            fsmEnable := False
            fsmComplete := True
            fsmCounter := 0
          }
          whenIsActive {
            when (fsmPending) {
              goto(stateWrite)
            }
          }
        }

        val stateWrite : State = new State {
          whenIsActive {
            when (save.isPmpCfg) {
              val overwrite = save.writeData.subdivideIn(8 bits)
              for (i <- 0 until 4) {
                when (~pmp.cfg(save.___cfgN @@ U(i, 2 bits))(lBit)) {
                  pmp.cfg(save.___cfgN @@ U(i, 2 bits)).assignFromBits(overwrite(i))
                }
              }
              goto(stateCfg)
            }
            when (save.isPmpAddr) {
              when (~pmp.cfg(save.___Ncfg)(lBit)) {
                pmp.addr(save.___Ncfg) := save.writeData.asUInt
              }
              goto(stateAddr)
            }
            when (save.isMpuCfg) {
              val overwrite = save.writeData.subdivideIn(8 bits)
              for (i <- 0 until 4) mpu.cfg(save.___cfgN @@ U(i, 2 bits)).assignFromBits(overwrite(i))
              goto(stateCfg)
            }
            when (save.isMpuAddr) {
              mpu.addr(save.___Ncfg) := save.writeData.asUInt
              goto(stateAddr)
            }
          }
          onExit (fsmEnable := True)
        }

        val stateCfg : State = new State {
          onEntry (fsmCounter := save.___cfgN @@ U(0, 2 bits))
          whenIsActive {
            fsmCounter := fsmCounter + 1
            when (fsmCounter(1 downto 0) === 3) {
              goto(stateIdle)
            }
          }
        }

        val stateAddr : State = new State {
          onEntry (fsmCounter := save.___Ncfg)
          whenIsActive (goto(stateIdle))
        }

        when (save.isPmpCfg) {
          setter.io.addr := pmp.addr(fsmCounter)
        }.elsewhen (save.isMpuCfg) {
          setter.io.addr := mpu.addr(fsmCounter)
        } otherwise {
          setter.io.addr := save.writeData.asUInt
        }
        
        when (fsmEnable) {
          when ((save.isPmpCfg | save.isPmpAddr) & ~pmp.cfg(fsmCounter)(lBit)) {
            pmp.base(fsmCounter) := setter.io.base
            pmp.mask(fsmCounter) := setter.io.mask
          } otherwise {
            mpu.base(fsmCounter) := setter.io.base
            mpu.mask(fsmCounter) := setter.io.mask
          }
        }
      }
    }

    pipeline plug new Area {
      def getPmpHits(address : UInt) = {
        (0 until regions).map(i =>
            ((address & pmp.mask(U(i, log2Up(regions) bits))) === pmp.base(U(i, log2Up(regions) bits))) & 
            (pmp.cfg(i)(lBit) | ~mMode) & (pmp.cfg(i)(aBits) === NAPOT)
        )
      }
      def getMpuHits(address : UInt) = {
        (0 until regions).map(i =>
            ((address & mpu.mask(U(i, log2Up(regions) bits))) === mpu.base(U(i, log2Up(regions) bits))) & 
             (mpu.cfg(i)(aBits) === NAPOT)
        )
      }
      def getAccess(cfg : Vector[Bits], hits : IndexedSeq[Bool], bit : Int) = {
        (hits zip cfg).map({ case (i, cfg) => i & cfg(bit) }).orR
      }

      val dGuard = new Area {
        val address = dPort.bus.cmd(0).virtualAddress
        dPort.bus.rsp.physicalAddress := address
        dPort.bus.rsp.isIoAccess := ioRange(address)
        dPort.bus.rsp.exception := False
        dPort.bus.rsp.refilling := False
        dPort.bus.rsp.allowExecute := False
        dPort.bus.busy := False

        val pmpHits = getPmpHits(address(31 downto cutoff))
        val mpuHits = getMpuHits(address(31 downto cutoff))
        val pmpR = getAccess(pmp.cfg, pmpHits, rBit)
        val pmpW = getAccess(pmp.cfg, pmpHits, wBit)
        val mpuL = getAccess(mpu.cfg, mpuHits, lBit)
        val mpuR = (uMode & getAccess(mpu.cfg, mpuHits, rBit)) | (sMode & ~mpuL)
        val mpuW = (uMode & getAccess(mpu.cfg, mpuHits, wBit)) | (sMode & ~mpuL)

        when(~pmpHits.orR) {
          dPort.bus.rsp.isPaging := False
          dPort.bus.rsp.allowRead := mMode
          dPort.bus.rsp.allowWrite := mMode
        } otherwise {
          when(~mpuHits.orR) {
            dPort.bus.rsp.isPaging := False
            dPort.bus.rsp.allowRead := pmpR
            dPort.bus.rsp.allowWrite := pmpW
          } otherwise {
            dPort.bus.rsp.isPaging := True
            dPort.bus.rsp.allowRead := pmpR & mpuR
            dPort.bus.rsp.allowWrite := pmpW & mpuW
          }
        }
      }

      val iGuard = new Area {
        val address = iPort.bus.cmd(0).virtualAddress
        iPort.bus.rsp.physicalAddress := address
        iPort.bus.rsp.isIoAccess := ioRange(address)
        iPort.bus.rsp.exception := False
        iPort.bus.rsp.refilling := False
        iPort.bus.rsp.allowRead := False
        iPort.bus.rsp.allowWrite := False
        iPort.bus.busy := False

        val pmpHits = getPmpHits(address(31 downto cutoff))
        val mpuHits = getMpuHits(address(31 downto cutoff))
        val pmpX = getAccess(pmp.cfg, pmpHits, xBit)
        val mpuL = getAccess(mpu.cfg, mpuHits, lBit)
        val mpuX = (uMode & getAccess(mpu.cfg, mpuHits, xBit)) | (sMode & ~mpuL)

        when(~pmpHits.orR) {
          iPort.bus.rsp.isPaging := False
          iPort.bus.rsp.allowExecute := mMode
        } otherwise {
          when(~mpuHits.orR) {
            iPort.bus.rsp.isPaging := False
            iPort.bus.rsp.allowExecute := pmpX
          } otherwise {
            iPort.bus.rsp.isPaging := True
            iPort.bus.rsp.allowExecute := pmpX & mpuX
          }
        }
      }
    }
  }
}