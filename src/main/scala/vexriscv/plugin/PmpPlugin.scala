/*
 * Copyright (c) 2021 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

package vexriscv.plugin

import vexriscv.{VexRiscv, _}
import vexriscv.plugin.CsrPlugin.{_}
import vexriscv.plugin.MemoryTranslatorPort.{_}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

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

  def grain = 7
}

class PmpSetter() extends Component with Pmp {
  val io = new Bundle {
    val addr = in UInt(xlen bits)
    val base, mask = out UInt(xlen - grain bits)
  }

  val ones = io.addr & ~(io.addr + 1)
  io.base := io.addr(xlen - 1 - grain downto 0) ^ ones(xlen - 1 - grain downto 0)
  io.mask := ~ones(xlen - grain downto 1)
}

case class ProtectedMemoryTranslatorPort(bus : MemoryTranslatorBus)

class PmpPlugin(regions : Int, sRegions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator with Pmp {
  assert(regions % 4 == 0 & sRegions % 4 == 0)
  assert(regions <= 16 & sRegions <= 16)

  var setter : PmpSetter = null
  var dPort, iPort : ProtectedMemoryTranslatorPort = null
  
  override def newTranslationPort(priority : Int, args : Any): MemoryTranslatorBus = {
    val port = ProtectedMemoryTranslatorPort(MemoryTranslatorBus(new MemoryTranslatorBusParameter(0, 0)))
    priority match {
      case PRIORITY_INSTRUCTION => iPort = port
      case PRIORITY_DATA => dPort = port
    }
    port.bus
  }

  override def setup(pipeline: VexRiscv): Unit = {
    setter = new PmpSetter()
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline.config._
    import pipeline._
    import Riscv._

    val csrService = pipeline.service(classOf[CsrInterface])
    val privilegeService = pipeline.service(classOf[PrivilegeService])

    val pmpaddr = Mem(UInt(xlen bits), regions)
    val pmpcfg = Reg(Bits(8 * regions bits)) init(0)
    val mBase, mMask = Mem(UInt(xlen - grain bits), regions)
    val mCfgRegion = pmpcfg.subdivideIn(8 bits)
    val mCfgRegister = pmpcfg.subdivideIn(xlen bits)
    
    val spmpaddr = Mem(UInt(xlen bits), sRegions)
    val spmpcfg = Reg(Bits(8 * sRegions bits)) init(0)
    val sBase, sMask = Mem(UInt(xlen - grain bits), sRegions)
    val sCfgRegion = spmpcfg.subdivideIn(8 bits)
    val sCfgRegister = spmpcfg.subdivideIn(xlen bits)

    val lockMask = Reg(Bits(4 bits)) init(B"4'0")

    execute plug new Area {
      import execute._

      // val mMask0 = mMask(U"4'x0")
      // val mMask1 = mMask(U"4'x1")
      // val mMask2 = mMask(U"4'x2")
      // val mMask3 = mMask(U"4'x3")
      // val mMask4 = mMask(U"4'x4")
      // val mMask5 = mMask(U"4'x5")
      // val mMask6 = mMask(U"4'x6")
      // val mMask7 = mMask(U"4'x7")
      // val mMask8 = mMask(U"4'x8")
      // val mMask9 = mMask(U"4'x9")
      // val mMask10 = mMask(U"4'xa")
      // val mMask11 = mMask(U"4'xb")
      // val mMask12 = mMask(U"4'xc")
      // val mMask13 = mMask(U"4'xd")
      // val mMask14 = mMask(U"4'xe")
      // val mMask15 = mMask(U"4'xf")
      // val mBase0 = mBase(U"4'x0")
      // val mBase1 = mBase(U"4'x1")
      // val mBase2 = mBase(U"4'x2")
      // val mBase3 = mBase(U"4'x3")
      // val mBase4 = mBase(U"4'x4")
      // val mBase5 = mBase(U"4'x5")
      // val mBase6 = mBase(U"4'x6")
      // val mBase7 = mBase(U"4'x7")
      // val mBase8 = mBase(U"4'x8")
      // val mBase9 = mBase(U"4'x9")
      // val mBase10 = mBase(U"4'xa")
      // val mBase11 = mBase(U"4'xb")
      // val mBase12 = mBase(U"4'xc")
      // val mBase13 = mBase(U"4'xd")
      // val mBase14 = mBase(U"4'xe")
      // val mBase15 = mBase(U"4'xf")

      val csrAddress = input(INSTRUCTION)(csrRange)
      
      val cfgAccess = input(IS_PMP_CFG) | input(IS_SPMP_CFG)
      val addrAccess = input(IS_PMP_ADDR) | input(IS_SPMP_ADDR)

      val machine = privilegeService.isMachine()
      val supervisor = privilegeService.isSupervisor()
      val mAccess = (input(IS_PMP_ADDR) | input(IS_PMP_CFG)) & machine
      val sAccess = (input(IS_SPMP_ADDR) | input(IS_SPMP_CFG)) & (machine | supervisor)

      val readEnable = arbitration.isValid && input(IS_CSR) && input(CSR_READ_OPCODE)
      val writeEnable = arbitration.isValid && input(IS_CSR) && input(CSR_WRITE_OPCODE)

      val mWrite = writeEnable & mAccess
      val mRead = readEnable & mAccess
      val sWrite = writeEnable & sAccess
      val sRead = readEnable & sAccess

      val mPmpIndex = csrAddress(log2Up(regions) - 1 downto 0).asUInt
      val mPmpSelect = mPmpIndex(log2Up(regions) - 3 downto 0)
      val sPmpIndex = csrAddress(log2Up(sRegions) - 1 downto 0).asUInt
      val sPmpSelect = mPmpIndex(log2Up(sRegions) - 3 downto 0)

      val readAddr = Mux(sAccess, spmpaddr.readAsync(sPmpIndex).asBits, pmpaddr.readAsync(mPmpIndex).asBits)
      val readCfg = Mux(sAccess, sCfgRegister(sPmpSelect), mCfgRegister(mPmpSelect))
      val readToWrite = Mux(cfgAccess, readCfg, readAddr)
      val writeSrc = input(SRC1)
      val writeData = input(INSTRUCTION)(13).mux(
        False -> writeSrc,
        True -> Mux(
          input(INSTRUCTION)(12),
          readToWrite & ~writeSrc,
          readToWrite | writeSrc
        )
      )
      
      val writer = new Area {
        when (cfgAccess) {
          when (mRead | sRead) {
            output(REGFILE_WRITE_DATA).assignFromBits(readCfg)
          }
          when (mWrite) {
            switch(mPmpSelect) {
              for (i <- 0 until (regions / 4)) {
                is(i) {
                  for (j <- Range(0, xlen, 8)) {
                    val bitRange = j + xlen * i + lBit downto j + xlen * i
                    val overwrite = writeData.subdivideIn(8 bits)(j / 8)
                    val locked = mCfgRegister(i).subdivideIn(8 bits)(j / 8)(lBit)
                    lockMask(j / 8) := locked
                    when (~locked) {
                      pmpcfg(bitRange).assignFromBits(overwrite)
                    }
                  }
                }
              }
            }
          }.elsewhen (sWrite) {
            switch(sPmpSelect) {
              for (i <- 0 until (sRegions / 4)) {
                is(i) {
                  for (j <- Range(0, xlen, 8)) {
                    val bitRange = j + xlen * i + lBit downto j + xlen * i
                    val overwrite = writeData.subdivideIn(8 bits)(j / 8)
                    val locked = sCfgRegister(i).subdivideIn(8 bits)(j / 8)(lBit)
                    lockMask(j / 8) := locked
                    when (~locked) {
                      spmpcfg(bitRange).assignFromBits(overwrite)
                    }
                  }
                }
              }
            }
          }
        }.elsewhen (addrAccess & (mRead | sRead)) {
          output(REGFILE_WRITE_DATA) := readAddr
        }
        val mLocked = mCfgRegion(mPmpIndex)(lBit)
        val sLocked = sCfgRegion(sPmpIndex)(lBit)
        pmpaddr.write(mPmpIndex, writeData.asUInt, ~mLocked & mWrite & addrAccess)
        spmpaddr.write(sPmpIndex, writeData.asUInt, ~sLocked & sWrite & addrAccess)
      }

      val controller = new StateMachine {
        val mWidth = log2Up(regions)
        val sWidth = log2Up(sRegions)

        val counter = Reg(UInt(4 bits)) init(0)
        val enable = RegInit(False)

        val stateIdle : State = new State with EntryPoint {
          onEntry {
            lockMask := B"4'x0"
            enable := False
            counter := 0
          }
          onExit {
            enable := True
            arbitration.haltItself := True
          }
          whenIsActive {
            when (mWrite) {
              when (cfgAccess) {
                counter := (mPmpIndex(mWidth - 3 downto 0) @@ U"2'00").resized
                goto(stateCfg)
              }.elsewhen (addrAccess) {
                counter := mPmpIndex.resized
                goto(stateAddr)
              }
            }.elsewhen (sWrite) {
              when (cfgAccess) {
                counter := (sPmpIndex(sWidth - 3 downto 0) @@ U"2'00").resized
                goto(stateCfg)
              }.elsewhen (addrAccess) {
                counter := sPmpIndex.resized
                goto(stateAddr)
              }
            }
          }
        }

        val stateCfg : State = new State {
          whenIsActive {
            counter := counter + 1
            when (counter(1 downto 0) === 3) {
              goto(stateIdle)
            } otherwise {
              arbitration.haltItself := True
            }
          }
        }

        val stateAddr : State = new State {
          whenIsActive {
            goto(stateIdle)
          }
        }

        when (mAccess) {
          when (cfgAccess) {
            setter.io.addr := pmpaddr(counter.resized) 
          } otherwise {
            when (counter === mPmpIndex) {
              setter.io.addr := writeData.asUInt
            } otherwise {
              setter.io.addr := pmpaddr.readAsync(counter.resized)
            }
          }
        } otherwise {
          when (cfgAccess) {
            setter.io.addr := spmpaddr(counter.resized) 
          } otherwise {
            when (counter === sPmpIndex) {
              setter.io.addr := writeData.asUInt
            } otherwise {
              setter.io.addr := spmpaddr.readAsync(counter.resized)
            }
          }
        }
        
        when (enable) {
          when (mAccess &
                ((cfgAccess & ~lockMask(counter(1 downto 0))) | 
                 (addrAccess & ~mCfgRegion(counter.resized)(lBit)))) {
            mBase(counter.resized) := setter.io.base
            mMask(counter.resized) := setter.io.mask
          }.elsewhen (sAccess &
                ((cfgAccess & ~lockMask(counter(1 downto 0))) | 
                 (addrAccess & ~sCfgRegion(counter.resized)(lBit)))) {
            sBase(counter.resized) := setter.io.base
            sMask(counter.resized) := setter.io.mask
          }
        }
      }
    }

    pipeline plug new Area {
      def mGetHits(address : UInt) = {
        (0 until regions).map(i =>
            ((address & mMask(U(i, log2Up(regions) bits))) === mBase(U(i, log2Up(regions) bits))) & 
            (mCfgRegion(i)(lBit) | ~privilegeService.isMachine()) & mCfgRegion(i)(aBits) =/= OFF
        )
      }

      def sGetHits(address : UInt) = {
        (0 until sRegions).map(i =>
            ((address & sMask(U(i, log2Up(sRegions) bits))) === sBase(U(i, log2Up(sRegions) bits))) & 
            (sCfgRegion(i)(lBit) | privilegeService.isUser()) & ~privilegeService.isMachine() &
            sCfgRegion(i)(aBits) =/= OFF
        )
      }

      val dGuard = new Area {
        val address = dPort.bus.cmd(0).virtualAddress
        dPort.bus.rsp.physicalAddress := address
        dPort.bus.rsp.isIoAccess := ioRange(address)
        dPort.bus.rsp.exception := False
        dPort.bus.rsp.refilling := False
        dPort.bus.rsp.allowExecute := False
        dPort.bus.busy := False

        val machineHits = mGetHits(address(31 downto grain))
        val machineHit0 = OHMasking.first(machineHits)
        val machineRead = MuxOH(machineHit0, mCfgRegion.map(cfg => cfg(rBit)))
        val machineWrite = MuxOH(machineHit0, mCfgRegion.map(cfg => cfg(wBit)))

        val supervisorHits = sGetHits(address(31 downto grain))
        val supervisorHit0 = OHMasking.first(supervisorHits)
        val supervisorRead = MuxOH(supervisorHit0, sCfgRegion.map(cfg => cfg(rBit)))
        val supervisorWrite = MuxOH(supervisorHit0, sCfgRegion.map(cfg => cfg(wBit)))
        val supervisorLocked = MuxOH(supervisorHit0, sCfgRegion.map(cfg => cfg(lBit)))

        val machineMode = privilegeService.isMachine()
        val userMode = privilegeService.isUser()

        when(~machineHits.orR) {
          dPort.bus.rsp.allowRead := privilegeService.isMachine()
          dPort.bus.rsp.allowWrite := privilegeService.isMachine()
          dPort.bus.rsp.isPaging := False
        } otherwise {
          when(~supervisorHits.orR) {
            dPort.bus.rsp.allowRead := machineRead
            dPort.bus.rsp.allowWrite := machineWrite
            dPort.bus.rsp.isPaging := False
          } otherwise {
            dPort.bus.rsp.allowRead := machineRead & supervisorRead & (userMode ^ supervisorLocked)
            dPort.bus.rsp.allowWrite := machineWrite & supervisorWrite & (userMode ^ supervisorLocked)
            dPort.bus.rsp.isPaging := True
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

        val machineHits = mGetHits(address(31 downto grain))
        val machineHit0 = OHMasking.first(machineHits)
        val machineExecute = MuxOH(machineHit0, mCfgRegion.map(cfg => cfg(xBit)))

        val supervisorHits = sGetHits(address(31 downto grain))
        val supervisorHit0 = OHMasking.first(supervisorHits)
        val supervisorExecute = MuxOH(supervisorHit0, sCfgRegion.map(cfg => cfg(xBit)))
        val supervisorLocked = MuxOH(supervisorHit0, sCfgRegion.map(cfg => cfg(lBit)))

        val machineMode = privilegeService.isMachine()
        val userMode = privilegeService.isUser()

        when(~machineHits.orR) {
          iPort.bus.rsp.allowExecute := machineMode
          iPort.bus.rsp.isPaging := False
        } otherwise {
          when(~supervisorHits.orR) {
            iPort.bus.rsp.allowExecute := machineExecute
            iPort.bus.rsp.isPaging := False
          } otherwise {
            iPort.bus.rsp.allowExecute := machineExecute & supervisorExecute & (userMode ^ supervisorLocked)
            iPort.bus.rsp.isPaging := True
          }
        }
      }
    }
  }
}