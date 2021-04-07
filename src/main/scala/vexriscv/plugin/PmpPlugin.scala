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
}

class PmpSetter() extends Component with Pmp {
  val io = new Bundle {
    val a = in Bits(2 bits)
    val addr = in UInt(xlen bits)
    val prevHi = in UInt(30 bits)
    val boundLo, boundHi = out UInt(30 bits)
  }

  val shifted = io.addr(29 downto 0)
  io.boundLo := shifted
  io.boundHi := shifted

  switch (io.a) {
    is (TOR) {
      io.boundLo := io.prevHi
    }
    is (NA4) {
      io.boundHi := shifted + 1
    }
    is (NAPOT) {
      val mask = io.addr & ~(io.addr + 1)
      val boundLo = (io.addr ^ mask)(29 downto 0)
      io.boundLo := boundLo
      io.boundHi := boundLo + ((mask + 1) |<< 3)(29 downto 0)
    }
  }
}

case class ProtectedMemoryTranslatorPort(bus : MemoryTranslatorBus)

class PmpPlugin(regions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator with Pmp {

  assert(regions % 4 == 0)
  assert(regions <= 16)

  var setter : PmpSetter = null
  val ports = ArrayBuffer[ProtectedMemoryTranslatorPort]()

  // FIXME (don't copy from CSR plugin)
  object IS_CSR_ extends Stageable(Bool)
  object CSR_WRITE_OPCODE_ extends Stageable(Bool)
  object CSR_READ_OPCODE_ extends Stageable(Bool)

  override def newTranslationPort(priority : Int, args : Any): MemoryTranslatorBus = {
    val port = ProtectedMemoryTranslatorPort(MemoryTranslatorBus(new MemoryTranslatorBusParameter(0, 0)))
    ports += port
    port.bus
  }

  override def setup(pipeline: VexRiscv): Unit = {
   import vexriscv.Riscv._
    setter = new PmpSetter()

    // FIXME
    val decoderService = pipeline.service(classOf[DecoderService])
    decoderService.addDefault(IS_CSR_, False)
    decoderService.add(List(
      CSRRW  -> List[(Stageable[_ <: BaseType],Any)](IS_CSR_ -> True),
      CSRRS  -> List[(Stageable[_ <: BaseType],Any)](IS_CSR_ -> True),
      CSRRC  -> List[(Stageable[_ <: BaseType],Any)](IS_CSR_ -> True),
      CSRRWI -> List[(Stageable[_ <: BaseType],Any)](IS_CSR_ -> True),
      CSRRSI -> List[(Stageable[_ <: BaseType],Any)](IS_CSR_ -> True),
      CSRRCI -> List[(Stageable[_ <: BaseType],Any)](IS_CSR_ -> True)
    ))
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline.config._
    import pipeline._
    import Riscv._

    val csrService = pipeline.service(classOf[CsrInterface])
    val privilegeService = pipeline.service(classOf[PrivilegeService])

    val pmpaddr = Mem(UInt(xlen bits), regions)
    val pmpcfg = Reg(Bits(8 * regions bits)) init(0)
    val boundLo, boundHi = Mem(UInt(30 bits), regions)
    val cfgRegion = pmpcfg.subdivideIn(8 bits)
    val cfgRegister = pmpcfg.subdivideIn(xlen bits)

    // FIXME
    decode plug new Area {
      import decode._
      val imm = IMM(input(INSTRUCTION))
      insert(CSR_WRITE_OPCODE_) := ! (
            (input(INSTRUCTION)(14 downto 13) === B"01" && input(INSTRUCTION)(rs1Range) === 0)
         || (input(INSTRUCTION)(14 downto 13) === B"11" && imm.z === 0)
      )
      insert(CSR_READ_OPCODE_) := input(INSTRUCTION)(13 downto 7) =/= B"0100000"
    }

    execute plug new Area {
      import execute._

      // TODO: remove
      val pmpcfg_ = pmpcfg
      val boundLo0 = boundLo(U"4'x0")
      val boundHi0 = boundHi(U"4'x0")
      val boundLo1 = boundLo(U"4'x1")
      val boundHi1 = boundHi(U"4'x1")
      val boundLo2 = boundLo(U"4'x2")
      val boundHi2 = boundHi(U"4'x2")
      val boundLo3 = boundLo(U"4'x3")
      val boundHi3 = boundHi(U"4'x3")
      val boundLo4 = boundLo(U"4'x4")
      val boundHi4 = boundHi(U"4'x4")
      val boundLo5 = boundLo(U"4'x5")
      val boundHi5 = boundHi(U"4'x5")
      val boundLo6 = boundLo(U"4'x6")
      val boundHi6 = boundHi(U"4'x6")
      val boundLo7 = boundLo(U"4'x7")
      val boundHi7 = boundHi(U"4'x7")
      val boundLo8 = boundLo(U"4'x8")
      val boundHi8 = boundHi(U"4'x8")
      val boundLo9 = boundLo(U"4'x9")
      val boundHi9 = boundHi(U"4'x9")
      val boundLo10 = boundLo(U"4'xa")
      val boundHi10 = boundHi(U"4'xa")
      val boundLo11 = boundLo(U"4'xb")
      val boundHi11 = boundHi(U"4'xb")
      val boundLo12 = boundLo(U"4'xc")
      val boundHi12 = boundHi(U"4'xc")
      val boundLo13 = boundLo(U"4'xd")
      val boundHi13 = boundHi(U"4'xd")
      val boundLo14 = boundLo(U"4'xe")
      val boundHi14 = boundHi(U"4'xe")
      val boundLo15 = boundLo(U"4'xf")
      val boundHi15 = boundHi(U"4'xf")

      // copied from CSR plugin
      val csrAddress = input(INSTRUCTION)(csrRange)
      val addrAccess = csrAddress(11 downto 4) === 0x3b
      val cfgAccess = csrAddress(11 downto 4) === 0x3a
      val pmpWrite = arbitration.isValid && input(IS_CSR_) && input(CSR_WRITE_OPCODE_) & (addrAccess | cfgAccess)
      val pmpRead = arbitration.isValid && input(IS_CSR_) && input(CSR_READ_OPCODE_) & (addrAccess | cfgAccess)
      val writeEnable = pmpWrite & !arbitration.isStuck
      val readEnable = pmpRead & !arbitration.isStuck

      // TODO: support masked CSR operations
      val ioWritePayload = input(SRC1)

      // TODO: rename
      val ioIndex = csrAddress(3 downto 0).asUInt
      val ioConfig = cfgAccess
      val ioWriteValid = writeEnable

      val cfgSelect = ioIndex(1 downto 0)
      val pmpcfgN = cfgRegister(cfgSelect)
      val pmpNcfg = pmpcfgN.subdivideIn(8 bits)
      
      val csr = new Area {
        when (pmpRead | pmpWrite) {
          when (ioConfig) {
            when (ioWriteValid) {
              switch(cfgSelect) {
                for (i <- 0 until (regions / 4)) {
                  is(i) {
                    for (j <- Range(0, xlen, 8)) {
                      val bitRange = j + xlen * i + lBit downto j + xlen * i
                      val overwrite = ioWritePayload.subdivideIn(8 bits)(j / 8)
                      val locked = cfgRegister(i).subdivideIn(8 bits)(j / 8)(lBit)
                      when (~locked) {
                        pmpcfg(bitRange).assignFromBits(overwrite)
                        if (j != 0 || i != 0) {
                          when (overwrite(lBit) & overwrite(aBits) === TOR) {
                            pmpcfg(j + xlen * i - 1) := True
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            output(REGFILE_WRITE_DATA).assignFromBits(pmpcfgN)
          } otherwise {
            when (ioWriteValid) {
              val lock = pmpNcfg(ioIndex(1 downto 0))(lBit)
              pmpaddr.write(
                ioIndex,
                ioWritePayload.asUInt,
                ioWriteValid & ~ioConfig & ~lock
              )
            }
            output(REGFILE_WRITE_DATA) := pmpaddr.readAsync(ioIndex).asBits
          }
        }
      }
      
      val controller = new Area {
        val enable = RegInit(False)
        val counter = Reg(UInt(log2Up(regions) bits)) init(0)
        val setNext = RegInit(False)
        
        when (pmpWrite | enable) {
          when (ioConfig) {
            when (~enable) {
              enable := True
              arbitration.haltItself := True
              counter := ioIndex(1 downto 0) @@ U"2'00"
            } otherwise {
              counter := counter + 1
              when (counter(1 downto 0) === 3) {
                enable := False
              } otherwise {
                arbitration.haltItself := True
              }
            }
          } otherwise {
            when (~enable) {
              enable := True
              counter := ioIndex
              arbitration.haltItself := True
              when (ioIndex =/= (regions - 1)) {
                setNext := True
              } otherwise {
                setNext := False
              }
            }.elsewhen (setNext) {
              arbitration.haltItself := True
              counter := counter + 1
              setNext := False
            } otherwise {
              enable := False
            }
          }
        }

        when (ioConfig) {
          setter.io.a := ioWritePayload.subdivideIn(8 bits)(counter(1 downto 0))(aBits)
          setter.io.addr := pmpaddr(counter)
        } otherwise {
          setter.io.a := cfgRegion(counter)(aBits)
          when (setNext) {
            setter.io.addr := ioWritePayload.asUInt
          } otherwise {
            setter.io.addr := pmpaddr(counter)
          }
        }

        when (counter === 0) {
          setter.io.prevHi := 0
        } otherwise {
          setter.io.prevHi := boundHi(counter - 1)
        }
        when (enable & ~cfgRegion(counter)(lBit)) {
          boundLo(counter) := setter.io.boundLo
          boundHi(counter) := setter.io.boundHi
        }
      }
    
    }

    pipeline plug new Area {
      for (port <- ports) yield new Area {
        val address = port.bus.cmd(0).virtualAddress
        port.bus.rsp.physicalAddress := address
        port.bus.rsp.isIoAccess := ioRange(address)
        port.bus.rsp.isPaging := False
        port.bus.rsp.exception := False
        port.bus.rsp.refilling := False
        port.bus.busy := False

        val machine = privilegeService.isMachine()
        val floor = address(31 downto 2)
        
        port.bus.rsp.allowRead := machine
        port.bus.rsp.allowWrite := machine
        port.bus.rsp.allowExecute := machine
        
        for (i <- regions - 1 to 0 by -1) {
          when (floor >= boundLo(U(i, log2Up(regions) bits)) & floor < boundHi(U(i, log2Up(regions) bits))) {
            port.bus.rsp.isPaging := True
            when ((cfgRegion(i)(lBit) | ~machine) & cfgRegion(i)(aBits) =/= 0) {
              port.bus.rsp.allowRead := cfgRegion(i)(rBit)
              port.bus.rsp.allowWrite := cfgRegion(i)(wBit)
              port.bus.rsp.allowExecute := cfgRegion(i)(xBit)
            }
          }
        }
      }
    }
  }
}