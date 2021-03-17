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

case class PmpInfo(preset : Bits = B"8'0") extends Bundle {
  val r = preset(0)
  val w = preset(1)
  val x = preset(2)
  val a = preset(4 downto 3)
  val l = preset(7)

  override def asBits : Bits = {
    B(8 bits, 7 -> l, (4 downto 3) -> a, 2 -> x, 1 -> w, 0 -> r)
  }
}

case class PmpConf(preset : Bits = B"32'0") extends Bundle {
  val infos = preset.subdivideIn(8 bits).reverse.map(x => new PmpInfo(x))

  def asMask : Bits = {
    val locks = infos.map(info => info.l)
    locks.map(l => ~(l ## l ## l ## l ## l ## l ## l ## l))
      .reverse.foldLeft(Bits(0 bits))(_ ## _)
  }

  override def asBits : Bits = {
    infos.map(info => info.asBits).reverse.foldLeft(Bits(0 bits))(_ ## _)
  }
}

class Pmp(configs : Int) extends Component {
  assert(configs % 4 == 0)

  val io = new Bundle {
    val enable, write, sel = in Bool
    val index = in UInt(4 bits)
    val readAddr = out Bits(32 bits)
    val readConf = out Bits(32 bits)
    val writeAddr = in Bits(32 bits)
    val writeConf = in Bits(32 bits)
  }

  val pmpConfs = Mem(new PmpConf(), configs / 4)
  val pmpAddrs = Mem(Bits(32 bits), configs)

  val pmpConf = pmpConfs.readAsync(io.index(3 downto 2), readFirst)
  io.readConf := pmpConf.asBits

  pmpConfs.write(
    io.index(3 downto 2),
    new PmpConf(io.writeConf),
    io.enable & io.write & io.sel,
    pmpConf.asMask
  )

  val locked = pmpConf.infos(io.index(1 downto 0)).l
  io.readAddr := pmpAddrs.readWriteSync(
    io.index,
    io.writeAddr,
    io.enable,
    io.write & ~locked & ~io.sel
  )

}

/*
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
    val valid, locked = RegInit(False)
    val start, end = Reg(UInt(32 bits)) init(0)
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
  val napotStart = (state.addr ^ mask) |<< 2
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
*/

case class ProtectedMemoryTranslatorPort(bus : MemoryTranslatorBus)

class PmpPlugin(regions : Int, ioRange : UInt => Bool) extends Plugin[VexRiscv] with MemoryTranslator {

  // Each pmpcfg# CSR configures four regions.
  assert((regions % 4) == 0)
  
  def pmpcfg0 = 0x3a0
  def pmpaddr0 = 0x3b0
  
  //val pmps = ArrayBuffer[Pmp]()
  val dPorts = ArrayBuffer[ProtectedMemoryTranslatorPort]()
  val iPorts = ArrayBuffer[ProtectedMemoryTranslatorPort]()

  val pmp = new Pmp(regions)

  override def newTranslationPort(priority : Int, args : Any): MemoryTranslatorBus = {
    val port = ProtectedMemoryTranslatorPort(MemoryTranslatorBus(new MemoryTranslatorBusParameter(0, 0)))
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
        csrService.r(pmpaddr0 + region, pmp.io.readAddr)
        csrService.w(pmpaddr0 + region, pmp.io.writeAddr)
        csrService.onWrite(pmpaddr0 + region) {
          pmp.io.sel := False
          pmp.io.enable := True
          pmp.io.write := False
        }
        csrService.onWrite(pmpaddr0 + region) {
          pmp.io.sel := True
          pmp.io.enable := True
          pmp.io.write := True
        }
      }

      // Instantiate pmpcfg0 ... pmpcfg# CSRs.
      for (pmpNcfg <- Range(0, regions, 4)) {
        csrService.r(pmpcfg0 + pmpNcfg / 4, pmp.io.readConf)
        csrService.w(pmpcfg0 + pmpNcfg / 4, pmp.io.writeConf)
        csrService.onWrite(pmpcfg0 + pmpNcfg / 4) {
          pmp.io.sel := False
          pmp.io.enable := True
          pmp.io.write := False
        }
        csrService.onWrite(pmpcfg0 + pmpNcfg / 4) {
          pmp.io.sel := True
          pmp.io.enable := True
          pmp.io.write := True
        }
      }

      /*
      def check(address : UInt) : ArrayBuffer[Bool] = {
        pmps.map(pmp => pmp.region.valid &
                        pmp.region.start <= address &
                        pmp.region.end > address &
                       (pmp.region.locked | ~privilegeService.isMachine()))
      }

      for (port <- (dPorts ++ iPorts)) yield new Area {
        port.bus.rsp.physicalAddress := port.bus.cmd(0).virtualAddress
        port.bus.rsp.isIoAccess := ioRange(port.bus.rsp.physicalAddress)
        port.bus.rsp.isPaging := False
        port.bus.rsp.exception := False
        port.bus.rsp.refilling := False
        port.bus.busy := False
      }

      // Only PMP R/W rules apply to data ports. X is not allowed.
      for (port <- dPorts) yield new Area {
        port.bus.rsp.allowExecute := False

        val hits = check(port.bus.cmd(0).virtualAddress)
        when(~hits.orR) {
          port.bus.rsp.allowRead := privilegeService.isMachine()
          port.bus.rsp.allowWrite := privilegeService.isMachine()
        } otherwise {
          val firstHit = OHMasking.first(hits)
          port.bus.rsp.allowRead := MuxOH(firstHit, pmps.map(_.state.r))
          port.bus.rsp.allowWrite := MuxOH(firstHit, pmps.map(_.state.w))
        }
      }

      // Only PMP X rules apply to instruction ports. R/W are not allowed.
      for (port <- iPorts) yield new Area {
        port.bus.rsp.allowRead := False
        port.bus.rsp.allowWrite := False

        val hits = check(port.bus.cmd(0).virtualAddress)
        when(~hits.orR) {
          port.bus.rsp.allowExecute := privilegeService.isMachine()
        } otherwise {
          port.bus.rsp.allowExecute := MuxOH(OHMasking.first(hits), pmps.map(_.state.x))
        }
      }
      */
    }
  }
}
