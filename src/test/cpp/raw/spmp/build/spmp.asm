
build/spmp.elf:     file format elf32-littleriscv


Disassembly of section .crt_section:

80000000 <_start>:
80000000:	fff00e13          	li	t3,-1
80000004:	00000e97          	auipc	t4,0x0
80000008:	1c0e8e93          	addi	t4,t4,448 # 800001c4 <fail>
8000000c:	00000f13          	li	t5,0
80000010:	00000317          	auipc	t1,0x0
80000014:	1b430313          	addi	t1,t1,436 # 800001c4 <fail>
80000018:	00000393          	li	t2,0
8000001c:	00000097          	auipc	ra,0x0
80000020:	05808093          	addi	ra,ra,88 # 80000074 <mtrap>
80000024:	30509073          	csrw	mtvec,ra
80000028:	0000b0b7          	lui	ra,0xb
8000002c:	30309073          	csrw	mideleg,ra
80000030:	000010b7          	lui	ra,0x1
80000034:	f0d08093          	addi	ra,ra,-243 # f0d <_start-0x7ffff0f3>
80000038:	3a009073          	csrw	pmpcfg0,ra
8000003c:	200000b7          	lui	ra,0x20000
80000040:	3b009073          	csrw	pmpaddr0,ra
80000044:	fff00093          	li	ra,-1
80000048:	3b109073          	csrw	pmpaddr1,ra
8000004c:	00000097          	auipc	ra,0x0
80000050:	04808093          	addi	ra,ra,72 # 80000094 <test0>
80000054:	34109073          	csrw	mepc,ra
80000058:	000020b7          	lui	ra,0x2
8000005c:	80008093          	addi	ra,ra,-2048 # 1800 <_start-0x7fffe800>
80000060:	3000b073          	csrc	mstatus,ra
80000064:	000010b7          	lui	ra,0x1
80000068:	80008093          	addi	ra,ra,-2048 # 800 <_start-0x7ffff800>
8000006c:	3000a073          	csrs	mstatus,ra
80000070:	30200073          	mret

80000074 <mtrap>:
80000074:	342020f3          	csrr	ra,mcause
80000078:	15e09663          	bne	ra,t5,800001c4 <fail>
8000007c:	341e9073          	csrw	mepc,t4
80000080:	30200073          	mret

80000084 <strap>:
80000084:	142020f3          	csrr	ra,scause
80000088:	12709e63          	bne	ra,t2,800001c4 <fail>
8000008c:	14131073          	csrw	sepc,t1
80000090:	10200073          	sret

80000094 <test0>:
80000094:	00000e13          	li	t3,0
80000098:	00000e97          	auipc	t4,0x0
8000009c:	12ce8e93          	addi	t4,t4,300 # 800001c4 <fail>
800000a0:	00000317          	auipc	t1,0x0
800000a4:	12430313          	addi	t1,t1,292 # 800001c4 <fail>
800000a8:	00000097          	auipc	ra,0x0
800000ac:	fdc08093          	addi	ra,ra,-36 # 80000084 <strap>
800000b0:	10509073          	csrw	stvec,ra
800000b4:	1b1a20b7          	lui	ra,0x1b1a2
800000b8:	91808093          	addi	ra,ra,-1768 # 1b1a1918 <_start-0x64e5e6e8>
800000bc:	90009073          	csrw	0x900,ra
800000c0:	1f1e20b7          	lui	ra,0x1f1e2
800000c4:	d1c08093          	addi	ra,ra,-740 # 1f1e1d1c <_start-0x60e1e2e4>
800000c8:	90109073          	csrw	0x901,ra
800000cc:	9b9aa0b7          	lui	ra,0x9b9aa
800000d0:	99808093          	addi	ra,ra,-1640 # 9b9a9998 <pass+0x1b9a97c8>
800000d4:	90209073          	csrw	0x902,ra
800000d8:	9f9ea0b7          	lui	ra,0x9f9ea
800000dc:	d9c08093          	addi	ra,ra,-612 # 9f9e9d9c <pass+0x1f9e9bcc>
800000e0:	90309073          	csrw	0x903,ra
800000e4:	220000b7          	lui	ra,0x22000
800000e8:	91009073          	csrw	0x910,ra
800000ec:	220000b7          	lui	ra,0x22000
800000f0:	00808093          	addi	ra,ra,8 # 22000008 <_start-0x5dfffff8>
800000f4:	91109073          	csrw	0x911,ra
800000f8:	220000b7          	lui	ra,0x22000
800000fc:	01008093          	addi	ra,ra,16 # 22000010 <_start-0x5dfffff0>
80000100:	91209073          	csrw	0x912,ra
80000104:	220000b7          	lui	ra,0x22000
80000108:	01808093          	addi	ra,ra,24 # 22000018 <_start-0x5dffffe8>
8000010c:	91309073          	csrw	0x913,ra
80000110:	220000b7          	lui	ra,0x22000
80000114:	02008093          	addi	ra,ra,32 # 22000020 <_start-0x5dffffe0>
80000118:	91409073          	csrw	0x914,ra
8000011c:	220000b7          	lui	ra,0x22000
80000120:	02808093          	addi	ra,ra,40 # 22000028 <_start-0x5dffffd8>
80000124:	91509073          	csrw	0x915,ra
80000128:	220000b7          	lui	ra,0x22000
8000012c:	03008093          	addi	ra,ra,48 # 22000030 <_start-0x5dffffd0>
80000130:	91609073          	csrw	0x916,ra
80000134:	220000b7          	lui	ra,0x22000
80000138:	03808093          	addi	ra,ra,56 # 22000038 <_start-0x5dffffc8>
8000013c:	91709073          	csrw	0x917,ra
80000140:	220000b7          	lui	ra,0x22000
80000144:	04008093          	addi	ra,ra,64 # 22000040 <_start-0x5dffffc0>
80000148:	91809073          	csrw	0x918,ra
8000014c:	220000b7          	lui	ra,0x22000
80000150:	04808093          	addi	ra,ra,72 # 22000048 <_start-0x5dffffb8>
80000154:	91909073          	csrw	0x919,ra
80000158:	220000b7          	lui	ra,0x22000
8000015c:	05008093          	addi	ra,ra,80 # 22000050 <_start-0x5dffffb0>
80000160:	91009073          	csrw	0x910,ra
80000164:	220000b7          	lui	ra,0x22000
80000168:	05808093          	addi	ra,ra,88 # 22000058 <_start-0x5dffffa8>
8000016c:	91a09073          	csrw	0x91a,ra
80000170:	220000b7          	lui	ra,0x22000
80000174:	06008093          	addi	ra,ra,96 # 22000060 <_start-0x5dffffa0>
80000178:	91b09073          	csrw	0x91b,ra
8000017c:	220000b7          	lui	ra,0x22000
80000180:	06808093          	addi	ra,ra,104 # 22000068 <_start-0x5dffff98>
80000184:	91c09073          	csrw	0x91c,ra
80000188:	220000b7          	lui	ra,0x22000
8000018c:	07008093          	addi	ra,ra,112 # 22000070 <_start-0x5dffff90>
80000190:	91d09073          	csrw	0x91d,ra
80000194:	220000b7          	lui	ra,0x22000
80000198:	07808093          	addi	ra,ra,120 # 22000078 <_start-0x5dffff88>
8000019c:	91e09073          	csrw	0x91e,ra
800001a0:	00000e97          	auipc	t4,0x0
800001a4:	014e8e93          	addi	t4,t4,20 # 800001b4 <test1>
800001a8:	00200f13          	li	t5,2
800001ac:	30200073          	mret
800001b0:	0140006f          	j	800001c4 <fail>

800001b4 <test1>:
800001b4:	01c0006f          	j	800001d0 <pass>

800001b8 <test2>:
800001b8:	0180006f          	j	800001d0 <pass>

800001bc <test3>:
800001bc:	0140006f          	j	800001d0 <pass>

800001c0 <test4>:
800001c0:	0100006f          	j	800001d0 <pass>

800001c4 <fail>:
800001c4:	f0100137          	lui	sp,0xf0100
800001c8:	f2410113          	addi	sp,sp,-220 # f00fff24 <pass+0x700ffd54>
800001cc:	01c12023          	sw	t3,0(sp)

800001d0 <pass>:
800001d0:	f0100137          	lui	sp,0xf0100
800001d4:	f2010113          	addi	sp,sp,-224 # f00fff20 <pass+0x700ffd50>
800001d8:	00012023          	sw	zero,0(sp)
