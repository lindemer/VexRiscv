
build/spmp.elf:     file format elf32-littleriscv


Disassembly of section .crt_section:

80000000 <_start>:
80000000:	fff00e13          	li	t3,-1
80000004:	00000f17          	auipc	t5,0x0
80000008:	0ccf0f13          	addi	t5,t5,204 # 800000d0 <fail>
8000000c:	00000e97          	auipc	t4,0x0
80000010:	0c4e8e93          	addi	t4,t4,196 # 800000d0 <fail>
80000014:	00000097          	auipc	ra,0x0
80000018:	05808093          	addi	ra,ra,88 # 8000006c <mtrap>
8000001c:	30509073          	csrw	mtvec,ra
80000020:	0000b0b7          	lui	ra,0xb
80000024:	30309073          	csrw	mideleg,ra
80000028:	000010b7          	lui	ra,0x1
8000002c:	f0d08093          	addi	ra,ra,-243 # f0d <_start-0x7ffff0f3>
80000030:	3a009073          	csrw	pmpcfg0,ra
80000034:	200000b7          	lui	ra,0x20000
80000038:	3b009073          	csrw	pmpaddr0,ra
8000003c:	fff00093          	li	ra,-1
80000040:	3b109073          	csrw	pmpaddr1,ra
80000044:	00000097          	auipc	ra,0x0
80000048:	03808093          	addi	ra,ra,56 # 8000007c <test0>
8000004c:	34109073          	csrw	mepc,ra
80000050:	000020b7          	lui	ra,0x2
80000054:	80008093          	addi	ra,ra,-2048 # 1800 <_start-0x7fffe800>
80000058:	3000b073          	csrc	mstatus,ra
8000005c:	000010b7          	lui	ra,0x1
80000060:	80008093          	addi	ra,ra,-2048 # 800 <_start-0x7ffff800>
80000064:	3000a073          	csrs	mstatus,ra
80000068:	30200073          	mret

8000006c <mtrap>:
8000006c:	341f1073          	csrw	mepc,t5
80000070:	30200073          	mret

80000074 <strap>:
80000074:	141e9073          	csrw	sepc,t4
80000078:	10200073          	sret

8000007c <test0>:
8000007c:	00000e13          	li	t3,0
80000080:	00000f17          	auipc	t5,0x0
80000084:	050f0f13          	addi	t5,t5,80 # 800000d0 <fail>
80000088:	00000e97          	auipc	t4,0x0
8000008c:	048e8e93          	addi	t4,t4,72 # 800000d0 <fail>
80000090:	00000097          	auipc	ra,0x0
80000094:	fe408093          	addi	ra,ra,-28 # 80000074 <strap>
80000098:	10509073          	csrw	stvec,ra
8000009c:	00000e97          	auipc	t4,0x0
800000a0:	034e8e93          	addi	t4,t4,52 # 800000d0 <fail>
800000a4:	00000093          	li	ra,0
800000a8:	90009073          	csrw	0x900,ra
800000ac:	00000093          	li	ra,0
800000b0:	91009073          	csrw	0x910,ra
800000b4:	00000093          	li	ra,0
800000b8:	91109073          	csrw	0x911,ra
800000bc:	00000f17          	auipc	t5,0x0
800000c0:	010f0f13          	addi	t5,t5,16 # 800000cc <test1>
800000c4:	30200073          	mret
800000c8:	0080006f          	j	800000d0 <fail>

800000cc <test1>:
800000cc:	0100006f          	j	800000dc <pass>

800000d0 <fail>:
800000d0:	f0100137          	lui	sp,0xf0100
800000d4:	f2410113          	addi	sp,sp,-220 # f00fff24 <pass+0x700ffe48>
800000d8:	01c12023          	sw	t3,0(sp)

800000dc <pass>:
800000dc:	f0100137          	lui	sp,0xf0100
800000e0:	f2010113          	addi	sp,sp,-224 # f00fff20 <pass+0x700ffe44>
800000e4:	00012023          	sw	zero,0(sp)
