
build/mpu.elf:     file format elf32-littleriscv


Disassembly of section .crt_section:

80000000 <_start>:
80000000:	fff00e13          	li	t3,-1
80000004:	00000e97          	auipc	t4,0x0
80000008:	3c4e8e93          	addi	t4,t4,964 # 800003c8 <fail>
8000000c:	00000f13          	li	t5,0
80000010:	00000397          	auipc	t2,0x0
80000014:	3b838393          	addi	t2,t2,952 # 800003c8 <fail>
80000018:	00000293          	li	t0,0
8000001c:	00000317          	auipc	t1,0x0
80000020:	06030313          	addi	t1,t1,96 # 8000007c <mtrap>
80000024:	30531073          	csrw	mtvec,t1
80000028:	0000b337          	lui	t1,0xb
8000002c:	30231073          	csrw	medeleg,t1
80000030:	00002337          	lui	t1,0x2
80000034:	f1d30313          	addi	t1,t1,-227 # 1f1d <_start-0x7fffe0e3>
80000038:	3a031073          	csrw	pmpcfg0,t1
8000003c:	20000337          	lui	t1,0x20000
80000040:	fff30313          	addi	t1,t1,-1 # 1fffffff <_start-0x60000001>
80000044:	3b031073          	csrw	pmpaddr0,t1
80000048:	30000337          	lui	t1,0x30000
8000004c:	fff30313          	addi	t1,t1,-1 # 2fffffff <_start-0x50000001>
80000050:	3b131073          	csrw	pmpaddr1,t1
80000054:	00000317          	auipc	t1,0x0
80000058:	04830313          	addi	t1,t1,72 # 8000009c <test0>
8000005c:	34131073          	csrw	mepc,t1
80000060:	00002337          	lui	t1,0x2
80000064:	80030313          	addi	t1,t1,-2048 # 1800 <_start-0x7fffe800>
80000068:	30033073          	csrc	mstatus,t1
8000006c:	00001337          	lui	t1,0x1
80000070:	80030313          	addi	t1,t1,-2048 # 800 <_start-0x7ffff800>
80000074:	30032073          	csrs	mstatus,t1
80000078:	30200073          	mret

8000007c <mtrap>:
8000007c:	34202373          	csrr	t1,mcause
80000080:	35e31463          	bne	t1,t5,800003c8 <fail>
80000084:	341e9073          	csrw	mepc,t4
80000088:	30200073          	mret

8000008c <strap>:
8000008c:	14202373          	csrr	t1,scause
80000090:	32531c63          	bne	t1,t0,800003c8 <fail>
80000094:	14139073          	csrw	sepc,t2
80000098:	10200073          	sret

8000009c <test0>:
8000009c:	00000e13          	li	t3,0
800000a0:	00000e97          	auipc	t4,0x0
800000a4:	328e8e93          	addi	t4,t4,808 # 800003c8 <fail>
800000a8:	00000397          	auipc	t2,0x0
800000ac:	32038393          	addi	t2,t2,800 # 800003c8 <fail>
800000b0:	00000317          	auipc	t1,0x0
800000b4:	fdc30313          	addi	t1,t1,-36 # 8000008c <strap>
800000b8:	10531073          	csrw	stvec,t1
800000bc:	1b1a2337          	lui	t1,0x1b1a2
800000c0:	91830313          	addi	t1,t1,-1768 # 1b1a1918 <_start-0x64e5e6e8>
800000c4:	90031073          	csrw	0x900,t1
800000c8:	1f1e2337          	lui	t1,0x1f1e2
800000cc:	d1c30313          	addi	t1,t1,-740 # 1f1e1d1c <_start-0x60e1e2e4>
800000d0:	90131073          	csrw	0x901,t1
800000d4:	9b9aa337          	lui	t1,0x9b9aa
800000d8:	99830313          	addi	t1,t1,-1640 # 9b9a9998 <pass+0x1b9a95c4>
800000dc:	90231073          	csrw	0x902,t1
800000e0:	9f9ea337          	lui	t1,0x9f9ea
800000e4:	d9c30313          	addi	t1,t1,-612 # 9f9e9d9c <pass+0x1f9e99c8>
800000e8:	90331073          	csrw	0x903,t1
800000ec:	22001337          	lui	t1,0x22001
800000f0:	fff30313          	addi	t1,t1,-1 # 22000fff <_start-0x5dfff001>
800000f4:	91031073          	csrw	0x910,t1
800000f8:	22002337          	lui	t1,0x22002
800000fc:	fff30313          	addi	t1,t1,-1 # 22001fff <_start-0x5dffe001>
80000100:	91131073          	csrw	0x911,t1
80000104:	22003337          	lui	t1,0x22003
80000108:	fff30313          	addi	t1,t1,-1 # 22002fff <_start-0x5dffd001>
8000010c:	91231073          	csrw	0x912,t1
80000110:	22004337          	lui	t1,0x22004
80000114:	fff30313          	addi	t1,t1,-1 # 22003fff <_start-0x5dffc001>
80000118:	91331073          	csrw	0x913,t1
8000011c:	22005337          	lui	t1,0x22005
80000120:	fff30313          	addi	t1,t1,-1 # 22004fff <_start-0x5dffb001>
80000124:	91431073          	csrw	0x914,t1
80000128:	22006337          	lui	t1,0x22006
8000012c:	fff30313          	addi	t1,t1,-1 # 22005fff <_start-0x5dffa001>
80000130:	91531073          	csrw	0x915,t1
80000134:	22007337          	lui	t1,0x22007
80000138:	fff30313          	addi	t1,t1,-1 # 22006fff <_start-0x5dff9001>
8000013c:	91631073          	csrw	0x916,t1
80000140:	22008337          	lui	t1,0x22008
80000144:	fff30313          	addi	t1,t1,-1 # 22007fff <_start-0x5dff8001>
80000148:	91731073          	csrw	0x917,t1
8000014c:	22009337          	lui	t1,0x22009
80000150:	fff30313          	addi	t1,t1,-1 # 22008fff <_start-0x5dff7001>
80000154:	91831073          	csrw	0x918,t1
80000158:	2200a337          	lui	t1,0x2200a
8000015c:	fff30313          	addi	t1,t1,-1 # 22009fff <_start-0x5dff6001>
80000160:	91931073          	csrw	0x919,t1
80000164:	2200b337          	lui	t1,0x2200b
80000168:	fff30313          	addi	t1,t1,-1 # 2200afff <_start-0x5dff5001>
8000016c:	91a31073          	csrw	0x91a,t1
80000170:	2200c337          	lui	t1,0x2200c
80000174:	fff30313          	addi	t1,t1,-1 # 2200bfff <_start-0x5dff4001>
80000178:	91b31073          	csrw	0x91b,t1
8000017c:	2200d337          	lui	t1,0x2200d
80000180:	fff30313          	addi	t1,t1,-1 # 2200cfff <_start-0x5dff3001>
80000184:	91c31073          	csrw	0x91c,t1
80000188:	2200e337          	lui	t1,0x2200e
8000018c:	fff30313          	addi	t1,t1,-1 # 2200dfff <_start-0x5dff2001>
80000190:	91d31073          	csrw	0x91d,t1
80000194:	2200f337          	lui	t1,0x2200f
80000198:	fff30313          	addi	t1,t1,-1 # 2200efff <_start-0x5dff1001>
8000019c:	91e31073          	csrw	0x91e,t1
800001a0:	22010337          	lui	t1,0x22010
800001a4:	fff30313          	addi	t1,t1,-1 # 2200ffff <_start-0x5dff0001>
800001a8:	91f31073          	csrw	0x91f,t1
800001ac:	00000e97          	auipc	t4,0x0
800001b0:	014e8e93          	addi	t4,t4,20 # 800001c0 <test1>
800001b4:	00200f13          	li	t5,2
800001b8:	30200073          	mret
800001bc:	20c0006f          	j	800003c8 <fail>

800001c0 <test1>:
800001c0:	00100e13          	li	t3,1
800001c4:	00000e97          	auipc	t4,0x0
800001c8:	204e8e93          	addi	t4,t4,516 # 800003c8 <fail>
800001cc:	00000397          	auipc	t2,0x0
800001d0:	1fc38393          	addi	t2,t2,508 # 800003c8 <fail>
800001d4:	88000337          	lui	t1,0x88000
800001d8:	00032383          	lw	t2,0(t1) # 88000000 <pass+0x7fffc2c>
800001dc:	00732023          	sw	t2,0(t1)
800001e0:	88004337          	lui	t1,0x88004
800001e4:	00032383          	lw	t2,0(t1) # 88004000 <pass+0x8003c2c>
800001e8:	00732023          	sw	t2,0(t1)
800001ec:	88008337          	lui	t1,0x88008
800001f0:	00032383          	lw	t2,0(t1) # 88008000 <pass+0x8007c2c>
800001f4:	00732023          	sw	t2,0(t1)
800001f8:	8800c337          	lui	t1,0x8800c
800001fc:	00032383          	lw	t2,0(t1) # 8800c000 <pass+0x800bc2c>
80000200:	00732023          	sw	t2,0(t1)
80000204:	88010337          	lui	t1,0x88010
80000208:	00032383          	lw	t2,0(t1) # 88010000 <pass+0x800fc2c>
8000020c:	00732023          	sw	t2,0(t1)
80000210:	88014337          	lui	t1,0x88014
80000214:	00032383          	lw	t2,0(t1) # 88014000 <pass+0x8013c2c>
80000218:	00732023          	sw	t2,0(t1)
8000021c:	88018337          	lui	t1,0x88018
80000220:	00032383          	lw	t2,0(t1) # 88018000 <pass+0x8017c2c>
80000224:	00732023          	sw	t2,0(t1)
80000228:	8801c337          	lui	t1,0x8801c
8000022c:	00032383          	lw	t2,0(t1) # 8801c000 <pass+0x801bc2c>
80000230:	00732023          	sw	t2,0(t1)
80000234:	00000e97          	auipc	t4,0x0
80000238:	01ce8e93          	addi	t4,t4,28 # 80000250 <test2>
8000023c:	00700f13          	li	t5,7
80000240:	80000337          	lui	t1,0x80000
80000244:	ffc30313          	addi	t1,t1,-4 # 7ffffffc <pass+0xfffffc28>
80000248:	00732023          	sw	t2,0(t1)
8000024c:	17c0006f          	j	800003c8 <fail>

80000250 <test2>:
80000250:	00200e13          	li	t3,2
80000254:	00000e97          	auipc	t4,0x0
80000258:	174e8e93          	addi	t4,t4,372 # 800003c8 <fail>
8000025c:	00000397          	auipc	t2,0x0
80000260:	01838393          	addi	t2,t2,24 # 80000274 <test3>
80000264:	00f00293          	li	t0,15
80000268:	88028337          	lui	t1,0x88028
8000026c:	00732023          	sw	t2,0(t1) # 88028000 <pass+0x8027c2c>
80000270:	1580006f          	j	800003c8 <fail>

80000274 <test3>:
80000274:	00300e13          	li	t3,3
80000278:	00000e97          	auipc	t4,0x0
8000027c:	150e8e93          	addi	t4,t4,336 # 800003c8 <fail>
80000280:	00000397          	auipc	t2,0x0
80000284:	01838393          	addi	t2,t2,24 # 80000298 <test4>
80000288:	00d00293          	li	t0,13
8000028c:	88024337          	lui	t1,0x88024
80000290:	00032383          	lw	t2,0(t1) # 88024000 <pass+0x8023c2c>
80000294:	1340006f          	j	800003c8 <fail>

80000298 <test4>:
80000298:	00400e13          	li	t3,4
8000029c:	00000e97          	auipc	t4,0x0
800002a0:	12ce8e93          	addi	t4,t4,300 # 800003c8 <fail>
800002a4:	00000397          	auipc	t2,0x0
800002a8:	01838393          	addi	t2,t2,24 # 800002bc <test5>
800002ac:	00c00293          	li	t0,12
800002b0:	88030337          	lui	t1,0x88030
800002b4:	00030067          	jr	t1 # 88030000 <pass+0x802fc2c>
800002b8:	1100006f          	j	800003c8 <fail>

800002bc <test5>:
800002bc:	00500e13          	li	t3,5
800002c0:	00000e97          	auipc	t4,0x0
800002c4:	108e8e93          	addi	t4,t4,264 # 800003c8 <fail>
800002c8:	00000397          	auipc	t2,0x0
800002cc:	10038393          	addi	t2,t2,256 # 800003c8 <fail>
800002d0:	00000317          	auipc	t1,0x0
800002d4:	01830313          	addi	t1,t1,24 # 800002e8 <test6>
800002d8:	14131073          	csrw	sepc,t1
800002dc:	08000313          	li	t1,128
800002e0:	10033073          	csrc	sstatus,t1
800002e4:	10200073          	sret

800002e8 <test6>:
800002e8:	00600e13          	li	t3,6
800002ec:	00000e97          	auipc	t4,0x0
800002f0:	0dce8e93          	addi	t4,t4,220 # 800003c8 <fail>
800002f4:	00000397          	auipc	t2,0x0
800002f8:	0d438393          	addi	t2,t2,212 # 800003c8 <fail>
800002fc:	88004337          	lui	t1,0x88004
80000300:	00032383          	lw	t2,0(t1) # 88004000 <pass+0x8003c2c>
80000304:	88008337          	lui	t1,0x88008
80000308:	00732023          	sw	t2,0(t1) # 88008000 <pass+0x8007c2c>
8000030c:	8800c337          	lui	t1,0x8800c
80000310:	00032383          	lw	t2,0(t1) # 8800c000 <pass+0x800bc2c>
80000314:	00732023          	sw	t2,0(t1)
80000318:	88014337          	lui	t1,0x88014
8000031c:	00032383          	lw	t2,0(t1) # 88014000 <pass+0x8013c2c>
80000320:	88018337          	lui	t1,0x88018
80000324:	00732023          	sw	t2,0(t1) # 88018000 <pass+0x8017c2c>
80000328:	8801c337          	lui	t1,0x8801c
8000032c:	00032383          	lw	t2,0(t1) # 8801c000 <pass+0x801bc2c>
80000330:	00732023          	sw	t2,0(t1)
80000334:	88024337          	lui	t1,0x88024
80000338:	00032383          	lw	t2,0(t1) # 88024000 <pass+0x8023c2c>
8000033c:	88028337          	lui	t1,0x88028
80000340:	00732023          	sw	t2,0(t1) # 88028000 <pass+0x8027c2c>
80000344:	8802c337          	lui	t1,0x8802c
80000348:	00032383          	lw	t2,0(t1) # 8802c000 <pass+0x802bc2c>
8000034c:	00732023          	sw	t2,0(t1)
80000350:	88034337          	lui	t1,0x88034
80000354:	00032383          	lw	t2,0(t1) # 88034000 <pass+0x8033c2c>
80000358:	88038337          	lui	t1,0x88038
8000035c:	00732023          	sw	t2,0(t1) # 88038000 <pass+0x8037c2c>
80000360:	8803c337          	lui	t1,0x8803c
80000364:	00032383          	lw	t2,0(t1) # 8803c000 <pass+0x803bc2c>
80000368:	00732023          	sw	t2,0(t1)
8000036c:	00000397          	auipc	t2,0x0
80000370:	01438393          	addi	t2,t2,20 # 80000380 <test7>
80000374:	00f00293          	li	t0,15
80000378:	00732823          	sw	t2,16(t1)
8000037c:	04c0006f          	j	800003c8 <fail>

80000380 <test7>:
80000380:	00700e13          	li	t3,7
80000384:	00000e97          	auipc	t4,0x0
80000388:	044e8e93          	addi	t4,t4,68 # 800003c8 <fail>
8000038c:	00000397          	auipc	t2,0x0
80000390:	01838393          	addi	t2,t2,24 # 800003a4 <test8>
80000394:	00d00293          	li	t0,13
80000398:	88000337          	lui	t1,0x88000
8000039c:	00032383          	lw	t2,0(t1) # 88000000 <pass+0x7fffc2c>
800003a0:	0280006f          	j	800003c8 <fail>

800003a4 <test8>:
800003a4:	00800e13          	li	t3,8
800003a8:	00000e97          	auipc	t4,0x0
800003ac:	020e8e93          	addi	t4,t4,32 # 800003c8 <fail>
800003b0:	00000397          	auipc	t2,0x0
800003b4:	02438393          	addi	t2,t2,36 # 800003d4 <pass>
800003b8:	00c00293          	li	t0,12
800003bc:	88000337          	lui	t1,0x88000
800003c0:	00030067          	jr	t1 # 88000000 <pass+0x7fffc2c>
800003c4:	0040006f          	j	800003c8 <fail>

800003c8 <fail>:
800003c8:	f01003b7          	lui	t2,0xf0100
800003cc:	f2438393          	addi	t2,t2,-220 # f00fff24 <pass+0x700ffb50>
800003d0:	01c3a023          	sw	t3,0(t2)

800003d4 <pass>:
800003d4:	f01003b7          	lui	t2,0xf0100
800003d8:	f2038393          	addi	t2,t2,-224 # f00fff20 <pass+0x700ffb4c>
800003dc:	0003a023          	sw	zero,0(t2)
