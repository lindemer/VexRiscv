/*
 * Copyright (c) 2020 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

#define TEST_ID   x28
#define MEPC      x30

#define mseccfg   0x390

#define PMPCFG0   0x13121110
#define PMPCFG1   0x17161514
#define PMPCFG2   0x93929190
#define PMPCFG3   0x97969594

#define PMPADDR0  0x22000000 // NA4      0x88000000
#define PMPADDR1  0x22000001 // NA4  R   0x88000004
#define PMPADDR2  0x22000002 // NA4   W  0x88000008
#define PMPADDR3  0x22000003 // NA4  RW  0x8800000c
#define PMPADDR4  0x22000004 // NA4    X 0x88000010
#define PMPADDR5  0x22000005 // NA4  R X 0x88000014
#define PMPADDR6  0x22000006 // NA4   WX 0x88000018
#define PMPADDR7  0x22000007 // NA4  RWX 0x8800001c
#define PMPADDR8  0x22000008 // NA4 L    0x88000020
#define PMPADDR9  0x22000009 // NA4 LR   0x88000024
#define PMPADDR10 0x2200000a // NA4 L W  0x88000028
#define PMPADDR11 0x2200000b // NA4 LRW  0x8800002c
#define PMPADDR12 0x2200000c // NA4 L  X 0x88000030
#define PMPADDR13 0x2200000d // NA4 LR X 0x88000034
#define PMPADDR14 0x2200000e // NA4 L WX 0x88000038
#define PMPADDR15 0x2200000f // NA4 LRWX 0x8800003c

.global _start
_start:
    la x1, trap
    csrw mtvec, x1
    j test0

.global trap
trap:
    csrw mepc, MEPC
    mret

test0:
    li TEST_ID, 0
    la MEPC, fail

    li x5, PMPCFG0
    csrw pmpcfg0, x5
    li x5, PMPCFG1
    csrw pmpcfg1, x5
    li x5, PMPCFG2
    csrw pmpcfg2, x5
    li x5, PMPCFG3
    csrw pmpcfg3, x5
    li x5, PMPADDR0
    csrw pmpaddr0, x5
    li x5, PMPADDR1
    csrw pmpaddr1, x5
    li x5, PMPADDR2
    csrw pmpaddr2, x5
    li x5, PMPADDR3
    csrw pmpaddr3, x5
    li x5, PMPADDR4
    csrw pmpaddr4, x5
    li x5, PMPADDR5
    csrw pmpaddr5, x5
    li x5, PMPADDR6
    csrw pmpaddr6, x5
    li x5, PMPADDR7
    csrw pmpaddr7, x5
    li x5, PMPADDR8
    csrw pmpaddr8, x5
    li x5, PMPADDR9
    csrw pmpaddr9, x5
    li x5, PMPADDR10
    csrw pmpaddr10, x5
    li x5, PMPADDR11
    csrw pmpaddr11, x5
    li x5, PMPADDR12
    csrw pmpaddr12, x5
    li x5, PMPADDR13
    csrw pmpaddr13, x5
    li x5, PMPADDR14
    csrw pmpaddr14, x5
    li x5, PMPADDR15
    csrw pmpaddr15, x5

test1:
    li TEST_ID, 1
    la MEPC, fail

    j test2
    li x6, 0x88000000

    // legal machine read/write with MML=0
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)
    lw x7, 0x04(x6)
    sw x7, 0x04(x6)
    lw x7, 0x08(x6)
    sw x7, 0x08(x6)
    lw x7, 0x0c(x6)
    sw x7, 0x0c(x6)
    lw x7, 0x10(x6)
    sw x7, 0x10(x6)
    lw x7, 0x14(x6)
    sw x7, 0x14(x6)
    lw x7, 0x18(x6)
    sw x7, 0x18(x6)
    lw x7, 0x1c(x6)
    sw x7, 0x1c(x6)
    lw x7, 0x24(x6) // LR
    lw x7, 0x2c(x6) // LRW
    lw x7, 0x34(x6) // LR X
    lw x7, 0x3c(x6) // LRWX
    sw x7, 0x28(x6) // L W
    sw x7, 0x2c(x6) // LRW
    sw x7, 0x38(x6) // L WX
    sw x7, 0x3c(x6) // LRWX

    la MEPC, test2
    lw x7, 0x20(x6)
    j fail

test2:
    li TEST_ID, 2
    la MEPC, fail

    csrsi mseccfg, 0x1
    li x6, 0x88000000
    j pass

    // legal machine read/write with MML=1
    lw x7, 0x08(x6) //   W
    lw x7, 0x18(x6) //   WX
    sw x7, 0x08(x6) //   W
    sw x7, 0x18(x6) //   WX
    lw x7, 0x24(x6) // LR
    lw x7, 0x2c(x6) // LRW
    lw x7, 0x34(x6) // LR X
    lw x7, 0x38(x6) // L WX
    lw x7, 0x3c(x6) // LRWX
    sw x7, 0x2c(x6) // LRW

    la MEPC, test3
    sw x7, 0x3c(x6) // LRWX
    j fail

test3:
    li TEST_ID, 3
    la MEPC, test4
    csrci mseccfg, 0x1
    li x6, 0x80
    csrc mstatus, x6

    // jump to U-mode
    mret
    j fail

test4:
    li TEST_ID, 4
    la MEPC, fail
    j pass

test5:
    li TEST_ID, 5
    la MEPC, fail
    j pass

test6:
    li TEST_ID, 6
    la MEPC, fail
    j pass

test7:
    li TEST_ID, 7
    la MEPC, fail
    j pass

fail:
    li x2, 0xf00fff24
    sw TEST_ID, 0(x2)

pass:
    li x2, 0xf00fff20
    sw x0, 0(x2)
