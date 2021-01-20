/*
 * Copyright (c) 2021 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

#define TEST_ID     x28

// intended values loaded into these temporaries
#define MEPC        x29
#define SEPC         x7
#define MCAUSE      x30
#define SCAUSE       x5

#define INST_ACCESS   1
#define INST_PAGE    12
#define LOAD_ACCESS   5
#define LOAD_PAGE    13
#define STORE_ACCESS  7
#define STORE_PAGE   15

#define ampcfg0   0x900
#define ampcfg1   0x901
#define ampcfg2   0x902
#define ampcfg3   0x903

#define ampaddr0  0x910
#define ampaddr1  0x911
#define ampaddr2  0x912
#define ampaddr3  0x913
#define ampaddr4  0x914
#define ampaddr5  0x915
#define ampaddr6  0x916
#define ampaddr7  0x917
#define ampaddr8  0x918
#define ampaddr9  0x919
#define ampaddr10 0x91a
#define ampaddr11 0x91b
#define ampaddr12 0x91c
#define ampaddr13 0x91d
#define ampaddr14 0x91e
#define ampaddr15 0x91f

#define AMPCFG0   0x13121110
#define AMPCFG1   0x17161514
#define AMPCFG2   0x93929190
#define AMPCFG3   0x97969594

#define AMPADDR0  0x22000000 // NA4  
#define AMPADDR1  0x22000001 // NA4  R
#define AMPADDR2  0x22000002 // NA4   W
#define AMPADDR3  0x22000003 // NA4  RW
#define AMPADDR4  0x22000004 // NA4    X
#define AMPADDR5  0x22000005 // NA4  R X
#define AMPADDR6  0x22000006 // NA4   WX
#define AMPADDR7  0x22000007 // NA4  RWX
#define AMPADDR8  0x22000008 // NA4 L
#define AMPADDR9  0x22000009 // NA4 LR
#define AMPADDR10 0x2200000a // NA4 L W
#define AMPADDR11 0x2200000b // NA4 LRW
#define AMPADDR12 0x2200000c // NA4 L  X
#define AMPADDR13 0x2200000d // NA4 LR X
#define AMPADDR14 0x2200000e // NA4 L WX
#define AMPADDR15 0x2200000f // NA4 LRWX

.global _start
_start:
    li TEST_ID, -1
    la MEPC, fail
    li MCAUSE, 0
    la SEPC, fail
    li SCAUSE, 0

    // global machine trap handler
    la x6, mtrap
    csrw mtvec, x6
    
    // exceptions 12, 13, 15 (page faults)
    li x6, 0xb000
    csrw medeleg, x6

    // jump into S-mode
    la x6, test0
    csrw mepc, x6
    li x6, 0x1800
    csrc mstatus, x6
    li x6, 0x0800
    csrs mstatus, x6
    mret

.global mtrap
mtrap:
    csrr x6, mcause
    bne x6, MCAUSE, fail
    csrw mepc, MEPC
    mret

.global strap
strap:
    csrr x6, scause
    bne x6, SCAUSE, fail
    csrw sepc, SEPC
    sret

test0:
    li TEST_ID, 0
    la MEPC, fail
    la SEPC, fail

    // global supervisor trap handler
    la x6, strap
    csrw stvec, x6

    // initialize amp
    li x6, AMPCFG0
    csrw ampcfg0, x6
    li x6, AMPCFG1
    csrw ampcfg1, x6
    li x6, AMPCFG2
    csrw ampcfg2, x6
    li x6, AMPCFG3
    csrw ampcfg3, x6
    li x6, AMPADDR0
    csrw ampaddr0, x6
    li x6, AMPADDR1
    csrw ampaddr1, x6
    li x6, AMPADDR2
    csrw ampaddr2, x6
    li x6, AMPADDR3
    csrw ampaddr3, x6
    li x6, AMPADDR4
    csrw ampaddr4, x6
    li x6, AMPADDR5
    csrw ampaddr5, x6
    li x6, AMPADDR6
    csrw ampaddr6, x6
    li x6, AMPADDR7
    csrw ampaddr7, x6
    li x6, AMPADDR8
    csrw ampaddr8, x6
    li x6, AMPADDR9
    csrw ampaddr9, x6
    li x6, AMPADDR10
    csrw ampaddr10, x6
    li x6, AMPADDR11
    csrw ampaddr11, x6
    li x6, AMPADDR12
    csrw ampaddr12, x6
    li x6, AMPADDR13
    csrw ampaddr13, x6
    li x6, AMPADDR14
    csrw ampaddr14, x6
    li x6, AMPADDR15
    csrw ampaddr15, x6

    // attempt M-mode operation
    la MEPC, test1
    li MCAUSE, 2
    mret
    j fail

test1:
    li TEST_ID, 1
    la MEPC, fail

    // attempt reads and writes to unlocked amp regions
    la SEPC, fail
    li x6, 0x88000000
    lw x7, 0x00(x6)
    lw x7, 0x04(x6)
    lw x7, 0x08(x6)
    lw x7, 0x0c(x6)
    lw x7, 0x10(x6)
    lw x7, 0x14(x6)
    lw x7, 0x18(x6)
    lw x7, 0x1c(x6)
    sw x7, 0x00(x6)
    sw x7, 0x04(x6)
    sw x7, 0x08(x6)
    sw x7, 0x0c(x6)
    sw x7, 0x10(x6)
    sw x7, 0x14(x6)
    sw x7, 0x18(x6)
    sw x7, 0x1c(x6)

    // attempt write to restricted PMP region
    la MEPC, test2
    li MCAUSE, STORE_ACCESS
    li x6, 0x7ffffffc
    sw x7, 0x00(x6) 
    j fail

test2:
    li TEST_ID, 2
    la MEPC, fail

    // attempt permitted writes to locked amp regions
    la SEPC, fail
    li x6, 0x88000000
    sw x7, 0x28(x6)
    sw x7, 0x2c(x6)
    sw x7, 0x38(x6)
    sw x7, 0x3c(x6)

    // attempt write to restricted locked amp region
    la SEPC, test3
    li SCAUSE, STORE_PAGE
    sw x7, 0x30(x6)
    j fail

test3:
    li TEST_ID, 3
    la MEPC, fail

    // attempt permitted reads to locked amp regions
    la SEPC, fail
    li x6, 0x88000000
    lw x7, 0x24(x6)
    lw x7, 0x2c(x6)
    lw x7, 0x34(x6)
    lw x7, 0x3c(x6)

    // attempt read from restricted locked amp region
    la SEPC, test4
    li SCAUSE, LOAD_PAGE
    lw x7, 0x20(x6)
    j fail

test4:
    li TEST_ID, 4
    la MEPC, fail
    la SEPC, test5

    // attempt execution from restricted locked amp region
    li SCAUSE, INST_PAGE
    li x6, 0x88000020 
    jr x6
    j fail

test5:
    li TEST_ID, 5
    la MEPC, fail
    la SEPC, fail

    // jump to U-mode
    la x6, test6
    csrw sepc, x6
    li x6, 0x80
    csrc sstatus, x6
    sret

test6:
    li TEST_ID, 6
    la MEPC, fail

    // attempt permitted writes to amp regions
    la SEPC, fail
    li x6, 0x88000000
    sw x7, 0x08(x6)
    sw x7, 0x0c(x6)
    sw x7, 0x18(x6)
    sw x7, 0x1c(x6)

    // attempt write to restricted amp region
    la SEPC, test7
    li SCAUSE, STORE_PAGE
    sw x7, 0x10(x6)
    j fail

test7:
    li TEST_ID, 7
    la MEPC, fail

    // attempt permitted reads to amp regions
    la SEPC, fail
    li x6, 0x88000000
    lw x7, 0x04(x6)
    lw x7, 0x0c(x6)
    lw x7, 0x14(x6)
    lw x7, 0x1c(x6)

    // attempt read from restricted amp region
    la SEPC, test8
    li SCAUSE, LOAD_PAGE
    lw x7, 0x00(x6)
    j fail
    
test8:
    li TEST_ID, 8
    la MEPC, fail
    la SEPC, pass

    // attempt execution from restricted amp region
    li SCAUSE, INST_PAGE
    li x6, 0x88000000 
    jr x6
    j fail

fail:
    li x7, 0xf00fff24
    sw TEST_ID, 0(x7)

pass:
    li x7, 0xf00fff20
    sw x0, 0(x7)
