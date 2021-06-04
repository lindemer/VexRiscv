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

#define PMPCFG0    0x00001f1d
#define PMPADDR0   0x1fffffff // NAPOT R X
#define PMPADDR1   0x2fffffff // NAPOT RWX

#define mpucfg0   0x900
#define mpucfg1   0x901
#define mpucfg2   0x902
#define mpucfg3   0x903

#define mpuaddr0  0x910
#define mpuaddr1  0x911
#define mpuaddr2  0x912
#define mpuaddr3  0x913
#define mpuaddr4  0x914
#define mpuaddr5  0x915
#define mpuaddr6  0x916
#define mpuaddr7  0x917
#define mpuaddr8  0x918
#define mpuaddr9  0x919
#define mpuaddr10 0x91a
#define mpuaddr11 0x91b
#define mpuaddr12 0x91c
#define mpuaddr13 0x91d
#define mpuaddr14 0x91e
#define mpuaddr15 0x91f

#define MPUCFG0   0x1b1a1918
#define MPUCFG1   0x1f1e1d1c
#define MPUCFG2   0x9b9a9998
#define MPUCFG3   0x9f9e9d9c

#define MPUADDR0  0x22000fff // NAPOT  
#define MPUADDR1  0x22001fff // NAPOT  R
#define MPUADDR2  0x22002fff // NAPOT   W
#define MPUADDR3  0x22003fff // NAPOT  RW
#define MPUADDR4  0x22004fff // NAPOT    X
#define MPUADDR5  0x22005fff // NAPOT  R X
#define MPUADDR6  0x22006fff // NAPOT   WX
#define MPUADDR7  0x22007fff // NAPOT  RWX
#define MPUADDR8  0x22008fff // NAPOT L
#define MPUADDR9  0x22009fff // NAPOT LR
#define MPUADDR10 0x2200afff // NAPOT L W
#define MPUADDR11 0x2200bfff // NAPOT LRW
#define MPUADDR12 0x2200cfff // NAPOT L  X
#define MPUADDR13 0x2200dfff // NAPOT LR X
#define MPUADDR14 0x2200efff // NAPOT L WX
#define MPUADDR15 0x2200ffff // NAPOT LRWX

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

    // initialize PMP
    li x6, PMPCFG0
    csrw pmpcfg0, x6
    li x6, PMPADDR0
    csrw pmpaddr0, x6
    li x6, PMPADDR1
    csrw pmpaddr1, x6

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

    // initialize MPU
    li x6, MPUCFG0
    csrw mpucfg0, x6
    li x6, MPUCFG1
    csrw mpucfg1, x6
    li x6, MPUCFG2
    csrw mpucfg2, x6
    li x6, MPUCFG3
    csrw mpucfg3, x6
    li x6, MPUADDR0
    csrw mpuaddr0, x6
    li x6, MPUADDR1
    csrw mpuaddr1, x6
    li x6, MPUADDR2
    csrw mpuaddr2, x6
    li x6, MPUADDR3
    csrw mpuaddr3, x6
    li x6, MPUADDR4
    csrw mpuaddr4, x6
    li x6, MPUADDR5
    csrw mpuaddr5, x6
    li x6, MPUADDR6
    csrw mpuaddr6, x6
    li x6, MPUADDR7
    csrw mpuaddr7, x6
    li x6, MPUADDR8
    csrw mpuaddr8, x6
    li x6, MPUADDR9
    csrw mpuaddr9, x6
    li x6, MPUADDR10
    csrw mpuaddr10, x6
    li x6, MPUADDR11
    csrw mpuaddr11, x6
    li x6, MPUADDR12
    csrw mpuaddr12, x6
    li x6, MPUADDR13
    csrw mpuaddr13, x6
    li x6, MPUADDR14
    csrw mpuaddr14, x6
    li x6, MPUADDR15
    csrw mpuaddr15, x6

    // attempt M-mode operation
    la MEPC, test1
    li MCAUSE, 2
    mret
    j fail

test1:
    li TEST_ID, 1
    la MEPC, fail

    // attempt reads and writes to unlocked MPU regions
    la SEPC, fail
    li x6, 0x88000000
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)
    li x6, 0x88004000
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)
    li x6, 0x88008000
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)
    li x6, 0x8800c000
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)
    li x6, 0x88010000
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)
    li x6, 0x88014000
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)
    li x6, 0x88018000
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)
    li x6, 0x8801c000
    lw x7, 0x00(x6)
    sw x7, 0x00(x6)

    // attempt write to restricted PMP region
    la MEPC, test2
    li MCAUSE, STORE_ACCESS
    li x6, 0x7ffffffc
    sw x7, 0x00(x6) 
    j fail

test2:
    li TEST_ID, 2
    la MEPC, fail

    // attempt permitted writes to locked MPU regions
    la SEPC, fail
    li x6, 0x88000000
    sw x7, 0x28(x6)
    sw x7, 0x2c(x6)
    sw x7, 0x38(x6)
    sw x7, 0x3c(x6)

    // attempt write to restricted locked MPU region
    la SEPC, test3
    li SCAUSE, STORE_PAGE
    sw x7, 0x30(x6)
    j fail

test3:
    li TEST_ID, 3
    la MEPC, fail

    // attempt permitted reads to locked MPU regions
    la SEPC, fail
    li x6, 0x88000000
    lw x7, 0x24(x6)
    lw x7, 0x2c(x6)
    lw x7, 0x34(x6)
    lw x7, 0x3c(x6)

    // attempt read from restricted locked MPU region
    la SEPC, test4
    li SCAUSE, LOAD_PAGE
    lw x7, 0x20(x6)
    j fail

test4:
    li TEST_ID, 4
    la MEPC, fail
    la SEPC, test5

    // attempt execution from restricted locked MPU region
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

    // attempt permitted writes to MPU regions
    la SEPC, fail
    li x6, 0x88000000
    sw x7, 0x08(x6)
    sw x7, 0x0c(x6)
    sw x7, 0x18(x6)
    sw x7, 0x1c(x6)

    // attempt write to restricted MPU region
    la SEPC, test7
    li SCAUSE, STORE_PAGE
    sw x7, 0x10(x6)
    j fail

test7:
    li TEST_ID, 7
    la MEPC, fail

    // attempt permitted reads to MPU regions
    la SEPC, fail
    li x6, 0x88000000
    lw x7, 0x04(x6)
    lw x7, 0x0c(x6)
    lw x7, 0x14(x6)
    lw x7, 0x1c(x6)

    // attempt read from restricted MPU region
    la SEPC, test8
    li SCAUSE, LOAD_PAGE
    lw x7, 0x00(x6)
    j fail
    
test8:
    li TEST_ID, 8
    la MEPC, fail
    la SEPC, pass

    // attempt execution from restricted MPU region
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
