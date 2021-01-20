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

#define PMPCFG0    0x00000f0d
#define PMPADDR0   0x20000000 // TOR  RX
#define PMPADDR1   0x30000000 // TOR RWX

#define spmpcfg0   0x900
#define spmpcfg1   0x901
#define spmpcfg2   0x902
#define spmpcfg3   0x903

#define spmpaddr0  0x910
#define spmpaddr1  0x911
#define spmpaddr2  0x912
#define spmpaddr3  0x913
#define spmpaddr4  0x914
#define spmpaddr5  0x915
#define spmpaddr6  0x916
#define spmpaddr7  0x917
#define spmpaddr8  0x918
#define spmpaddr9  0x919
#define spmpaddr10 0x91a
#define spmpaddr11 0x91b
#define spmpaddr12 0x91c
#define spmpaddr13 0x91d
#define spmpaddr14 0x91e
#define spmpaddr15 0x91f

#define SPMPCFG0   0x13121110
#define SPMPCFG1   0x17161514
#define SPMPCFG2   0x93929190
#define SPMPCFG3   0x97969594

#define SPMPADDR0  0x22000000 // NA4  
#define SPMPADDR1  0x22000001 // NA4  R
#define SPMPADDR2  0x22000002 // NA4   W
#define SPMPADDR3  0x22000003 // NA4  RW
#define SPMPADDR4  0x22000004 // NA4    X
#define SPMPADDR5  0x22000005 // NA4  R X
#define SPMPADDR6  0x22000006 // NA4   WX
#define SPMPADDR7  0x22000007 // NA4  RWX
#define SPMPADDR8  0x22000008 // NA4 L
#define SPMPADDR9  0x22000009 // NA4 LR
#define SPMPADDR10 0x2200000a // NA4 L W
#define SPMPADDR11 0x2200000b // NA4 LRW
#define SPMPADDR12 0x2200000c // NA4 L  X
#define SPMPADDR13 0x2200000d // NA4 LR X
#define SPMPADDR14 0x2200000e // NA4 L WX
#define SPMPADDR15 0x2200000f // NA4 LRWX

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

    // initialize sPMP
    li x6, SPMPCFG0
    csrw spmpcfg0, x6
    li x6, SPMPCFG1
    csrw spmpcfg1, x6
    li x6, SPMPCFG2
    csrw spmpcfg2, x6
    li x6, SPMPCFG3
    csrw spmpcfg3, x6
    li x6, SPMPADDR0
    csrw spmpaddr0, x6
    li x6, SPMPADDR1
    csrw spmpaddr1, x6
    li x6, SPMPADDR2
    csrw spmpaddr2, x6
    li x6, SPMPADDR3
    csrw spmpaddr3, x6
    li x6, SPMPADDR4
    csrw spmpaddr4, x6
    li x6, SPMPADDR5
    csrw spmpaddr5, x6
    li x6, SPMPADDR6
    csrw spmpaddr6, x6
    li x6, SPMPADDR7
    csrw spmpaddr7, x6
    li x6, SPMPADDR8
    csrw spmpaddr8, x6
    li x6, SPMPADDR9
    csrw spmpaddr9, x6
    li x6, SPMPADDR10
    csrw spmpaddr10, x6
    li x6, SPMPADDR11
    csrw spmpaddr11, x6
    li x6, SPMPADDR12
    csrw spmpaddr12, x6
    li x6, SPMPADDR13
    csrw spmpaddr13, x6
    li x6, SPMPADDR14
    csrw spmpaddr14, x6
    li x6, SPMPADDR15
    csrw spmpaddr15, x6

    // attempt M-mode operation
    la MEPC, test1
    li MCAUSE, 2
    mret
    j fail

test1:
    li TEST_ID, 1
    la MEPC, fail

    // attempt reads and writes to unlocked sPMP regions
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

    // attempt permitted writes to locked sPMP regions
    la SEPC, fail
    li x6, 0x88000000
    sw x7, 0x28(x6)
    sw x7, 0x2c(x6)
    sw x7, 0x38(x6)
    sw x7, 0x3c(x6)

    // attempt write to restricted locked sPMP region
    la SEPC, test3
    li SCAUSE, STORE_PAGE
    sw x7, 0x30(x6)
    j fail

test3:
    li TEST_ID, 3
    la MEPC, fail

    // attempt permitted reads to locked sPMP regions
    la SEPC, fail
    li x6, 0x88000000
    lw x7, 0x24(x6)
    lw x7, 0x2c(x6)
    lw x7, 0x34(x6)
    lw x7, 0x3c(x6)

    // attempt read from restricted locked sPMP region
    la SEPC, test4
    li SCAUSE, LOAD_PAGE
    lw x7, 0x20(x6)
    j fail

test4:
    li TEST_ID, 4
    la MEPC, fail
    la SEPC, test5

    // attempt execution from restricted locked sPMP region
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

    // attempt permitted writes to sPMP regions
    la SEPC, fail
    li x6, 0x88000000
    sw x7, 0x08(x6)
    sw x7, 0x0c(x6)
    sw x7, 0x18(x6)
    sw x7, 0x1c(x6)

    // attempt write to restricted sPMP region
    la SEPC, test7
    li SCAUSE, STORE_PAGE
    sw x7, 0x10(x6)
    j fail

test7:
    li TEST_ID, 7
    la MEPC, fail

    // attempt permitted reads to sPMP regions
    la SEPC, fail
    li x6, 0x88000000
    lw x7, 0x04(x6)
    lw x7, 0x0c(x6)
    lw x7, 0x14(x6)
    lw x7, 0x1c(x6)

    // attempt read from restricted sPMP region
    la SEPC, test8
    li SCAUSE, LOAD_PAGE
    lw x7, 0x00(x6)
    j fail
    
test8:
    li TEST_ID, 8
    la MEPC, fail
    la SEPC, pass

    // attempt execution from restricted sPMP region
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
