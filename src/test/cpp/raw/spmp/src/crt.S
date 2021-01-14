/*
 * Copyright (c) 2020 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

#define TEST_ID      x28
#define STRAP_RETURN x6
#define STRAP_CAUSE  x7
#define MTRAP_RETURN x29
#define MTRAP_CAUSE  x30

#define IACCE_FAULT  1
#define IPAGE_FAULT 12
#define LACCE_FAULT  5
#define LPAGE_FAULT 13
#define SACCE_FAULT  7
#define SPAGE_FAULT 15

#define PMPCFG0    0x00000f0d
#define PMPADDR0   0x20000000 // TOR RX
#define PMPADDR1   0xffffffff // TOR RWX

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
#define spmpaddr10 0x910
#define spmpaddr11 0x91a
#define spmpaddr12 0x91b
#define spmpaddr13 0x91c
#define spmpaddr14 0x91d
#define spmpaddr15 0x91e

#define SPMPCFG0   0x1b1a1918
#define SPMPCFG1   0x1f1e1d1c
#define SPMPCFG2   0x9b9a9998
#define SPMPCFG3   0x9f9e9d9c

#define SPMPADDR0  0x22000000 // NAPOT  
#define SPMPADDR1  0x22000008 // NAPOT  R
#define SPMPADDR2  0x22000010 // NAPOT   W
#define SPMPADDR3  0x22000018 // NAPOT  RW
#define SPMPADDR4  0x22000020 // NAPOT    X
#define SPMPADDR5  0x22000028 // NAPOT  R X
#define SPMPADDR6  0x22000030 // NAPOT   WX
#define SPMPADDR7  0x22000038 // NAPOT  RWX
#define SPMPADDR8  0x22000040 // NAPOT L
#define SPMPADDR9  0x22000048 // NAPOT LR  
#define SPMPADDR10 0x22000050 // NAPOT L W
#define SPMPADDR11 0x22000058 // NAPOT LRW
#define SPMPADDR12 0x22000060 // NAPOT L  X
#define SPMPADDR13 0x22000068 // NAPOT LR X
#define SPMPADDR14 0x22000070 // NAPOT L WX
#define SPMPADDR15 0x22000078 // NAPOT LRWX

.global _start
_start:
    li TEST_ID, -1
    la MTRAP_RETURN, fail
    li MTRAP_CAUSE, 0
    la STRAP_RETURN, fail
    li STRAP_CAUSE, 0

    // global machine trap handler
    la x1, mtrap
    csrw mtvec, x1
    
    // exceptions 12, 13, 15 (page faults)
    li x1, 0xb000
    csrw mideleg, x1

    // initialize PMP
    li x1, PMPCFG0
    csrw pmpcfg0, x1
    li x1, PMPADDR0
    csrw pmpaddr0, x1
    li x1, PMPADDR1
    csrw pmpaddr1, x1

    // jump into S-mode
    la x1, test0
    csrw mepc, x1
    li x1, 0x1800
    csrc mstatus, x1
    li x1, 0x0800
    csrs mstatus, x1
    mret

.global mtrap
mtrap:
    csrr x1, mcause
    bne x1, MTRAP_CAUSE, fail
    csrw mepc, MTRAP_RETURN
    mret

.global strap
strap:
    csrr x1, scause
    bne x1, STRAP_CAUSE, fail
    csrw sepc, STRAP_RETURN
    sret

test0:
    li TEST_ID, 0
    la MTRAP_RETURN, fail
    la STRAP_RETURN, fail

    // global supervisor trap handler
    la x1, strap
    csrw stvec, x1

    // initialize sPMP
    li x1, SPMPCFG0
    csrw spmpcfg0, x1
    li x1, SPMPCFG1
    csrw spmpcfg1, x1
    li x1, SPMPCFG2
    csrw spmpcfg2, x1
    li x1, SPMPCFG3
    csrw spmpcfg3, x1
    li x1, SPMPADDR0
    csrw spmpaddr0, x1
    li x1, SPMPADDR1
    csrw spmpaddr1, x1
    li x1, SPMPADDR2
    csrw spmpaddr2, x1
    li x1, SPMPADDR3
    csrw spmpaddr3, x1
    li x1, SPMPADDR4
    csrw spmpaddr4, x1
    li x1, SPMPADDR5
    csrw spmpaddr5, x1
    li x1, SPMPADDR6
    csrw spmpaddr6, x1
    li x1, SPMPADDR7
    csrw spmpaddr7, x1
    li x1, SPMPADDR8
    csrw spmpaddr8, x1
    li x1, SPMPADDR9
    csrw spmpaddr9, x1
    li x1, SPMPADDR10
    csrw spmpaddr10, x1
    li x1, SPMPADDR11
    csrw spmpaddr11, x1
    li x1, SPMPADDR12
    csrw spmpaddr12, x1
    li x1, SPMPADDR13
    csrw spmpaddr13, x1
    li x1, SPMPADDR14
    csrw spmpaddr14, x1
    li x1, SPMPADDR15
    csrw spmpaddr15, x1

    // attempt M-mode operation
    la MTRAP_RETURN, test1
    li MTRAP_CAUSE, 2
    mret
    j fail

// attempt unlocked sPMP and PMP accesses from S-mode
test1:
    j pass

// attempt locked sPMP accesses from S-mode
test2:
    j pass

// attempt locked sPMP and PMP accesses from U-mode
test3:
    j pass

// attempt unlocked sPMP accesses from U-mode
test4:
    j pass
    
fail:
    li x2, 0xf00fff24
    sw TEST_ID, 0(x2)

pass:
    li x2, 0xf00fff20
    sw x0, 0(x2)
