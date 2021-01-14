/*
 * Copyright (c) 2020 Samuel Lindemer <samuel.lindemer@ri.se>
 *
 * SPDX-License-Identifier: MIT
 */

#define TEST_ID    x28
#define STRAP_RA   x29
#define MTRAP_RA   x30

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

#define SPMPCFG0   0x00000000
#define SPMPCFG1   0x00000000
#define SPMPCFG2   0x00000000
#define SPMPCFG3   0x00000000

#define SPMPADDR0  0x00000000 // OFF
#define SPMPADDR1  0x00000000 // OFF
#define SPMPADDR2  0x00000000 // OFF
#define SPMPADDR3  0x00000000 // OFF
#define SPMPADDR4  0x00000000 // OFF
#define SPMPADDR5  0x00000000 // OFF
#define SPMPADDR6  0x00000000 // OFF
#define SPMPADDR7  0x00000000 // OFF
#define SPMPADDR8  0x00000000 // OFF
#define SPMPADDR9  0x00000000 // OFF
#define SPMPADDR10 0x00000000 // OFF
#define SPMPADDR11 0x00000000 // OFF
#define SPMPADDR12 0x00000000 // OFF
#define SPMPADDR13 0x00000000 // OFF
#define SPMPADDR14 0x00000000 // OFF
#define SPMPADDR15 0x00000000 // OFF

.global _start
_start:
    li TEST_ID, -1
    la MTRAP_RA, fail
    la STRAP_RA, fail

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
    csrw mepc, MTRAP_RA
    mret

.global strap
strap:
    csrw sepc, STRAP_RA
    sret

test0:
    li TEST_ID, 0
    la MTRAP_RA, fail
    la STRAP_RA, fail

    // global supervisor trap handler
    la x1, strap
    csrw stvec, x1
    la STRAP_RA, fail

    // initialize sPMP
    li x1, SPMPCFG0
    csrw spmpcfg0, x1
    li x1, SPMPADDR0
    csrw spmpaddr0, x1
    li x1, SPMPADDR1
    csrw spmpaddr1, x1

    // attempt M-mode operation
    la MTRAP_RA, test1
    mret
    j fail

test1:
    j pass 
    
fail:
    li x2, 0xf00fff24
    sw TEST_ID, 0(x2)

pass:
    li x2, 0xf00fff20
    sw x0, 0(x2)
