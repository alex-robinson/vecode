!********************************************************************

! Code converted using TO_F90 by Alan Miller
! Date: 2014-12-08  Time: 12:21:26

!**  BUFFER COMMON: DATA TRANSFER CLIMATE <-> TERRESTR VEG MODEL  ***
!********************************************************************
COMMON /geogbio/ maskb(it,ns), frilb(it,ns), flonb(ns)
COMMON /climate/ tatb(it,ns),  prcb(it,ns), prcb5(it,ns),  &
    gdd0b(it,ns),tatmsmin(it,ns),tatmsmax(it,ns)
COMMON /biota/ st(it,ns),    sg(it,ns),    sd(it,ns), sg4(it,ns),  &
    snlt(it,ns),  anup(it,ns),  blai(it,ns,2), pnpp(it,ns),  &
    nstat(it,ns), b12(it,ns),   b34(it,ns),  &
    b1(it,ns),    b2(it,ns),    b3(it,ns),    b4(it,ns),  &
    bc14(it,ns),    bc13(it,ns), anup13(it,ns)
