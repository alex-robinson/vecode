!**********************************************************************

! Code converted using TO_F90 by Alan Miller
! Date: 2014-12-08  Time: 12:21:22

COMMON /observ_dat/ &
!**********************************************************************
!    Last modification 27.12.98
!**********************************************************************

! OBSERV
!********
 tauo(ix,noc,4),  &
    tlev(it,jt,noc),    slev(it,jt,noc),   ss(it,noc),  &
    sst(it,noc,12),     ssta(it,ns,12),    friceobs(it,noc,12),  &
    stobs(it,ns),       sgobs(it,ns),      sdobs(it,ns), blaiobs(it,ns,2), &

! OBSERV_INT
!************
 sst_i(LT,noc,12), ss_i(LT,noc), &

! CLIMATO_CONSTRAINTS
!********************
 tgmin, tgmax, prcmin, prcmax, tvmin, tvmax, fatmxmin, fatmxmax,  &
    ismin, ismax, inmin, inmax, xamxmin, xamxmax, &

! SPINUP TIME
!************
ispinup
