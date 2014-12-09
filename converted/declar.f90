
integer, parameter :: &
        it = 18, ix=19, ns=7,  ns1=8,   ns2=9, nz=10,  nz1=11,  &
        LT=72,  lx=73, noc=3, noc1=4, noc2=5, jt=20,  jx=21,  &
        nt=17,  nst=6,  nsl=2,ism=240, jsm=87,  &
        ifn=77, jfn=51, nif=11, njf=3, nzlwr = 16

! Code converted using TO_F90 by Alan Miller
! Date: 2014-12-08  Time: 12:21:14

!================= ATMOSPHERE =========================================
!om   IT         - number of latitudinal belts
!om   IX = IT+1  - number of borders between lat. belts
!om   NS         - number of sectors
!om   NS1 = NS+1 - number of sectors border
!om   NT         - number of tracers
!om   NZ         - number of layers in atmopshere
!om   NZ1=NZ+1   - number of levels in atmosphere
!om   NZLWR      - number of levels in atmosphere for radiative model
!================= OCEAN ===============================================
!om   LT         - number of latitudinal belts
!om   LX         - number of borders between latitudinal belts
!om   NOC        - number of ocean basins
!om   NOC1 = NOC+1
!om   NOC2 = NOC+2
!om   JT         - number of layers in ocean
!om   JX = JT+1  - number of levels in ocean
!================= LAND =========================================
!om   NST        - number of surface types
!om   NSL        - number of soil layers
