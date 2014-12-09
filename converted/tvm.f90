!********************************************************************

! Code converted using TO_F90 by Alan Miller
! Date: 2014-12-08  Time: 12:18:08

SUBROUTINE tvm
!********************************************************************
!      TERRESTRIAL VEGETATION ANNUAL MODEL

!  Purpose: Computation of forest/grass/desert ratio;
!           NPP and living and soil biomass

!  By: V.Brovkin
!  Modifyed by: A.Ganopolski
!  Last modification: 17.11.97
!********************************************************************
INCLUDE 'declar.inc'
INCLUDE 'params.inc'
INCLUDE 'bio.inc'
INCLUDE 'buffer.inc'
!********************************************************************
! input data: annual mean temperature in degr. Celc. - ave_t
!             annual mean precipitation, mm/year - ave_pr
!             growing degree days above 0, degr. Celc - gdd0
!             CO2 concentration in the atmosphere, ppm - co2

! output data: st(i,j) - forest's share in cell i,j
!              sg(i,j) - grass share in cell i,j
!              sd(i,j) - desert's share in cell i,j
!              snlt(i,j) -needle leaved trees ratio
!              st(i,j)+sg(i,j)+sd(i,j)=1, 0<= snlt(i,j)<= 1
!              plai(i,j) - annual average of leaf area index, m2/m2
!              anup(i,j) - annual uptake of carbon in cell, Gt -> kg/m2 ???

! COMMON block described in buffer.inc
!********************************************************************

!...    SPATIAL LOOP

DO k=1,ns
  DO i=1,it
    IF (maskb(i,k) == 1) THEN
      
      ave_t= tatb(i,k)
      ave_pr=prcb(i,k)
      ave_pr05=prcb5(i,k)
      gdd0=gdd0b(i,k)
! co2 enrichment
      co2=pab
      lat=i
      lon=k
      
!      if (KTVM.eq.1) then
!   equilibrium run
      
!... calculation of initial forest ratio and amount of carbon in pools based
!       on equilbrium state; carbon uptake equals to steady state amount
      
!          call CCSTAT
      
!                else
      
!... calculation of dynamics of carbon pools and forest ratio
      
      IF (ini_step == 0.AND.klsr == 0) THEN
        CALL ccstat
        CALL ccstat_isotope
      ELSE
        CALL ccdyn
        IF (ktvm == 1) THEN
          DO kprom=1,19
            CALL ccdyn
          END DO
        END IF
      END IF
      
!       endif
      
      
    END IF
    
  END DO
END DO

ini_step=1

RETURN
END SUBROUTINE tvm

!********************************************************************

SUBROUTINE ccstat
!********************************************************************
INCLUDE 'declar.inc'
INCLUDE 'params.inc'
INCLUDE 'bio.inc'
INCLUDE 'buffer.inc'
!********************************************************************
!...1) calculation of initial carbon cycle parameters

CALL ccparam

! calculation of equilibrium storages

! leaves biomass
! b1t is leaves phytomass for trees, b1g - for grass (kg C/m2)
! t1t is residence time of carbon in trees, t1g - in grass (years)

b1t(lat,lon)=k1t*t1t*npp
b1g(lat,lon)=k1g*t1g*npp

!   stems and roots biomass

b2t(lat,lon)=(1-k1t)*t2t*npp
b2g(lat,lon)=(1-k1g)*t2g*npp

!   litter

b3t(lat,lon)=(k0t*b1t(lat,lon)/t1t+k2t/t2t*b2t(lat,lon))*t3t
b3g(lat,lon)=(k0g*b1g(lat,lon)/t1g+k2g/t2g*b2g(lat,lon))*t3g

! mortmass and soil organic matter

b4t(lat,lon)=(k3t/t3t*b3t(lat,lon))*t4t
b4g(lat,lon)=(k4g/t2g*b2g(lat,lon)+k3g/t3g*b3g(lat,lon))*t4g

! initialization of fraction dynamic variables

st(lat,lon)=forshare_st
sd(lat,lon)=desshare_st
snlt(lat,lon)=nlshare_st
sg(lat,lon)=1.-st(lat,lon)-sd(lat,lon)

CALL climpar

RETURN
END SUBROUTINE ccstat

SUBROUTINE ccstat_isotope
!********************************************************************
INCLUDE 'declar.inc'
INCLUDE 'params.inc'
INCLUDE 'bio.inc'
INCLUDE 'buffer.inc'
!********************************************************************
! initialization of isotopes ratios

b1t14(lat,lon)=c14init*EXP(-c14tdec*t1t)*b1t(lat,lon)
b1g14(lat,lon)=c14init*EXP(-c14tdec*t1g)*b1g(lat,lon)

b1t13(lat,lon)=b1t(lat,lon)*c13init*c13atm
b1g13(lat,lon)=b1g(lat,lon)*c13init*c13atm

!   stems and roots biomass

b2t14(lat,lon)=b2t(lat,lon)*c14init*EXP(-c14tdec*t2t)
b2g14(lat,lon)=b2g(lat,lon)*c14init*EXP(-c14tdec*t2g)

b2t13(lat,lon)=b2t(lat,lon)*c13init*c13atm
b2g13(lat,lon)=b2g(lat,lon)*c13init*c13atm

!   litter

b3t14(lat,lon)=b3t(lat,lon)*c14init*EXP(-c14tdec*t3t)
b3g14(lat,lon)=b3g(lat,lon)*c14init*EXP(-c14tdec*t3g)

b3t13(lat,lon)=b3t(lat,lon)*c13init*c13atm
b3g13(lat,lon)=b3g(lat,lon)*c13init*c13atm

! mortmass and soil organic matter

b4t14(lat,lon)=b4t(lat,lon)*c14init*EXP(-c14tdec*t4t)
b4g14(lat,lon)=b4g(lat,lon)*c14init*EXP(-c14tdec*t4g)

b4t13(lat,lon)=b4t(lat,lon)*c13init*c13atm
b4g13(lat,lon)=b4g(lat,lon)*c13init*c13atm

c13ratio=c13init

RETURN
END SUBROUTINE ccstat_isotope


!********************************************************************

SUBROUTINE ccdyn
!********************************************************************
INCLUDE 'declar.inc'
INCLUDE 'params.inc'
INCLUDE 'bio.inc'
INCLUDE 'buffer.inc'
!********************************************************************
! temporal var*/
REAL :: tempor1,tempor2,tempor3,tempor4,db2,fd,dst,dd,nld,  &
    dstime,tempor5,tempor6,dsg,dsd,temp_sg,temp_st

! calculation of current carbon cycle parameters

CALL ccparam

! calculation of fraction dynamic variables

fd=forshare_st-st(lat,lon)
dd=desshare_st-sd(lat,lon)
nld=nlshare_st-snlt(lat,lon)
g4d=g4share_st-sg4(lat,lon)
temp_st=st(lat,lon)
temp_sg=sg(lat,lon)

! calculation of forest dynamics; exponential filtre
dst=forshare_st-fd*EXP(-1./t2t)-st(lat,lon)
st(lat,lon)=st(lat,lon)+dst
snlt(lat,lon)=nlshare_st-nld*EXP(-1./t2t)

! desert dynamics; exponential filtre
dsd=desshare_st-dd*EXP(-1./t2g)-sd(lat,lon)
tempor1=sd(lat,lon)+dsd+st(lat,lon)

! calculation of characteristic time of desert propagation
IF (tempor1 > 0.9) THEN
  dstime=t2g*(1-tempor1)*10.+t2t*(tempor1-0.9)*10
  dsd=desshare_st-dd*EXP(-1./dstime)-sd(lat,lon)
END IF

sd(lat,lon)=sd(lat,lon)+dsd
dsg=-dst-dsd

sg(lat,lon)=1.-st(lat,lon)-sd(lat,lon)
sg4(lat,lon)=g4share_st-g4d*EXP(-1./t2g)

IF (sg(lat,lon) < 0) sg(lat,lon)=0
IF (st(lat,lon) < 0) st(lat,lon)=0
IF (sd(lat,lon) < 0) sd(lat,lon)=0


! calculation of dynamics of storages

! calculation of changes of storages due to conservation law

! correction for trees

tempor1=b4t(lat,lon)
tempor2=b3t(lat,lon)
tempor3=b4t14(lat,lon)
tempor4=b3t14(lat,lon)
tempor5=b4t13(lat,lon)
tempor6=b3t13(lat,lon)


IF(st(lat,lon) > 0) THEN
  IF(dst > 0) THEN
    
    b4t(lat,lon)=(b4t(lat,lon)*temp_st +b4g(lat,lon)*dst)/st(lat,lon)
    b3t(lat,lon)=(b3t(lat,lon)*temp_st +b3g(lat,lon)*dst)/st(lat,lon)
    
    b4t14(lat,lon)=(b4t14(lat,lon)*temp_st +b4g14(lat,lon)*dst)/st(lat,lon)
    b3t14(lat,lon)=(b3t14(lat,lon)*temp_st +b3g14(lat,lon)*dst)/st(lat,lon)
    
    b4t13(lat,lon)=(b4t13(lat,lon)*temp_st +b4g13(lat,lon)*dst)/st(lat,lon)
    b3t13(lat,lon)=(b3t13(lat,lon)*temp_st +b3g13(lat,lon)*dst)/st(lat,lon)
    
    
  END IF
  
  b2t(lat,lon)=b2t(lat,lon)*temp_st/st(lat,lon)
  b1t(lat,lon)=b1t(lat,lon)*temp_st/st(lat,lon)
  
  b2t14(lat,lon)=b2t14(lat,lon)*temp_st/st(lat,lon)
  b1t14(lat,lon)=b1t14(lat,lon)*temp_st/st(lat,lon)
  
  b2t13(lat,lon)=b2t13(lat,lon)*temp_st/st(lat,lon)
  b1t13(lat,lon)=b1t13(lat,lon)*temp_st/st(lat,lon)
  
END IF

! correction for grass

IF(sg(lat,lon) > 0) THEN
  IF(dst > 0) THEN
    
    b4g(lat,lon)=b4g(lat,lon)*(temp_sg-dst) /sg(lat,lon)
    b3g(lat,lon)=b3g(lat,lon)*(temp_sg-dst) /sg(lat,lon)
    
    b4g14(lat,lon)=b4g14(lat,lon)*(temp_sg-dst) /sg(lat,lon)
    b3g14(lat,lon)=b3g14(lat,lon)*(temp_sg-dst) /sg(lat,lon)
    
    b4g13(lat,lon)=b4g13(lat,lon)*(temp_sg-dst) /sg(lat,lon)
    b3g13(lat,lon)=b3g13(lat,lon)*(temp_sg-dst) /sg(lat,lon)
    
    
  ELSE
    
    b4g(lat,lon)=(b4g(lat,lon)*temp_sg-tempor1*dst) /sg(lat,lon)
    b3g(lat,lon)=(b3g(lat,lon)*temp_sg-tempor2*dst) /sg(lat,lon)
    
    b4g14(lat,lon)=(b4g14(lat,lon)*temp_sg -tempor3*dst)/sg(lat,lon)
    b3g14(lat,lon)=(b3g14(lat,lon)*temp_sg -tempor4*dst)/sg(lat,lon)
    
    b4g13(lat,lon)=(b4g13(lat,lon)*temp_sg -tempor5*dst)/sg(lat,lon)
    b3g13(lat,lon)=(b3g13(lat,lon)*temp_sg -tempor6*dst)/sg(lat,lon)
    
  END IF
  
  b2g(lat,lon)=b2g(lat,lon)*temp_sg/sg(lat,lon)
  b1g(lat,lon)=b1g(lat,lon)*temp_sg/sg(lat,lon)
  
  b2g14(lat,lon)=b2g14(lat,lon)*temp_sg/sg(lat,lon)
  b1g14(lat,lon)=b1g14(lat,lon)*temp_sg/sg(lat,lon)
  
  b2g13(lat,lon)=b2g13(lat,lon)*temp_sg/sg(lat,lon)
  b1g13(lat,lon)=b1g13(lat,lon)*temp_sg/sg(lat,lon)
  
END IF


! slow soil organic matter

b4t(lat,lon)=b4t(lat,lon)+k3t/t3t*b3t(lat,lon) -b4t(lat,lon)/t4t
b4g(lat,lon)=b4g(lat,lon)+k4g/t2g*b2g(lat,lon)+k3g/t3g*  &
    b3g(lat,lon)-b4g(lat,lon)/t4g

b4t14(lat,lon)=b4t14(lat,lon)+k3t/t3t *b3t14(lat,lon)  &
    -b4t14(lat,lon)/t4t
b4g14(lat,lon)=b4g14(lat,lon)+k4g/t2g *b2g14(lat,lon)+k3g/t3g*  &
    b3g14(lat,lon)-b4g14(lat,lon)/t4g

b4t13(lat,lon)=b4t13(lat,lon)+k3t/t3t *b3t13(lat,lon)  &
    -b4t13(lat,lon)/t4t
b4g13(lat,lon)=b4g13(lat,lon)+k4g/t2g *b2g13(lat,lon)+k3g/t3g*  &
    b3g13(lat,lon)-b4g13(lat,lon)/t4g

!   fast soil organic matter

b3t(lat,lon)=b3t(lat,lon)+b1t(lat,lon)/t1t*k0t+  &
    k2t/t2t*b2t(lat,lon)-b3t(lat,lon)/t3t
b3g(lat,lon)=b3g(lat,lon)+b1g(lat,lon)/t1g*k0g+  &
    k2g/t2g*b2g(lat,lon)-b3g(lat,lon)/t3g

b3t14(lat,lon)=b3t14(lat,lon) +b1t14(lat,lon)/t1t*k0t+  &
    k2t/t2t*b2t14(lat,lon) -b3t14(lat,lon)/t3t

b3g14(lat,lon)=b3g14(lat,lon) +b1g14(lat,lon)/t1g*k0g+  &
    k2g/t2g*b2g14(lat,lon) -b3g14(lat,lon)/t3g

b3t13(lat,lon)=b3t13(lat,lon) +b1t13(lat,lon)/t1t*k0t+  &
    k2t/t2t*b2t13(lat,lon) -b3t13(lat,lon)/t3t

b3g13(lat,lon)=b3g13(lat,lon) +b1g13(lat,lon)/t1g*k0g+  &
    k2g/t2g*b2g13(lat,lon) -b3g13(lat,lon)/t3g

! leaves biomass


b1t(lat,lon)=b1t(lat,lon)+k1t*npp-b1t(lat,lon)/t1t
b1g(lat,lon)=k1g*npp*t1g


b1t14(lat,lon)=b1t14(lat,lon)+k1t*npp*c14atm -b1t14(lat,lon)/t1t
 b1g14(lat,lon)=k1g*npp*c14atm*t1g

b1t13(lat,lon)=b1t13(lat,lon)+k1t*npp*c13atm* c13frac-b1t13(lat,lon)/t1t

b1g13(lat,lon)=k1g*npp*c13atm*(c13frac*(1-sg4(lat,lon))+  &
    c13frac4*sg4(lat,lon))*t1g

!   stems and roots biomass

b2t(lat,lon)=b2t(lat,lon)+(1-k1t)*npp-b2t(lat,lon)/t2t
b2g(lat,lon)=b2g(lat,lon)+(1-k1g)*npp-b2g(lat,lon)/t2g


b2t14(lat,lon)=b2t14(lat,lon)+(1-k1t)*npp*c14atm -b2t14(lat,lon)/t2t

b2g14(lat,lon)=b2g14(lat,lon)+(1-k1g)*npp*c14atm -b2g14(lat,lon)/t2g

b2t13(lat,lon)=b2t13(lat,lon)+(1-k1t)*npp*c13atm  &
    *c13frac-b2t13(lat,lon)/t2t
b2g13(lat,lon)=b2g13(lat,lon)+(1-k1g)*npp*c13atm  &
    *(c13frac*(1-sg4(lat,lon))+ c13frac4*sg4(lat,lon))-b2g13(lat,lon)/t2g

!   c14 annual decay
b1t14(lat,lon)=b1t14(lat,lon)*(1-c14tdec)
b2t14(lat,lon)=b2t14(lat,lon)*(1-c14tdec)
b3t14(lat,lon)=b3t14(lat,lon)*(1-c14tdec)
b4t14(lat,lon)=b4t14(lat,lon)*(1-c14tdec)
b1g14(lat,lon)=b1g14(lat,lon)*(1-c14tdec)
b2g14(lat,lon)=b2g14(lat,lon)*(1-c14tdec)
b3g14(lat,lon)=b3g14(lat,lon)*(1-c14tdec)
b4g14(lat,lon)=b4g14(lat,lon)*(1-c14tdec)

CALL climpar

RETURN
END SUBROUTINE ccdyn

!********************************************************************

SUBROUTINE climpar
!********************************************************************
INCLUDE 'declar.inc'
INCLUDE 'params.inc'
INCLUDE 'bio.inc'
INCLUDE 'buffer.inc'
REAL :: tempor1
!********************************************************************

! calculation of annual averaged LAI - lai

laig=b1g(lat,lon)*deng
lait=b1t(lat,lon)*(dentn*snlt(lat,lon)+ dentd*(1-snlt(lat,lon)))

blai(lat,lon,1)=lait
blai(lat,lon,2)=laig

! calculation of annual carbon uptake

IF (ktvm >= 2) THEN
  tempor1=b1(lat,lon)+b2(lat,lon)+ b3(lat,lon)+b4(lat,lon)
  tempor2=bc13(lat,lon)
  
ELSE
  tempor1=0
END IF

b1(lat,lon)=b1t(lat,lon)*st(lat,lon)+b1g(lat,lon)*sg(lat,lon)
b2(lat,lon)=b2t(lat,lon)*st(lat,lon)+b2g(lat,lon)*sg(lat,lon)
b3(lat,lon)=b3t(lat,lon)*st(lat,lon)+b3g(lat,lon)*sg(lat,lon)
b4(lat,lon)=b4t(lat,lon)*st(lat,lon)+b4g(lat,lon)*sg(lat,lon)
b12(lat,lon)=b1(lat,lon)+b2(lat,lon)
b34(lat,lon)=b3(lat,lon)+b4(lat,lon)

bc14(lat,lon)=(b1t14(lat,lon)+ b2t14(lat,lon)+b3t14(lat,lon)+  &
    b4t14(lat,lon))*st(lat,lon)+ (b1g14(lat,lon)+b2g14(lat,lon)+  &
    b3g14(lat,lon)+b4g14(lat,lon)) *sg(lat,lon)

! print *,lat,lon,b1t14(lat,lon),b1g14(lat,lon),b4t14(lat,lon)

bc13(lat,lon)=(b1t13(lat,lon)+ b2t13(lat,lon)+b3t13(lat,lon)+  &
    b4t13(lat,lon))*st(lat,lon)+ (b1g13(lat,lon)+b2g13(lat,lon)+  &
    b3g13(lat,lon)+b4g13(lat,lon)) *sg(lat,lon)

anup(lat,lon)=(b1(lat,lon)+b2(lat,lon) +b3(lat,lon)+b4(lat,lon)-tempor1)
anup13(lat,lon)=bc13(lat,lon)-tempor2

!...      NET PRIMARY PRODUCTION

pnpp(lat,lon)=npp*(1-sd(lat,lon))

RETURN
END SUBROUTINE climpar

!********************************************************************

SUBROUTINE ccparam
!********************************************************************

INCLUDE 'declar.inc'
INCLUDE 'params.inc'
INCLUDE 'bio.inc'
INCLUDE 'buffer.inc'
REAL :: npp1,npp2,db1,db2,db3,avefor,differ,pcr
!********************************************************************
! calculation of current cycle parameters

! potential trees share

avefor=ave_pr*ave_pr*ave_pr*ave_pr
differ=gdd0-gdd0_min
db1=-bet*differ
db2=gamm*differ
db3=differ*differ

IF(differ < 0) THEN
  forshare_st=0
ELSE
  forshare_st=(1-EXP(db1))*avefor/(avefor+a*db3*EXP(db2))
END IF
IF (forshare_st > fmax) forshare_st=fmax

! potential desert share - desshare_st

desshare_st=0

! northern deserts
!v change! permafrost soils version - no cold deserts!

!v       if(gdd0.lt.100) desshare_st=1

!v      if(gdd0.ge.100.and.gdd0.lt.gdd0_min)
!v     >      desshare_st=(gdd0_min-gdd0)/(gdd0_min-100.)

! southern deserts

IF (gdd0 >= gdd0_max) THEN
  
  pcr=acr*EXP(gamm/2.*differ)
  
  IF (ave_pr05 <= pcr) THEN
    desshare_st=1
    forshare_st=0
  ELSE
    db2=(ave_pr05-pcr)/EXP(gamm*differ)
    desshare_st=1.03/(1+ades*db2*db2)-0.03
    IF (desshare_st < 0) desshare_st=0
  END IF
  
END IF

! calculation of NPP, Lieth's formula

db1=-v1*ave_pr
IF (gdd0 >= gdd0_max) db1=-v1*ave_pr05
db2=-v2*ave_t
npp1=(1.-EXP(db1))
npp2=1./(1.+v3*EXP(db2))
IF(npp1 < npp2) THEN
  npp=nppmax*npp1
ELSE
  npp=nppmax*npp2
END IF

! CO2 enrichment factor

npp=npp*(1+0.25/ALOG(2.)*ALOG(co2/280.))

!v change! npp in cold regions
IF(gdd0 < gdd0_min) THEN
  IF(gdd0 >= 100) THEN
    npp=npp*(gdd0_min-gdd0)/(gdd0_min-100.)
  ELSE
    npp=0
  END IF
END IF


! allocation factors and residence time of leaves biomass

k1t=c1t+c2t/(1+c3t*npp)
k1g=c1g+c2g/(1+c3g*npp)

t1t=d1t+d2t/(1+d3t*npp)
t1g=d1g+d2g/(1+d3g*npp)

!   residence time of stems and roots biomass

t2t=e1t+e2t/(1+e3t*npp)
t2g=e1g+e2g/(1+e3g*npp)

!   residence time of fast carbon pool

t3t=16*EXP(-ps5*(ave_t-soilt))
t3g=40*EXP(-ps5*(ave_t-soilt))

! residence time of slow soil organic matter



!v parameterization of permafrost effect on carbon residence time
ave_tresh=-10.

IF(ave_t > ave_tresh) THEN
  pstemp=-ps5*(ave_t-soilt)
  
ELSE
  pstem=-ps5*(ave_t-soilt)- (2.38+ps5*(ave_t-soilt))*(ave_t-ave_tresh)/30.
  pstemp=2.38
END IF

!v no respiration if gdd0 is below 100 dd
IF(gdd0 <= 100) pstemp=6.

t3t=16*EXP(pstemp)
t3g=40*EXP(pstemp)
t4t=900*EXP(pstemp)
t4g=t4t

!calculation of potential nedleleaves trees ratio

nlshare_st=(t1t-t1td)/(t1tn-t1td)
IF (nlshare_st > 1) nlshare_st=1
IF (nlshare_st < 0) nlshare_st=0

!calculation of c4 grass fraction

! if(TATMSMIN(lat,lon).lt.14) then
IF(tatmsmin(lat,lon) < 12) THEN
  g4share_st=0
ELSE
!   if(TATMSMIN(lat,lon).lt.17) then
!    g4share_st=1-(17-TATMSMIN(lat,lon))/(17.-14.)
  IF(tatmsmin(lat,lon) < 15.5) THEN
    g4share_st=1-(15.5-tatmsmin(lat,lon))/3.5
  ELSE
    g4share_st=1
  END IF
END IF

! print *, lat,lon,TATMSMIN(lat,lon),g4share_st,sg4(lat,lon)
RETURN
END SUBROUTINE ccparam

!********************************************************************

SUBROUTINE initcpar
!********************************************************************

INCLUDE 'declar.inc'
INCLUDE 'params.inc'
INCLUDE 'bio.inc'
INCLUDE 'buffer.inc'
!********************************************************************

! initialisation of variables
ades=0.0011
acr=28
a=7000.
bet=0.002
gamm=0.00017
gdd0_min=800.
gdd0_max=1800.
fmax=0.9
nppmax=1.46
v1=0.000664
v2=0.119
v3=3.73
c1t=0.046
c2t=0.58
c3t=1.6
c1g=0.069
c2g=0.38
c3g=1.6
d1t=0.22
d2t=7.19
d3t=5.5
d1g=0.6
d2g=0.41
d3g=6.0
e1t=17.9
e2t=167.3
e3t=15.
e1g=0.67
e2g=50.5
e3g=100.
f1t=0.43
f2t=24.3
f3t=13.
f1g=0.34
f2g=17.8
f3g=50.
k2t=1.
k3t=0.017
k0t=0.6
k0g=0.2
k2g=0.55
k4g=0.025
k3g=0.013
t3g=1.
t1tn=4
t1td=1
deng=20
dentd=20
dentn=6

! change!
!  ps5=0.04
ps5=0.07
soilt=5
c14init=100.
c14tdec=1./8240.
!        the same decay rate of c14 is determined in ocn_bio.f: C14dec
c13frac=1-18./1000.
c13frac4=1-5./1000.
c13init=c13frac

RETURN
END SUBROUTINE initcpar

!********************************************************************

SUBROUTINE initmast
!********************************************************************

INCLUDE 'declar.inc'
INCLUDE 'bio.inc'
INCLUDE 'buffer.inc'
!********************************************************************

! land mask initialization, MASKB(i,j)=1 if there is land in cell (i,j)
! carea(i,j) is an area of cell (i,j) in mln sq.km.

DO n=1,ns
  DO i=1,it
    carea(i,n)=calc_area(10.,flonb(n),i)*frilb(i,n)
  END DO
END DO

RETURN
END SUBROUTINE initmast

!********************************************************************

REAL FUNCTION calc_area(x,y,i)
!********************************************************************
pi=3.14159
rad=6.371

! calculation of land area of cell with x degrees latitude size,
! y - latitude size, i is a number of step from Northern pole,
! unit - mln sq.km.

area=2*pi*x/360.*rad*2*pi*y/360.*rad* COS(2*pi*(90.+(0.5-i)*x)/360.)
calc_area=area

END FUNCTION calc_area
