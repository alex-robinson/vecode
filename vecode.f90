
module vecode 

    implicit none 

!     INCLUDE  'declar.f90'              
!     INCLUDE  'params.f90'
!     INCLUDE  'buffer.f90'      
!     INCLUDE  'bio.f90'
!     INCLUDE  'observ.f90'

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.d0)

    integer, parameter :: it = 18, ns = 7  !, &
!         it = 18, ix=19,  ns=7,   ns1=8,   ns2=9,  nz=10,  nz1=11,  &
!         LT=72,   lx=73,  noc=3,  noc1=4,  noc2=5, jt=20,  jx=21,  &
!         nt=17,   nst=6,  nsl=2,  ism=240, jsm=87,  &
!         ifn=77,  jfn=51, nif=11, njf=3,   nzlwr = 16

!     real(sp) :: a,bet,gamm,fmax,avecube,tmin,npp,nppmax,v1,v2,v3,  &
!                 c1t,c2t,c3t,c1g,c2g,c3g,  &
!                 d1t,d2t,d3t,d1g,d2g,d3g,  &
!                 e1t,e2t,e3t,e1g,e2g,e3g,  &
!                 f1t,f2t,f3t,f1g,f2g,f3g,  &
!                 k1t,k2t,k3t,k1g,k2g,k3g,  &
!                 t1t,t2t,t3t,t4t,t1g,t2g,t3g,t4g,  &
!                 ps1,ps2,ps3,ps4,ps5,soilt,  &
!                 forshare_st,t1tn,t1td, desshare_st,nlshare_st,g4share_st,  &
!                 deng,dentd,dentn,laig,lait,  &
!                 ave_t,ave_pr,ave_pr05,desmin,desmax,  &
!                 ades, acr, k0t, k0g, k4g,  &
!                 c14init,c14tdec,c13init,c13frac,c13ratio,c13frac4,c14ratio

!     COMMON /biodat/ a,bet,gamm,fmax,avecube,tmin,npp,nppmax,v1,v2,v3,  &
!     c1t,c2t,c3t,c1g,c2g,c3g, d1t,d2t,d3t,d1g,d2g,d3g,  &
!     e1t,e2t,e3t,e1g,e2g,e3g, f1t,f2t,f3t,f1g,f2g,f3g,  &
!     b1t(it,ns),b2t(it,ns),b3t(it,ns),b4t(it,ns),  &
!     b1g(it,ns),b2g(it,ns),b3g(it,ns),b4g(it,ns),  &
!     b1t14(it,ns),b2t14(it,ns),b3t14(it,ns),b4t14(it,ns),  &
!     b1g14(it,ns),b2g14(it,ns),b3g14(it,ns),b4g14(it,ns),  &
!     b1t13(it,ns),b2t13(it,ns),b3t13(it,ns),b4t13(it,ns),  &
!     b1g13(it,ns),b2g13(it,ns),b3g13(it,ns),b4g13(it,ns),  &
!     k1t,k2t,k3t,k1g,k2g,k3g, t1t,t2t,t3t,t4t,t1g,t2g,t3g,t4g,  &
!     ps1,ps2,ps3,ps4,ps5,soilt, forshare_st,desshare_st,g4share_st,  &
!     nlshare_st,t1tn,t1td, deng,dentd,dentn,laig,lait,desmin,desmax,  &
!     gdd0, gdd0_min,gdd0_max,co2,  &
!     ave_t,ave_pr,ave_pr05,carea(it,ns),lon,lat,init_flag(it,ns),  &
!     ades, acr, k0t, k0g, k4g,  &
!     c14init,c14tdec,c13init,c13frac,c13ratio,c13frac4,c14ratio,  &
!     soil13c(it,ns),carea_glac(it,ns)
   
!     ! ## observ.inc ###

!     COMMON /observ_dat/ &
!     !********
!      tauo(ix,noc,4),  &
!         tlev(it,jt,noc),    slev(it,jt,noc),   ss(it,noc),  &
!         sst(it,noc,12),     ssta(it,ns,12),    friceobs(it,noc,12),  &
!         stobs(it,ns),       sgobs(it,ns),      sdobs(it,ns), blaiobs(it,ns,2), &

!     ! OBSERV_INT
!     !************
!      sst_i(LT,noc,12), ss_i(LT,noc), &

!     ! CLIMATO_CONSTRAINTS
!     !********************
!      tgmin, tgmax, prcmin, prcmax, tvmin, tvmax, fatmxmin, fatmxmax,  &
!         ismin, ismax, inmin, inmax, xamxmin, xamxmax, &

!     ! SPINUP TIME
!     !************
!     ispinup

!     ! ## bio.inc ###
!     COMMON /geogbio/ maskb(it,ns), frilb(it,ns), flonb(ns)
!     COMMON /climate/ tatb(it,ns),  prcb(it,ns), prcb5(it,ns),  &
!         gdd0b(it,ns),tatmsmin(it,ns),tatmsmax(it,ns)
!     COMMON /biota/ st(it,ns),    sg(it,ns),    sd(it,ns), sg4(it,ns),  &
!         snlt(it,ns),  anup(it,ns),  blai(it,ns,2), pnpp(it,ns),  &
!         nstat(it,ns), b12(it,ns),   b34(it,ns),  &
!         b1(it,ns),    b2(it,ns),    b3(it,ns),    b4(it,ns),  &
!         bc14(it,ns),    bc13(it,ns), anup13(it,ns)


!     ! ## params.inc ###

!     CHARACTER(8) ENAME 
!     CHARACTER(512) INFLDR 
!     CHARACTER(512) OUTFLDR 
!     CHARACTER(512) RESTART_IN 
     
!     COMMON /PARAMS/                                                  &
!       NAME,                                                          &
!       NFLDR,OUTFLDR,                                                 &
!       TIME,  KCAL,  KLSR, KCFC,                                      &
!       LWR,  KLSF, KES, KOCN, KOCNW,  KTVM,                           &
!       SEM,   KOCAR, KSLR,  KCCC,  KCCR,  KGEU,  KDFLX,               &
!       SED, KWEATH, KCORAL, KIRON, KLAND, KYEDOM, KFRAC,              &
!       SOLC,  KCO2,  KCO2D, KOUT,  K5,    KFB, KW_FBA,                &
!       ZLEV, KW_ZLEV, KRH, KW_RH, K2RH, KWV, SHRH, KCOD,              &
!       OUT_AFX,KOUT_A2GC,KOUT_A2GF,KOUT_A2ZT,KOUT_A2ZX,KOUT_A3D,      &
!       OUT_OCNF,KOUT_OCNS,KOUT_OCNT,KOUT_OCNX,KOUT_Q3D,KOUT_SLR2D,    &
!       OUT_A2G, KOUT_HIST, KOUT_CPL, RESTART_IN,                      &
!       COUP,  KENDY, KENDM, KMON, KMON2, KBIO, KOCEAN, KOCEANO,       &
!       ODS,   NSEMI, KINVAL, KREST,                                   &
!       ODD, KODEMIL,                                                  &
!       YRSR, NYRA,  NYRC, NYRMX, MONMX, NDYMX,  NTSMX, NYRREW1,       &
!       TS,   NDAY,  NYR,  NYRO,  MON,   MONO,   NJUL,  NYRREW2,       &
!       FOUT, NAOUT, NSEM,                                             &
!       JUL,  TST,   TSTO, TSTOC, TSTIC, TDAY,  TYER,   BTIME,         &
!       I, G,  T0,  SIGM, CKARM, R, CLE,  CLS,  CLM, CHSNW,            &
!       W, RO, CR,  RA, CAP, CAV,  HATM,  ERAD,  P0, S0,               &
!       MEG, FCOR(IX), FCORT(IT), FCORU(IX), ABSF(IX),                 &
!       BL, HEATM, ZSURF, GTO, GTT, TAMG, FROCNMIN,                    &
!       EPS, CCD, CCDRF,                                          &
!       TOA,                                                           &
!       A, PA0, PAR, PAB, C14ATM, C13ATM, O18ATM, emis, CUMCO2,        &
!       RFA,                                                           &
!       FIT(IT),         FIX(IX),          DY,             DPHI,       &
!       SINT(IT),        COST(IT),         SINU(IX),       COSU(IX),   &
!       SQRA(IT,NS),     SQRO(IT,NS),                                  &
!       SQA,             SQO,              SQL,             sea_level, &
!       OVOL,            DX(IT),                                       &
!       FROCN(IT,NS),    FRLND(IT,NS),     FRGLC(IT,NS),   NOA(IT,NS), &
!       FRIOG(IT),       SIGNF(IT),                                    &
!       FLON(NS),        FRS(NS),          FRSU(NS1),                  &
!       DXT(IT,NS),      DXX(IX,NS),       DXU(IX,NS1),   DXTT(IT,NS1),&
!       ZTS(IT,NS),      ZTSC(IT,NS2),     ZL(NZ),        ZL2(NZ1),    &
!       PL(NZ),          PL2(NZ1),         DPL(NZ),                    &
!       DPX(IT,NS1,NZ),  DPY(IX,NS,NZ),    DPT(IT,NS,NZ),              &
!       PX(IT,NS1),      PY(IX,NS),        PT(IT,NS),                  & 
!       ZSXM(IT,NS1),    ZSYM(IX,NS),                                  &                   
                                                                        
! ! Ocean                                                                 
                                                                         
!       FITO(LT),        FIXO(LX),         DYO,                        &               
!       SINTO(LT),       COSTO(LT),        SINXO(LX),      COSXO(LX),  &
!       ODXT(LT,NOC),    ODXX(LX,NOC),     ZT(JT),                     &
!       ZX(JX),          ZZ(JT),           ZZT(JX),                    &
!       MASKT(LT,NOC),   MASKX(LT,NOC1),   MASKY(LX,NOC),              &
!       MGT(LT,JT,NOC),  MFX(LT,JT,NOC1),  MFY(LX,JT,NOC),             &
!       MFZ(LT,JX,NOC),  MGX(LX,JX,NOC),   FRIOB(LT,NOC1),             &
!       HOC(LT,NOC),     HOCX(LT,NOC1),    HOCY(LX,NOC),               &
!       SQRA2(LT,NS),    SQRO2(LT,NOC),    DVOL(LT,JT,NOC),            &
!       FCORTO(LX),      FCORXO(LX),                                   &
!       FSB, FIE, FBS,                                                 &
                                                                        
! ! Land                                                                  
                                                                        
!       HORO(IT,NS),     HOROL(IT,NS),     HOROG(IT,NS),               &
!       SIGORO(IT,NS),   ZTST(IT,NS,NST),                              &
!       PSUR(IT,NS),     PSURC(IT,NS2)

    character(len=256) :: INFLDR 
        
    real(sp) :: st(it,ns), sg(it,ns), sd(it,ns)
    real(sp) :: sg4(it,ns),  &
        snlt(it,ns),  anup(it,ns),  blai(it,ns,2), pnpp(it,ns),  &
        nstat(it,ns), b12(it,ns),   b34(it,ns),  &
        b1(it,ns),    b2(it,ns),    b3(it,ns),    b4(it,ns),  &
        bc14(it,ns),    bc13(it,ns), anup13(it,ns)
    real(sp) :: carea(it,ns), carea_glac(it,ns)

    integer :: maskb(it,ns)
    real(sp) :: tatb(it,ns), prcb(it,ns), prcb5(it,ns), gdd0b(it,ns)
    real(sp) :: tatmsmin(it,ns), tatmsmax(it,ns)
    real(sp) :: gdd0, gdd0_min, gdd0_max,co2, pab
    integer :: ini_step, klsr, ktvm, kprom

    real(sp) :: &
    a,bet,gamm,fmax,avecube,tmin,npp,nppmax,v1,v2,v3,  &
    c1t,c2t,c3t,c1g,c2g,c3g,  &
    d1t,d2t,d3t,d1g,d2g,d3g,  &
    e1t,e2t,e3t,e1g,e2g,e3g,  &
    f1t,f2t,f3t,f1g,f2g,f3g,  &
    k1t,k2t,k3t,k1g,k2g,k3g,  &
    t1t,t2t,t3t,t4t,t1g,t2g,t3g,t4g,  &
    ps1,ps2,ps3,ps4,ps5,soilt,  &
    forshare_st,t1tn,t1td, desshare_st,nlshare_st,g4share_st,  &
    deng,dentd,dentn,laig,lait,  &
    ave_t,ave_pr,ave_pr05,desmin,desmax,  &
    ades, acr, k0t, k0g, k4g,  &
    c14init,c14tdec,c13init,c13frac,c13ratio,c13frac4,c14ratio

    real(sp) :: &
    b1t(it,ns),b2t(it,ns),b3t(it,ns),b4t(it,ns),  &
    b1g(it,ns),b2g(it,ns),b3g(it,ns),b4g(it,ns),  &
    b1t14(it,ns),b2t14(it,ns),b3t14(it,ns),b4t14(it,ns),  &
    b1g14(it,ns),b2g14(it,ns),b3g14(it,ns),b4g14(it,ns),  &
    b1t13(it,ns),b2t13(it,ns),b3t13(it,ns),b4t13(it,ns),  &
    b1g13(it,ns),b2g13(it,ns),b3g13(it,ns),b4g13(it,ns)

    integer :: lat, lon 

    real(sp) :: &
    frilb(it,ns), flonb(ns)

    ! Params
    real(sp) :: c13atm, c14atm

contains

    subroutine init_tvm
        !  Purpose: preparation of initial conditions and parameters
        !           for terrsetrial vegetation  module
        !  By A.Ganopolski  
        !  Last modification: 10.12.2004
        !  Adapted to Fortran90: ajr, 08.12.2014

        implicit none 

        integer :: i, n  


        !...1) Initial distribution of vegetation

        if (KLSR.eq.0) then

            st   = 0.0
            sg   = 0.0
            sd   = 0.0
            blai = 0.0

        endif
      
        !...2) Land mask initialisation
   
        call INITMAST

        !...3) Initialisation of  TVM parameters

        call INITCPAR

        !     getting correct land area     
        open (555,file=trim(INFLDR)//'GEO/landarea.dat')
        do i=1,IT
            read(555,*) (carea(i,n),n=1,NS)
        enddo
        close (555)

        !     getting correct glacial land area
        open (555,file=trim(INFLDR)//'GEO/landarea_glac.dat')
        do i=1,IT
            read(555,*) (carea_glac(i,n),n=1,NS)
!             print *,(carea_glac(i,n),n=1,NS)
        enddo
        close (555)

        return 

    end subroutine init_tvm 

    subroutine tvm

        implicit none 

        !      TERRESTRIAL VEGETATION ANNUAL MODEL

        !  Purpose: Computation of forest/grass/desert ratio;
        !           NPP and living and soil biomass

        !  By: V.Brovkin
        !  Modifyed by: A.Ganopolski
        !  Last modification: 17.11.97


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


        !...    SPATIAL LOOP

        integer :: k, i 

        do k=1,NS
          do i=1,IT
            if (maskb(i,k) == 1) then
              
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
            
              if (ini_step == 0.AND.klsr == 0) then
                CALL ccstat
                CALL ccstat_isotope
              else
                CALL ccdyn
                if (ktvm == 1) then
                  do kprom=1,19
                    CALL ccdyn
                  end do
                end if
              end if
              
        !       endif
              
              
            end if
            
          end do
        end do

        ini_step=1

        return

    end subroutine tvm

    subroutine ccstat

        implicit none 

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

        return

    end subroutine ccstat

    subroutine ccstat_isotope

        implicit none 

        ! initialization of isotopes ratios

        b1t14(lat,lon)=c14init*exp(-c14tdec*t1t)*b1t(lat,lon)
        b1g14(lat,lon)=c14init*exp(-c14tdec*t1g)*b1g(lat,lon)

        b1t13(lat,lon)=b1t(lat,lon)*c13init*c13atm
        b1g13(lat,lon)=b1g(lat,lon)*c13init*c13atm

        !   stems and roots biomass

        b2t14(lat,lon)=b2t(lat,lon)*c14init*exp(-c14tdec*t2t)
        b2g14(lat,lon)=b2g(lat,lon)*c14init*exp(-c14tdec*t2g)

        b2t13(lat,lon)=b2t(lat,lon)*c13init*c13atm
        b2g13(lat,lon)=b2g(lat,lon)*c13init*c13atm

        !   litter

        b3t14(lat,lon)=b3t(lat,lon)*c14init*exp(-c14tdec*t3t)
        b3g14(lat,lon)=b3g(lat,lon)*c14init*exp(-c14tdec*t3g)

        b3t13(lat,lon)=b3t(lat,lon)*c13init*c13atm
        b3g13(lat,lon)=b3g(lat,lon)*c13init*c13atm

        ! mortmass and soil organic matter

        b4t14(lat,lon)=b4t(lat,lon)*c14init*exp(-c14tdec*t4t)
        b4g14(lat,lon)=b4g(lat,lon)*c14init*exp(-c14tdec*t4g)

        b4t13(lat,lon)=b4t(lat,lon)*c13init*c13atm
        b4g13(lat,lon)=b4g(lat,lon)*c13init*c13atm

        c13ratio=c13init

        return

    end subroutine ccstat_isotope




    subroutine ccdyn

        ! temporal var*/
        real(sp) :: tempor1,tempor2,tempor3,tempor4,db2,fd,dst,dd,nld,  &
                    dstime,tempor5,tempor6,dsg,dsd,temp_sg,temp_st
        real(sp) :: g4d 

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
        dst=forshare_st-fd*exp(-1./t2t)-st(lat,lon)
        st(lat,lon)=st(lat,lon)+dst
        snlt(lat,lon)=nlshare_st-nld*exp(-1./t2t)

        ! desert dynamics; exponential filtre
        dsd=desshare_st-dd*exp(-1./t2g)-sd(lat,lon)
        tempor1=sd(lat,lon)+dsd+st(lat,lon)

        ! calculation of characteristic time of desert propagation
        if (tempor1 > 0.9) then
          dstime=t2g*(1-tempor1)*10.+t2t*(tempor1-0.9)*10
          dsd=desshare_st-dd*exp(-1./dstime)-sd(lat,lon)
        end if

        sd(lat,lon)=sd(lat,lon)+dsd
        dsg=-dst-dsd

        sg(lat,lon)=1.-st(lat,lon)-sd(lat,lon)
        sg4(lat,lon)=g4share_st-g4d*exp(-1./t2g)

        if (sg(lat,lon) < 0) sg(lat,lon)=0
        if (st(lat,lon) < 0) st(lat,lon)=0
        if (sd(lat,lon) < 0) sd(lat,lon)=0


        ! calculation of dynamics of storages

        ! calculation of changes of storages due to conservation law

        ! correction for trees

        tempor1=b4t(lat,lon)
        tempor2=b3t(lat,lon)
        tempor3=b4t14(lat,lon)
        tempor4=b3t14(lat,lon)
        tempor5=b4t13(lat,lon)
        tempor6=b3t13(lat,lon)


        if(st(lat,lon) > 0) then
          if(dst > 0) then
            
            b4t(lat,lon)=(b4t(lat,lon)*temp_st +b4g(lat,lon)*dst)/st(lat,lon)
            b3t(lat,lon)=(b3t(lat,lon)*temp_st +b3g(lat,lon)*dst)/st(lat,lon)
            
            b4t14(lat,lon)=(b4t14(lat,lon)*temp_st +b4g14(lat,lon)*dst)/st(lat,lon)
            b3t14(lat,lon)=(b3t14(lat,lon)*temp_st +b3g14(lat,lon)*dst)/st(lat,lon)
            
            b4t13(lat,lon)=(b4t13(lat,lon)*temp_st +b4g13(lat,lon)*dst)/st(lat,lon)
            b3t13(lat,lon)=(b3t13(lat,lon)*temp_st +b3g13(lat,lon)*dst)/st(lat,lon)
            
            
          end if
          
          b2t(lat,lon)=b2t(lat,lon)*temp_st/st(lat,lon)
          b1t(lat,lon)=b1t(lat,lon)*temp_st/st(lat,lon)
          
          b2t14(lat,lon)=b2t14(lat,lon)*temp_st/st(lat,lon)
          b1t14(lat,lon)=b1t14(lat,lon)*temp_st/st(lat,lon)
          
          b2t13(lat,lon)=b2t13(lat,lon)*temp_st/st(lat,lon)
          b1t13(lat,lon)=b1t13(lat,lon)*temp_st/st(lat,lon)
          
        end if

        ! correction for grass

        if(sg(lat,lon) > 0) then
          if(dst > 0) then
            
            b4g(lat,lon)=b4g(lat,lon)*(temp_sg-dst) /sg(lat,lon)
            b3g(lat,lon)=b3g(lat,lon)*(temp_sg-dst) /sg(lat,lon)
            
            b4g14(lat,lon)=b4g14(lat,lon)*(temp_sg-dst) /sg(lat,lon)
            b3g14(lat,lon)=b3g14(lat,lon)*(temp_sg-dst) /sg(lat,lon)
            
            b4g13(lat,lon)=b4g13(lat,lon)*(temp_sg-dst) /sg(lat,lon)
            b3g13(lat,lon)=b3g13(lat,lon)*(temp_sg-dst) /sg(lat,lon)
            
            
          else
            
            b4g(lat,lon)=(b4g(lat,lon)*temp_sg-tempor1*dst) /sg(lat,lon)
            b3g(lat,lon)=(b3g(lat,lon)*temp_sg-tempor2*dst) /sg(lat,lon)
            
            b4g14(lat,lon)=(b4g14(lat,lon)*temp_sg -tempor3*dst)/sg(lat,lon)
            b3g14(lat,lon)=(b3g14(lat,lon)*temp_sg -tempor4*dst)/sg(lat,lon)
            
            b4g13(lat,lon)=(b4g13(lat,lon)*temp_sg -tempor5*dst)/sg(lat,lon)
            b3g13(lat,lon)=(b3g13(lat,lon)*temp_sg -tempor6*dst)/sg(lat,lon)
            
          end if
          
          b2g(lat,lon)=b2g(lat,lon)*temp_sg/sg(lat,lon)
          b1g(lat,lon)=b1g(lat,lon)*temp_sg/sg(lat,lon)
          
          b2g14(lat,lon)=b2g14(lat,lon)*temp_sg/sg(lat,lon)
          b1g14(lat,lon)=b1g14(lat,lon)*temp_sg/sg(lat,lon)
          
          b2g13(lat,lon)=b2g13(lat,lon)*temp_sg/sg(lat,lon)
          b1g13(lat,lon)=b1g13(lat,lon)*temp_sg/sg(lat,lon)
          
        end if


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

        return

    end subroutine ccdyn



    subroutine climpar

        implicit none 

        real(sp) :: tempor1, tempor2

        ! calculation of annual averaged LAI - lai

        laig=b1g(lat,lon)*deng
        lait=b1t(lat,lon)*(dentn*snlt(lat,lon)+ dentd*(1-snlt(lat,lon)))

        blai(lat,lon,1)=lait
        blai(lat,lon,2)=laig

        ! calculation of annual carbon uptake

        if (ktvm >= 2) then
          tempor1=b1(lat,lon)+b2(lat,lon)+ b3(lat,lon)+b4(lat,lon)
          tempor2=bc13(lat,lon)
          
        else
          tempor1=0
        end if

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

        return

    end subroutine climpar



    subroutine ccparam

        implicit none 

        real(sp) :: npp1,npp2,db1,db2,db3,avefor,differ,pcr

        real(sp) :: ave_tresh, pstemp, pstem

        ! calculation of current cycle parameters

        ! potential trees share

        avefor=ave_pr*ave_pr*ave_pr*ave_pr
        differ=gdd0-gdd0_min
        db1=-bet*differ
        db2=gamm*differ
        db3=differ*differ

        if(differ < 0) then
          forshare_st=0
        else
          forshare_st=(1-exp(db1))*avefor/(avefor+a*db3*exp(db2))
        end if
        if (forshare_st > fmax) forshare_st=fmax

        ! potential desert share - desshare_st

        desshare_st=0

        ! northern deserts
        !v change! permafrost soils version - no cold deserts!

        !v       if(gdd0.lt.100) desshare_st=1

        !v      if(gdd0.ge.100.and.gdd0.lt.gdd0_min)
        !v     >      desshare_st=(gdd0_min-gdd0)/(gdd0_min-100.)

        ! southern deserts

        if (gdd0 >= gdd0_max) then
          
          pcr=acr*exp(gamm/2.*differ)
          
          if (ave_pr05 <= pcr) then
            desshare_st=1
            forshare_st=0
          else
            db2=(ave_pr05-pcr)/exp(gamm*differ)
            desshare_st=1.03/(1+ades*db2*db2)-0.03
            if (desshare_st < 0) desshare_st=0
          end if
          
        end if

        ! calculation of NPP, Lieth's formula

        db1=-v1*ave_pr
        if (gdd0 >= gdd0_max) db1=-v1*ave_pr05
        db2=-v2*ave_t
        npp1=(1.-exp(db1))
        npp2=1./(1.+v3*exp(db2))
        if(npp1 < npp2) then
          npp=nppmax*npp1
        else
          npp=nppmax*npp2
        end if

        ! CO2 enrichment factor

        npp=npp*(1+0.25/ALOG(2.0)*ALOG(co2/280.0))

        !v change! npp in cold regions
        if(gdd0 < gdd0_min) then
          if(gdd0 >= 100) then
            npp=npp*(gdd0_min-gdd0)/(gdd0_min-100.0)
          else
            npp=0
          end if
        end if


        ! allocation factors and residence time of leaves biomass

        k1t=c1t+c2t/(1+c3t*npp)
        k1g=c1g+c2g/(1+c3g*npp)

        t1t=d1t+d2t/(1+d3t*npp)
        t1g=d1g+d2g/(1+d3g*npp)

        !   residence time of stems and roots biomass

        t2t=e1t+e2t/(1+e3t*npp)
        t2g=e1g+e2g/(1+e3g*npp)

        !   residence time of fast carbon pool

        t3t=16*exp(-ps5*(ave_t-soilt))
        t3g=40*exp(-ps5*(ave_t-soilt))

        ! residence time of slow soil organic matter



        !v parameterization of permafrost effect on carbon residence time
        ave_tresh=-10.0

        if(ave_t > ave_tresh) then
          pstemp=-ps5*(ave_t-soilt)
          
        else
          pstem=-ps5*(ave_t-soilt)- (2.38+ps5*(ave_t-soilt))*(ave_t-ave_tresh)/30.0
          pstemp=2.38
        end if

        !v no respiration if gdd0 is below 100 dd
        if(gdd0 <= 100) pstemp=6.0

        t3t=16*exp(pstemp)
        t3g=40*exp(pstemp)
        t4t=900*exp(pstemp)
        t4g=t4t

        !calculation of potential nedleleaves trees ratio

        nlshare_st=(t1t-t1td)/(t1tn-t1td)
        if (nlshare_st > 1) nlshare_st=1
        if (nlshare_st < 0) nlshare_st=0

        !calculation of c4 grass fraction

        ! if(TATMSMIN(lat,lon).lt.14) then
        if(tatmsmin(lat,lon) < 12) then
          g4share_st=0
        else
        !   if(TATMSMIN(lat,lon).lt.17) then
        !    g4share_st=1-(17-TATMSMIN(lat,lon))/(17.-14.)
          if(tatmsmin(lat,lon) < 15.5) then
            g4share_st=1-(15.5-tatmsmin(lat,lon))/3.5
          else
            g4share_st=1
          end if
        end if

        ! print *, lat,lon,TATMSMIN(lat,lon),g4share_st,sg4(lat,lon)
        return

    end subroutine ccparam



    subroutine initcpar

        implicit none

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
        e3t=15.0
        e1g=0.67
        e2g=50.5
        e3g=100.0
        f1t=0.43
        f2t=24.3
        f3t=13.0
        f1g=0.34
        f2g=17.8
        f3g=50.0
        k2t=1.0
        k3t=0.017
        k0t=0.6
        k0g=0.2
        k2g=0.55
        k4g=0.025
        k3g=0.013
        t3g=1.0
        t1tn=4
        t1td=1
        deng=20
        dentd=20
        dentn=6

        ! change!
        !  ps5=0.04
        ps5=0.07
        soilt=5
        c14init=100.0
        c14tdec=1.0/8240.0
        ! the same decay rate of c14 is determined in ocn_bio.f: C14dec
        c13frac=1-18.0/1000.0
        c13frac4=1-5.0/1000.0
        c13init=c13frac

        return 

    end subroutine initcpar

    subroutine initmast

        implicit none
        ! land mask initialization, MASKB(i,j)=1 if there is land in cell (i,j)
        ! carea(i,j) is an area of cell (i,j) in mln sq.km.

        integer :: n, i 

        do n=1,ns
          do i=1,it
            carea(i,n)=calc_area(10.0,flonb(n),i)*frilb(i,n)
          end do
        end do 

        return

    end subroutine initmast

    elemental function calc_area(x,y,i) result(area)

        implicit none 

        real(sp), intent(IN) :: x, y
        integer,  intent(IN) :: i 
        real(sp) :: area
        
        real(sp), parameter :: pi  = 3.14159
        real(sp), parameter :: rad = 6.371

        ! calculation of land area of cell with x degrees latitude size,
        ! y - latitude size, i is a number of step from Northern pole,
        ! unit - mln sq.km.

        area=2.0*pi*x/360.0*rad*2.0*pi*y/360.0*rad* cos(2.0*pi*(90.0+(0.5-i)*x)/360.0)

        return

    end function calc_area
    

end module vecode 