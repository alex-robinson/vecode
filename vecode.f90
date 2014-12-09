
module vecode 

    implicit none 

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.d0)

    real(sp) :: st, sg, sd
    real(sp) :: sg4,  &
        snlt,  anup,  blai(2), pnpp,  &
        nstat, b12,   b34,  &
        b1,    b2,    b3,    b4,  &
        bc14,    bc13, anup13

    integer :: maskb
    real(sp) :: tatb, prcb, prcb5, gdd0b
    real(sp) :: tatmsmin, tatmsmax
    real(sp) :: gdd0, gdd0_min, gdd0_max,co2, pab
    integer :: ini_step, ktvm, kprom

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
    b1t,b2t,b3t,b4t,  &
    b1g,b2g,b3g,b4g,  &
    b1t14,b2t14,b3t14,b4t14,  &
    b1g14,b2g14,b3g14,b4g14,  &
    b1t13,b2t13,b3t13,b4t13,  &
    b1g13,b2g13,b3g13,b4g13

!     integer :: lat, lon 

    real(sp) :: &
    frilb, flonb

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

        !...1) Initial distribution of vegetation

        st   = 0.0
        sg   = 0.0
        sd   = 0.0
        blai = 0.0
        
        ktvm = 1   ! vegetation model   | OFF | STATIC  | DYNAMIC  | DYN+CC

        !...3) Initialisation of  TVM parameters

        call INITCPAR

        return 

    end subroutine init_tvm 

    subroutine tvm(tann,pann,pann5,gdd,pco2, &
                   forest,grass,desert,needles,carbon_uptake)

        implicit none 

        real(sp), intent(IN)    :: tann, pann, pann5, gdd, pco2
        real(sp), intent(INOUT) :: forest,grass,desert,needles,carbon_uptake
        integer :: k 

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

        ! output data: st - forest's share in cell
        !              sg - grass share in cell
        !              sd - desert's share in cell
        !              snlt - needle leaved trees ratio
        !              st+sg+sd=1, 0<= snlt<= 1
        !              plai - annual average of leaf area index, m2/m2
        !              anup - annual uptake of carbon in cell, Gt -> kg/m2 ???

        ave_t    = tann
        ave_pr   = pann
        ave_pr05 = pann5
        gdd0     = gdd
        co2      = pco2

!         if (ini_step == 0) then
!             call init_tvm
!             call ccstat
!             call ccstat_isotope
!         else
!             call ccdyn
!             if (ktvm == 1) then
!                 do kprom=1,19
!                     call ccdyn
!                 end do
!             end if
!         end if

!         ini_step=1

        ini_step = 0 
        call init_tvm
        call ccstat
        call ccstat_isotope
        ini_step=1
        
        do kprom = 1, 40
            call ccdyn 
        end do 

        forest  = st 
        grass   = sg 
        desert  = sd 
        needles = snlt  
        carbon_uptake = anup 

        return

    end subroutine tvm

    subroutine ccstat

        implicit none 

        !...1) calculation of initial carbon cycle parameters

        call ccparam

        ! calculation of equilibrium storages

        ! leaves biomass
        ! b1t is leaves phytomass for trees, b1g - for grass (kg C/m2)
        ! t1t is residence time of carbon in trees, t1g - in grass (years)

        b1t=k1t*t1t*npp
        b1g=k1g*t1g*npp

        !   stems and roots biomass

        b2t=(1-k1t)*t2t*npp
        b2g=(1-k1g)*t2g*npp

        !   litter

        b3t=(k0t*b1t/t1t+k2t/t2t*b2t)*t3t
        b3g=(k0g*b1g/t1g+k2g/t2g*b2g)*t3g

        ! mortmass and soil organic matter

        b4t=(k3t/t3t*b3t)*t4t
        b4g=(k4g/t2g*b2g+k3g/t3g*b3g)*t4g

        ! initialization of fraction dynamic variables

        st=forshare_st
        sd=desshare_st
        snlt=nlshare_st
        sg=1.0-st-sd

        call climpar

        return

    end subroutine ccstat

    subroutine ccstat_isotope

        implicit none 

        ! initialization of isotopes ratios

        b1t14=c14init*exp(-c14tdec*t1t)*b1t
        b1g14=c14init*exp(-c14tdec*t1g)*b1g

        b1t13=b1t*c13init*c13atm
        b1g13=b1g*c13init*c13atm

        !   stems and roots biomass

        b2t14=b2t*c14init*exp(-c14tdec*t2t)
        b2g14=b2g*c14init*exp(-c14tdec*t2g)

        b2t13=b2t*c13init*c13atm
        b2g13=b2g*c13init*c13atm

        !   litter

        b3t14=b3t*c14init*exp(-c14tdec*t3t)
        b3g14=b3g*c14init*exp(-c14tdec*t3g)

        b3t13=b3t*c13init*c13atm
        b3g13=b3g*c13init*c13atm

        ! mortmass and soil organic matter

        b4t14=b4t*c14init*exp(-c14tdec*t4t)
        b4g14=b4g*c14init*exp(-c14tdec*t4g)

        b4t13=b4t*c13init*c13atm
        b4g13=b4g*c13init*c13atm

        c13ratio=c13init

        return

    end subroutine ccstat_isotope




    subroutine ccdyn

        ! temporal var*/
        real(sp) :: tempor1,tempor2,tempor3,tempor4,db2,fd,dst,dd,nld,  &
                    dstime,tempor5,tempor6,dsg,dsd,temp_sg,temp_st
        real(sp) :: g4d 

        ! calculation of current carbon cycle parameters

        call ccparam

        ! calculation of fraction dynamic variables

        fd=forshare_st-st
        dd=desshare_st-sd
        nld=nlshare_st-snlt
        g4d=g4share_st-sg4
        temp_st=st
        temp_sg=sg

        ! calculation of forest dynamics; exponential filter
        dst=forshare_st-fd*exp(-1./t2t)-st
        st=st+dst
        snlt=nlshare_st-nld*exp(-1./t2t)

        ! desert dynamics; exponential filter
        dsd=desshare_st-dd*exp(-1./t2g)-sd
        tempor1=sd+dsd+st

        ! calculation of characteristic time of desert propagation
        if (tempor1 > 0.9) then
          dstime=t2g*(1-tempor1)*10.+t2t*(tempor1-0.9)*10
          dsd=desshare_st-dd*exp(-1./dstime)-sd
        end if

        sd=sd+dsd
        dsg=-dst-dsd

        sg=1.-st-sd
        sg4=g4share_st-g4d*exp(-1./t2g)

        if (sg < 0) sg=0
        if (st < 0) st=0
        if (sd < 0) sd=0


        ! calculation of dynamics of storages

        ! calculation of changes of storages due to conservation law

        ! correction for trees

        tempor1=b4t
        tempor2=b3t
        tempor3=b4t14
        tempor4=b3t14
        tempor5=b4t13
        tempor6=b3t13


        if(st > 0) then
          if(dst > 0) then
            
            b4t=(b4t*temp_st +b4g*dst)/st
            b3t=(b3t*temp_st +b3g*dst)/st
            
            b4t14=(b4t14*temp_st +b4g14*dst)/st
            b3t14=(b3t14*temp_st +b3g14*dst)/st
            
            b4t13=(b4t13*temp_st +b4g13*dst)/st
            b3t13=(b3t13*temp_st +b3g13*dst)/st
            
            
          end if
          
          b2t=b2t*temp_st/st
          b1t=b1t*temp_st/st
          
          b2t14=b2t14*temp_st/st
          b1t14=b1t14*temp_st/st
          
          b2t13=b2t13*temp_st/st
          b1t13=b1t13*temp_st/st
          
        end if

        ! correction for grass

        if(sg > 0) then
          if(dst > 0) then
            
            b4g=b4g*(temp_sg-dst) /sg
            b3g=b3g*(temp_sg-dst) /sg
            
            b4g14=b4g14*(temp_sg-dst) /sg
            b3g14=b3g14*(temp_sg-dst) /sg
            
            b4g13=b4g13*(temp_sg-dst) /sg
            b3g13=b3g13*(temp_sg-dst) /sg
            
            
          else
            
            b4g=(b4g*temp_sg-tempor1*dst) /sg
            b3g=(b3g*temp_sg-tempor2*dst) /sg
            
            b4g14=(b4g14*temp_sg -tempor3*dst)/sg
            b3g14=(b3g14*temp_sg -tempor4*dst)/sg
            
            b4g13=(b4g13*temp_sg -tempor5*dst)/sg
            b3g13=(b3g13*temp_sg -tempor6*dst)/sg
            
          end if
          
          b2g=b2g*temp_sg/sg
          b1g=b1g*temp_sg/sg
          
          b2g14=b2g14*temp_sg/sg
          b1g14=b1g14*temp_sg/sg
          
          b2g13=b2g13*temp_sg/sg
          b1g13=b1g13*temp_sg/sg
          
        end if


        ! slow soil organic matter

        b4t=b4t+k3t/t3t*b3t -b4t/t4t
        b4g=b4g+k4g/t2g*b2g+k3g/t3g*  &
            b3g-b4g/t4g

        b4t14=b4t14+k3t/t3t *b3t14  &
            -b4t14/t4t
        b4g14=b4g14+k4g/t2g *b2g14+k3g/t3g*  &
            b3g14-b4g14/t4g

        b4t13=b4t13+k3t/t3t *b3t13  &
            -b4t13/t4t
        b4g13=b4g13+k4g/t2g *b2g13+k3g/t3g*  &
            b3g13-b4g13/t4g

        !   fast soil organic matter

        b3t=b3t+b1t/t1t*k0t+  &
            k2t/t2t*b2t-b3t/t3t
        b3g=b3g+b1g/t1g*k0g+  &
            k2g/t2g*b2g-b3g/t3g

        b3t14=b3t14 +b1t14/t1t*k0t+  &
            k2t/t2t*b2t14 -b3t14/t3t

        b3g14=b3g14 +b1g14/t1g*k0g+  &
            k2g/t2g*b2g14 -b3g14/t3g

        b3t13=b3t13 +b1t13/t1t*k0t+  &
            k2t/t2t*b2t13 -b3t13/t3t

        b3g13=b3g13 +b1g13/t1g*k0g+  &
            k2g/t2g*b2g13 -b3g13/t3g

        ! leaves biomass


        b1t=b1t+k1t*npp-b1t/t1t
        b1g=k1g*npp*t1g


        b1t14=b1t14+k1t*npp*c14atm -b1t14/t1t
         b1g14=k1g*npp*c14atm*t1g

        b1t13=b1t13+k1t*npp*c13atm* c13frac-b1t13/t1t

        b1g13=k1g*npp*c13atm*(c13frac*(1-sg4)+  &
            c13frac4*sg4)*t1g

        !   stems and roots biomass

        b2t=b2t+(1-k1t)*npp-b2t/t2t
        b2g=b2g+(1-k1g)*npp-b2g/t2g


        b2t14=b2t14+(1-k1t)*npp*c14atm -b2t14/t2t

        b2g14=b2g14+(1-k1g)*npp*c14atm -b2g14/t2g

        b2t13=b2t13+(1-k1t)*npp*c13atm  &
            *c13frac-b2t13/t2t
        b2g13=b2g13+(1-k1g)*npp*c13atm  &
            *(c13frac*(1-sg4)+ c13frac4*sg4)-b2g13/t2g

        !   c14 annual decay
        b1t14=b1t14*(1-c14tdec)
        b2t14=b2t14*(1-c14tdec)
        b3t14=b3t14*(1-c14tdec)
        b4t14=b4t14*(1-c14tdec)
        b1g14=b1g14*(1-c14tdec)
        b2g14=b2g14*(1-c14tdec)
        b3g14=b3g14*(1-c14tdec)
        b4g14=b4g14*(1-c14tdec)

        call climpar

        return

    end subroutine ccdyn

    subroutine climpar

        implicit none 

        real(sp) :: tempor1, tempor2

        ! calculation of annual averaged LAI - lai

        laig=b1g*deng
        lait=b1t*(dentn*snlt+ dentd*(1-snlt))

        blai(1)=lait
        blai(2)=laig

        ! calculation of annual carbon uptake

        if (ktvm >= 2) then
          tempor1=b1+b2+ b3+b4
          tempor2=bc13
          
        else
          tempor1=0
        end if

        b1=b1t*st+b1g*sg
        b2=b2t*st+b2g*sg
        b3=b3t*st+b3g*sg
        b4=b4t*st+b4g*sg
        b12=b1+b2
        b34=b3+b4

        bc14=(b1t14+ b2t14+b3t14+  &
            b4t14)*st+ (b1g14+b2g14+  &
            b3g14+b4g14) *sg

        ! print *,lat,lon,b1t14,b1g14,b4t14

        bc13=(b1t13+ b2t13+b3t13+  &
            b4t13)*st+ (b1g13+b2g13+  &
            b3g13+b4g13) *sg

        anup=(b1+b2 +b3+b4-tempor1)
        anup13=bc13-tempor2

        !...      NET PRIMARY PRODUCTION

        pnpp=npp*(1-sd)

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
          
          pcr=acr*exp(gamm/2.0*differ)
          
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
        npp1=(1.0-exp(db1))
        npp2=1.0/(1.0+v3*exp(db2))
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

        ! if(TATMSMIN.lt.14) then
        if(tatmsmin < 12) then
          g4share_st=0
        else
        !   if(TATMSMIN.lt.17) then
        !    g4share_st=1-(17-TATMSMIN)/(17.-14.)
          if(tatmsmin < 15.5) then
            g4share_st=1-(15.5-tatmsmin)/3.5
          else
            g4share_st=1
          end if
        end if

        ! print *, lat,lon,TATMSMIN,g4share_st,sg4
        return

    end subroutine ccparam



    subroutine initcpar

        implicit none

        ! initialisation of variables
        ades=0.0011
        acr=28
        a=7000.0
        bet=0.002
        gamm=0.00017
        gdd0_min=800.0
        gdd0_max=1800.0
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

end module vecode 