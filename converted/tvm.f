*********************************************************************
                SUBROUTINE TVM
*********************************************************************
*      TERRESTRIAL VEGETATION ANNUAL MODEL
*
*  Purpose: Computation of forest/grass/desert ratio;
*           NPP and living and soil biomass
*
*  By: V.Brovkin
*  Modifyed by: A.Ganopolski
*  Last modification: 17.11.97
*********************************************************************
       INCLUDE 'declar.inc'
       INCLUDE 'params.inc'
       INCLUDE 'bio.inc'
       INCLUDE 'buffer.inc'
*********************************************************************
c input data: annual mean temperature in degr. Celc. - ave_t
c             annual mean precipitation, mm/year - ave_pr
c             growing degree days above 0, degr. Celc - gdd0
c             CO2 concentration in the atmosphere, ppm - co2
c
c output data: st(i,j) - forest's share in cell i,j
c              sg(i,j) - grass share in cell i,j
c              sd(i,j) - desert's share in cell i,j
c              snlt(i,j) -needle leaved trees ratio
c              st(i,j)+sg(i,j)+sd(i,j)=1, 0<= snlt(i,j)<= 1
c              plai(i,j) - annual average of leaf area index, m2/m2
c              anup(i,j) - annual uptake of carbon in cell, Gt -> kg/m2 ???
c
c COMMON block described in buffer.inc
*********************************************************************

c...    SPATIAL LOOP

	do k=1,NS
	do i=1,IT
	if (MASKB(i,k).eq.1) then

	    ave_t= TATB(i,k)
	    ave_pr=PRCB(i,k)
            ave_pr05=PRCB5(i,k)
	    gdd0=GDD0B(i,k)
* co2 enrichment
            co2=PAB
            lat=i
	    lon=k

c	     if (KTVM.eq.1) then
c   equilibrium run

c... calculation of initial forest ratio and amount of carbon in pools based
c       on equilbrium state; carbon uptake equals to steady state amount

c	         call CCSTAT

c                else

c... calculation of dynamics of carbon pools and forest ratio
		 
 	         if (INI_STEP.eq.0.and.KLSR.eq.0) then
 	           call CCSTAT
 	           call CCSTAT_ISOTOPE
 	         else
 	           call CCDYN
	           if (KTVM.eq.1) then
		      do kprom=1,19
 	              call CCDYN
	              enddo
	              endif
 	         endif

c 	     endif
		  

	    endif

	enddo
	enddo

	INI_STEP=1

	return
	end

*********************************************************************
              SUBROUTINE CCSTAT
*********************************************************************
       INCLUDE 'declar.inc'
       INCLUDE 'params.inc'
       INCLUDE 'bio.inc'
       INCLUDE 'buffer.inc'
*********************************************************************
C...1) calculation of initial carbon cycle parameters

	call CCPARAM

* calculation of equilibrium storages

* leaves biomass
* b1t is leaves phytomass for trees, b1g - for grass (kg C/m2)
* t1t is residence time of carbon in trees, t1g - in grass (years)

	b1t(lat,lon)=k1t*t1t*npp
	b1g(lat,lon)=k1g*t1g*npp

*   stems and roots biomass

	b2t(lat,lon)=(1-k1t)*t2t*npp
	b2g(lat,lon)=(1-k1g)*t2g*npp

*   litter

	b3t(lat,lon)=(k0t*b1t(lat,lon)/t1t+k2t/t2t*b2t(lat,lon))*t3t
	b3g(lat,lon)=(k0g*b1g(lat,lon)/t1g+k2g/t2g*b2g(lat,lon))*t3g

* mortmass and soil organic matter

	b4t(lat,lon)=(k3t/t3t*b3t(lat,lon))*t4t
	b4g(lat,lon)=(k4g/t2g*b2g(lat,lon)+k3g/t3g*b3g(lat,lon))*t4g

* initialization of fraction dynamic variables
        
        st(lat,lon)=forshare_st
        sd(lat,lon)=desshare_st
        snlt(lat,lon)=nlshare_st
        sg(lat,lon)=1.-st(lat,lon)-sd(lat,lon)

	call CLIMPAR
	
	return
	end
	
              SUBROUTINE CCSTAT_ISOTOPE
*********************************************************************
       INCLUDE 'declar.inc'
       INCLUDE 'params.inc'
       INCLUDE 'bio.inc'
       INCLUDE 'buffer.inc'
*********************************************************************
* initialization of isotopes ratios

	b1t14(lat,lon)=c14init*exp(-c14tdec*t1t)*b1t(lat,lon)
	b1g14(lat,lon)=c14init*exp(-c14tdec*t1g)*b1g(lat,lon)

	b1t13(lat,lon)=b1t(lat,lon)*c13init*c13atm
	b1g13(lat,lon)=b1g(lat,lon)*c13init*c13atm

*   stems and roots biomass

	b2t14(lat,lon)=b2t(lat,lon)*c14init*exp(-c14tdec*t2t)
	b2g14(lat,lon)=b2g(lat,lon)*c14init*exp(-c14tdec*t2g)

	b2t13(lat,lon)=b2t(lat,lon)*c13init*c13atm
	b2g13(lat,lon)=b2g(lat,lon)*c13init*c13atm

*   litter

	b3t14(lat,lon)=b3t(lat,lon)*c14init*exp(-c14tdec*t3t)
	b3g14(lat,lon)=b3g(lat,lon)*c14init*exp(-c14tdec*t3g)

	b3t13(lat,lon)=b3t(lat,lon)*c13init*c13atm
	b3g13(lat,lon)=b3g(lat,lon)*c13init*c13atm

* mortmass and soil organic matter

	b4t14(lat,lon)=b4t(lat,lon)*c14init*exp(-c14tdec*t4t)
	b4g14(lat,lon)=b4g(lat,lon)*c14init*exp(-c14tdec*t4g)

	b4t13(lat,lon)=b4t(lat,lon)*c13init*c13atm
	b4g13(lat,lon)=b4g(lat,lon)*c13init*c13atm
	
	c13ratio=c13init
	
	return
	end
	
	
*********************************************************************
              SUBROUTINE CCDYN
*********************************************************************
       INCLUDE 'declar.inc'
       INCLUDE 'params.inc'
       INCLUDE 'bio.inc'
       INCLUDE 'buffer.inc'
*********************************************************************
* temporal var*/
	REAL tempor1,tempor2,tempor3,tempor4,db2,fd,dst,dd,nld,
     >        dstime,tempor5,tempor6,dsg,dsd,temp_sg,temp_st
        
* calculation of current carbon cycle parameters

	call CCPARAM
	
* calculation of fraction dynamic variables

	fd=forshare_st-st(lat,lon)
	dd=desshare_st-sd(lat,lon)
	nld=nlshare_st-snlt(lat,lon)
	g4d=g4share_st-sg4(lat,lon)
        temp_st=st(lat,lon)
        temp_sg=sg(lat,lon)
        
* calculation of forest dynamics; exponential filtre
        dst=forshare_st-fd*exp(-1./t2t)-st(lat,lon)
        st(lat,lon)=st(lat,lon)+dst
        snlt(lat,lon)=nlshare_st-nld*exp(-1./t2t)
	
* desert dynamics; exponential filtre
        dsd=desshare_st-dd*exp(-1./t2g)-sd(lat,lon)
        tempor1=sd(lat,lon)+dsd+st(lat,lon)

* calculation of characteristic time of desert propagation
        if (tempor1.gt.0.9) then
             dstime=t2g*(1-tempor1)*10.+t2t*(tempor1-0.9)*10
             dsd=desshare_st-dd*exp(-1./dstime)-sd(lat,lon)
        endif
        
        sd(lat,lon)=sd(lat,lon)+dsd
        dsg=-dst-dsd

        sg(lat,lon)=1.-st(lat,lon)-sd(lat,lon)
	sg4(lat,lon)=g4share_st-g4d*exp(-1./t2g)

	if (sg(lat,lon).lt.0) sg(lat,lon)=0
	if (st(lat,lon).lt.0) st(lat,lon)=0
	if (sd(lat,lon).lt.0) sd(lat,lon)=0
	
	 
* calculation of dynamics of storages

* calculation of changes of storages due to conservation law 

* correction for trees

        tempor1=b4t(lat,lon)
        tempor2=b3t(lat,lon)
        tempor3=b4t14(lat,lon)
        tempor4=b3t14(lat,lon)
        tempor5=b4t13(lat,lon)
        tempor6=b3t13(lat,lon)

	 	
        if(st(lat,lon).gt.0) then
          if(dst.gt.0) then  

            b4t(lat,lon)=(b4t(lat,lon)*temp_st
     >         +b4g(lat,lon)*dst)/st(lat,lon)
            b3t(lat,lon)=(b3t(lat,lon)*temp_st
     >         +b3g(lat,lon)*dst)/st(lat,lon)

            b4t14(lat,lon)=(b4t14(lat,lon)*temp_st
     >         +b4g14(lat,lon)*dst)/st(lat,lon)
            b3t14(lat,lon)=(b3t14(lat,lon)*temp_st
     >         +b3g14(lat,lon)*dst)/st(lat,lon)

            b4t13(lat,lon)=(b4t13(lat,lon)*temp_st
     >         +b4g13(lat,lon)*dst)/st(lat,lon)
            b3t13(lat,lon)=(b3t13(lat,lon)*temp_st
     >         +b3g13(lat,lon)*dst)/st(lat,lon)


          endif         

          b2t(lat,lon)=b2t(lat,lon)*temp_st/st(lat,lon)
          b1t(lat,lon)=b1t(lat,lon)*temp_st/st(lat,lon)

          b2t14(lat,lon)=b2t14(lat,lon)*temp_st/st(lat,lon)
          b1t14(lat,lon)=b1t14(lat,lon)*temp_st/st(lat,lon)

          b2t13(lat,lon)=b2t13(lat,lon)*temp_st/st(lat,lon)
          b1t13(lat,lon)=b1t13(lat,lon)*temp_st/st(lat,lon)

        endif

* correction for grass

        if(sg(lat,lon).gt.0) then	      
                if(dst.gt.0) then  
                
                 b4g(lat,lon)=b4g(lat,lon)*(temp_sg-dst)
     >           /sg(lat,lon)
                 b3g(lat,lon)=b3g(lat,lon)*(temp_sg-dst)
     >           /sg(lat,lon)

                 b4g14(lat,lon)=b4g14(lat,lon)*(temp_sg-dst)
     >           /sg(lat,lon)
                 b3g14(lat,lon)=b3g14(lat,lon)*(temp_sg-dst)
     >           /sg(lat,lon)

                 b4g13(lat,lon)=b4g13(lat,lon)*(temp_sg-dst)
     >           /sg(lat,lon)
                 b3g13(lat,lon)=b3g13(lat,lon)*(temp_sg-dst)
     >           /sg(lat,lon)


                else 

           b4g(lat,lon)=(b4g(lat,lon)*temp_sg-tempor1*dst)
     >     /sg(lat,lon)
           b3g(lat,lon)=(b3g(lat,lon)*temp_sg-tempor2*dst)
     >     /sg(lat,lon)

           b4g14(lat,lon)=(b4g14(lat,lon)*temp_sg
     >     -tempor3*dst)/sg(lat,lon)
           b3g14(lat,lon)=(b3g14(lat,lon)*temp_sg
     >     -tempor4*dst)/sg(lat,lon)

           b4g13(lat,lon)=(b4g13(lat,lon)*temp_sg
     >     -tempor5*dst)/sg(lat,lon)
           b3g13(lat,lon)=(b3g13(lat,lon)*temp_sg
     >     -tempor6*dst)/sg(lat,lon)

                endif

         b2g(lat,lon)=b2g(lat,lon)*temp_sg/sg(lat,lon)
         b1g(lat,lon)=b1g(lat,lon)*temp_sg/sg(lat,lon)

         b2g14(lat,lon)=b2g14(lat,lon)*temp_sg/sg(lat,lon)
         b1g14(lat,lon)=b1g14(lat,lon)*temp_sg/sg(lat,lon)

         b2g13(lat,lon)=b2g13(lat,lon)*temp_sg/sg(lat,lon)
         b1g13(lat,lon)=b1g13(lat,lon)*temp_sg/sg(lat,lon)

	endif 

              
* slow soil organic matter

	b4t(lat,lon)=b4t(lat,lon)+k3t/t3t*b3t(lat,lon)
     *               -b4t(lat,lon)/t4t
	b4g(lat,lon)=b4g(lat,lon)+k4g/t2g*b2g(lat,lon)+k3g/t3g*
     *  b3g(lat,lon)-b4g(lat,lon)/t4g

	b4t14(lat,lon)=b4t14(lat,lon)+k3t/t3t
     *                 *b3t14(lat,lon)
     *                 -b4t14(lat,lon)/t4t
	b4g14(lat,lon)=b4g14(lat,lon)+k4g/t2g
     *                 *b2g14(lat,lon)+k3g/t3g*
     *  b3g14(lat,lon)-b4g14(lat,lon)/t4g

	b4t13(lat,lon)=b4t13(lat,lon)+k3t/t3t
     *                 *b3t13(lat,lon)
     *                 -b4t13(lat,lon)/t4t
	b4g13(lat,lon)=b4g13(lat,lon)+k4g/t2g
     *                 *b2g13(lat,lon)+k3g/t3g*
     *  b3g13(lat,lon)-b4g13(lat,lon)/t4g

*   fast soil organic matter

	b3t(lat,lon)=b3t(lat,lon)+b1t(lat,lon)/t1t*k0t+
     *  k2t/t2t*b2t(lat,lon)-b3t(lat,lon)/t3t
	b3g(lat,lon)=b3g(lat,lon)+b1g(lat,lon)/t1g*k0g+
     *  k2g/t2g*b2g(lat,lon)-b3g(lat,lon)/t3g

	b3t14(lat,lon)=b3t14(lat,lon)
     *      +b1t14(lat,lon)/t1t*k0t+
     *  k2t/t2t*b2t14(lat,lon)
     *  -b3t14(lat,lon)/t3t

	b3g14(lat,lon)=b3g14(lat,lon)
     *  +b1g14(lat,lon)/t1g*k0g+
     *  k2g/t2g*b2g14(lat,lon)
     *  -b3g14(lat,lon)/t3g

	b3t13(lat,lon)=b3t13(lat,lon)
     *       +b1t13(lat,lon)/t1t*k0t+
     *  k2t/t2t*b2t13(lat,lon)
     *  -b3t13(lat,lon)/t3t

	b3g13(lat,lon)=b3g13(lat,lon)
     *  +b1g13(lat,lon)/t1g*k0g+
     *  k2g/t2g*b2g13(lat,lon)
     *  -b3g13(lat,lon)/t3g

* leaves biomass


	b1t(lat,lon)=b1t(lat,lon)+k1t*npp-b1t(lat,lon)/t1t
	b1g(lat,lon)=k1g*npp*t1g


	b1t14(lat,lon)=b1t14(lat,lon)+k1t*npp*c14atm
     *                 -b1t14(lat,lon)/t1t

     	b1g14(lat,lon)=k1g*npp*c14atm*t1g

     	
	b1t13(lat,lon)=b1t13(lat,lon)+k1t*npp*c13atm*
     *                 c13frac-b1t13(lat,lon)/t1t

     	
	b1g13(lat,lon)=k1g*npp*c13atm*(c13frac*(1-sg4(lat,lon))+
     *                 c13frac4*sg4(lat,lon))*t1g

*   stems and roots biomass

	b2t(lat,lon)=b2t(lat,lon)+(1-k1t)*npp-b2t(lat,lon)/t2t
	b2g(lat,lon)=b2g(lat,lon)+(1-k1g)*npp-b2g(lat,lon)/t2g


	b2t14(lat,lon)=b2t14(lat,lon)+(1-k1t)*npp*c14atm
     *                 -b2t14(lat,lon)/t2t

	b2g14(lat,lon)=b2g14(lat,lon)+(1-k1g)*npp*c14atm
     *                 -b2g14(lat,lon)/t2g

	b2t13(lat,lon)=b2t13(lat,lon)+(1-k1t)*npp*c13atm
     *                 *c13frac-b2t13(lat,lon)/t2t
     	
	b2g13(lat,lon)=b2g13(lat,lon)+(1-k1g)*npp*c13atm
     *                 *(c13frac*(1-sg4(lat,lon))+
     *                 c13frac4*sg4(lat,lon))-b2g13(lat,lon)/t2g

*   c14 annual decay
	b1t14(lat,lon)=b1t14(lat,lon)*(1-c14tdec)	
	b2t14(lat,lon)=b2t14(lat,lon)*(1-c14tdec)	
	b3t14(lat,lon)=b3t14(lat,lon)*(1-c14tdec)	
	b4t14(lat,lon)=b4t14(lat,lon)*(1-c14tdec)	
	b1g14(lat,lon)=b1g14(lat,lon)*(1-c14tdec)	
	b2g14(lat,lon)=b2g14(lat,lon)*(1-c14tdec)	
	b3g14(lat,lon)=b3g14(lat,lon)*(1-c14tdec)	
	b4g14(lat,lon)=b4g14(lat,lon)*(1-c14tdec)	

	call CLIMPAR
	
	return
	end

*********************************************************************
	    SUBROUTINE CLIMPAR
*********************************************************************
       INCLUDE 'declar.inc'
       INCLUDE 'params.inc'
       INCLUDE 'bio.inc'
       INCLUDE 'buffer.inc'
       REAL tempor1
*********************************************************************

* calculation of annual averaged LAI - lai

	 laig=b1g(lat,lon)*deng
         lait=b1t(lat,lon)*(dentn*snlt(lat,lon)+
     *   dentd*(1-snlt(lat,lon)))
     
  	 BLAI(lat,lon,1)=lait
  	 BLAI(lat,lon,2)=laig

* calculation of annual carbon uptake

	if (KTVM.ge.2) then 
	   tempor1=b1(lat,lon)+b2(lat,lon)+
     >          b3(lat,lon)+b4(lat,lon)
	   tempor2=bc13(lat,lon)
	   
        else 
           tempor1=0
        endif
        
        b1(lat,lon)=b1t(lat,lon)*st(lat,lon)+b1g(lat,lon)*sg(lat,lon)
        b2(lat,lon)=b2t(lat,lon)*st(lat,lon)+b2g(lat,lon)*sg(lat,lon)
        b3(lat,lon)=b3t(lat,lon)*st(lat,lon)+b3g(lat,lon)*sg(lat,lon)
        b4(lat,lon)=b4t(lat,lon)*st(lat,lon)+b4g(lat,lon)*sg(lat,lon)
        b12(lat,lon)=b1(lat,lon)+b2(lat,lon)
        b34(lat,lon)=b3(lat,lon)+b4(lat,lon)
        
        bc14(lat,lon)=(b1t14(lat,lon)+
     >     b2t14(lat,lon)+b3t14(lat,lon)+
     >     b4t14(lat,lon))*st(lat,lon)+
     >      (b1g14(lat,lon)+b2g14(lat,lon)+
     >      b3g14(lat,lon)+b4g14(lat,lon))
     >      *sg(lat,lon)

*	print *,lat,lon,b1t14(lat,lon),b1g14(lat,lon),b4t14(lat,lon)

        bc13(lat,lon)=(b1t13(lat,lon)+
     >     b2t13(lat,lon)+b3t13(lat,lon)+
     >     b4t13(lat,lon))*st(lat,lon)+
     >      (b1g13(lat,lon)+b2g13(lat,lon)+
     >      b3g13(lat,lon)+b4g13(lat,lon))
     >      *sg(lat,lon)

	anup(lat,lon)=(b1(lat,lon)+b2(lat,lon)
     >       +b3(lat,lon)+b4(lat,lon)-tempor1)
	anup13(lat,lon)=bc13(lat,lon)-tempor2
	
C...      NET PRIMARY PRODUCTION

        pnpp(lat,lon)=npp*(1-sd(lat,lon))

	return
	end

*********************************************************************
	    SUBROUTINE CCPARAM
*********************************************************************

       INCLUDE 'declar.inc'
       INCLUDE 'params.inc'
       INCLUDE 'bio.inc'
       INCLUDE 'buffer.inc'
       REAL  npp1,npp2,db1,db2,db3,avefor,differ,pcr
*********************************************************************
* calculation of current cycle parameters

* potential trees share

       avefor=ave_pr*ave_pr*ave_pr*ave_pr
       differ=gdd0-gdd0_min
       db1=-bet*differ
       db2=gamm*differ
       db3=differ*differ

       if(differ.lt.0) then 
          forshare_st=0
       else 
          forshare_st=(1-exp(db1))*avefor/(avefor+a*db3*exp(db2))
       endif
       if (forshare_st.gt.fmax) forshare_st=fmax

* potential desert share - desshare_st

       desshare_st=0
	 
* northern deserts 
cv change! permafrost soils version - no cold deserts!

cv       if(gdd0.lt.100) desshare_st=1
         
cv      if(gdd0.ge.100.and.gdd0.lt.gdd0_min)
cv     >      desshare_st=(gdd0_min-gdd0)/(gdd0_min-100.)

* southern deserts

 	 if (gdd0.ge.gdd0_max) then
 	 
            pcr=acr*exp(gamm/2.*differ)

            if (ave_pr05.le.pcr) then 
                desshare_st=1
                forshare_st=0
            else
                db2=(ave_pr05-pcr)/exp(gamm*differ)
                desshare_st=1.03/(1+ades*db2*db2)-0.03
                if (desshare_st.lt.0) desshare_st=0
            endif
            
         endif

* calculation of NPP, Lieth's formula

	db1=-v1*ave_pr
 	if (gdd0.ge.gdd0_max) db1=-v1*ave_pr05
	db2=-v2*ave_t
	npp1=(1.-exp(db1))
	npp2=1./(1.+v3*exp(db2))
	if(npp1.lt.npp2) then
                npp=nppmax*npp1
	    else
                npp=nppmax*npp2
	endif

* CO2 enrichment factor

        npp=npp*(1+0.25/ALOG(2.)*ALOG(co2/280.))

cv change! npp in cold regions
        if(gdd0.lt.gdd0_min) then
        if(gdd0.ge.100) then
        npp=npp*(gdd0_min-gdd0)/(gdd0_min-100.)
        else
        npp=0
        endif
        endif
         

* allocation factors and residence time of leaves biomass

	k1t=c1t+c2t/(1+c3t*npp)
	k1g=c1g+c2g/(1+c3g*npp)

	t1t=d1t+d2t/(1+d3t*npp)
	t1g=d1g+d2g/(1+d3g*npp)

*   residence time of stems and roots biomass

	t2t=e1t+e2t/(1+e3t*npp)
	t2g=e1g+e2g/(1+e3g*npp)

*   residence time of fast carbon pool

        t3t=16*exp(-ps5*(ave_t-soilt))  
        t3g=40*exp(-ps5*(ave_t-soilt))  

* residence time of slow soil organic matter



cv parameterization of permafrost effect on carbon residence time
        ave_tresh=-10.

        if(ave_t.gt.ave_tresh) then 
        pstemp=-ps5*(ave_t-soilt)

        else 
            pstem=-ps5*(ave_t-soilt)-
     *    (2.38+ps5*(ave_t-soilt))*(ave_t-ave_tresh)/30.         
           pstemp=2.38
         endif

cv no respiration if gdd0 is below 100 dd
        if(gdd0.le.100) pstemp=6.

        t3t=16*exp(pstemp)
        t3g=40*exp(pstemp)
        t4t=900*exp(pstemp)
        t4g=t4t

*calculation of potential nedleleaves trees ratio

	nlshare_st=(t1t-t1td)/(t1tn-t1td)
	if (nlshare_st.gt.1) nlshare_st=1
	if (nlshare_st.lt.0) nlshare_st=0

*calculation of c4 grass fraction

*	if(TATMSMIN(lat,lon).lt.14) then
	if(TATMSMIN(lat,lon).lt.12) then
            g4share_st=0
        else    
*	  if(TATMSMIN(lat,lon).lt.17) then
*	   g4share_st=1-(17-TATMSMIN(lat,lon))/(17.-14.)
	  if(TATMSMIN(lat,lon).lt.15.5) then
	   g4share_st=1-(15.5-TATMSMIN(lat,lon))/3.5
	else
	   g4share_st=1
	endif
	endif

*	print *, lat,lon,TATMSMIN(lat,lon),g4share_st,sg4(lat,lon)
	return
	end

*********************************************************************
	    SUBROUTINE INITCPAR
*********************************************************************

       INCLUDE 'declar.inc'
       INCLUDE 'params.inc'
       INCLUDE 'bio.inc'
       INCLUDE 'buffer.inc'
*********************************************************************

* initialisation of variables
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
*
* change!
*	 ps5=0.04
         ps5=0.07
	 soilt=5
	 c14init=100.
	 c14tdec=1./8240.
*        the same decay rate of c14 is determined in ocn_bio.f: C14dec
	 c13frac=1-18./1000.
	 c13frac4=1-5./1000.
	 c13init=c13frac
	 
 	 return
	 end

*********************************************************************
	    SUBROUTINE INITMAST
*********************************************************************

       INCLUDE 'declar.inc'
       INCLUDE 'bio.inc'
       INCLUDE 'buffer.inc'
*********************************************************************

* land mask initialization, MASKB(i,j)=1 if there is land in cell (i,j)
* carea(i,j) is an area of cell (i,j) in mln sq.km.

	do n=1,NS 
	 do i=1,IT
	    carea(i,n)=calc_area(10.,FLONB(n),i)*FRILB(i,n)
	 enddo
	enddo 

	 return
	 end

*********************************************************************
	real function calc_area(x,y,i)
*********************************************************************
	pi=3.14159
	rad=6.371

* calculation of land area of cell with x degrees latitude size,
* y - latitude size, i is a number of step from Northern pole,
* unit - mln sq.km.

	area=2*pi*x/360.*rad*2*pi*y/360.*rad*
     *  cos(2*pi*(90.+(0.5-i)*x)/360.)
	calc_area=area

	end
