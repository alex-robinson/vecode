*********************************************************************  
                  SUBROUTINE INI_CLIMBER  
*********************************************************************  
*                 Initialization routine         
*
*  Purpose:  Reading and defenition of flags, parameters, 
*            initial conditions
* 
*  By: A.Ganopolski 
*  Last modification: 10.12.2004 
*  E.Bauer            11.10.2007: Netcdf Enriched Output (no sicopols.F)
*                                                        CLIMBER-2.4
*********************************************************************  
      include 'netcdf.inc'
      INCLUDE 'declar.inc'            
      INCLUDE 'params.inc'
      INCLUDE 'neo24.inc'
      INCLUDE 'simenv_mod_f.inc'

      integer status 
      integer binfac
      parameter (binfac = 4)
*********************************************************************  

C...  DETERMINE ROOT OUTPUT FOLDER LOCATION
      call COMMANDARGS

cmp c  make sure no simu_ok file is present before the simulation
cmp #ifndef simenv
cmp       call system("rm -f "// trim(OUTFLDR) // "/simu_ok")
cmp #elif
cmp       call system("rm -rf "// trim(OUTFLDR) // "/test/")
cmp       call system("mkdir -p "// trim(OUTFLDR) // "/test/")
cmp #endif

      print *, "(INI) SUFFIX FOR OUTPUT FILES:", simenv_run_char

C...  EXTERNAL FLAGS & PARAMETERS      
      
c    ============
      call FLAGS
c    ============      
  
c...  INITIALISATION

c    =============================
      if (KLSR.ne.0) call RESTART
      call PARAM
      call GEO_LND
      call GEO_OCN
      if (KRFA.ne.0) call DATA_RF
      call ROOT 
      call INI_SINSOL(trim(INFLDR)//"/INP/")
      call SINSOL(BTIME)
      call INIT_ATM
      call INIT_ASI
      call INIT_COP
      if (KOCN.ne.0)  call SDOWN 
      if (KOCN.ne.0)  call INIT_OCN 
      if (KOCN.eq.0)  call DATA_OCN 
      if (KOCN.eq.1)  call DATA_FTO 
      if (KTVM.ne.0)  call INIT_TVM
      if (KTVM.eq.0)  call DATA_VEG 
      if (KOCAR.ne.0) call INIT_OCC
      if (KSOLC.ne.0) call DATA_SOL
      if (KCO2D.ne.0) call DATA_CO2
                      call PCO2
      if (KCCC.ne.0)  call ECO2(0)
      if (KFB.eq.1)   call FBAR
      if(KSED.ne.0) call init_sed
      if(KWEATH.ne.0) call init_weav
      call RUNINFO
cmp
      if (KES.eq.2) call READCONS
c    =============================

cts   Open files here to optimize performance in ensemble mode

cmp   Do not create any output in ES mode
      if (KES .eq. 1) return

      call INIT_NEO
c    =============================

      if (KNETCDF.eq.1) then
        if (KOUT_HIST .eq.1) then
          status = 
     >    NF_OPEN( 
     >    trim(OUTFLDR)//'OUT/history.nc'//trim(simenv_run_char),
cts        status = NF_OPEN(' OUT/history.'//simenv_run_char//'.nc', ! yields:  Abnormal Termination (see netcdf)
     >           IOR(NF_WRITE,NF_SHARE), NCID_HIST)
           if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_CPL  .eq.1) then
          status = NF_OPEN(
     >      trim(OUTFLDR)//'OUT/coupling_grl.nc'//trim(simenv_run_char),
     >           IOR(NF_WRITE,NF_SHARE), NCID_CPL  )
          if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_AFX.eq.1) then
          status = 
     >      NF_OPEN( trim(OUTFLDR)//'OUT/afx.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_AFX)
          if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif

        if (KOUT_A2GC.eq.1) then
          status = 
     >    NF_OPEN( trim(OUTFLDR)//'OUT/a2g_c.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_A2GC)
          if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_A2GF.eq.1) then
          status = 
     >    NF_OPEN( trim(OUTFLDR)//'OUT/a2g_f.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_A2GF)
          if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_A2ZT.eq.1) then
          status = 
     >     NF_OPEN( trim(OUTFLDR)//'OUT/a2zt.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_A2ZT)
          if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_A2ZX.eq.1) then
          status = 
     >     NF_OPEN( trim(OUTFLDR)//'OUT/a2zx.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_A2ZX)
          if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_A3D.eq.1) then
          status = 
     >      NF_OPEN( trim(OUTFLDR)//'OUT/a3d.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_A3D)
          if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_OCNF.eq.1) then
          status = 
     >     NF_OPEN( trim(OUTFLDR)//'OUT/ocnf.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_OCNF)
          if (status .NE. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_OCNS.eq.1) then
             status = 
     >     NF_OPEN( trim(OUTFLDR)//'OUT/ocns.nc'//trim(simenv_run_char),
     >                IOR(NF_WRITE, NF_SHARE), NCID_OCNS)
             if (status .NE. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_OCNT.eq.1) then
             status = 
     >     NF_OPEN( trim(OUTFLDR)//'OUT/ocnt.nc'//trim(simenv_run_char),
     $            IOR(NF_WRITE, NF_SHARE), NCID_OCNT)
             if (status .ne. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_OCNX.eq.1) then
          status = 
     >     NF_OPEN( trim(OUTFLDR)//'OUT/ocnx.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_OCNX)
          if (status .NE. NF_NOERR) call HANDLE_ERR(status)
        endif
        if (KOUT_Q3D.eq.1) then
          status = 
     >      NF_OPEN( trim(OUTFLDR)//'OUT/q3d.nc'//trim(simenv_run_char),
     >             IOR(NF_WRITE, NF_SHARE), NCID_Q3D)
          if (status .NE. NF_NOERR) call HANDLE_ERR(status)
        endif
  
      else
        open (101,
     >    file=trim(OUTFLDR)//'OUT/history.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=HIST_NO*binfac)
        open (102,
     >    file=trim(OUTFLDR)//'OUT/a2g_f.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=18*11*15*60*binfac)
        open (103,
     >    file=trim(OUTFLDR)//'OUT/a2g_c.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=18*11*15*60*binfac)
        open (104,
     >    file=trim(OUTFLDR)//'OUT/a3d.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=18*7*12*10*6*binfac)
        open (105,
     >    file=trim(OUTFLDR)//'OUT/a2zt.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=IT*NZ1*2*12*binfac)
        open (106,
     >    file=trim(OUTFLDR)//'OUT/a2zx.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=IX*NZ1*2*12*binfac)
        open (107,
     >    file=trim(OUTFLDR)//'OUT/afx.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=IX*12*4*binfac)
        open (108,
     >    file=trim(OUTFLDR)//'OUT/ocnt.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=LT*NOC*JT*6*binfac)
        open (109,
     >    file=trim(OUTFLDR)//'OUT/ocnx.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=LX*NOC2*JX*binfac)
        open (110,
     >    file=trim(OUTFLDR)//'OUT/ocns.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=LT*NOC1*2*binfac)
        open (111,
     >    file=trim(OUTFLDR)//'OUT/ocnf.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=LX*NOC1*2*binfac)
cmp  Do not yet have a NetCDF equivalent (most are not tested)
        open (112,
     >    file=trim(OUTFLDR)//'OUT/sica.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=2*4*13*binfac)
cjf     >     form='unformatted',access='direct',recl=2*4*binfac)
        open (113,
     >    file=trim(OUTFLDR)//'OUT/ocnb.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=LT*NOC*JT*4*12*binfac)
        open (114,
     >    file=trim(OUTFLDR)//'OUT/ocntb.dat'//trim(simenv_run_char),
     >  form='unformatted',access='direct',recl=LT*NOC*11*3*36*4*binfac)
        open (115,
     >    file=trim(OUTFLDR)//'OUT/ocni.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=LT*NOC1*2*4*binfac)
        open (116,
     >    file=trim(OUTFLDR)//'OUT/q3d.dat'//trim(simenv_run_char),
     >    form='unformatted',access='direct',recl=18*7*12*16*3*binfac)
        open  (117,
     >    file=trim(OUTFLDR)//'OUT/co3.dat'//trim(simenv_run_char),
     >    form='unformatted', access='direct',recl=LT*NOC*JT*7*binfac)
        open  (118,
     >  file=trim(OUTFLDR)//'OUT/shell_bury.dat'//trim(simenv_run_char),
cmp  i_bur_rec_max set to 20 in parameter.sediment.include
     >   form='unformatted', access='direct',recl=LT*NOC*JT*20*4*binfac)

        open (272,
     >  file=trim(OUTFLDR)//'OUT/biodyn.dat'//trim(simenv_run_char))
     
      endif 

cmp
      if (KFB.ne.0) then
         open (233,
     >      file=trim(OUTFLDR)//'INP/CLD.dat'//trim(simenv_run_char))
      endif

      if(KW_ZLEV.ne.0) 
     >  open (201,
     >      file=trim(OUTFLDR)//'zlevel.dat'//trim(simenv_run_char))
      if(KW_RH.ne.0) 
     >  open (202,file=trim(OUTFLDR)//'RH3D.dat'//trim(simenv_run_char))
      
      return
      end


*********************************************************************  
      subroutine close_climber
*********************************************************************  
      include 'netcdf.inc'
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'
      INCLUDE 'neo24.inc'
      integer status 
*********************************************************************  
ccc      call RUNINFO

c    =============================
c      call INIT_NEO
c    =============================

      if (KNETCDF.eq.1) then
      if (KOUT_HIST  .eq.1) then
        status = nf_close(NCID_HIST)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_CPL  .eq.1) then
        status = nf_close(NCID_CPL  )
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_AFX.eq.1) then
        status = nf_close(NCID_AFX)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_A2GC.eq.1) then
        status = nf_close(NCID_A2GC)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_A2GF.eq.1) then
        status = nf_close(NCID_A2GF)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_A2ZT.eq.1) then
        status = nf_close(NCID_A2ZT)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_A2ZX.eq.1) then
        status = nf_close(NCID_A2ZX)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_A3D.eq.1) then
        status = nf_close(NCID_A3D)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_OCNF.eq.1) then
        status = nf_close(NCID_OCNF)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_OCNS.eq.1) then
        status = nf_close(NCID_OCNS)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_OCNT.eq.1) then
        status = nf_close(NCID_OCNT)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_OCNX.eq.1) then
        status = nf_close(NCID_OCNX)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif
      if (KOUT_Q3D.eq.1) then
        status = nf_close(NCID_Q3D)
        if (status .ne. NF_NOERR) call HANDLE_ERR(status)
      endif

      else
        close(101)    
        close(102)
        close(103)
        close(104)
        close(105)
        close(106)
        close(107)
        close(108)
        close(109)
        close(110)
        close(111)
        close(112)
        close(113)
        close(114)
        close(115)
        close(116)
        close(117)
        close(118)

        close(272)
      endif

	close(555)
        close(201)
        close(202)

cmp
      if (KFB.ne.0) then
	 close(233)
      endif


c  c  write a file to disk to indicate that the job is over
c  #ifndef simenv
c        call system("touch "// trim(OUTFLDR) //"/simu_ok")
c  #else
c        call system("touch "// trim(OUTFLDR) // "/test/simu_ok" 
c       >    //trim(simenv_run_char))
c  #endif


      end      

*********************************************************************  
                      SUBROUTINE FLAGS  
*********************************************************************  
*
*  Purpose:  Reading of external flags, defenition of some internals 
* 
*  By: A.Ganopolski 
*  Last modification: 10.12.2004 
*********************************************************************  
	  INCLUDE 'declar.inc'            
	  INCLUDE 'params.inc'
      INCLUDE 'paramsmod.inc'
      INCLUDE 'atmos.inc'
      INCLUDE 'simenv_mod_f.inc' 
      INCLUDE 'control.inc'  
*********************************************************************  
         
C ...  Flags and run parameters defenition 
         
      open (1,file=trim(OUTFLDR)//'run') 
       
      read (1,*)
      read (1,*) KTIME
      read (1,*) KCAL
      read (1,*) NYRSR
      read (1,*)
      read (1,*) NYRMX 
      read (1,*) 
      read (1,*) KLSR
      read (1,*) KLWR
      read (1,*) RESTART_IN
      read (1,*)
      read (1,*) KOCN
      read (1,*) KOCNW
      read (1,*) KES
      read (1,*) KTVM 
      read (1,*) KOCAR
      read (1,*) KSLR
      read (1,*) KCFC
      read (1,*)
      read (1,*) KCCC
      read (1,*) KCCR
      read (1,*) KDFLX
      read (1,*) KSOLC
      read (1,*) KCO2
      read (1,*) KCO2D
      read (1,*) KSED
      read (1,*) KWEATH      
      read (1,*) KCORAL      
      read (1,*) KIRON      
      read (1,*) KLAND
      read (1,*) KYEDOM            
      read (1,*) 
      read (1,*) KOUT 
      read (1,*) NFOUT
      read (1,*) NAOUT
      read (1,*) 
      read (1,*) PA
      read (1,*) PAR
      read (1,*) PAB
      read (1,*) CUMCO2
      read (1,*) 
      read (1,*) KRFA
      read (1,*)
      read (1,*) KFB
      read (1,*) KW_FBA
      read (1,*) 
      read (1,*) KZLEV
      read (1,*) KW_ZLEV
      read (1,*) 
      read (1,*) KRH
      read (1,*) KW_RH
      read (1,*) K2RH
      read (1,*) KWV
      read (1,*) 
      read (1,*) KCOD
      read (1,*) 
      read (1,*) KOUT_HIST
      read (1,*) KOUT_AFX
      read (1,*) KOUT_A2G
      read (1,*) KOUT_A2ZT
      read (1,*) KOUT_A2ZX
      read (1,*) KOUT_A3D
      read (1,*) KOUT_OCNF
      read (1,*) KOUT_OCNS
      read (1,*) KOUT_OCNT
      read (1,*) KOUT_OCNX
      read (1,*) KOUT_Q3D
      read (1,*) KOUT_SLR2D     
      read (1,*) KOUT_CPL
      close (1)

      open (1,file=trim(INFLDR)//'INP/par.dat')

      read (1,*)
      read (1,*)
      read (1,*) AHOC
      read (1,*) Kz_upper
      read (1,*) Kz_lower
      read (1,*) Kz_ml
      read (1,*) Kz_so
      read (1,*) Kz_z
      read (1,*) Kz_dz
      read (1,*) ocn_drag
cmp      read (1,*) a_Kv
cmp      read (1,*) b_Kv
cmp      read (1,*) c_Kv
cmp      read (1,*) ml_Kv
      read (1,*)
      read (1,*) tau0
      read (1,*) tau1
      read (1,*) tau2
      read (1,*) specQ2
cmp   Replace specQ by 1/specQ**2 for a linear behaviour
cmp   wrt the lapse rate profile, useful in sensitivity 
cmp   analyses (perrette@pik-potsdam.de, 23.10.2012)
      specQ = 1/sqrt(specQ2)
      read (1,*)
      read (1,*) T_c1
      read (1,*) T_c2
      read (1,*) T_c3
      read (1,*) ACO21
      read (1,*) CTR
      read (1,*)
      read (1,*) a1_co2
      read (1,*)
      read (1,*) cc1
      read (1,*)
      read (1,*) SHRH
      read (1,*)
      read (1,*) aa
      read (1,*) bb
      read (1,*)
      read (1,*) scale_AD 
      read (1,*) scale_AID
      read (1,*) 
      read (1,*) AOVER
      read (1,*) CADJF
      read (1,*) 
      read (1,*) ALB_SI

      close (1)

c**************************************************************************

***************************************************************************************
 
c       init always by
c        iok=simenv_ini_f()
c       check the return code of model coupling functions at least here
#ifdef simenv
        if(iok.ne.0) stop
c        print *, 'iok',iok

c       get current run number
        iok=simenv_get_run_f(simenv_i4_run,simenv_run_char)
c        print *,        'iok,simenv_i4_run,simenv_run_char',
c     #		         iok,simenv_i4_run,simenv_run_char
c  adjustment of experiment parameter:
 
cmp   First a few "meta-parameter", which control the combination
cmp   but are not a new parameterization, just to make things easier

cmp   Give the possibility of having uniform diffusivity between (for upper and lower only)
      iok=simenv_get_f('Kz_uni',Kz_uni,Kz_uni) ! uniform Kz, overwrite above
      if (Kz_uni .gt. 0.0) then
        Kz_upper = Kz_uni
        Kz_lower = Kz_uni
      endif
cmp   Now give the possibility of having uniform diffusivity in ALL areas
      iok=simenv_get_f('Kz_uni2',Kz_uni2,Kz_uni2) ! uniform Kz, overwrite above
      if (Kz_uni2 .gt. 0.0) then
        Kz_upper = Kz_uni2
        Kz_lower = Kz_uni2
        Kz_so = Kz_uni2
        Kz_ml = Kz_uni2
      endif
cmp   Now give the possibility of having uniform diffusivity in ALL areas expect ML
      iok=simenv_get_f('Kz_uni15',Kz_uni15,Kz_uni15) ! uniform Kz, overwrite above
      if (Kz_uni15 .gt. 0.0) then
        Kz_upper = Kz_uni15
        Kz_lower = Kz_uni15
        Kz_so = Kz_uni15
      endif
cmp   Or shift upper and lower diff by the same amount
      iok=simenv_get_f('Kz_shift',Kz_shift,Kz_shift) ! shift default profile to simulate old a_Kv
      if (Kz_shift .gt. 0.0) then
        Kz_upper = Kz_upper + Kz_shift
        Kz_lower = Kz_lower + Kz_shift
      endif
cmp   Or shift all diff by the same amount
      iok=simenv_get_f('Kz_shift2',Kz_shift2,Kz_shift2) ! like shift but for all 
      if (Kz_shift2 .gt. 0.0) then
        Kz_upper = Kz_upper + Kz_shift2
        Kz_lower = Kz_lower + Kz_shift2
        Kz_so = Kz_so + Kz_shift2
        Kz_ml = Kz_ml + Kz_shift2
      endif

cmp   Now the actual parameters      

      iok=simenv_get_f('ahoc',AHOC,AHOC)
      iok=simenv_get_f('Kz_upper',Kz_upper,Kz_upper)
      iok=simenv_get_f('Kz_lower',Kz_lower,Kz_lower)
      iok=simenv_get_f('Kz_ml',Kz_ml,Kz_ml)
      iok=simenv_get_f('Kz_so',Kz_so,Kz_so)
      iok=simenv_get_f('Kz_z',Kz_z,Kz_z)
      iok=simenv_get_f('Kz_dz',Kz_dz,Kz_dz)
      iok=simenv_get_f('ocn_drag',ocn_drag,ocn_drag) 
      iok=simenv_get_f('ALB_SI',ALB_SI,ALB_SI) 
cmp      iok=simenv_get_f('a_kv',a_Kv,a_Kv)
cmp      iok=simenv_get_f('b_kv',b_Kv,b_Kv)
      iok=simenv_get_f('aover',AOVER,AOVER)
      iok=simenv_get_f('tau0',tau0,tau0)
c      iok=simenv_get_f('tau1',tau1,tau1)
c      iok=simenv_get_f('tau2',tau2,tau2)
cmp      iok=simenv_get_f('specq',specQ,specQ)
      iok=simenv_get_f('specq2',specQ2,specQ2)
      specQ = 1/sqrt(specQ2) ! specQ2 is more "linear"
      iok=simenv_get_f('t_c3',T_c3,T_c3)
      iok=simenv_get_f('aco21',ACO21,ACO21)
      iok=simenv_get_f('ctr',CTR,CTR)
      iok=simenv_get_f('a1_co2',a1_co2,a1_co2)
      iok=simenv_get_f('cc1',cc1,cc1)
      iok=simenv_get_f('aa',aa,aa)
      iok=simenv_get_f('bb',bb,bb)
      iok=simenv_get_f('SHRH',SHRH,SHRH)
      iok=simenv_get_f('scale_AD',scale_AD,scale_AD)
      iok=simenv_get_f('scale_AID',scale_AID,scale_AID)
      iok=simenv_get_f('CADJF',CADJF,CADJF)

c       because there is no generic SimEnv output, 1 output variable is written:  
      iok=simenv_put_f('irun',simenv_i4_run)
      iok=simenv_put_f('irun2',simenv_run_char)
#endif
c ---------------------------------------------------------------------
c       finish always by
c        iok=simenv_end_f()
c --------------------------------------------------------------------- 

c...  Number of steps to be done
      
      NTSMX=360*NYRMX 
      
      print *, 'INI NURMX, NTSMX=', NTSMX
      
c...  Initial time (astronom)      
      
      if (KTIME.eq.1) then
       NYRA=NYRSR+NYR-KCAL*1999
      else
       NYRA=NYRSR-KCAL*1999
      endif
      
      BTIME=NYRA 
      
c... Atmosphere-only/coupled model flag
      
      if (KOCN.ne.0) then 
       KCOUP=1
      else
       KCOUP=0
      endif
      
c... Flags check
      
      if (KSEM.ne.0) KGEU=1 
      
c... Time parameters control

      NAOUT=MAX0(NAOUT,1)
      NAOUT=MIN0(NAOUT,NYRMX)
      
      if (KOUT.eq.1) NFOUT=MAX0(NFOUT,NAOUT)      

c ajr  Adjust restart filename to include input dir
      RESTART_IN = trim(INFLDR)//trim(RESTART_IN)

      return
      end
      
********************************************************************* 
                   SUBROUTINE PARAM 
************************ INI **************************************** 
*
*  Purpose: Defenition of model parameters
*
*  By A.Ganopolski  
*  Last modification: 10.12.2004
********************************************************************* 
      INCLUDE 'declar.inc'                    
      INCLUDE 'params.inc'                              
*********************************************************************
 
c...   Physical constants
 
      PI  = 3.1415926
      G   = 9.81
      T0  = 273.15 
      SIGM= 5.67E-8
      CKARM=0.4
      R   = 287. 
      OMEG= 0.729E-4 
      RW  = 1000.
      RO  = 1.03E3
      RA  = 1.3 
      CR  = 4.E6 
      CAP  = 1000.
      CAV=5./7.*CAP
      ERAD= 6.37E6
      P0  = 101200.
      CLE = 2.50E6
      CLS = 2.83E6
      CLM = 3.30E5
      CHSNW =2.0 
cmp		For the CMIP5 EMICS intercomparison:
cmp      S0 = 1365.763
      S0=1365. 
      TDAY=86400.
      TYER=360.*TDAY

      C13ATM=1000.-6.7
      C14ATM=100.

c... Model parameters

      HATM= 8400.
      HEATM=2000.
      HBL = 1500.
      ! top-of-the atmosphere height, effectively 
      ! maximum tropopause height (should be 25km, but set to 50km to
      ! avoid crash for simulation with very high (> x4) CO2 concentration)
      HTOA = 50.E3 
      ZSURF=100.
      GTO  =6.E-3
      GTT  =6.E-5
      TAMG =288.
      FROCNMIN=0.7
cmp      AOVER=0.8E4
cjf      AHOC=1.0E3
      ZHOC=1000.
      EPS=0.000 
            
      FSB=1.
      FIE=1.

c      CCD=0.5E-5
      CCD=1.E-5
c      CCD=2.E-5
      CCDRF=1.
      
c... Atmospheric and oceanic time steps      
      
      TST  =TDAY
      TSTOC=TDAY*5 
      TSTIC=TDAY*3
      
       
      return
      end
      
*********************************************************************  
                      SUBROUTINE GEO_LND  
*********************************************************************  
*
*  Purpose: Initialisation program reading model layout and preparing
*           oceanic masks and ocean/land fractions for every grid cell         
*
*    
*  By: A.Ganopolski
*  Last modification: 10.12.2004
********************************************************************* 
      INCLUDE 'declar.inc'              
      INCLUDE 'params.inc'
*********************************************************************
                            
      open (1,file=trim(INFLDR)//'GEO/geo_lnd.dat')
      
c...  LATITUDINAL STEP

      read (1,*)
      read (1,*) DYD
      DPHI=PI*DYD/180.
      DY=ERAD*PI*DYD/180.
      
c...  T-POINT LATITUDES

      read (1,*)      
      read (1,*)      
      read (1,*)
      read (1,*) (FIT(i),i=1,IT)
      
c...  X-POINT LATITUDES

      read (1,*)      
      read (1,*)      
      read (1,*)
      read (1,*) (FIX(i),i=1,IX)      
      
c...  SECTOR WIDTH      
      
      read (1,*)
      read (1,*) FLONG
      
      do n=1,NS
       FLON(n)=FLONG
       FRS(n)=FLON(n)/360.
      enddo
      
      do n=1,NS1
       n1=n-1
       n2=n
       if (n.eq.1)   n1=NS
       if (n.eq.NS1) n2=1
       FRSU(n)=0.5*(FRS(n1)+FRS(n2))
      enddo

c...  OCEAN/LAND FRACTION                  
                  
      read (1,*)
      read (1,*)
      read (1,*)
      do i=1,18
      read (1,*) nn,(FROCN(i,n),n=1,NS)
      enddo
           
      do i=1,IT
       do n=1,NS
        FRLND(i,n)=1.-FROCN(i,n)
       enddo
      enddo       
                  
c...  SECTOR-BASIN CORRESPONDENCE (NOA)                  
                  
      read (1,*)
      read (1,*)
      read (1,*)
      do i=1,18
      read (1,*) nn,(NOA(i,n),n=1,NS)
      enddo
     
C...  GLACIER FRACTION
                  
      read (1,*)
      read (1,*)
      read (1,*)
      do i=1,18
      read (1,*) nn,(FRGLC(i,n),n=1,NS)
      enddo
      
C...  OROGRAPHY
c     icefree land elevation
                  
      read (1,*)
      read (1,*)
      read (1,*)
      do i=1,18
      read (1,*) nn,(HOROL(i,n),n=1,NS)
      enddo
      
c     ice sheets elevation
                  
      read (1,*)
      read (1,*)
      read (1,*)
      do i=1,18
      read (1,*) nn,(HOROG(i,n),n=1,NS)
      enddo
      
C...  SIGMA OROGRAPHY
                  
      read (1,*)
      read (1,*)
      read (1,*)
      do i=1,18
      read (1,*) nn,(SIGORO(i,n),n=1,NS)
      enddo

      
C...  ZONAL PARAMETERS

      do i=1,IT
        fitr=PI*FIT(i)/180.
        COST(i)=COS(fitr)
        SINT(i)=SIN(fitr)
       if (i.le.9) then
        SIGNF(i)=1.
       else
        SIGNF(i)=-1.
       endif
        fcorti=1.4E-4*SINT(i) 
        FCORT(i)=SIGNF(i)*AMAX1(ABS(fcorti),4.E-5)
        FCOR(i)=fcorti
       do n=1,NS
        DXT(i,n)=2.*PI*ERAD*COST(i)*FRS(n)
       enddo 
      enddo 
      
      do i=1,IX
        fiur=PI*FIX(i)/180. 
        SINU(i)=SIN(fiur)
        COSU(i)=COS(fiur)
       if (i.le.IT/2) then
        SIGNFx=1.
       else
        SIGNFx=-1.
       endif
        fcorui=1.4E-4*SINU(i) 
        FCORU(i)=SIGNFx*AMAX1(ABS(fcorui),3.E-5)
       do n=1,NS
        DXX(i,n)=2.*PI*ERAD*COSU(i)*FRS(n)
       enddo 
       do n=1,NS1
        DXU(i,n)=2.*PI*ERAD*COSU(i)*FRSU(n)
       enddo 
      enddo
      
      do n=1,NS1
        n1=n-1
        if (n.eq.1) n1=NS
        n2=n
        if (n.eq.NS1) n2=1
       do i=1,IT
        DXTT(i,n)=0.5*(DXT(i,n1)+DXT(i,n2))
       enddo
      enddo 
       
c...  Averaged land elevation (ice free + ice sheets) 

      do i=1,IT
       do n=1,NS
        HORO(i,n)=(1.-FRGLC(i,n))*HOROL(i,n)+FRGLC(i,n)*HOROG(i,n) 
       enddo
      enddo 
        
c...  Earth and ocean squares 

      do i=1,IT
       do n=1,NS
        DX(i)=2.*PI*COS(PI*FIT(i)/180.)*ERAD*FRS(n)
        SQRA(i,n)=DX(i)*DY
        SQRO(i,n)=SQRA(i,n)*FROCN(i,n)
        SQA=SQA+SQRA(i,n)
        SQO=SQO+SQRO(i,n)
        SQL=SQL+SQRA(i,n)*FRLND(i,n)
       enddo
      enddo
      close (1)
      
      
      return
      end
      
*********************************************************************  
                      SUBROUTINE GEO_OCN  
*********************************************************************  
*
*  Purpose: Initialisation program preparating ocean geography masks,
*           oceanic grid, and zonal parameters for oceanic grid         
*
*    
*  By: A.Ganopolski
*  Last modification: 10.12.2004
********************************************************************* 
      INCLUDE 'declar.inc'              
      INCLUDE 'params.inc'
      INCLUDE 'mbiota.inc'
      INCLUDE 'marine_bio.inc'
      INCLUDE 'svat.inc'
      common /hoc/ HOCO(LT,NOC)
*********************************************************************
*     2.5deg resolution version
*********************************************************************

      open (1,file=trim(INFLDR)//'GEO/geo_ocn.dat')
      
      read (1,*)
      read (1,*) DFIO

c...  Latitudinal coordinates

      DYO=PI*ERAD*DFIO/180.

      do i=1,LT
       FITO(i)=90.-(i-0.5)*DFIO
      enddo
      
      do i=1,LX
       FIXO(i)=90.-(i-1)*DFIO
      enddo  
                    
C...  ZONAL PARAMETERS

      do i=1,LT
       
        fitr=PI*FITO(i)/180.
        COSTO(i)=COS(fitr)
        SINTO(i)=SIN(fitr)
        FCORTO(i)=1.4E-4*SINTO(i)
      
      enddo 
      
      do i=1,LX
        fiur=PI*FIXO(i)/180. 
        SINXO(i)=SIN(fiur)
        COSXO(i)=COS(fiur)
        
       if (i.le.LX/2) then
        SIGNFx=1.
       else
        SIGNFx=-1.
       endif
       
        FCORXO(i)=SIGNFx*AMAX1(ABS(1.4E-4*SINXO(i)),4.E-5)

      enddo
      
c...  Levels depth and layer thickness      
      
      read (1,*)
      read (1,*) ZX 
      
      do j=1,JT
       ZZ(j)=ZX(j+1)-ZX(j)
       ZT(j)=0.5*(ZX(j+1)+ZX(j))
      enddo 
      
      do j=2,JT
       ZZT(j)=ZT(j)-ZT(j-1)
      enddo 

c...  OCEAN DEPTH                                
      
      read (1,*)
      read (1,*)
      read (1,*)

      do i=1,LT
       read (1,*) nn,(HOCO(i,n),n=1,NOC)
       do n=1,NOC
        HOCO(i,n)=1000.*HOCO(i,n) 
       enddo   
      enddo
      
c...  FLUX X-MASK        
        
      read (1,*)
      read (1,*)
      read (1,*)
      do i=1,LT
       read (1,*) nn,(MASKX(i,n),n=1,NOC1)
      enddo 
        
c    ===================
      call OCN_MASKER(0)
c    ===================
      
      close (1)

c initial alkalimity fluxes
       do n=1,NOC
        do i=1,LT
           j=1
c MGT(i,j,n) is ocean grid mask: 1 if it is ocean, 0 otherwise
          if (MGT(i,j,n).eq.1) then
       ter_DIC=ter_DIC+RUNOC_weav_BIC_oc(i,n)*12./1000.
       ter_ALK=ter_ALK+RUNOC_weav_BIC_oc(i,n)*12./1000.
       ter_DIC13=ter_DIC13+RUNOC_weav_13BIC_oc(i,n)*12./1000.
          endif
         enddo
        enddo      

c flux on the ocean grid mask

      do i=1,LT
        l=(i+3)/4
       do n=1,NOC
       if (MASKT(i,n).eq.1) then

        RUNOC_WEAV_BIC_oc(i,n)=0.25*RUNOC_WEAV_BIC(l,n)
        RUNOC_WEAV_13BIC_oc(i,n)=0.25*RUNOC_WEAV_13BIC(l,n)

       endif
       enddo
      enddo
      return
      end
      
*********************************************************************  
                      SUBROUTINE OCN_MASKER(KODM)  
*********************************************************************  
*
*  Purpose: Initialisation program preparating ocean geography masks,
*           oceanic grid, and zonal parameters for oceanic grid         
*
*    
*  By: A.Ganopolski
*  Last modification: 12.03.2006
********************************************************************* 
      INCLUDE 'declar.inc'              
      INCLUDE 'params.inc'
      INCLUDE 'ocean.inc'
      common /hoc/ HOCO(LT,NOC)
      DIMENSION HOCP(LT,NOC), OXP(LT,NOC), SUMOS(IT), ODXTO(LT,NOC)
      DIMENSION ABARP(LX,JT), O4(LX,JX,NOC,5)                
*********************************************************************
      
c...  Oceanic fraction from atmospheric grid
      
      do i=1,LT
       ii=(i-1)/4 + 1
       do n=1,NOC
          FRIOB(i,n)=0.
        do m=1,NS
        if (NOA(ii,m).eq.n) 
     >    FRIOB(i,n)=FRIOB(i,n)+FRS(m)*FROCN(ii,m)
        enddo
       enddo
      enddo

c...  OCEAN MASK

      do i=1,LT
       do n=1,NOC
        if (HOCO(i,n).ne.0.and.FRIOB(i,n).gt.0.01) then
         MASKT(i,n)=1
        else
         MASKT(i,n)=0 
        endif
       enddo
      enddo 
      
c... Smoothed depth         
      
      do n=1,NOC
       do i=3,LT-2
        sum=0.
        HOCP(i,n)=0.
        HOC(i,n)=HOCO(i,n)
        do l=1,5
         k=i+l-3
         if (MASKT(k,n).eq.1) then
          sum=sum+1.
          HOCP(i,n)=HOCP(i,n)+HOCO(k,n)
         endif
        enddo
         if (sum.ne.0.) HOCP(i,n)=HOCP(i,n)/sum
       enddo
      enddo
         
      do n=1,NOC
       do i=3,LT-2
        HOC(i,n)=HOCP(i,n)
       enddo
      enddo
      
c... Set depth to depth of nearest model level

      do n=1,NOC
       do i=1,LT
        do j=2,JX
         if (HOC(i,n).le.ZX(j)) goto 1
        enddo
1       continue
         delt_z1=ZX(j)-HOC(i,n)
         delt_z2=HOC(i,n)-ZX(j-1)
        if (delt_z1.gt.delt_z2) then
          HOC(i,n)=ZX(j-1)
         else
          HOC(i,n)=ZX(j)
        endif
       enddo
      enddo            

c... Border depth      
      
      do n=2,NOC
       do i=1,LT
        HOCX(i,n)=AMIN1(HOC(i,n),HOC(i,n-1))
       enddo
      enddo  
      
      do n=1,NOC
       do i=2,LX-1
        HOCY(i,n)=AMIN1(HOC(i,n),HOC(i-1,n))
       enddo
      enddo  

c...  FLUX Y-MASK        
        
      do n=1,NOC
       do i=2,LT
        if (MASKT(i-1,n).ne.0.and.MASKT(i,n).ne.0) then
         MASKY(i,n)=1
        else
         MASKY(i,n)=0
        endif  
       enddo
      enddo
      
c...  GRID-CELL T-MASK

      do n=1,NOC
       do i=1,LT    
        do j=1,JT
          if(MASKT(i,n).eq.1. and .ZX(j+1).le.HOC(i,n)) then
             MGT(i,j,n)=1
          else
             MGT(i,j,n)=0
          endif           
        enddo
       enddo
      enddo
      
c... GRID_CELL X-mask
      
      do n=1,NOC
       do i=1,LX
         i1=MAX0(i-1,1)
         i2=MIN0(i,LT)
         if (MGT(i1,1,n)+MGT(i2,1,n).ge.1) then
          MGX(i,1,n)=1 
          else
          MGX(i,1,n)=0
         endif     
        do j=2,JX
         j1=MAX0(j-1,1)
         j2=MIN0(j,JT)
          if ((MGT(i1,j1,n)+MGT(i1,j2,n)+MGT(i2,j1,n)+MGT(i2,j2,n))
     >     .ge.1) then
             MGX(i,j,n)=1
          else
             MGX(i,j,n)=0
          endif 
        enddo
       enddo
      enddo
      
c...  FLUX LONGITUDE(FX)-MASK

      do n=1,NOC1
       do i=1,LT    
        do j=1,JT
          nmn=n-1
          if (nmn.eq.0) nmn=NOC
          npn=n
          if (npn.eq.NOC1) npn=1
          if(MGT(i,j,nmn).eq.1. and .MGT(i,j,npn).eq.1) then
             MFX(i,j,n)=MASKX(i,n)
          else
             MFX(i,j,n)=0
          endif           
        enddo
       enddo
      enddo
      
      
c...  FLUX LATITUDE(FY)-MASK

      do n=1,NOC
       do i=2,LT    
        do j=1,JT
          if(MGT(i-1,j,n).eq.1. and .MGT(i,j,n).eq.1) then
             MFY(i,j,n)=MASKY(i,n)
          else
             MFY(i,j,n)=0
          endif           
        enddo
       enddo
      enddo

c...  FLUX VERTICAL(FZ)-MASK

      do n=1,NOC
       do i=1,LT 
        do j=1,JT
          if(MGT(i,j,n).eq.1) then
             MFZ(i,j,n)=1
          else
             MFZ(i,j,n)=0
          endif           
        enddo
       enddo
      enddo

                    
c...  Basin width

      do n=1,NOC
       do i=1,LT
        ODXTO(i,n)=2.*PI*COSTO(i)*ERAD*FRIOB(i,n)
        ODXT(i,n)=ODXTO(i,n)
       enddo
      enddo

c...  Smoothed basin width

      do n=1,NOC
      do i=3,LT-2
        sum=0.
        OXP(i,n)=0.
       do l=1,5
        k=i+l-3
        if (MASKT(k,n).eq.1) then
         sum=sum+1.
         OXP(i,n)=OXP(i,n)+ODXTO(k,n)
        endif
       enddo
       if (sum.eq.5.) then
         OXP(i,n)=OXP(i,n)/sum
        else
         OXP(i,n)=ODXT(i,n)
       endif  
      enddo
      enddo
         
      do n=1,NOC
       do i=3,LT-2
        ODXT(i,n)=OXP(i,n)
       enddo
      enddo
          
c... New basin fraction and border length          
          
      do n=1,NOC
       do i=1,LT
        FRIOB(i,n)=ODXT(i,n)/(2.*PI*COSTO(i)*ERAD)
       enddo
       do i=2,LX-1
        ODXX(i,n)=SQRT(ODXT(i-1,n)*ODXT(i,n))
       enddo
      enddo
       
c... Total fraction

      GFRIO=0.
      GLOBL=0.
      
      do i=1,LT 
        FRIOB(i,4)=0. 
       do n=1,NOC 
        FRIOB(i,4)=FRIOB(i,4)+FRIOB(i,n)
       enddo
        GFRIO=GFRIO+FRIOB(i,4)*COSTO(i)
        GLOBL=GLOBL+COSTO(i)
      enddo 

c...  Earth and ocean squares, ocean volume

      SQO=0.

      do i=1,LT
       do n=1,NOC
        SQRO2(i,n)=ODXT(i,n)*DYO
        SQO=SQO+SQRO2(i,n)
       enddo
      enddo

      do i=1,LT
       do n=1,NS
        SQRA2(i,n)=PI*ERAD*(COSXO(i)+COSXO(i+1))*DYO*FRS(n)
       enddo
      enddo 
        
      OVOL=0.

      do i=1,LT
       do n=1,NOC
        do j=1,JT
         if (MGT(i,j,n).eq.1)  then
           DVOL(i,j,n)=SQRO2(i,n)*ZZ(j)
           OVOL=OVOL+DVOL(i,j,n)
         endif  
        enddo
       enddo
      enddo  
      
      close (1)
      
cmp  Just write ocean grid info for postprocessing
      if (.false.) then
c Write ocean cells area (2D)
         open(999,file=trim(OUTFLDR)//'sqrocn.dat')
c         write(999,'(72f20.0)') SQRO2
         write(999,'(3f20.0)') ((SQRO2(i,n),n=1,NOC),i=1,LT)
         close(999)
c Write ocean cells volume (3D)
         open(998,file=trim(OUTFLDR)//'dvol.dat')
         write(998,'(3f20.0)') (((DVOL(i,j,n),n=1,NOC),i=1,LT),j=1,20)
         close(998)
      endif
      if (.false.) then
c Write atmosphere cells area (2D)
         open(999,file=trim(OUTFLDR)//'sqratm.dat')
         write(999,'(7f20.0)') ((SQRA(i,n),n=1,7),i=1,18)
         close(999)
         stop
      endif

100   continue

      return
      end

******************************************************************** 
                     SUBROUTINE DATA_OCN
********************************************************************
*         Surface daily ocean input for atmosphere-only run                           
*
*  By: A.Ganopolski
*  Last modification: 10.12.2004
******************************************************************** 
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'          
      INCLUDE 'buffer.inc'
      INCLUDE 'observ.inc'
      INCLUDE 'atmos.inc'
      INCLUDE 'ocean.inc'
      INCLUDE 'svat.inc'               
      INCLUDE 'simenv_mod_f.inc'         
********************************************************************

c...1)  Model oceanic data for atmosphere-only model
         
      
c.. Model daily SST and sea ice characteristics for atmsopheric grid

      open (1,
!     >   file=trim(OUTFLDR)//'INP/ocean_mod.dat'//trim(simenv_run_char))
!     >   file=trim(OUTFLDR)//'INP/ocean_mod.dat')
     >   file=trim(INFLDR)//'INP/ocean_mod.dat')
              
      read (1,'(18E14.7)') (((SSTM(i,n,m),i=1,LT),n=1,NS),m=1,360)
      read (1,'(18E14.7)') (((FRIM(i,n,m),i=1,LT),n=1,NS),m=1,360)
      read (1,'(18E14.7)') (((HICM(i,n,m),i=1,LT),n=1,NS),m=1,360)
      
      close (1)
      
      return
      end

******************************************************************** 
                     SUBROUTINE DATA_FTO
********************************************************************
*          Implied ocean flux input for SLAB-ocean version                          
*
*  By: A.Ganopolski
*  Last modification: 10.12.2004
******************************************************************** 
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'          
      INCLUDE 'buffer.inc'
      INCLUDE 'observ.inc'
      INCLUDE 'atmos.inc'
      INCLUDE 'ocean.inc'
      INCLUDE 'svat.inc'     
      INCLUDE 'simenv_mod_f.inc'         
********************************************************************

c...1)  Net ocean surface heat flux for SLAB version

      open (1,
!     >  file=trim(INFLDR)//'FTO/fto_mod.dat'//trim(simenv_run_char))
     >  file=trim(INFLDR)//'INP/fto_mod.dat')
      read (1,'(72E15.7)') ((FTONET(i,n),i=1,LT),n=1,NOC)
      close (1)
      
      return
      end

******************************************************************** 
                     SUBROUTINE DATA_VEG
********************************************************************
*                Vegetation charactreristics                           
*
*  By: A.Ganopolski
*  Last modification: 10.12.2004
******************************************************************** 
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'          
      INCLUDE 'buffer.inc'
      INCLUDE 'observ.inc'
      INCLUDE 'atmos.inc'
      INCLUDE 'ocean.inc'
      INCLUDE 'svat.inc'               
      INCLUDE 'simenv_mod_f.inc'         
********************************************************************
      
      open (1, 
     > file=trim(INFLDR)//'INP/vegetat_mod.dat')
!     > file=trim(OUTFLDR)//'INP/vegetat_mod.dat'//trim(simenv_run_char))

      do i=1,IT
       read (1,'(35f8.5)')  (STOBS(i,n),n=1,7),
     >                      (SGOBS(i,n),n=1,7),
     >                      (SDOBS(i,n),n=1,7),
     >                      (BLAIOBS(i,n,1),n=1,7),
     >                      (BLAIOBS(i,n,2),n=1,7) 
       enddo

      close (1)

      return
      end

******************************************************************** 
                    SUBROUTINE INIT_COP
********************************************************************
*                COUPLER INITIALIZATION MODULE                               
*                                                                   
*  Purpose: Defenition of initial conditions
*
*  By: A.Ganopolski
*  Last modification: 10.12.2004
******************************************************************** 
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'          
      INCLUDE 'ocean.inc'
      INCLUDE 'coup.inc'
******************************************************************** 
     
       do i=1,LT
        do n=1,NOC
         FTAO(i,n)=0.
         FSAO(i,n)=0.
         FTOI(i,n)=0.
         QICE(i,n)=0.
         TAU_O(i,n)=0.
        enddo
       enddo

      if (KLSR.eq.0) then
      
      do i=1,LT
       do n=1,NS
        TSUR_i(i,n,2)=T0
       enddo
      enddo 
      
      endif
      
      return
      end 

******************************************************************** 
                    SUBROUTINE INIT_ASI
********************************************************************
*                ASI INITIALIZATION MODULE                               
*                                                                   
*  Purpose: Defenition of initial conditions  
*
*  By: A.Ganopolski
*  Last modification: 10.12.2004
******************************************************************** 
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'          
      INCLUDE 'buffer.inc'
      INCLUDE 'observ.inc'
      INCLUDE 'atmos.inc'
      INCLUDE 'ocean.inc'
      INCLUDE 'svat.inc'               
********************************************************************

c... Orographic roughness
      
      do i=1,IT
       do n=1,NS
        ZRORO(i,n)=0.041*SIGORO(i,n)**0.71
       enddo
      enddo 
      
      do i=1,IT
       do n=1,NS
          if (SNMAS(i,n).gt.1000.) SNMAS(i,n)=1000.
        do ntp=1,NST
          if (FRSTP(i,n,ntp).lt.0.0001) TSUR(i,n,ntp)=T0
        enddo  
       enddo
      enddo    

c... INITIALISATION OF SURFACE CHARACTERISTIC FOR "START" (KSR=0)

      if (KLSR.eq.0) then
      
      do i=1,IT
       do n=1,NS

          FLWR(i,n,1,1)=SIGM*T0**4
          TSUR(i,n,1)=TM(i,1,NOA(i,n))+T0
          TSUR(i,n,2)=T0
          TASUR(i,n,1)=TSUR(i,n,1)
          TASUR(i,n,2)=TSUR(i,n,2)
          CDAVE(i,n)=1.E-3
          
        do ntp=3,6
        
c... Screen temperature

          TSUR(i,n,ntp)=T0
          TASUR(i,n,ntp)=TSUR(i,n,ntp)
          
c... Soil moisture          
         
          WSOILMX(i,n,ntp,1)=0.1*0.3
          WSOIL(i,n,ntp,1)  =0.0*0.3
          RWSOIL(i,n,ntp,1) =0.0
          WSOILMX(i,n,ntp,2)=0.9*0.3
          WSOIL(i,n,ntp,2)  =0.9*0.3
          RWSOIL(i,n,ntp,2) =1.0

         enddo
       
       do ntp=1,NST      
         call SPAR
       enddo
       
       enddo
      enddo    
      endif                     

      return
      end                              
       
******************************************************************** 
                     SUBROUTINE INIT_ATM
********************************************************************
*              ATMOSPHERIC INITIALIZATION MODULE                               
*                                                                   
*  Purpose: Defenition of initial conditions and atmospheric
*           parameters   
*
*  By: A.Ganopolski
*  Last modification: 10.12.2004
******************************************************************** 
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'
      INCLUDE 'observ.inc'          
      INCLUDE 'atmos.inc'
      INCLUDE 'svat.inc'
********************************************************************

C...1) ATMOSPHERIC LEVELS/LAYERS

c...1.0) P- and Z-levels

      do k=1,NZ
       PL(k)=1.05-0.1*k
       DPL(k)=0.1*P0/G
       ZL(k)=-HATM*ALOG(PL(k))
       PL2(k)=1.00-0.1*(k-1)
       ZL2(k)=-HATM*LOG(PL2(k))
      enddo 
       ZL2(NZ1)=30000.

c...1.1) Orography and atmospheric column mass

      do i=1,IT
       do n=1,NS
        ZTS(i,n)=HORO(i,n)*FRLND(i,n)
        PSUR(i,n)=P0*EXP(-ZTS(i,n)/HATM)
       enddo
      enddo
      
      call CYCLE(ZTS,ZTSC) 
        
c...1.2) DPX - effective layer thickness on X-border

      do n=1,NS1
       do i=1,IT
       
        PX(i,n)=0.
       
        ZSX=0.5*(ZTSC(i,n)+ZTSC(i,n+1))
        ZSXM(i,n)=ZSX

        do k=1,NZ
           if (ZL2(k).le.ZSX.and.ZL2(k+1).ge.ZSX) then
                    DPX(i,n,k)=(ZL2(k+1)-ZSX)/(ZL2(k+1)-ZL2(k))*DPL(k)
           else if (ZL2(k).gt.ZSX) then 
                    DPX(i,n,k)=DPL(k)
           else if (ZL2(k+1).lt.ZSX) then
                    DPX(i,n,k)=0.
          endif
          
           if (i.ge.4.and.i.le.7.and.n.eq.7.and.k.eq.1) DPX(i,n,k)=0.0
         
           PX(i,n)=PX(i,n)+DPX(i,n,k)
           
        enddo       

       enddo
      enddo

c...1.3) DPY - effective layer thickness on Y-border

      do n=1,NS
       do i=2,IT
           
        PY(i,n)=0.
       
        ZSY=AMAX1(ZTS(i,n),ZTS(i-1,n))
        ZSYM(i,n)=ZSY

        do k=1,NZ
           if (ZL2(k).le.ZSY.and.ZL2(k+1).ge.ZSY) then
                    DPY(i,n,k)=(ZL2(k+1)-ZSY)/(ZL2(k+1)-ZL2(k))*DPL(k)
           else if (ZL2(k).gt.ZSY) then 
                    DPY(i,n,k)=DPL(k)
           else if (ZL2(k+1).lt.ZSY) then
                    DPY(i,n,k)=0.
          endif

           PY(i,n)=PY(i,n)+DPY(i,n,k)

        enddo       

       enddo
      enddo
      
c...1.4) DPT - effective layer thickness in T-points

      do n=1,NS
       do i=1,IT
       
           PT(i,n)=0.
           ZST=ZTS(i,n)

        do k=1,NZ
           if (ZL2(k).le.ZST.and.ZL2(k+1).ge.ZST) then
                    DPT(i,n,k)=(ZL2(k+1)-ZST)/(ZL2(k+1)-ZL2(k))*DPL(k)
           else if (ZL2(k).gt.ZST) then 
                    DPT(i,n,k)=DPL(k)
           else if (ZL2(k+1).lt.ZST) then
                    DPT(i,n,k)=0.
          endif

           PT(i,n)=PT(i,n)+DPT(i,n,k)

        enddo       

       enddo
      enddo

C...2) INITIALIZATION OF ATMOSPHERIC CHARACTERISTICS FOR "START" REGIME
      
      if (KLSR.eq.0) then
      
c...2.1) Initial tropopause height

      do n=1,NS1
       do i=1,IX
        fir=PI*FIX(i)/180.
        HTROPX(i,n)=12000.+4000.*COS(2.*fir)
       enddo
      enddo          
      
      do n=1,NS
       do i=1,IT
        fir=PI*FIT(i)/180.
        HTROP(i,n)=12000.+4000.*COS(2.*fir)
       enddo
      enddo          

C..2.2) Initial atmospheric temperature and humidity

      do n=1,NS
       do i=1,IT
        TAM(i,n)=T0
        TASURA(i,n)=T0
        QSAT=FQSAT(TAM(i,n))
        RQAM(i,n)=0.7
        QAM(i,n)=RQAM(i,n)*QSAT
       enddo
      enddo  
      
      endif

C...2.3) Solar insolation and zenit angle
      
                                 
      return
      end
********************************************************************* 
                    SUBROUTINE INIT_OCN 
********************************************************************* 
*              OCEAN MODEL INITIALISATION                                 
*
*  Purpose: preparation of initial conditions and parameters
*           for ocean module
*
*  By A.Ganopolski  
*  Last modification: 10.12.2004
********************************************************************* 
	  INCLUDE 'declar.inc'           
	  INCLUDE 'params.inc'
	  INCLUDE 'observ.inc'
	  INCLUDE 'atmos.inc'
	  INCLUDE 'ocean.inc'
          INCLUDE 'paramsmod.inc'
	
	  DIMENSION ABARP(LX,JT), O4(LX,JX,NOC,5)                
********************************************************************* 

C...1)    PARAMETERS   ...

c...     overturning coefficient
 
      do 100 n=1,NOC
     
      do j=1,JT
      
       do i=1,8
         ABARP(i,j)=0.1*AOVER
       enddo
              
       do i=9,53
        if (FRIOB(i,n).gt.0.01) then
         ABARP(i,j)=AOVER/(2.*PI*FRIOB(i,n))
         ABARP(i,j)=MIN(ABARP(i,j),AOVER)
cmp         ABARP(i,j)=AMIN1(ABARP(i,j),AOVER) 
cmp the above is commented because AMIN1 issues a warning when AOVER 
cmp is declared as REAL
        endif 
       enddo
      
      enddo
      
      do i=54,65
       do j=1,JT-2
        ABARP(i,j)=0.001*AOVER
       enddo
       do j=JT-1,JT
        ABARP(i,j)=0.1*AOVER
       enddo 
      enddo 
        
      do i=66,LX
       do j=1,JT
        ABARP(i,j)=0.01*AOVER
       enddo 
      enddo
      
c...  smoothing

      do i=3,LX-2
       do j=1,JT
        ABAR(i,j,n)=
     >    0.1*ABARP(i-2,j)+0.2*ABARP(i-1,j)+0.4*ABARP(i,j)+
     >    0.1*ABARP(i+2,j)+0.2*ABARP(i+1,j)
       enddo
      enddo       

100   continue

c...2.2.1) defenition of initial T & S 

      if (KLSR.eq.0) then

      do n=1,NOC
       do i=1,LT
        do j=1,JT
      
            TM(i,j,n)=25.*COSTO(i)**2*EXP(-ZT(j)/1000.)
            SM(i,j,n)=34.7
           
        enddo
       enddo
      enddo
      
      call DENS

      endif
      

c...2.7) coefficients of diffusion

      do n=1,NOC1
       do i=1,LT
        do j=1,JT
          AHZ=0.5*AHOC*(1.+EXP(-ZT(j)/ZHOC))
          Ax(i,j,n)=AHZ
        enddo
       enddo
      enddo
         
             
      do j=1,JT
         AHZ=0.5*AHOC*(1.+1.*EXP(-ZT(j)/ZHOC))
       do n=1,NOC
        do i=1,LT
           Ay(i,j,n)=AHZ
cts       Ay(i,j,n)=AHZ*(0.5+ABS(SINTO(i))*COSTO(i))  ! diffusivity scheme from CLIMBER2.3
c         if (i.ge.56) then
c           Ay(i,j,n)=0.5*AHZ
c         else
c           Ay(i,j,n)=AHZ
c         endif 

        enddo
       enddo
      enddo   

cmp   New parameter names for vertical diffusivity 
cmp   Kz_ml     : mixed layer depth (upper 50 m)
cmp   Kz_upper  : upper layer (0 < < 2500m)
cmp   Kz_lower  : deep ocean (> 2500 m)
cmp   Kz_z      : depth of the transition zone between upper and lower
cmp   Kz_dz     : controls extent of the transition zone (about 250m)
cmp   Kz_so     : southern ocean

      print *, 'Ocean vertical diffusivity'
      print *,"Kz_upper",Kz_upper
      print *,"Kz_lower",Kz_lower
      print *,"Kz_so",Kz_so
      print *,"Kz_ml",Kz_ml
      print *,"Kz_z",Kz_z
      print *,"Kz_dz",Kz_dz

      Az_sum = 0.
      iAz_count = 0.

      do n=1,NOC
       do i=1,LT
        do j=1,JX
cts        Az(i,j,n)=0.9E-4+1.05E-4/PI*ATAN(4.5E-3*(ZX(j)-2500.))
cmp        Az(i,j,n)=a_Kv+b_Kv/PI*ATAN(c_Kv*(ZX(j)-2500.))
        Az(i,j,n)=(Kz_upper+Kz_lower)/2
     >       -(Kz_upper - Kz_lower)/PI*ATAN((ZX(j)-Kz_z)/Kz_dz)

!     Special parameterizations for southern ocean and mixed layer
        if (i.le.60.and.i.ge.56.and.j.le.14) Az(i,j,n)=Kz_so
cmp        if (i.le.60.and.i.ge.56.and.j.le.14) Az(i,j,n)=3.0E-4
cts        if (i.gt.12.and.j.le.2) Az(i,j,n)=1.0E-4
        if (j.le.2) Az(i,j,n)= Kz_ml
cmp        if (j.le.2) Az(i,j,n)=1.0E-4

        Az_sum = Az_sum + Az(i,j,n)
        iAz_count = iAz_count + 1
        enddo
       enddo
      enddo

      print *, '--> unweighted Average: ',Az_sum/iAz_count

      return
      end

********************************************************************* 
                    SUBROUTINE INIT_OCC 
********************************************************************* 
*              OCEAN CARBON CYCLE MODEL INITIALISATION                                 
*
*  Purpose: preparation of initial conditions and parameters
*           for ocean carbon cycle model
*
*  By V.Brovkin  
*  Last modification: 04.04.2001
********************************************************************* 
	INCLUDE 'declar.inc'           
	INCLUDE 'params.inc'
	INCLUDE 'observ.inc'
	INCLUDE 'atmos.inc'
	INCLUDE 'ocean.inc'
        INCLUDE 'marine_bio.inc'
        INCLUDE 'mbiota.inc'
********************************************************************* 

c     =================
       call INITMBIOPAR
c     =================

c     ============================
      if(KLSR.eq.0) call INIT_MB
c	call INIT_MB
c     ============================

      do n=1,NOC
       do i=1,LT
            FOPO4(i,n) = 0.0
            FONO3(i,n) = 0.0
            FOSI(i,n)  = 0.0
            FOO2(i,n)  = 0.0
            FOALK(i,n) = 0.0
            FODIC(i,n) = 0.0
            FODOC(i,n) = 0.0
            FODOCS(i,n) = 0.0
            FOC13(i,n) = 0.0
            FODOC13(i,n) = 0.0
            FODOCS13(i,n) = 0.0
       enddo
      enddo

c     =================
	CALL OCN_BIO      
c     =================

c..3) definition of global hypsometric curve
      open(2,file=trim(INFLDR)//'GEO/bgc_hyps.dat',status='old')
      do j=1,JX
        read(2,'(I8,F8.3)') I,fhypso(j)
        if (j.le.2) fhypso(j)=0.
      enddo

        fhypT(1)=1.
        do j=2,jT
        fhypT(j)=(1.-fhypso(j))*1.124

         if(fhypt(j).gt.1.0) fhypt(j)=1.
* sediment hypsometry: steep till 500 m
        fhypso_sed(j)=1-fhypT(j)
cv        print *,j,fhypso_sed(j)

       enddo
        fhypso_sed(JX)=1.

      close(2)

      OVOL1=0.
      do j=1,JT
      do i=1,LT
       do n=1,NOC
         if (MGT(i,j,n).eq.1)  then
           OVOL1=OVOL1+DVOL(i,J,n)*fhypT(j)
         endif
        enddo
       enddo
      enddo

c       vol_cor=OVOL1/OVOL
c test
c       print *,'cor ocean volume',OVOL1,OVOL,OVOL1/OVOL

c initial alkalinity frtom restart file
       total_alk=0.
       do n=1,NOC
        do i=1,LT
          do j=1,JT
          if (MGT(i,j,n).eq.1) then
        total_alk=total_alk+OALK(i,J,n)*
     <  DVOL(i,J,n)*fhypt(j)/OVOL1
          endif
         enddo
        enddo
       enddo

        OALK_ini=total_alk
c        print *,'OALK_ini',OALK_ini

      close(2)

c weathering flux on the ocean grid mask

      do i=1,LT
        l=(i+3)/4
       do n=1,NOC
       if (MASKT(i,n).eq.1) then

        RUNOC_WEAV_BIC_oc(i,n)=0.25*RUNOC_WEAV_BIC(l,n)
        RUNOC_WEAV_13BIC_oc(i,n)=0.25*RUNOC_WEAV_13BIC(l,n)

       endif
       enddo
      enddo

c coral and sea level areas

       open(2,file=trim(INFLDR)//'GEO/etopo2_clim.txt')       
       do nbelt=1,LT
       do nsect=1,NOC
       do lev=1,300
       read(2,*) i,j,k,TOPOF(nsect,nbelt,lev),
     > AREA_DEPTH(nsect,nbelt,lev),index
c       print *,nsect,nbelt,lev,TOPOF(nsect,nbelt,lev),
c     > AREA_DEPTH(nsect,nbelt,lev),index
       enddo
       enddo
       enddo 
       close(2)

      return
      end


********************************************************************* 
                    SUBROUTINE INIT_TVM 
********************************************************************* 
*                 TVM MODEL INITIALISATION                                 
*
*  Purpose: preparation of initial conditions and parameters
*           for terrsetrial vegetation  module
*
*  By A.Ganopolski  
*  Last modification: 10.12.2004
********************************************************************* 
      INCLUDE  'declar.inc'              
      INCLUDE  'params.inc'
      INCLUDE  'buffer.inc'      
      INCLUDE  'bio.inc'
      INCLUDE  'observ.inc'
********************************************************************* 

c...1) Initial distribution of vegetation

      if (KLSR.eq.0) then

      do i=1,IT
       do n=1,NS
        ST(i,n)= 0.
        SG(i,n)= 0.
        SD(i,n)= 1.
        BLAI(i,n,1)=0.
        BLAI(i,n,2)=0.
       enddo
      enddo
      
      endif
      
c...2) Land mask initialisation
   
      call INITMAST

c...3) Initialisation of  TVM parameters

      call INITCPAR

c     getting correct land area     
      open (555,file=trim(INFLDR)//'GEO/landarea.dat')
      do i=1,IT
      read(555,*) (carea(i,n),n=1,NS)
      enddo
      close (555)

c     getting correct glacial land area
      open (555,file=trim(INFLDR)//'GEO/landarea_glac.dat')
      do i=1,IT
      read(555,*) (carea_glac(i,n),n=1,NS)
c      print *,(carea_glac(i,n),n=1,NS)
      enddo
      close (555)

 
      return
      end


*********************************************************************  
                    SUBROUTINE ROOT 
*********************************************************************  
*  Purpose: Computation of run-off rooting
*
*  By: A.Ganopolski
*  Last modification: 10.12.2004
********************************************************************* 
      INCLUDE  'declar.inc'              
      INCLUDE  'params.inc'
      INCLUDE  'svat.inc'
       
      DIMENSION  IWFR(IT,NS,4), IIFR(IT,NS,4), CONT(IT,NOC)
*********************************************************************
 
C...1) Reading of input runoff rooting file
 
      open (1,file=trim(INFLDR)//'GEO/runoff.dat')

      read (1,*)
      read (1,*)
      read (1,*)
      read (1,'(7(4i1,2x))') (((IWFR(i,n,m),m=1,4),n=1,NS),i=1,IT)  

      close(1)

C...2) Reading of input calving rooting file
 
      open (1,file=trim(INFLDR)//'GEO/iceoff.dat')

      read (1,*)
      read (1,*)
      read (1,*)
      read (1,'(7(4i1,2x))') (((IIFR(i,n,m),m=1,4),n=1,NS),i=1,IT)  

      close(1)
      
C...3) Relative  rooting function

      do i=1,IT
       do n=1,NS
       
        do k=1,IT
         do l=1,NOC
           FROFF(i,n,k,l)=0.
           FRIOFF(i,n,k,l)=0.
          enddo
         enddo 
         
c+++++++ RUNOFF +++++++++++++++++++++++++          
       
       if (FRLND(i,n).gt.0.) then
       
         msum=0
        do m=1,4
         msum=msum+IWFR(i,n,m)
        enddo
        
        if (msum.eq.0) then
          if (FROCN(i,n).gt.0.) then
            FROFF(i,n,i,NOA(i,n))=1.
           else 
            print *,'ALARM!!! msum=0 for i n=',i,n
           endif 
            goto 3
          endif
            
         sum=msum
         amsum=1./sum
          
        do m=1,4
         if (IWFR(i,n,m).ne.0) then
         
c...     North   ...         
         
          if (m.eq.1) then
           k=i
1          k=k-1           
           l=n
           if (FROCN(k,l).eq.0.)  goto 1
          endif 
          
c...     East   ...         
         
          if (m.eq.2) then
           k=i
           l=n
4          l=l+1
           if (l.gt.NS) l=1
           if (FROCN(k,l).eq.0.) goto 4
          endif 
          
c...     South   ...         
         
          if (m.eq.3) then
           k=i
2          k=k+1           
           l=n
           if (FROCN(k,l).eq.0.) goto 2
          endif 
          
c...     West   ...         
         
          if (m.eq.4) then
           k=i
           l=n-1
           if (l.lt.1) l=NS
           if (FROCN(k,l).eq.0.) 
     >          print *,'ALARM!!! No ocean West i,n=',i,n
          endif 
          
         FROFF(i,n,k,NOA(k,l))=IWFR(i,n,m)*amsum
         
        endif
        enddo
        
3       continue
       endif
       
c +++++++ ICEOFF +++++++++++++++++++++++++++++++       

         msum=0
        do m=1,4
         msum=msum+IIFR(i,n,m)
        enddo
        
        if (msum.eq.0) then
          if (FROCN(i,n).gt.0.) then
            FRIOFF(i,n,i,NOA(i,n))=1.
           else 
            print *,'ALARM ICE!!! msum=0 for i n=',i,n
           endif 
            goto 13
          endif
            
            
         sum=msum
         amsum=1./sum
          
        do m=1,4
         if (IIFR(i,n,m).ne.0) then
         
c...     North   ...         
         
          if (m.eq.1) then
           k=i
10         k=k-1           
           l=n
           if (FROCN(k,l).eq.0.)  goto 10
          endif 
          
c...     East   ...         
         
          if (m.eq.2) then
           k=i
           l=n
11         l=l+1
           if (l.gt.NS) l=1
           if (FROCN(k,l).eq.0.) goto 11
          endif 
          
c...     South   ...         
         
          if (m.eq.3) then
           k=i
12          k=k+1           
           l=n
           if (FROCN(k,l).eq.0.) goto 12
          endif 
          
c...     West   ...         
         
          if (m.eq.4) then
           k=i
           l=n-1
           if (l.lt.1) l=NS
           if (FROCN(k,l).eq.0.) 
     >          print *,'ALARM!!! No ocean West i,n=',i,n
          endif 
          
         FRIOFF(i,n,k,NOA(k,l))=IIFR(i,n,m)*amsum
         
        endif
        enddo
        
13       continue
       
       enddo
      enddo
      
C...4)  Sel-fcontrol

c...4.1) Conservation

      do i=1,IT
       do n=1,NS
       if (FRLND(i,n).gt.0.) then
        sum=0.
         do k=1,IT
          do l=1,NOC
           sum=sum+FROFF(i,n,k,l)
          enddo
         enddo
        if (ABS(sum-1.).gt.0.001) 
     >    print *,'ALARM!!! No conservation RUNO i,n=',i,n,sum
       endif 
       enddo
      enddo   

          
      return
      end    
      
*********************************************************************  
                    SUBROUTINE RUNINFO
*********************************************************************  
*       Basic run parameters and heading of screen output
*
*  By: A.Ganopolski
*  Last modification: 10.12.2004
********************************************************************* 
      INCLUDE  'declar.inc'              
      INCLUDE  'params.inc'
      CHARACTER*8  NAME1, NAME2 
      CHARACTER*12 NAME3, NAME4, NAME5 
********************************************************************* 
      NAME3=' START TIME:'
      NAME4='   END TIME:'
      NAME5=' RUN LENGTH:'
      
      if (KTIME.eq.0) then
       NYREND=NYRSR
      else
cmp       NYREND=NYRSR+NYRMX
       NYREND=NYRSR+NYRMX-1
      endif  

      print *,'***************************
     >                              PIK, Potsdam '
      print *,'****** CLIMBER-2.5 ********         
     >                     (C) CLIMBER, 1995-2011'
      print *,'***************************'
      print *
      print '(A12,I10,A12,I10,A12,I10)',
     >  NAME3,NYRSR,NAME4,NYREND,NAME5,NYRMX 
      print *
      print *,'  NYR    CO2   T_GB   T_NH   T_SH    PRC    SST     SS    
     > TOC   RBTP   I_NH   I_SH   NADW   AABW   VSAH'       
      print *,'=========================================================
     >=============================================='
      
      return
      end 

************************************************************************
      SUBROUTINE DATA_RF
************************************************************************
*     Read radiative forcing anomaly data for all forcings except GHGs,
*     solar and volcanic forcings (the latters are read separately and
*     activated via the switches KSOLC, KCO2 and KCO2D). These include:
*     - Aerosol direct effect 
*     - Aerosol indirect effect on clouds 
*     - Land-use albedo change
*     - Tropospheric and Stratospheric ozone
*     - Stratospheric H2O from CH4 oxidation
*     - Black carbon on snow 
************************************************************************

      INCLUDE 'declar.inc'            
      INCLUDE 'params.inc'
      INCLUDE 'atmos.inc'
      INCLUDE 'paramsmod.inc' ! for scale_AD and scale_AID


c     RCP RF time-series 
      REAL RF_AeD_t(800),RF_AeID_t(800),
     >     RF_LandUse_t(800), RF_O3_t(800),
     >     RF_stratH2O_t(800), RF_BCSNOW_t(800)

c     2D patterns of RF anoamalies (fixed)
      REAL RF_AeD_2D(IT,NS),RF_AeID_2D(IT,NS),
     >     RF_LandUse_2D(IT,NS), RF_O3_2D(IT,NS),
     >     RF_stratH2O_2D(IT,NS), RF_BCSNOW_2D(IT,NS)

c     Reconstructed 2D RF time-series 
      REAL RF_AeD_2Dt(IT,NS,800),RF_AeID_2Dt(IT,NS,800),
     >     RF_LandUse_2Dt(IT,NS,800), RF_O3_2Dt(IT,NS,800),
     >     RF_stratH2O_2Dt(IT,NS,800), RF_BCSNOW_2Dt(IT,NS,800)

      INTEGER YEAR_RF(800), ISTART, IEND, TSHIFT
      CHARACTER*100 string
      REAL AVE  ! result of average

      if (KTIME.ne.1.or.KCAL.ne.1) then
         print *, 'Error, must be a real time experiment. Stop.'
         stop
      endif
      if (KSOLC.eq.0.or.KCO2.eq.0.or.KCO2D.eq.0) then
         print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         print *, 'Warning : check CO2 and solar input' 
         print *, '(At least one of them if fixed)'
         print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      endif
         
      print *, 'Load radiative forcing data'

c    Read time series RF 
c ---------------------------------------------------------------------

      open (111,
     $     file=trim(INFLDR)//'INP/RF_TS.dat',iostat=ios,status='old',
     $     action='read')
      read (111,*)
      do i=1,800
         read (111,*,iostat=ios) YEAR_RF(i), RF_AeD_t(i),RF_AeID_t(i),
     $        RF_O3_t(i), RF_LandUse_t(i), RF_stratH2O_t(i),
     $        RF_BCSNOW_t(i)
         if (ios.ne.0) exit
      enddo
      close(111)

      TSHIFT=NYRSR-YEAR_RF(1)
      ISTART=max(TSHIFT+1,1)
      IEND=ISTART+NYRMX-1

c     Scale RF Forcing Aerosol to account for uncertainty 
c ---------------------------------------------------------------------
      do m=ISTART,IEND 
          RF_AeD_t(m)= RF_AeD_t(m)*scale_AD
          RF_AeID_t(m)= RF_AeID_t(m)*scale_AID
      enddo


c     Add the various contributions (in the case of uniform forcing)
c ---------------------------------------------------------------------

      if (KRFA.eq.1) then

         do m=1,NYRMX
            mt=m+TSHIFT
            if (mt.gt.0) then
               RFglob=RF_AeD_t(mt)+RF_AeID_t(mt)+RF_LandUse_t(mt)
     $              +RF_O3_t(mt)+RF_stratH2O_t(mt)+RF_BCSNOW_t(mt)
            else
               RFglob=0
            endif
            do n=1,NS
               do i=1,IT
                  RF_ALL_2Dt(i,n,m)=RFglob;
               enddo
            enddo
         enddo
c         return

      endif  !(KRFA.eq.1)


c     Temporal scaling of the patterns
c--------------------------------------------------------------------

      if (KRFA.eq.2) then


c  --------------------------------------------------------------------
c     Read 2D patterns for RF
c  --------------------------------------------------------------------
c  Aerosol Direct Effect 
      open (111,file=trim(INFLDR)//'INP/RF_2D_AeD.dat')
      read (111,'(7E16.6)') ((RF_AeD_2D(i,n),n=1,NS),i=1,IT)
      close(111)
c  Aerosol Indirect Effect
      open (111,file=trim(INFLDR)//'INP/RF_2D_AeID.dat')
      read (111,'(7E16.6)') ((RF_AeID_2D(i,n),n=1,NS),i=1,IT)
      close(111)
c  O3
      open (111,file=trim(INFLDR)//'INP/RF_2D_O3.dat') 
      read (111,'(7E16.6)') ((RF_O3_2D(i,n),n=1,NS),i=1,IT)
      close(111)
c  Land Use
      open (111,file=trim(INFLDR)//'INP/RF_2D_LandUse.dat')
      read (111,'(7E16.6)') ((RF_LandUse_2D(i,n),n=1,NS),i=1,IT)
      close(111)
c  Stratospheric H2O from CH4 oxidation
cmp set a uniform pattern for CH4 oxidation
      !open (111,file=trim(INFLDR)//'INP/RF_2D_stratH2O.dat')
      !read (111,'(7E16.6)') ((RF_stratH2O_2D(i,n),n=1,NS),i=1,IT)
      !close(111)
      do n=1,NS
	 do i=1,IT
	    RF_stratH2O_2D(i,n)=1.
	 enddo
      enddo
c  Black carbon on snow
      open (111,file=trim(INFLDR)//'INP/RF_2D_BCSNOW.dat')
      read (111,'(7E16.6)') ((RF_BCSNOW_2D(i,n),n=1,NS),i=1,IT)
      close(111)

c  --------------------------------------------------------------------
c     Scale 2D patterns with RF time-series
c  --------------------------------------------------------------------

c     Aerosol Direct Effect 
         call AVECL2(RF_AeD_2D,AVE)
         do m=1,NYRMX
            mt=m+TSHIFT
            if (mt.gt.0) then
               scale=RF_AeD_t(mt)/AVE
            else
               scale=0
            endif
            do n=1,NS
               do i=1,IT
                  RF_AeD_2Dt(i,n,m)=RF_AeD_2D(i,n)*scale
               enddo
            enddo
         enddo

c     Aerosol Indirect Effect 
         call AVECL2(RF_AeID_2D,AVE)
         do m=1,NYRMX
            mt=m+TSHIFT
            if (mt.gt.0) then
               scale=RF_AeID_t(mt)/AVE
            else
               scale=0
            endif
            do n=1,NS
               do i=1,IT
                  RF_AeID_2Dt(i,n,m)=RF_AeID_2D(i,n)*scale
               enddo
            enddo
         enddo

c     O3
         call AVECL2(RF_O3_2D,AVE)
         do m=1,NYRMX
            mt=m+TSHIFT
            if (mt.gt.0) then
               scale=RF_O3_t(mt)/AVE
            else
               scale=0
            endif
            do n=1,NS
               do i=1,IT
                  RF_O3_2Dt(i,n,m)=RF_O3_2D(i,n)*scale
               enddo
            enddo
         enddo

c     Land Use
         call AVECL2(RF_LandUse_2D,AVE)
         do m=1,NYRMX
            mt=m+TSHIFT
            if (mt.gt.0) then
               scale=RF_LandUse_t(mt)/AVE
            else
               scale=0
            endif
            do n=1,NS
               do i=1,IT
                  RF_LandUse_2Dt(i,n,m)=RF_LandUse_2D(i,n)*scale
               enddo
            enddo
        enddo

c     Black carbon on snow
         call AVECL2(RF_BCSNOW_2D,AVE)
         do m=1,NYRMX
            mt=m+TSHIFT
            if (mt.gt.0) then
               scale=RF_BCSNOW_t(mt)/AVE
            else
               scale=0
            endif
            do n=1,NS
               do i=1,IT
                  RF_BCSNOW_2Dt(i,n,m)=RF_BCSNOW_2D(i,n)*scale
               enddo
            enddo
        enddo

c     Stratospheric H2O from CH4 oxidation
         call AVECL2(RF_stratH2O_2D,AVE)
         do m=1,NYRMX
            mt=m+TSHIFT
            if (mt.gt.0) then
               scale=RF_stratH2O_t(mt)/AVE
            else
               scale=0
            endif
            do n=1,NS
               do i=1,IT
                  RF_stratH2O_2Dt(i,n,m)=RF_stratH2O_2D(i,n)*scale
               enddo
            enddo
         enddo


c  --------------------------------------------------------------------
c     Add the various contributions
c  --------------------------------------------------------------------

          do m=1,NYRMX
            do n=1,NS
               do i=1,IT
                  RF_ALL_2Dt(i,n,m)=RF_AeD_2Dt(i,n,m)+RF_AeID_2Dt(i,n,m)
     $                 +RF_LandUse_2Dt(i,n,m)+RF_O3_2Dt(i,n,m)
     $                 +RF_stratH2O_2Dt(i,n,m)+RF_BCSNOW_2Dt(i,n,m)
               enddo
            enddo
         enddo        

      endif  ! (KRFA.eq.2)


      ! For checking

      if (.false.) then
      ! 2D fields
      open(111,file=trim(OUTFLDR)//'RF_2Dt.dat')
      do m=1,NYRMX
         do i=1,18
            write(111,'(7F7.2)') (RF_ALL_2Dt(i,n,m),n=1,7)
         enddo
      enddo
      close(111)

      !timeseries 

      open(111,file=trim(OUTFLDR)//'RF_glob.dat')
      do m=1,NYRMX
      sum=0.0
         do i=1,18
            do n=1,7
               sum=sum+RF_ALL_2Dt(i,n,m)*SQRA(i,n)
            enddo
         enddo 
         write(111,'(I4, F7.2)') NYRSR+m-1, sum/SQA
      enddo
      close(111)

      endif

      return
      end



*************************************************************************
      SUBROUTINE AVECL2(VAR2D,AVE)

*************************************************************************
*     Return the spatially averaged value on Climber 2 grid
*************************************************************************
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'
      ! INPUT
      REAL VAR2D(IT,NS)
      ! OUTPUT
      REAL AVE
      ! LOCAL
      REAL SUM

      SUM=0.0
      
      do n=1,NS
         do i=1,IT
            SUM=SUM+VAR2D(i,n)*SQRA(i,n)
         enddo
      enddo
      AVE=SUM/SQA

      end


*************************************************************************
      SUBROUTINE READCONS

*************************************************************************
*     Read climatological constraints for Climber 2 output variables
*************************************************************************
      INCLUDE 'declar.inc'
      INCLUDE 'params.inc'
      INCLUDE 'observ.inc'

      open(77,file=trim(INFLDR)//'INP/constraints.dat')
      read(77,*)
      read(77,*)
      read(77,*) iSPINUP
      read(77,*) tgmin, tgmax     
      read(77,*) inmin, inmax 
      read(77,*) ismin, ismax 
      read(77,*) prcmin, prcmax
      read(77,*) tvmin, tvmax 
      read(77,*) fatmxmin, fatmxmax
      read(77,*) xamxmin, xamxmax
      close(77)
      
      print *, 'The simulation assumes a spinup time of ', iSPINUP    
   
      end

*********************************************************************  
                      SUBROUTINE INIT_WEAV  
*********************************************************************  
******************************************************************** 
      INCLUDE 'declar.inc'              
      INCLUDE 'params.inc'
      INCLUDE 'svat.inc'
*********************************************************************
                            
      open (1,file=trim(INFLDR)//'GEO/lit_map.dat')
c volcanic degassing

      read (1,*)      
      read (1,*)  VOLC_CO2_OUTG ,VOLC_13CO2_OUTG 
c .. carbonate 

      read (1,*)      
      read (1,*)      
      read (1,*)  
      read (1,*)            
      read (1,*)  CO2_cons(1)
      read (1,*)            
      do i=1,IT
      read (1,*) (FLIT_CAR(i,n),n=1,NS)
      enddo

c .. silicate 

      read (1,*)      
      read (1,*)      
      read (1,*)  CO2_cons(2)
      read (1,*)            

      do i=1,IT
      read (1,*) (FLIT_SIL(i,n),n=1,NS)
      enddo

c shelves
      read (1,*)
      read (1,*)
      do i=1,IT
      read (1,*) (FLIT_CAR_S(i,n),n=1,NS)
c      print *,(FLIT_CAR_S(i,n),n=1,NS)

      enddo

      read (1,*)
      read (1,*)
      do i=1,IT
      read (1,*) (FLIT_SIL_S(i,n),n=1,NS)
c      print *,(FLIT_SIL_S(i,n),n=1,NS)
      enddo
      
      return
      end

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine :  a r g s
c  Author     :  Alexander Robinson
c  Purpose    :  Get command line arguments from program call
c                *only obtains character arguments
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
      SUBROUTINE COMMANDARGS
        
        INCLUDE 'declar.inc' 
        INCLUDE 'params.inc'
        INCLUDE 'svat.inc'

        integer numarg
        character*512 arg1, EXEC, suffix

C       ! Define a default input and output folders, 
C       ! in case no argument is given        
        INFLDR  = "./"
        OUTFLDR = "./"

C       ! Find out if any command line arguments have been provided
        numarg = iargc()
        
        call getarg ( 0, EXEC )

C       write(*,*) "numarg=",numarg
        if ( numarg .gt. 0 ) then

C         ! Load the output folder from command line argument          
          call getarg ( 1, OUTFLDR )

C         ! Make sure that folder includes trailing slash
          i = len_trim(OUTFLDR)
          if ( OUTFLDR(i:i) .ne. "/" ) OUTFLDR(i+1:i+1) = "/"

        end if
        
C       ! If OUTFLDR is not "./", then running sicoX.x or climber.x,
C       ! change INFLDR to reflect extra directory structure
        if ( trim(OUTFLDR) .ne. "./" ) INFLDR = "climber25/"

C       ! Output output folder to standard out...
        write(*,*) "climber2.5 input  folder prefix: ", trim(INFLDR)
        write(*,*) "climber2.5 output folder prefix: ", trim(OUTFLDR)
      
        return

      end 
