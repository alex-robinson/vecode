*********************************************************************** 
*                      PARAMS.INC
*********************************************************************** 
*    Last modification 26.02.96 
***********************************************************************                     

      CHARACTER*8 ENAME
      CHARACTER*512 INFLDR
      CHARACTER*512 OUTFLDR
      CHARACTER*512 RESTART_IN

      COMMON /PARAMS/ 

* EXPERIMENT ID 
****************        

     >  ENAME, 

* OUTPUT FOLDER (prefix)
****************

     >  INFLDR,OUTFLDR,

* EXTERNAL FLAGS
****************
         
     >  KTIME,  KCAL,  KLSR, KCFC,
     >  KLWR,  KLSF, KES, KOCN, KOCNW,  KTVM,  
     >  KSEM,   KOCAR, KSLR,  KCCC,  KCCR,  KGEU,  KDFLX, 
     >  KSED, KWEATH, KCORAL, KIRON, KLAND, KYEDOM, KFRAC,
     >  KSOLC,  KCO2,  KCO2D, KOUT,  K5,    KFB, KW_FBA,
     >  KZLEV, KW_ZLEV, KRH, KW_RH, K2RH, KWV, SHRH, KCOD, 
     >  KOUT_AFX,KOUT_A2GC,KOUT_A2GF,KOUT_A2ZT,KOUT_A2ZX,KOUT_A3D,
     >  KOUT_OCNF,KOUT_OCNS,KOUT_OCNT,KOUT_OCNX,KOUT_Q3D,KOUT_SLR2D,
     >  KOUT_A2G, KOUT_HIST, KOUT_CPL, RESTART_IN,

* INTERNAL FLAGS
****************
         
     >  KCOUP,  KENDY, KENDM, KMON, KMON2, KBIO, KOCEAN, KOCEANO, 
     >  KODS,   NSEMI, KINVAL, KREST, 
     >  KODD, KODEMIL, 
      
* TIME PARAMETERS
******************
 
     >  NYRSR, NYRA,  NYRC, NYRMX, MONMX, NDYMX,  NTSMX, NYRREW1,
     >  NTS,   NDAY,  NYR,  NYRO,  MON,   MONO,   NJUL,  NYRREW2,
     >  NFOUT, NAOUT, NSEM, 
     >  DJUL,  TST,   TSTO, TSTOC, TSTIC, TDAY,  TYER,   BTIME,        
 
* CONSTANTS & MODEL PARAMETERS
*******************************
    
     >  PI, G,  T0,  SIGM, CKARM, R, CLE,  CLS,  CLM, CHSNW,
     >  RW, RO, CR,  RA, CAP, CAV,  HATM,  ERAD,  P0, S0,
     >  OMEG, FCOR(IX), FCORT(IT), FCORU(IX), ABSF(IX),
     >  HBL, HEATM, ZSURF, GTO, GTT, TAMG, FROCNMIN,
     >  ZHOC, EPS, CCD, CCDRF,
     >  HTOA, 
     
* CO2 CONCENTRATION
********************     
     
     >  PA, PA0, PAR, PAB, C14ATM, C13ATM, O18ATM, emis, CUMCO2,

* Radiative Forcing 20th Century and RCPs
*****************************************
     >  KRFA,

* GEOGRAPHY
************

     >   FIT(IT),         FIX(IX),          DY,             DPHI,
     >   SINT(IT),        COST(IT),         SINU(IX),       COSU(IX),
     >   SQRA(IT,NS),     SQRO(IT,NS),     
     >   SQA,             SQO,              SQL,             sea_level,
     >   OVOL,            DX(IT),

* Atmosphere
      
     >   FROCN(IT,NS),    FRLND(IT,NS),     FRGLC(IT,NS),   NOA(IT,NS),
     >   FRIOG(IT),       SIGNF(IT), 
     >   FLON(NS),        FRS(NS),          FRSU(NS1),
     >   DXT(IT,NS),      DXX(IX,NS),       DXU(IX,NS1),   DXTT(IT,NS1),
     >   ZTS(IT,NS),      ZTSC(IT,NS2),     ZL(NZ),        ZL2(NZ1),       
     >   PL(NZ),          PL2(NZ1),         DPL(NZ),       
     >   DPX(IT,NS1,NZ),  DPY(IX,NS,NZ),    DPT(IT,NS,NZ), 
     >   PX(IT,NS1),      PY(IX,NS),        PT(IT,NS),
     >   ZSXM(IT,NS1),    ZSYM(IX,NS),    
     
* Ocean       

     >   FITO(LT),        FIXO(LX),         DYO,
     >   SINTO(LT),       COSTO(LT),        SINXO(LX),      COSXO(LX),
     >   ODXT(LT,NOC),    ODXX(LX,NOC),     ZT(JT),
     >   ZX(JX),          ZZ(JT),           ZZT(JX),
     >   MASKT(LT,NOC),   MASKX(LT,NOC1),   MASKY(LX,NOC),    
     >   MGT(LT,JT,NOC),  MFX(LT,JT,NOC1),  MFY(LX,JT,NOC), 
     >   MFZ(LT,JX,NOC),  MGX(LX,JX,NOC),   FRIOB(LT,NOC1),
     >   HOC(LT,NOC),     HOCX(LT,NOC1),    HOCY(LX,NOC),
     >   SQRA2(LT,NS),    SQRO2(LT,NOC),    DVOL(LT,JT,NOC),
     >   FCORTO(LX),      FCORXO(LX), 
     >   FSB, FIE, FBS,
     
* Land

     >   HORO(IT,NS),     HOROL(IT,NS),     HOROG(IT,NS), 
     >   SIGORO(IT,NS),   ZTST(IT,NS,NST),
     >   PSUR(IT,NS),     PSURC(IT,NS2)
