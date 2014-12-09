***********************************************************************
      COMMON /OBSERV_DAT/
***********************************************************************
*    Last modification 27.12.98
***********************************************************************                    
      
* OBSERV
*********

     >   TAUO(IX,NOC,4),
     >   TLEV(IT,JT,NOC),    SLEV(IT,JT,NOC),   SS(IT,NOC),
     >   SST(IT,NOC,12),     SSTA(IT,NS,12),    FRICEOBS(IT,NOC,12),
     >   STOBS(IT,NS),       SGOBS(IT,NS),      SDOBS(IT,NS), 
     >   BLAIOBS(IT,NS,2),

* OBSERV_INT
*************
    
     >   SST_i(LT,NOC,12), SS_i(LT,NOC),

* CLIMATO_CONSTRAINTS
*********************
   
     > tgmin, tgmax, prcmin, prcmax, tvmin, tvmax, fatmxmin, fatmxmax, 
     > ismin, ismax, inmin, inmax, xamxmin, xamxmax,

* SPINUP TIME
*************
     > iSPINUP
