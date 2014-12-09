*********************************************************************
***  BUFFER COMMON: DATA TRANSFER CLIMATE <-> TERRESTR VEG MODEL  ***
*********************************************************************
        COMMON /GEOGBIO/ MASKB(IT,NS), FRILB(IT,NS), FLONB(NS)   
        COMMON /CLIMATE/ TATB(IT,NS),  PRCB(IT,NS), PRCB5(IT,NS),
     >     GDD0B(IT,NS),TATMSMIN(IT,NS),TATMSMAX(IT,NS)
	COMMON /BIOTA/   
     >     ST(IT,NS),    SG(IT,NS),    SD(IT,NS), SG4(IT,NS),
     >     SNLT(IT,NS),  ANUP(IT,NS),  BLAI(IT,NS,2), PNPP(IT,NS),
     >     NSTAT(IT,NS), B12(IT,NS),   B34(IT,NS),
     >     B1(IT,NS),    B2(IT,NS),    B3(IT,NS),    B4(IT,NS),
     >     BC14(IT,NS),    BC13(IT,NS), ANUP13(IT,NS) 
