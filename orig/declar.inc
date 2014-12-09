      PARAMETER (IT=18,  IX=19, NS=7,  NS1=8,   NS2=9, NZ=10,  NZ1=11,
     >            LT=72,  LX=73, NOC=3, NOC1=4, NOC2=5, JT=20,  JX=21, 
     >            NT=17,  NST=6,  NSL=2,ISM=240, JSM=87, 
     >            IFN=77, JFN=51, NIF=11, NJF=3, NZLWR = 16)
c================= ATMOSPHERE =========================================
com   IT         - number of latitudinal belts
com   IX = IT+1  - number of borders between lat. belts
com   NS         - number of sectors
com   NS1 = NS+1 - number of sectors border
com   NT         - number of tracers
com   NZ         - number of layers in atmopshere
com   NZ1=NZ+1   - number of levels in atmosphere
com   NZLWR      - number of levels in atmosphere for radiative model
c================= OCEAN ===============================================
com   LT         - number of latitudinal belts
com   LX         - number of borders between latitudinal belts
com   NOC        - number of ocean basins
com   NOC1 = NOC+1
com   NOC2 = NOC+2
com   JT         - number of layers in ocean
com   JX = JT+1  - number of levels in ocean
c================= LAND =========================================
com   NST        - number of surface types
com   NSL        - number of soil layers
