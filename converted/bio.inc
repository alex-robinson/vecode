        REAL a,bet,gamm,fmax,avecube,tmin,npp,nppmax,v1,v2,v3,
     * c1t,c2t,c3t,c1g,c2g,c3g,
     * d1t,d2t,d3t,d1g,d2g,d3g,
     * e1t,e2t,e3t,e1g,e2g,e3g,
     * f1t,f2t,f3t,f1g,f2g,f3g,
     * k1t,k2t,k3t,k1g,k2g,k3g,
     * t1t,t2t,t3t,t4t,t1g,t2g,t3g,t4g,
     * ps1,ps2,ps3,ps4,ps5,soilt,
     * forshare_st,t1tn,t1td, desshare_st,nlshare_st,g4share_st,
     * deng,dentd,dentn,laig,lait,
     * ave_t,ave_pr,ave_pr05,desmin,desmax,
     * ades, acr, k0t, k0g, k4g,
     * c14init,c14tdec,c13init,c13frac,c13ratio,c13frac4,c14ratio

        COMMON /BIODAT/ 
     *  a,bet,gamm,fmax,avecube,tmin,npp,nppmax,v1,v2,v3,
     * c1t,c2t,c3t,c1g,c2g,c3g,
     * d1t,d2t,d3t,d1g,d2g,d3g,
     * e1t,e2t,e3t,e1g,e2g,e3g,
     * f1t,f2t,f3t,f1g,f2g,f3g,
     * b1t(IT,NS),b2t(IT,NS),b3t(IT,NS),b4t(IT,NS),
     * b1g(IT,NS),b2g(IT,NS),b3g(IT,NS),b4g(IT,NS),
     * b1t14(IT,NS),b2t14(IT,NS),b3t14(IT,NS),b4t14(IT,NS),
     * b1g14(IT,NS),b2g14(IT,NS),b3g14(IT,NS),b4g14(IT,NS),
     * b1t13(IT,NS),b2t13(IT,NS),b3t13(IT,NS),b4t13(IT,NS),
     * b1g13(IT,NS),b2g13(IT,NS),b3g13(IT,NS),b4g13(IT,NS),
     * k1t,k2t,k3t,k1g,k2g,k3g,
     * t1t,t2t,t3t,t4t,t1g,t2g,t3g,t4g,
     * ps1,ps2,ps3,ps4,ps5,soilt,
     * forshare_st,desshare_st,g4share_st,
     * nlshare_st,t1tn,t1td,
     * deng,dentd,dentn,laig,lait,desmin,desmax,
     * gdd0, gdd0_min,gdd0_max,co2,
     * ave_t,ave_pr,ave_pr05,carea(IT,NS),lon,lat,init_flag(IT,NS),
     * ades, acr, k0t, k0g, k4g,
     * c14init,c14tdec,c13init,c13frac,c13ratio,c13frac4,c14ratio,
     * SOIL13C(IT,NS),carea_glac(IT,NS)

