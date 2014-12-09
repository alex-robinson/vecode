REAL :: a,bet,gamm,fmax,avecube,tmin,npp,nppmax,v1,v2,v3,  &
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

! Code converted using TO_F90 by Alan Miller
! Date: 2014-12-08  Time: 12:21:29

COMMON /biodat/ a,bet,gamm,fmax,avecube,tmin,npp,nppmax,v1,v2,v3,  &
    c1t,c2t,c3t,c1g,c2g,c3g, d1t,d2t,d3t,d1g,d2g,d3g,  &
    e1t,e2t,e3t,e1g,e2g,e3g, f1t,f2t,f3t,f1g,f2g,f3g,  &
    b1t(it,ns),b2t(it,ns),b3t(it,ns),b4t(it,ns),  &
    b1g(it,ns),b2g(it,ns),b3g(it,ns),b4g(it,ns),  &
    b1t14(it,ns),b2t14(it,ns),b3t14(it,ns),b4t14(it,ns),  &
    b1g14(it,ns),b2g14(it,ns),b3g14(it,ns),b4g14(it,ns),  &
    b1t13(it,ns),b2t13(it,ns),b3t13(it,ns),b4t13(it,ns),  &
    b1g13(it,ns),b2g13(it,ns),b3g13(it,ns),b4g13(it,ns),  &
    k1t,k2t,k3t,k1g,k2g,k3g, t1t,t2t,t3t,t4t,t1g,t2g,t3g,t4g,  &
    ps1,ps2,ps3,ps4,ps5,soilt, forshare_st,desshare_st,g4share_st,  &
    nlshare_st,t1tn,t1td, deng,dentd,dentn,laig,lait,desmin,desmax,  &
    gdd0, gdd0_min,gdd0_max,co2,  &
    ave_t,ave_pr,ave_pr05,carea(it,ns),lon,lat,init_flag(it,ns),  &
    ades, acr, k0t, k0g, k4g,  &
    c14init,c14tdec,c13init,c13frac,c13ratio,c13frac4,c14ratio,  &
    soil13c(it,ns),carea_glac(it,ns)

