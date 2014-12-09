


program test_vecode 

    use vecode 

    implicit none

    real(sp) :: tann, pann, pann5, gdd, pco2 
    real(sp) :: forest,grass,desert,needles,carbon_uptake
    real(sp) :: frac(5) 

    integer :: i, n, k

    tann  = 23.0 
    pann  = 1000.0 
    pann5 = 1000.0 
    gdd   = 2000.0 
    pco2  = 280.0 

    write(*,"(a1,10a)") "#", " ==== VECODE ===="
    write(*,"(a1,10a10)")"#", "K", "mmpa", "mmpa", "degdays", "ppm", "1", "1", "1", "1", "Gtpm2"
    write(*,"(1x,10a10)")  "tann", "pann", "pann5", "gdd", "pco2", &
                        "forest", "grass", "desert", "needles", "c_uptake"

    n = 1000
    open(10,file="LHS/lhs_np5_ns1000.txt")

    do i = 1, n
        read(10,*) frac 

        tann  =   -50.0 + (50.0+50.0)*frac(1)
        pann  =   0.0 + (4000.0-0.0) *frac(2)
!         pann5 =   0.0 + ( 800.0-0.0) *frac(3)
        pann5 = pann 
        gdd   =   0.0 + (5000.0-0.0) *frac(4)
!         pco2  = 200.0 + (560.0-200.0)*frac(5)

        call tvm(tann,pann,pann5,gdd,pco2, &
                 forest,grass,desert,needles,carbon_uptake)

        write(*,"(1x,10f10.2)") tann, pann, pann5, gdd, pco2, &
                                forest, grass, desert, needles, 0.0

    end do 

    return

end program test_vecode 