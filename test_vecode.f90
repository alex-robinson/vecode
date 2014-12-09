


program test_vecode 

    use vecode 

    implicit none

    real(sp) :: tann, pann, pann5, gdd, pco2 
    real(sp) :: forest,grass,desert,needles,carbon_uptake

    integer :: i 

    tann  = 290.0 
    pann  = 1000.0 
    pann5 = 1000.0 
    gdd   = 2000.0 
    pco2  = 280.0 

    write(*,*) "==== VECODE ===="
    write(*,"(10a10)")  "tann", "pann", "pann5", "gdd", "pco2", &
                        "forest", "grass", "desert", "needles", "c_uptake"

    do i = 1, 40 
!         tann = 230.0 + (i-1)*2
        pann5 = 0.0 + (i-1)*100
        call tvm(tann,pann,pann5,gdd,pco2, &
                 forest,grass,desert,needles,carbon_uptake)

        write(*,"(10f10.2)") tann, pann, pann5, gdd, pco2, &
                             forest, grass, desert, needles, 0.0

    end do 

    return

end program test_vecode 