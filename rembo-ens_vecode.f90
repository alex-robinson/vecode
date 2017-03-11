


program test_vecode 

    use vecode 
    use ncio 

    implicit none

    real(sp) :: pco2 
    real(sp), allocatable, dimension(:,:,:) :: tann, pann, pann5, gdd 
    real(sp), allocatable, dimension(:,:,:) :: forest,grass,desert,needles,carbon_uptake

    character(len=512) :: filename1, filename2
    character(len=512) :: filename 

    integer :: i, j, k
    integer :: nx, ny, nsim 

    pco2 = 280.0 

    nsim = 300 
    nx   = 76
    ny   = 141 

    allocate(tann(nsim,nx,ny),pann(nsim,nx,ny),pann5(nsim,nx,ny),gdd(nsim,nx,ny))
    allocate(forest(nsim,nx,ny),grass(nsim,nx,ny),desert(nsim,nx,ny), &
             needles(nsim,nx,ny),carbon_uptake(nsim,nx,ny))

    filename1 = "/Users/robinson/models/sicopolis.git/output/mis11_m11b/&
                &ensemble.dTmax.nc"
    filename2 = "/Users/robinson/models/sicopolis.git/output/mis11_m11b/&
                &ensemble.dVmax.nc"    
    filename  = filename2

    call load_rembo_slice(filename,tann,pann,pann5,gdd,pco2)

    do j = 1, ny
         do i = 1, nx
            do k = 1, nsim
                call tvm(tann(k,i,j),pann(k,i,j),pann5(k,i,j),gdd(k,i,j),pco2, &
                         forest(k,i,j),grass(k,i,j),desert(k,i,j), &
                         needles(k,i,j),carbon_uptake(k,i,j))
            end do 
        end do 
        write(*,*) j
    end do 

    call nc_write(filename,"ft",forest, dim1="sim",dim2="x",dim3="y")
    call nc_write(filename,"fg",grass,  dim1="sim",dim2="x",dim3="y")
    call nc_write(filename,"fd",desert, dim1="sim",dim2="x",dim3="y")
    call nc_write(filename,"fn",needles,dim1="sim",dim2="x",dim3="y")

    return

contains 

    subroutine load_rembo_slice(filename,tann,pann,pann5,gdd,pco2)

        implicit none 

        real(sp), dimension(:,:,:) :: tann, pann, pann5, gdd
        real(sp) :: pco2 
        character(len=*) :: filename 
        real(sp), allocatable, dimension(:,:,:) :: tjan, tjul

        allocate(tjan(size(tann,1),size(tann,2),size(tann,3)))
        allocate(tjul(size(tann,1),size(tann,2),size(tann,3)))
        
        call nc_read(filename,"tjan",tjan)
        call nc_read(filename,"tjul",tjul)
        call nc_read(filename,"pdds",gdd)
        call nc_read(filename,"pp",pann)
        
        tann  = (tjan+tjul)/2.0 
        pann5 = pann 
        pco2  = 280.0 

        return 

    end subroutine load_rembo_slice

end program test_vecode 
