


program test_vecode 

    use vecode 
    use ncio 

    implicit none

    real(sp) :: pco2 
    real(sp), allocatable, dimension(:,:) :: tann, pann, pann5, gdd 
    real(sp), allocatable, dimension(:,:) :: forest,grass,desert,needles,carbon_uptake

    character(len=512) :: filename 

    integer :: i, j, k
    integer :: nx, ny, nt 

    pco2 = 280.0 

    ! Load a simulation from a file and calculate vecode on it 

    filename  = "/Users/robinson/wrk/mypapers/mis11/wrk/data/mis11y_m11/&
                &dTfctr.0.651.itmc.-54.404.ppfc.-0.058/clima.nc"

    ! Determine dimensions 
    nx = nc_size(filename,"x")
    ny = nc_size(filename,"y")
    nt = nc_size(filename,"time")

    allocate(tann(nx,ny),pann(nx,ny),pann5(nx,ny),gdd(nx,ny))
    allocate(forest(nx,ny),grass(nx,ny),desert(nx,ny), &
             needles(nx,ny),carbon_uptake(nx,ny))

    do k = 1, nt 
        write(*,*) "timestep = ", k, nt

        call load_rembo_slice(filename,k,tann,pann,pann5,gdd,pco2)
 
        do j = 1, ny
        do i = 1, nx
                call tvm(tann(i,j),pann(i,j),pann5(i,j),gdd(i,j),pco2, &
                         forest(i,j),grass(i,j),desert(i,j), &
                         needles(i,j),carbon_uptake(i,j))
        end do 
        end do 

        call nc_write(filename,"ft",forest, dim1="x",dim2="y",dim3="time",start=[1,1,k],count=[nx,ny,1])
        call nc_write(filename,"fg",grass,  dim1="x",dim2="y",dim3="time",start=[1,1,k],count=[nx,ny,1])
        call nc_write(filename,"fd",desert, dim1="x",dim2="y",dim3="time",start=[1,1,k],count=[nx,ny,1])
        call nc_write(filename,"fn",needles,dim1="x",dim2="y",dim3="time",start=[1,1,k],count=[nx,ny,1])

    end do

    return

contains 

    subroutine load_rembo_slice(filename,k,tann,pann,pann5,gdd,pco2)

        implicit none 

        real(sp), dimension(:,:) :: tann, pann, pann5, gdd
        real(sp) :: pco2 
        character(len=*) :: filename 
        integer :: k 
        real(sp), allocatable, dimension(:,:) :: tjan, tjul
        integer :: nx, ny 

        nx = size(tann,1)
        ny = size(tann,2)

        allocate(tjan(nx,ny))
        allocate(tjul(nx,ny))
        
        call nc_read(filename,"tjan",tjan,start=[1,1,k],count=[nx,ny,1])
        call nc_read(filename,"tjul",tjul,start=[1,1,k],count=[nx,ny,1])
        call nc_read(filename,"pdds",gdd, start=[1,1,k],count=[nx,ny,1])
        call nc_read(filename,"pp",  pann,start=[1,1,k],count=[nx,ny,1])
        
        tann  = (tjan+tjul)/2.0 
        pann5 = pann 
        pco2  = 280.0 

        return 

    end subroutine load_rembo_slice

end program test_vecode 
