
! subroutine for annual rainfall assignment

subroutine annual_rainfall 
  use structure
  use parameter_var
  implicit none

  ! for normal random number generation function
  integer i4_normal_ab

  ! set a ro which is used for spatial autocorrelation ranging from 0.4 to 0.8
  real ro

  ! coefficient of variation from Fryxell 2005
  real cv 

  ! for spatial weight matirx 
  ! integer spwt_mat(MAX_X_DIM*MAX_Y_DIM,MAX_X_DIM*MAX_Y_DIM)
  integer, dimension(:,:), allocatable :: spwt_mat

  ! other temporary variables
  integer i, j, RF_N

  interface
    subroutine spwt_join(w,m,n)
      implicit none
      integer, dimension(:,:) ::  w
      integer :: m, n
    end subroutine spwt_join
  end interface

  ! setup the matrix
  allocate(spwt_mat(MAX_X_DIM*MAX_Y_DIM,MAX_X_DIM*MAX_Y_DIM))
  spwt_mat=0
  call spwt_join(spwt_mat,MAX_Y_DIM,MAX_X_DIM)

  ! write(*,*) ' '
  ! do y_dim = 1, MAX_Y_DIM*MAX_X_DIM
  !   write(*,*) (spwt_mat(y_dim, i), i=1,MAX_X_DIM*MAX_Y_DIM)
  ! end do

  ! set seed which is related to years, and changes each year
  seed = (year-1000)*100
  cv = 0.25
  ro = 0.8

  do y_dim = 1, MAX_Y_DIM

    ! For row average rainfall first
    ROW(y_dim)%AVG_RAINFALL_mu = AVG_N-((AVG_N-AVG_S)/(MAX_Y_DIM-1))*(y_dim-1)
    ROW(y_dim)%AVG_RAINFALL=i4_normal_ab(ROW(y_dim)%AVG_RAINFALL_mu, ROW(y_dim)%AVG_RAINFALL_mu*cv, seed)

    do x_dim = 1, MAX_X_DIM
      CELL(y_dim,x_dim)%RAINFALL_mu = i4_normal_ab(ROW(y_dim)%AVG_RAINFALL, ROW(y_dim)%AVG_RAINFALL*cv, seed*x_dim)
      CELL(y_dim,x_dim)%RAINFALL=CELL(y_dim,x_dim)%RAINFALL_mu
    end do

  end do

  ! write(*,'(10F)') ROW%AVG_RAINFALL_mu
  ! write(*,*) ' '
  ! write(*,'(10F)') ROW%AVG_RAINFALL

  ! write(*,*) ' '
  ! do y_dim = 1, MAX_Y_DIM
  !   write(*,'(10F)') (CELL(y_dim, i)%RAINFALL_mu, i=1,MAX_X_DIM)
  ! end do

  do y_dim=1,MAX_Y_DIM
    do x_dim=1,MAX_X_DIM

      j = 0
      RF_N = 0

      do i = 1, MAX_Y_DIM*MAX_X_DIM
        if (spwt_mat(x_dim+MAX_X_DIM*(y_dim-1), i) .eq. 1 .and. mod(i,MAX_X_DIM) .ne. 0) then
          RF_N=RF_N+CELL((i-mod(i,MAX_X_DIM))/MAX_X_DIM+1,mod(i,MAX_X_DIM))%RAINFALL_mu
          j=j+1
          !write(*,*) x_dim+MAX_X_DIM*(y_dim-1), i, spwt_mat(x_dim+MAX_X_DIM*(y_dim-1), i), CELL((i-mod(i,MAX_X_DIM))/MAX_X_DIM+1,mod(i,MAX_X_DIM))%RAINFALL_mu
          !write(*,*) RF_N
        end if 

        if (spwt_mat(x_dim+MAX_X_DIM*(y_dim-1), i) .eq. 1 .and. mod(i,MAX_X_DIM) .eq. 0) then
          RF_N=RF_N+CELL(i/MAX_X_DIM,MAX_X_DIM)%RAINFALL_mu
          j=j+1
          !write(*,*) x_dim+MAX_X_DIM*(y_dim-1), i, spwt_mat(x_dim+MAX_X_DIM*(y_dim-1), i), CELL(i/MAX_X_DIM,MAX_X_DIM)%RAINFALL_mu
          !write(*,*) RF_N
        end if
      end do
      ! write(*,*) 'j=', j
      RF_N=RF_N/j
      !write(*,*) RF_N
      CELL(y_dim,x_dim)%RAINFALL = ro*RF_N+((1-ro**2)*CELL(y_dim,x_dim)%RAINFALL_mu)**0.5
    end do
  end do

  ! do y_dim = 1, MAX_Y_DIM
  !   write(*,'(10F)') (CELL(y_dim, i)%RAINFALL, i=1,MAX_X_DIM)
  ! end do

end subroutine annual_rainfall
