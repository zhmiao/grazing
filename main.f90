program grazing_model
  
  ! Load structure and parameter module first
  use structure
  use parameter_var
  implicit none
  integer i
  
  ! # start the program with model initializtion 
  call model_initialization
  call grazing_initial

  ! # do a yearly and daily loop
  do year=start_yr, end_yr

    ! ## setup annual rainfall for each cell
    call annual_rainfall
    ! ## calculate grwoing days using annual rainfall for each cell
    call growth_days
 
    ! ## Set cell initial biomass for each plant species to zero in the begining of each year
    do y_dim = 1, MAX_Y_DIM
      do x_dim = 1, MAX_X_DIM

      ! if (mod(x_dim,2) .eq. 1) then
      !   CELL(y_dim,x_dim)%TOT_BIO_SPP(:)=1
      !   CELL(y_dim,x_dim)%SPP_K(:)=1
      ! else
      !   CELL(y_dim,x_dim)%TOT_BIO_SPP(:)=2
      !   CELL(y_dim,x_dim)%SPP_K(:)=2
      ! end if

        CELL(y_dim,x_dim)%TOT_BIO_SPP(:)=0

      end do
    end do

    ! CELL(:,:)%TOT_BIOMASS=0

    ! ## Loop for days
    do day=1,365

      ! write(*,*) 'day: ', day
      ! ### Calculate plant growth or decrease for each day
      call plant_growth

      ! ### Grazing options
      call grazing_process_opt

      ! ### Actual grazing processes and effects
      call grazing

      ! ### Model output configurations
      ! call model_output

  ! write(*,*) ' '
  ! do y_dim = 1, MAX_Y_DIM
  !   ! write(*,*) (CELL(y_dim, i)%SPP_GRAZED(:,:), i=1,MAX_X_DIM)
  !   ! write(*,*) (CELL(y_dim, i)%TOT_BIO_SPP(:), i=1,MAX_X_DIM)
  !   ! write(*,*) (ANI_AV_BIO(:), i=1,MAX_X_DIM)
  !   ! write(*,*) (CELL(y_dim, i)%AV_BIO_SPP(:), i=1,MAX_X_DIM)
  !   ! write(*,*) (CELL(y_dim, i)%AV_BIOMASS_P, i=1,MAX_X_DIM)
  !   ! write(*,*) (CELL(y_dim, i)%SS_PR_CLA(:), i=1,MAX_X_DIM)
  ! end do
  !
  ! do cur_ani=1,ANI_SPP_NUM
  ! do cur_cla=1,SITE_PREF(cur_ani)%SS_CLA_NUM
  ! ! write(*,*) SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla)
  ! ! write(*,*) SITE_PREF(cur_ani)%SPP_FORAGE(cur_cla,:)
  ! ! write(*,*) SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,:)
  ! ! write(*,*) SITE_PREF(cur_ani)%D_DMD(cur_cla)
  ! end do
  ! end do
  ! ! write(*,*)TOT_DMD(:)
  ! ! write(*,*) AV_BIOMASS_P

    end do  ! End looping for days

  end do    ! End looping for years

  write(*,*) ' '
  write(*,*) 'Program is complete.'

end program grazing_model
