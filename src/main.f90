program grazing_model
  
  ! Load structure and parameter module first
  use structure
  use parameter_var
  implicit none
  integer i
  
  ! # start the program with model initializtion 
  call model_ini
  call grazing_ini

  do temp_h=1,4

    if (temp_h .eq. 1) temp_f = 0.0
    if (temp_h .eq. 2) temp_f = 0.07
    if (temp_h .eq. 3) temp_f = 0.13
    if (temp_h .eq. 4) temp_f = 0.19

    ! if (temp_h .eq. 1) temp_f = 0.0
    ! if (temp_h .eq. 2) temp_f = 0.09
    ! if (temp_h .eq. 3) temp_f = 0.16
    ! if (temp_h .eq. 4) temp_f = 0.25

    do y_dim=1,MAX_Y_DIM
      do x_dim=1,MAX_X_DIM
        CELL(y_dim,x_dim)%TOT_BIO_SPP(:)=0
      end do
    end do 
  
    write(*,*) ''
    ! write(*,'(A5,F6.2)') 'DGI: ', temp_f


  ! # do a yearly and daily loop
  do year=start_yr, end_yr

    ! ## setup annual rainfall for each cell
    ! call annual_rainfall
    ! ## calculate grwoing days using annual rainfall for each cell
    call growth_days
 
    ! ## Yearly biomass initialization
    TOT_BIOMASS_Y=0

    ! ## Loop for days
    do day=1,365

      ! ### Calculate plant growth or decrease for each day
      call plant_growth

      ! ### Grazing options
      call grazing_process_opt

      ! ### Actual grazing processes
      call grazing

      ! ### Experimental grazing effects functions  
      call graz_eff_func

      ! ### Model output configurations
      call model_output

    end do  ! End looping for days

  end do    ! End looping for years

  end do

  ! write(*,*) ' '
  ! write(*,*) '^^^^^^^^^^^^^^^^^^^^^^^^'
  ! write(*,*) '< Program is complete. >'
  ! write(*,*) '^^^^^^^^^^^^^^^^^^^^^^^^'

end program grazing_model
