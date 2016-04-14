program grazing_model
  
  ! Load structure and parameter module first
  use structure
  use parameter_var
  implicit none
  integer i
  
  ! # start the program with model initializtion 
  call model_ini
  call grazing_ini

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

  write(*,*) ' '
  write(*,*) '^^^^^^^^^^^^^^^^^^^^^^^^'
  write(*,*) '< Program is complete. >'
  write(*,*) '^^^^^^^^^^^^^^^^^^^^^^^^'

end program grazing_model
