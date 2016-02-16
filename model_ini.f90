subroutine model_ini

  use parameter_var
  use structure

  implicit none
  integer i          ! used for looping
  integer getcwd     ! used for getcwd() function to get current working directory
  integer temp

  write(*,*) ' '
  write(*,*) 'PROGRAM START'
  write(*,*) ' '

  ! # Get working directory and check whether it is successful
  direrr=getcwd(CWD)
    if (direrr .eq. 0) then
      write(*,*) 'CURRENT WORKING DIRECTORY IS:', trim(CWD)
    else
      write(*,*) 'PATH IS NOT FOUND'
      stop
    end if

  ! # Open general simulation configuration file [sim_config.gr]
  open(SIM_CON_NUM, file=CWD(1:len_trim(CWD))//SIM_CON_NAME, action='read', iostat=ioerr)
    if (ioerr .ne. 0) then 
      write(*,*) 'General config file not opend'
      stop
    end if

  ! ## Read X, Y dimensions
  read(SIM_CON_NUM,*)
  read(SIM_CON_NUM,*)
  read(SIM_CON_NUM,*)

  read(SIM_CON_NUM,*,iostat=ioerr) MAX_Y_DIM
    if (ioerr .ne. 0) then
      write(*,*) 'Maximum y reading error'
      stop
    else
      write(*,*) 'Maximum y is', MAX_Y_DIM
      read(SIM_CON_NUM,*)
    end if

  read(SIM_CON_NUM,*,iostat=ioerr) MAX_X_DIM
    if (ioerr .ne. 0) then
      write(*,*) 'Maximum y reading error'
      stop
    else
      write(*,*) 'Maximum x is', MAX_X_DIM
    end if

  ! ### Allocate structure variables with spatial demensions
  allocate(CELL(MAX_Y_DIM,MAX_X_DIM))
  allocate(ROW(MAX_Y_DIM))

  ! ## Read begin and end years
  read(SIM_CON_NUM,*)
  read(SIM_CON_NUM,*)
  read(SIM_CON_NUM,*)

  read(SIM_CON_NUM,*,iostat=ioerr) start_yr
    if (ioerr .ne. 0) then
      write(*,*) 'Start year reading error'
      stop
    else
      write(*,*) 'Start year is:', start_yr
      read(SIM_CON_NUM,*)
    end if

  read(SIM_CON_NUM,*,iostat=ioerr) end_yr
    if (ioerr .ne. 0) then
      write(*,*) 'End year reading error'
      stop
    else
      write(*,*) 'End year is:', end_yr
    end if

  ! ## Read cell area
  read(SIM_CON_NUM,*)
  read(SIM_CON_NUM,*)
  read(SIM_CON_NUM,*)

  read(SIM_CON_NUM,*,iostat=ioerr) CELLAREA
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Cell area reading error'
      stop
    else
      write(*,*) 'Cell area is: ', CELLAREA
    end if

  ! ## Read plant species number
  read(SIM_CON_NUM,*)
  read(SIM_CON_NUM,*)
  read(SIM_CON_NUM,*)

  ! ### Plant species number, this doesn't need a switch
  read(SIM_CON_NUM,*,iostat=ioerr) PLA_SPP_NUM
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Plant species number reading error'
      stop
    else
      write(*,*) 'Plant species number is: ', PLA_SPP_NUM
    end if

  ! # Allocate variables with species dimension (Better to be set as a separate module function which allows easier modifications)
  allocate(UAV_RATE(PLA_SPP_NUM))
  allocate(RES_RATE(PLA_SPP_NUM))
  allocate(SPP_BIOMASS(PLA_SPP_NUM))
	allocate(K_CO(PLA_SPP_NUM))
	allocate(DECREASE_R(PLA_SPP_NUM))
	allocate(DECREASE_R_CO(PLA_SPP_NUM))
	allocate(R_MAX(PLA_SPP_NUM))
	allocate(R_MAX_CO(PLA_SPP_NUM))

	! # Initialized coefficient variable
	DECREASE_R_CO(:)=1
	R_MAX_CO(:)=1

  do y_dim = 1, MAX_Y_DIM
    do x_dim = 1, MAX_X_DIM
			allocate(CELL(y_dim,x_dim)%GROW_DAYS(PLA_SPP_NUM))
			allocate(CELL(y_dim,x_dim)%GROW_DAYS_CO(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%TOT_BIO_SPP(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%AV_BIO_SPP(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%AV_BIO_SPP_P(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%UAV_BIO_SPP(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_K(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_K_CO(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_LAI(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_N_CON(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_DEN(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_RES(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_C_CON(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_MOR(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_GRO(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_PS(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_RI(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_CC(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_NU(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_RT(PLA_SPP_NUM))
      allocate(CELL(y_dim,x_dim)%SPP_TRP(PLA_SPP_NUM))

			! Initialized growth variables coefficient
			CELL(y_dim,x_dim)%GROW_DAYS_CO(:)=1
			CELL(y_dim,x_dim)%SPP_K_CO(:)=1
    end do
  end do

  ! ## Read in some plant growth variables
  read(SIM_CON_NUM,*,iostat=ioerr) (K_CO(i), i=1,PLA_SPP_NUM)
  if (ioerr .ne. 0 ) then 
    write(*,*) 'Carrying capacity coefficient reading error'
    stop
  else
    write(*,*) 'Carrying capacity coefficients are: '
    write(*,*) K_CO
  end if

  read(SIM_CON_NUM,*,iostat=ioerr) (DECREASE_R(i), i=1,PLA_SPP_NUM)
  if (ioerr .ne. 0 ) then 
    write(*,*) 'Decrease rate reading error'
    stop
  else
    write(*,*) 'Decrease rates are: '
    write(*,*) DECREASE_R
  end if

  read(SIM_CON_NUM,*,iostat=ioerr) (R_MAX(i), i=1,PLA_SPP_NUM)
  if (ioerr .ne. 0 ) then 
    write(*,*) 'Maximum growth rate reading error'
    stop
  else
    write(*,*) 'Maximum growth rates are: '
    write(*,*) R_MAX
  end if

	close(SIM_CON_NUM)

  ! # Open echo file and keep open throughout the running time [echo.gr]
  open(ECHO_NUM, file=CWD(1:len_trim(CWD))//ECHO_NAME, status='replace', action='write', iostat=ioerr)
    if (ioerr .ne. 0) then 
      write(*,*) 'echo file not opend'
      stop
    end if

    write(*,*) ' '
    write(*,*) 'Model initialization done!!'

end subroutine model_ini
