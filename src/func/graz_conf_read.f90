! This subroutine runs when season has changed or on day one
subroutine graz_conf_read
  
  use parameter_var
  use structure
  implicit none

  integer :: i, j
  logical :: OK             ! Used to check whether one file has been opened

    ! # First close configuration opened for last season if there is a last season
    inquire(GR_CON_SEA, opened=OK)
    if(OK) close(GR_CON_SEA)

    ! # Open general grazing configuration file
    open(GR_CON_SEA, file=CWD(1:len_trim(CWD))//PRE_DIR_GC(1:len_trim(PRE_DIR_GC))//&
                SEASON(1:len_trim(SEASON))//'.gr', action='read', iostat=ioerr)

    if (ioerr .ne. 0) then
      write(*,*) 'Grazing configuration file not opened'
      stop
    end if

    ! ------------------------
    ! # Animal species number {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      ! ## If species difference switch is on
      if (SPP_SW .eq. 1) then
        read(GR_CON_SEA,*,iostat=ioerr) ANI_SPP_NUM
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Animal species number reading error'
          stop
        else
          write(ECHO_NUM,*) 'Animal species difference exist, number of species is: '
          write(ECHO_NUM,*) ANI_SPP_NUM
        end if

      else

      ! ## If species difference is off, species number is set to 1
        ANI_SPP_NUM=1
        read(GR_CON_SEA,*)      ! skip this line

      end if

      ! ## Allocate species different variables
      ! ### Deallocate variables first if it is not the first season
      if(allocated(FIX_GR_R))     deallocate(FIX_GR_R)
      if(allocated(MAX_SD))       deallocate(MAX_SD)
      if(allocated(MIN_SD))       deallocate(MIN_SD)
      if(allocated(ANI_NUM_SPP))  deallocate(ANI_NUM_SPP)
      if(allocated(MAX_INT))      deallocate(MAX_INT)
      if(allocated(TOT_DMD))      deallocate(TOT_DMD)
      if(allocated(ANI_COM_FAC))  deallocate(ANI_COM_FAC)
      if(allocated(ANI_AV_BIO))   deallocate(ANI_AV_BIO)
      if(allocated(DET_RATE))     deallocate(DET_RATE)
      if(allocated(SC_VAR_A))     deallocate(SC_VAR_A)
      if(allocated(SC_VAR_B))     deallocate(SC_VAR_B)
      if(allocated(N_RET_RATE))   deallocate(N_RET_RATE)
      do y_dim=1,MAX_Y_DIM
        do x_dim=1,MAX_X_DIM
          if(allocated(CELL(y_dim,x_dim)%SPP_GRAZED))  deallocate(CELL(y_dim,x_dim)%SPP_GRAZED)
          if(allocated(CELL(y_dim,x_dim)%SPP_DETACH))  deallocate(CELL(y_dim,x_dim)%SPP_DETACH)
          if(allocated(CELL(y_dim,x_dim)%SS_PR_CLA))   deallocate(CELL(y_dim,x_dim)%SS_PR_CLA)
        end do
      end do

      ! ### Allocate dimensions to variables that uses animal specise number
      allocate(FIX_GR_R(ANI_SPP_NUM))
      allocate(MAX_SD(ANI_SPP_NUM))
      allocate(MIN_SD(ANI_SPP_NUM))
      allocate(ANI_NUM_SPP(ANI_SPP_NUM))
      allocate(MAX_INT(ANI_SPP_NUM))
      allocate(TOT_DMD(ANI_SPP_NUM))
      allocate(ANI_COM_FAC(ANI_SPP_NUM))
      allocate(ANI_AV_BIO(ANI_SPP_NUM))
      allocate(DET_RATE(ANI_SPP_NUM))
      allocate(SC_VAR_A(ANI_SPP_NUM))
      allocate(SC_VAR_B(ANI_SPP_NUM))
      allocate(N_RET_RATE(ANI_SPP_NUM))
      do y_dim=1,MAX_Y_DIM
        do x_dim=1,MAX_X_DIM
          allocate(CELL(y_dim,x_dim)%SPP_GRAZED(ANI_SPP_NUM,PLA_SPP_NUM))
          allocate(CELL(y_dim,x_dim)%SPP_DETACH(ANI_SPP_NUM,PLA_SPP_NUM))
          allocate(CELL(y_dim,x_dim)%SS_PR_CLA(ANI_SPP_NUM))
        end do
      end do

      !}}}

    ! ------------------------
    ! # Animal competition factors {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      ! ## If competition switch is on
      if (COM_SW .eq. 1) then

        ! ### Competion can not exist when there is no animal difference and both fixed rate and stocking rate functions are off
        if(SPP_SW .eq. 0 .or. (FR_SW .eq. 0 .and. SD_SW .eq. 0)) then
          write(*,*) 'Error: SPP_SW and FR_SW or SD_SW should be turned on to have animal competition.'
          stop
        end if

        ! ### Read in competition factors
        read(GR_CON_SEA,*,iostat=ioerr) (ANI_COM_FAC(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Animal competition factors reading error'
          stop
        else
          write(ECHO_NUM,*) 'Animal competition exist, factors of species are: '
          write(ECHO_NUM,*) ANI_COM_FAC
        end if

      else

        ! ### When there is no competitions, the different species will get equal amount of resources
        ANI_COM_FAC(:)=1/ANI_SPP_NUM
        read(GR_CON_SEA,*)      ! skip this line

      end if
      !}}}

    ! ------------------------
    ! # Maximum and minimum management grazing amount modification {{{
    ! ------------------------

      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      ! ## Maximum amount of grazing management
      if (MAN_SW(2) .eq. 1) then
        read(GR_CON_SEA,*,iostat=ioerr) MAX_GR_AMT
        if (ioerr .ne. 0) then
          write(*,*) 'Maximum grazing amount configuration reading error'
          stop
        else
          write(ECHO_NUM,*) 'Grazing amount cannot exceed', MAX_GR_AMT
        end if

      else

        ! ### If maximum amount of grazing management switch is off, nothing changed
        read(GR_CON_SEA,*)        ! Skip this line

      end if

      ! ## Minimum amount of grazing management
      if (MAN_SW(3) .eq. 1) then
        read(GR_CON_SEA,*,iostat=ioerr) MIN_GR_AMT
        if (ioerr .ne. 0) then
          write(*,*) 'Minimum grazing amount configuration reading error'
          stop
        else
        write(ECHO_NUM,*) 'Grazing amount cannot less than', MIN_GR_AMT
        end if

      else

        ! ### If minimum amount of grazing management switch is off, minimum management grazing amount is zero. This variable is used for later demand modification
        MIN_GR_AMT=0
        read(GR_CON_SEA,*)        ! Skip this line

      end if
      !}}}

    ! ------------------------
    ! # Fixed rate {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      ! # Read in the grazing rate for each animal species
      if (FR_SW .eq. 1) then
        read(GR_CON_SEA,*,iostat=ioerr)(FIX_GR_R(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Fixed grazing rate reading error'
          stop
        else
          write(ECHO_NUM,*) 'Fixed grazing rate for each animal species are: '
          do cur_ani=1,ANI_SPP_NUM
            write(ECHO_NUM,*) FIX_GR_R(cur_ani)
          end do
        end if
      else
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end FR_SW cheking }}}

    ! ------------------------
    ! # Stocking rate function {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (SD_SW .eq. 1) then

        ! ## If both stocking density switch and fixed rate switch are on, stop the model
        if (FR_SW .eq. 1) then
          write(*,*) 'Error: Fixed grazing rate and Stoking density cannot both be turned on.'
          stop
        end if

        ! ## Read in maximum stoking density
        read(GR_CON_SEA,*,iostat=ioerr)(MAX_SD(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Maximum stocking density reading error'
          stop
        else
          write(ECHO_NUM,*) 'Maximum stocking density for each animal species are: '
          do cur_ani=1,ANI_SPP_NUM
            write(ECHO_NUM,*) MAX_SD(cur_ani)
          end do
        end if

        ! ## Read in minimum stoking density
        read(GR_CON_SEA,*,iostat=ioerr)(MIN_SD(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Minimum stocking density reading error'
          stop
        else
          write(ECHO_NUM,*) 'Minimum stocking density for each animal species are: '
          do cur_ani=1,ANI_SPP_NUM
            write(ECHO_NUM,*) MIN_SD(cur_ani)
          end do
        end if

        ! ## Read in maximum intake per animal
        read(GR_CON_SEA,*,iostat=ioerr)(MAX_INT(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Maximum animal intake reading error'
          stop
        else
          write(ECHO_NUM,*) 'Maximum animal intake for each animal species are: '
          do cur_ani=1,ANI_SPP_NUM
            write(ECHO_NUM,*) MAX_INT(cur_ani)
          end do
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line

      end if !end SD_SW checking }}}

    ! ------------------------
    ! # Detachment rate {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (DT_SW .eq. 1) then

        ! ## If Detachment is on while there is neither fixed rate or stocking density function, stop the model, because detachment is based on grazing rate
        if(FR_SW .eq. 0 .and. SD_SW .eq. 0) then
          write(*,*) 'Error: FR_SW or SD_SW must be turned on to have detachment effects.'
          stop
        end if

        read(GR_CON_SEA,*,iostat=ioerr)(DET_RATE(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Detachment rate reading error'
          stop
        else
          write(ECHO_NUM,*) 'Detachment rate for each animal species are: '
          do cur_ani=1,ANI_SPP_NUM
            write(ECHO_NUM,*) DET_RATE(cur_ani)
          end do
        end if
      else
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end DT_SW cheking }}}

    ! ------------------------
    ! # Soil compactness {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (SC_SW .eq. 1) then

        ! ## Stocking density function rate should be turned on to have soil compactness effects, becase soil compactness changes depends on stocking density
        if (SD_SW .eq. 0) then
          write(*,*) 'SD_SW must be turned on to have calculate soil compactness.'
          stop
        end if

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(SC_VAR_A(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Soil compactness variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Soil compactness variables A for each animal species are: '
          write(ECHO_NUM,*) SC_VAR_A
        end if

        ! ## Read in variable B
        read(GR_CON_SEA,*,iostat=ioerr)(SC_VAR_B(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Soil compactness variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Soil compactness variables B for each animal species are: '
          write(ECHO_NUM,*) SC_VAR_B
        end if

        ! ## Read in free soil compactness
        read(GR_CON_SEA,*,iostat=ioerr) SC_FREE
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Soil compactness variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Soil compactness when stocking density is 0 is: '
          write(ECHO_NUM,*) SC_FREE
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end SC_SW cheking }}}

    ! ------------------------
    ! # Mortality rate {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (MR_SW .eq. 1) then

        ! ## Allocate variables used in mortality effect calculation for each plant species. Do this only on day one.
        if(.not. allocated(MR_VAR_A))allocate(MR_VAR_A(PLA_SPP_NUM))
        if(.not. allocated(MR_VAR_B))allocate(MR_VAR_B(PLA_SPP_NUM))

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(MR_VAR_A(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Mortality effect variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Mortality effect variables A for each plant species are: '
          write(ECHO_NUM,*) MR_VAR_A
        end if

        ! ## Read in variable B
        read(GR_CON_SEA,*,iostat=ioerr)(MR_VAR_B(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Mortality effect variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Mortality effect variables B for each plant species are: '
          write(ECHO_NUM,*) MR_VAR_B
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end MR_SW cheking }}}

    ! ------------------------
    ! # Respiration rate {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (RS_SW .eq. 1) then

        ! ## Allocate variables
        if(.not. allocated(RES_RATE))allocate(RES_RATE(PLA_SPP_NUM))

        ! ## Read in respiration rate for each plant species each season
        read(GR_CON_SEA,*,iostat=ioerr)(RES_RATE(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Respiration rate reading error'
          stop
        else
          write(ECHO_NUM,*) 'Respiration rate for each plant species are: '
          write(ECHO_NUM,*) RES_RATE
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end RS_SW cheking }}}

    ! ------------------------
    ! # Nitrogen return {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      ! ## Any Nitrogen return switch would turn this on. Either from urine or feces or both.
      if (any(NR_SW(:) .eq. 1)) then

      ! ## Fixed rate and stocking density should be turned on.
        if(FR_SW .eq. 0 .and. SD_SW .eq. 0) then
          write(*,*) 'Error: FR_SW or SD_SW should be turned on to have N return.'
          stop
        end if

      ! ## Read in nitrogen return rate
        read(GR_CON_SEA,*,iostat=ioerr)(N_RET_RATE(cur_ani), cur_ani=1,ANI_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Nitrogen return rate reading error'
          stop
        else
          write(ECHO_NUM,*) 'Nitrogen return rate for each plant species are: '
          do cur_ani=1,ANI_SPP_NUM
            write(ECHO_NUM,*) N_RET_RATE(cur_ani)
          end do
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end NR_SW cheking }}}

    ! ------------------------
    ! # Soil compactness effects on plant growth rate {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (SC_EF_SW .eq. 1) then

        ! ## Allocate variables
        if(.not. allocated(SC_EF_VAR_A))allocate(SC_EF_VAR_A(PLA_SPP_NUM))
        if(.not. allocated(SC_EF_VAR_B))allocate(SC_EF_VAR_B(PLA_SPP_NUM))

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(SC_EF_VAR_A(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Soil compactness - plant growth effect variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Soil compactness - plant growth effect variables A for each plant species are: '
          write(ECHO_NUM,*) SC_EF_VAR_A
        end if

        ! ## Read in variable B
        read(GR_CON_SEA,*,iostat=ioerr)(SC_EF_VAR_B(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Soil compactness - plant growth effect variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Soil compactness - plant growth effect variables B for each plant species are: '
          write(ECHO_NUM,*) SC_EF_VAR_B
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end SC_EF_SW cheking }}}

    ! ------------------------
    ! # Effects from LAI on plant photosysthesis {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (LA_EF_SW(1) .eq. 1) then

        ! ## Allocate variables
        if(.not. allocated(LA_PS_VAR_A))allocate(LA_PS_VAR_A(PLA_SPP_NUM))
        if(.not. allocated(LA_PS_VAR_B))allocate(LA_PS_VAR_B(PLA_SPP_NUM))

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(LA_PS_VAR_A(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'LAI-photosysthesis effect variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'LAI-photosysthesis effect variables A for each plant species are: '
          write(ECHO_NUM,*) LA_PS_VAR_A
        end if

        ! ## Read in variable B
        read(GR_CON_SEA,*,iostat=ioerr)(LA_PS_VAR_B(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'LAI-photosysthesis effect variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'LAI-photosysthesis effect variables B for each plant species are: '
          write(ECHO_NUM,*) LA_PS_VAR_B
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end LA_EF_SW(1) cheking }}}

    ! ------------------------
    ! # Effects from LAI on plant rainfall interception {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (LA_EF_SW(2) .eq. 1) then

        ! ## Allocate variables
        if(.not. allocated(LA_RI_VAR_A))allocate(LA_RI_VAR_A(PLA_SPP_NUM))

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(LA_RI_VAR_A(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'LAI-rainfall interception effect variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'LAI-rainfall interception effect variables A for each plant species are: '
          write(ECHO_NUM,*) LA_RI_VAR_A
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end LA_EF_SW(2) cheking }}}

    ! ------------------------
    ! # Effects from LAI on plant transpiration {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (LA_EF_SW(4) .eq. 1) then

        ! ## Allocate variables
        if(.not. allocated(LA_TP_VAR_A))allocate(LA_TP_VAR_A(PLA_SPP_NUM))

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(LA_TP_VAR_A(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'LAI-transpiration effect variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'LAI-transpiration effect variables A for each plant species are: '
          write(ECHO_NUM,*) LA_TP_VAR_A
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end LA_EF_SW(4) cheking }}}
 
    ! ------------------------
    ! # N uptake {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (AN_EF_SW(1) .eq. 1) then

        ! ## Allocate variables 
        if(.not. allocated(AN_NU_VAR_A))allocate(AN_NU_VAR_A(PLA_SPP_NUM))
        if(.not. allocated(AN_NU_VAR_B))allocate(AN_NU_VAR_B(PLA_SPP_NUM))

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(AN_NU_VAR_A(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'N uptake variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'N uptake variables A for each plant species are: '
          write(ECHO_NUM,*) AN_NU_VAR_A
        end if

        ! ## Read in variable B
        read(GR_CON_SEA,*,iostat=ioerr)(AN_NU_VAR_B(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'N uptake variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'N uptake variables B for each plant species are: '
          write(ECHO_NUM,*) AN_NU_VAR_B
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end AN_EF_SW(1) cheking }}}

    ! ------------------------
    ! # C conversion {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (AN_EF_SW(2) .eq. 1) then

        ! ## Allocate variables 
        if(.not. allocated(AN_CC_VAR_A))allocate(AN_CC_VAR_A(PLA_SPP_NUM))
        if(.not. allocated(AN_CC_VAR_B))allocate(AN_CC_VAR_B(PLA_SPP_NUM))

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(AN_CC_VAR_A(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'C conversion variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'C conversion variables A for each plant species are: '
          write(ECHO_NUM,*) AN_CC_VAR_A
        end if

        ! ## Read in variable B
        read(GR_CON_SEA,*,iostat=ioerr)(AN_CC_VAR_B(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'C conversion variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'C conversion variables B for each plant species are: '
          write(ECHO_NUM,*) AN_CC_VAR_B
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end AN_EF_SW(2) cheking

      ! Read in potential conversion rate. This variable should be read regardless of the switch
      if(.not. allocated(POT_CC))allocate(POT_CC(PLA_SPP_NUM))

      read(GR_CON_SEA,*,iostat=ioerr)(POT_CC(cur_pla), cur_pla=1,PLA_SPP_NUM)

      if (ioerr .ne. 0 ) then 
        write(*,*) 'Potential C conversion rate reading error'
        stop
      else
        write(ECHO_NUM,*) 'Potential C conversion rate is: '
        write(ECHO_NUM,*) POT_CC
      end if
      ! }}}

    ! ------------------------
    ! # Root to shoot ratio {{{
    ! ------------------------
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)
      read(GR_CON_SEA,*)

      if (AN_EF_SW(3) .eq. 1) then

        ! ## Allocate variables 
        if(.not. allocated(AN_RT_VAR_A))allocate(AN_RT_VAR_A(PLA_SPP_NUM))
        if(.not. allocated(AN_RT_VAR_B))allocate(AN_RT_VAR_B(PLA_SPP_NUM))

        ! ## Read in variable A
        read(GR_CON_SEA,*,iostat=ioerr)(AN_RT_VAR_A(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Root to shoot ratio variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Root to shoot ratio variables A for each plant species are: '
          write(ECHO_NUM,*) AN_RT_VAR_A
        end if

        ! ## Read in variable B
        read(GR_CON_SEA,*,iostat=ioerr)(AN_RT_VAR_B(cur_pla), cur_pla=1,PLA_SPP_NUM)
        if (ioerr .ne. 0 ) then 
          write(*,*) 'Root to shoot ratio variables reading error'
          stop
        else
          write(ECHO_NUM,*) 'Root to shoot ratio variables B for each plant species are: '
          write(ECHO_NUM,*) AN_RT_VAR_B
        end if

      else
        read(GR_CON_SEA,*) ! Skip the line
        read(GR_CON_SEA,*) ! Skip the line

      end if ! end AN_EF_SW(3) cheking }}}

end subroutine
