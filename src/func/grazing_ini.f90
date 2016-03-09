subroutine grazing_ini
  
  use parameter_var
  use structure

  implicit none
  integer i     ! used for looping
  ! used for getcwd() function to get current working directory
  integer getcwd
  integer temp

  ! =========================
  ! #  Start reading switch files [switches.gr]
  ! =========================

  write(*,*) ' '
  write(*,*) '============================'
  write(*,*) 'Now start reading switches. '
  write(*,*) '0 is off, 1 is on. '
  write(*,*) '============================'
  write(*,*) ' '

  ! ## Read in switch file
  open(SWITCH, file=CWD(1:len_trim(CWD))//SWITCH_FILE, action='READ', iostat=ioerr)
  if (ioerr .ne. 0) then

    write(*,*) 'Process switch file not opened'
    stop

  else

    write(*,*) ' '
    write(*,*) '=============================='
    write(*,*) 'For grazing process switches: '
    write(*,*) '=============================='
    write(*,*) ' '

    ! =========================
    ! ## Begin with difference switches
    ! =========================
    read(SWITCH,*)
    read(SWITCH,*)
    read(SWITCH,*)

    ! ### Species difference switches
    read(SWITCH,*,iostat=ioerr) SPP_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) ' Animal species difference switch reading error'
      stop
    else
      write(*,*) 'Animal species differece switch is: '
      write(*,*) SPP_SW
      read(SWITCH,*)
    end if

    ! ### Species competition switch
    read(SWITCH,*,iostat=ioerr) COM_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) ' Animal competition switch reading error'
      stop
    else
      write(*,*) 'Animal competition switch is: '
      write(*,*) COM_SW
      read(SWITCH,*)
    end if

    ! ### Seasonal switches
    read(SWITCH,*,iostat=ioerr) SEA_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Seasonal diffrence switch reading error'
      stop
    else
      write(*,*) 'Seasonal difference switch is: '
      write(*,*) SEA_SW
      read(SWITCH,*)
    end if

    ! ### Management switches
    read(SWITCH,*,iostat=ioerr) SW_NUM
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Management difference switches reading error'
      stop
    else
      write(*,*) 'Number for management switches is: '
      write(*,*) SW_NUM
      allocate(MAN_SW(SW_NUM))
    end if
    read(SWITCH,*,iostat=ioerr) (MAN_SW(i), i=1,SW_NUM)
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Management diffrence switches reading error'
      stop
    else
      write(*,*) 'Management differece switches are: '
      write(*,*) MAN_SW
    end if

    ! =========================
    ! ## Grazing rate options
    ! =========================
    read(SWITCH,*)
    read(SWITCH,*)
    read(SWITCH,*)

    ! ### Fixed rate
    read(SWITCH,*,iostat=ioerr) FR_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Fixed rate switch reading error'
      stop
    else
      write(*,*) 'Fixed grazing rate switch is: '
      write(*,*) FR_SW
      read(SWITCH,*)
    end if

    ! ### Function of stocking density
    read(SWITCH,*,iostat=ioerr) SD_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Stocking density switch reading error'
      stop
    else
      write(*,*) 'Stocking density switch is: '
      write(*,*) SD_SW
      read(SWITCH,*)
    end if

    ! ### Unavailable biomass
    read(SWITCH,*,iostat=ioerr) UN_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Unavailable plant limitation switch reading error'
      stop
    else
      write(*,*) 'Unavailable plant biomass limitation switch is: '
      write(*,*) UN_SW
    end if

    ! =========================
    ! ## Selection options
    ! =========================
    read(SWITCH,*)
    read(SWITCH,*)
    read(SWITCH,*)

    ! ### Diet selection
    read(SWITCH,*,iostat=ioerr) SW_NUM
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Diet selection switches reading error'
      stop
    else
      write(*,*) 'Number for diet selection switches is: '
      write(*,*) SW_NUM
      allocate(DS_SW(SW_NUM))
      read(SWITCH,*,iostat=ioerr) (DS_SW(i), i=1,SW_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Diet selection switches reading error'
        stop
      else
        write(*,*) 'Diet selection switches are: '
        write(*,*) DS_SW
        read(SWITCH,*)
      end if
    end if

    ! ### Site selection
    read(SWITCH,*,iostat=ioerr) SW_NUM
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Site selection switches reading error'
      stop
    else
      write(*,*) 'Number for site selection switches is: '
      write(*,*) SW_NUM
      allocate(SS_SW(SW_NUM))
      read(SWITCH,*,iostat=ioerr) (SS_SW(i), i=1,SW_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Site selection switches reading error'
        stop
      else
        write(*,*) 'Site selection switches are: '
        write(*,*) SS_SW
      end if
    end if

    ! =========================
    ! ## First stage effects
    ! =========================

    write(*,*) ' '
    write(*,*) '=========================='
    write(*,*) 'For first stage switches: '
    write(*,*) '=========================='
    write(*,*) ' '

    read(SWITCH,*)
    read(SWITCH,*)
    read(SWITCH,*)

    ! ### Defoliation
    read(SWITCH,*,iostat=ioerr) DF_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Defoliation effect switch reading error'
      stop
    else
      write(*,*) 'Defoliation switch is: '
      write(*,*) DF_SW
      read(SWITCH,*)
    end if

    ! ### Detachment
    read(SWITCH,*,iostat=ioerr) DT_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Detachment switch reading error'
      stop
    else
      write(*,*) 'Detachment switch is: '
      write(*,*) DT_SW
      read(SWITCH,*)
    end if

    ! ### Mortality rate
    read(SWITCH,*,iostat=ioerr) MR_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Mortality rate effect switch reading error'
      stop
    else
      write(*,*) 'Mortality rate effect swith is: '
      write(*,*) MR_SW
      read(SWITCH,*)
    end if

    ! ### Soil compactness
    read(SWITCH,*,iostat=ioerr) SC_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Effects on soil compactness switch reading error'
      stop
    else
      write(*,*) 'Soil compactness effects swith is: '
      write(*,*) SC_SW
    end if

    ! =========================
    ! ## Second stage effects switches
    ! =========================

    write(*,*) ' '
    write(*,*) '==========================='
    write(*,*) 'For second stage switches: '
    write(*,*) '==========================='
    write(*,*) ' '

    read(SWITCH,*)
    read(SWITCH,*)
    read(SWITCH,*)

    ! ### LAI
    read(SWITCH,*,iostat=ioerr) LA_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'LAI effect switch reading error'
      stop
    else
      write(*,*) 'LAI effect effect swith is: '
      write(*,*) LA_SW
      read(SWITCH,*)
    end if

    ! ### Respiration
    read(SWITCH,*,iostat=ioerr) RS_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Respiration effect switch reading error'
      stop
    else
      write(*,*) 'Respiration effect swith is: '
      write(*,*) RS_SW
      read(SWITCH,*)
    end if

    ! ### Available N
    read(SWITCH,*,iostat=ioerr) SW_NUM
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Nitrogen return switches reading error'
      stop
    else
      write(*,*) 'Number for nitrogen return switches is: '
      write(*,*) SW_NUM
      allocate(NR_SW(SW_NUM))
      read(SWITCH,*,iostat=ioerr) (NR_SW(i), i=1,SW_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Nitrogen return switches reading error'
        stop
      else
        write(*,*) 'Nitrogen return switches are: '
        write(*,*) NR_SW
        read(SWITCH,*)
      end if
    end if

    ! ### Litter pool
    read(SWITCH,*,iostat=ioerr) LP_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Liiter pool effect switch reading error'
      stop
    else
      write(*,*) 'Litter pool effect swith is: '
      write(*,*) LP_SW
    end if


    ! =========================
    ! ## Third stage effects switches
    ! =========================

    write(*,*) ' '
    write(*,*) '=========================='
    write(*,*) 'For third stage switches: '
    write(*,*) '=========================='
    write(*,*) ' '

    read(SWITCH,*)
    read(SWITCH,*)
    read(SWITCH,*)

    ! ### Soil compactness effect on plant growth
    read(SWITCH,*,iostat=ioerr) SC_EF_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Soil compactness effect on plant growth switch reading error'
      stop
    else
      write(*,*) 'Soil compactness effect on plant growth swith is: '
      write(*,*) SC_EF_SW
      read(SWITCH,*)
    end if


    ! ### Effects from LAI
    read(SWITCH,*,iostat=ioerr) SW_NUM
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Switches for effects from LAI reading error'
      stop
    else
      write(*,*) 'Number of switches for effects from LAI is: '
      write(*,*) SW_NUM
      allocate(LA_EF_SW(SW_NUM))
      read(SWITCH,*,iostat=ioerr) (LA_EF_SW(i), i=1,SW_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Switches for effects from LAI reading error'
        stop
      else
        write(*,*) 'Switches for effects from LAI are: '
        write(*,*) LA_EF_SW
        read(SWITCH,*)
      end if
    end if

    ! ### Effects from available N
    read(SWITCH,*,iostat=ioerr) SW_NUM
    if (ioerr .ne. 0 ) then 
      write(*,*) 'Switches for effects from available N reading error'
      stop
    else
      write(*,*) 'Number of switches for effects from available N is: '
      write(*,*) SW_NUM
      allocate(AN_EF_SW(SW_NUM))
      read(SWITCH,*,iostat=ioerr) (AN_EF_SW(i), i=1,SW_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Switches for effects from available N reading error'
        stop
      else
        write(*,*) 'Switches for effects from available N are: '
        write(*,*) AN_EF_SW
      end if
    end if

    ! =========================
    ! ## If Effects
    ! =========================

    write(*,*) ' '
    write(*,*) '=================================================='
    write(*,*) 'If there should be grazing effects on plant growth'
    write(*,*) '=================================================='
    write(*,*) ' '

    read(SWITCH,*)
    read(SWITCH,*)
    read(SWITCH,*)

    ! ### If Effects
    read(SWITCH,*,iostat=ioerr) IF_EFF_SW
    if (ioerr .ne. 0 ) then 
      write(*,*) 'If Effects Switche Reading Error.'
      stop
    else
      write(*,*) 'If Effects Switche is: '
      write(*,*) IF_EFF_SW
    end if

    ! ### If Effects Grwothdays
    read(SWITCH,*,iostat=ioerr) IF_EFF_GD
    if (ioerr .ne. 0 ) then 
      write(*,*) 'If Effects Growthdays Switche Reading Error.'
      stop
    else
      write(*,*) 'If Effects Growthdays  Switche is: '
      write(*,*) IF_EFF_GD
    end if

    ! ### If Effects Carrying Capacity
    read(SWITCH,*,iostat=ioerr) IF_EFF_K
    if (ioerr .ne. 0 ) then 
      write(*,*) 'If Effects Carrying Capacity Switche Reading Error.'
      stop
    else
      write(*,*) 'If Effects Carrying Capacity Switche is: '
      write(*,*) IF_EFF_K
    end if

    ! ### If Effects R
    read(SWITCH,*,iostat=ioerr) IF_EFF_R
    if (ioerr .ne. 0 ) then 
      write(*,*) 'If Effects R Switche Reading Error.'
      stop
    else
      write(*,*) 'If Effects R Switche is: '
      write(*,*) IF_EFF_R
    end if

    ! ### If Effects DR
    read(SWITCH,*,iostat=ioerr) IF_EFF_DR
    if (ioerr .ne. 0 ) then 
      write(*,*) 'If Effects DR Switche Reading Error.'
      stop
    else
      write(*,*) 'If Effects DR Switche is: '
      write(*,*) IF_EFF_DR
      read(SWITCH,*)
    end if

  end if
  close(SWITCH)

  ! =========================
  ! # Read in all year config file [all_year_conf.gr]
  ! =========================

  open(AY_CON, FILE=CWD(1:len_trim(CWD))//AY_CON_F, ACTION='READ',IOSTAT=ioerr)
  if (ioerr .ne. 0) then
    write(*,*) 'All year config file not opened'
    stop
  else

    if (SEA_SW .eq. 1 .or. MAN_SW(1) .eq. 1 .or. UN_SW .eq. 1) then
      write(*,*) ' '
      write(*,*) '============================='
      write(*,*) 'For all year configurations: '
      write(*,*) '============================='
      write(*,*) ' '
    end if

    ! ## Seasonal differece configurations
    read(AY_CON,*)
    read(AY_CON,*)
    read(AY_CON,*)

    ! ### Check if seasonal difference switch is on
    if (SEA_SW .eq. 1) then

      ! #### Read season number and use it to set season directories
      read(AY_CON,*,iostat=ioerr) SEA_NUM
      if (ioerr .ne. 0) then
        write(*,*) 'Seasonal configuration reading error'
        stop
      else
        write(*,*) 'Seasonal difference exist, number of seasons is: '
        write(*,*) SEA_NUM

        ! ##### Set seasonal check points numbers
        allocate(SEA_CP(SEA_NUM+1))

        ! ##### Check whether use user input checks
        read(AY_CON,*,iostat=ioerr) SPCP_SW
        if (ioerr .ne. 0) then
          write(*,*) 'Seasonal configuration reading error'
          stop
        else

          ! #### Read in user specified check points switch
          write(*,*) 'Whether use user specified check points? 1=yes; 0=no'
          write(*,*) SPCP_SW
          if (SPCP_SW .eq. 1) then
            ! #### If the switch is on, read in user specified check points
            write(*,*) 'User specified seasonal check points'
            read(AY_CON,*,iostat=ioerr) (SEA_CP(i), i=1,SEA_NUM+1)
            if (ioerr .ne. 0) then
            write(*,*) 'Seasonal configuration reading error'
              stop
            else
              write(*,*) 'They are: '
              write(*,*) SEA_CP(:)
            end if
          else
            read(AY_CON,*) ! skip this line
          end if
        end if
      end if
      else
        ! #### When there is no seasonal difference, set season number to 1
        read(AY_CON,*)     ! skip line
        read(AY_CON,*)     ! skip line
        read(AY_CON,*)     ! skip line
        SEA_NUM=1
    end if

    ! ## Management configuration
    read(AY_CON,*)
    read(AY_CON,*)
    read(AY_CON,*)

    ! ### Rotational grazing period
    if (MAN_SW(1) .eq. 1) then
      ! #### Read in rotation period number
      read(AY_CON,*,iostat=ioerr) RO_NUM
      if (ioerr .ne. 0) then
        write(*,*) 'Rotational grazing configuration reading error'
        stop
      else
        write(*,*) 'Rotational grazing exist, number of rotation period is: '
        write(*,*) RO_NUM
        ! #### Set rotation check points number
        allocate(RO_CP(RO_NUM+1))
        read(AY_CON,*,iostat=ioerr) (RO_CP(i), i=1,RO_NUM+1)
          if (ioerr .ne. 0) then
            write(*,*) 'Rotational grazing configuration reading error'
            stop
          else
            write(*,*) 'Rotation period check points are: '
            write(*,*) RO_CP
          end if
      end if
    else
      read(AY_CON,*)     ! skip rotation period number line
      read(AY_CON,*)     ! skip check point line
    end if


    ! ## For unavailable biomass
    read(AY_CON,*)
    read(AY_CON,*)
    read(AY_CON,*)

    ! ### Read in the biomass unavaiable rate
    if (UN_SW .eq. 1) then
      read(AY_CON,*,iostat=ioerr)(UAV_RATE(i), i=1,PLA_SPP_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Unavailable biomass rate reading error'
        stop
      else
        write(*,*) 'Unavaliable biomass rate for each plant species are: '
        do i=1,PLA_SPP_NUM
          write(*,*) UAV_RATE(i)
        end do
      end if
    else
      read(AY_CON,*)        ! Skip this line
      UAV_RATE(:)=0
    end if

    ! ## For solar radiation
    read(AY_CON,*)
    read(AY_CON,*)
    read(AY_CON,*)

    ! ### Read in evapotranspiration variables
    if(LA_EF_SW(3) .eq. 1 .or. LA_EF_SW(4) .eq. 1) then

      ! #### Solar radiation
      allocate(SOLA_RAD(SEA_NUM))
      read(AY_CON,*,iostat=ioerr)(SOLA_RAD(i), i=1,SEA_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Solar radiation reading error'
        stop
      else
        write(*,*) 'Solar radiation for each season are: '
        do i=1,SEA_NUM
          write(*,*) SOLA_RAD(i)
        end do
      end if

      ! Soil albedo
      allocate(SOIL_ALB(SEA_NUM))
      read(AY_CON,*,iostat=ioerr)(SOIL_ALB(i), i=1,SEA_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Soil albedo reading error'
        stop
      else
        write(*,*) 'Soil albedo for each season are: '
        do i=1,SEA_NUM
          write(*,*) SOIL_ALB(i)
        end do
      end if

      ! Average temperature
      allocate(AVG_TEMP(SEA_NUM))
      read(AY_CON,*,iostat=ioerr)(AVG_TEMP(i), i=1,SEA_NUM)
      if (ioerr .ne. 0 ) then 
        write(*,*) 'Average temperature reading error'
        stop
      else
        write(*,*) 'Average temperature for each season are: '
        do i=1,SEA_NUM
          write(*,*) AVG_TEMP(i)
        end do
      end if

    else
      read(AY_CON,*)        ! Skip this line
      read(AY_CON,*)        ! Skip this line
      read(AY_CON,*)        ! Skip this line
    end if

  end if                    ! Total switch config file

  close(AY_CON)


  write(*,*) ''
  write(*,*) '=============================='
  write(*,*) ' Grazing initialization done!!'
  write(*,*) '=============================='
end subroutine grazing_ini
