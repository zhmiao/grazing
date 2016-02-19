subroutine grazing_process_opt
  
  use parameter_var
  use structure
  implicit none

! ------------------------
! # Basic settings
! ------------------------

  integer :: i, j

  ! ## These three variables are for function parser using
  ! real, dimension(:), allocatable :: test_result                   ! Testing result, will be real program input
  ! real, dimension(:), allocatable :: input_array                   ! Use to store input variables, different each time
  ! real(rn), dimension(:), allocatable :: func_out                  ! Use to store function output, in rn real type

  ! write(ECHO_NUM,*) ' '
  ! write(ECHO_NUM,*) 'Year: ', year
  ! write(ECHO_NUM,*) 'Day: ', day


! ------------------------
! # Season check
! ------------------------


  ! ## Seasonal difference switch check
  if (SEA_SW .eq. 1) then


    ! ## User check points switch check, when it is off, set check point 1 to 1, cp_2 to growth days, and cp_3 to end of the year
    if (SPCP_SW .eq. 0) then
      SEA_CP(1)=1
      SEA_CP(2)=sum(CELL(:,:)%GROW_DAYS)/(MAX_Y_DIM*MAX_X_DIM)
      SEA_CP(3)=365
    end if

    ! ## on day one, set season one, and set seasonal change switch to 1 and seasonal temp var to 1
    if (day .eq. 1) then
      write(SEASON,'(A4,I1)')'sea_',1
      SEA_CH_SW=1
      SEA_TEMP=1
    end if

    ! ## Set actual season directory
    do i=1,SEA_NUM
      if (day .gt. SEA_CP(i) .and. day .le. SEA_CP(i+1)) then 
        write(SEASON,'(A4,I1)')'sea_',i
        ! ### When season has changed, seasonal change switch is on, and tem var is increased, else, the switch is off
        if(i .gt. SEA_TEMP) then
          SEA_CH_SW=1
          SEA_TEMP=i
        else
          SEA_CH_SW=0
        end if
      end if
    end do

  else

    ! ## When there is no seasonal difference, season is always 1, and seasonal change switch is 1 on day one.
    write(SEASON,'(A4,I1)')'sea_',1
    if (day .eq. 1) then
      SEA_CH_SW=1
    else
      SEA_CH_SW=0
    end if

  end if


! ------------------------
! # Grazing cycle check
! ------------------------
  ! ## Check rotational management switch
  if (MAN_SW(1) .eq. 1) then                                                                  ! Check whether there is rotational grazing
    ! ### On day one, set grazing switch on
    if (day .eq. 1) then
      GR_SW=1
    end if
    ! ### On normal days, set grazing switch by mod() function
    do i=1,RO_NUM
      if (day .gt. RO_CP(i) .and. day .le. RO_CP(i+1)) then                                   ! Rotation period check
        GR_SW=mod(i,2)
      end if
    end do
  else
    ! ## If rotational switch is off, graizing switch is always on
    GR_SW=1
  end if

  ! ## Set cell level grazing switches according to global level grazing switch
  CELL(:,:)%GR_SW=GR_SW

! write(*,*) GR_SW
! write(*,*) SEASON

! ------------------------
! # Start grazing
! ------------------------

  ! ## Check whether there should be grazing. (Global graizng switch)
  if (GR_SW .eq. 1) then                                                                      ! Check global grazing switch

  ! ------------------------
  ! ### Set up cell available and unavailale plant biomass
  ! ------------------------

    ! #### Spatial dimension loop
    do y_dim=1,MAX_Y_DIM
      do x_dim=1,MAX_X_DIM

        ! #### Available and Unavailable biomass for each plant species 
        do cur_pla=1,PLA_SPP_NUM

          ! ##### Unavailable biomass is unavailable rate times carrying capacity
          CELL(y_dim,x_dim)%UAV_BIO_SPP(cur_pla)=CELL(y_dim,x_dim)%SPP_K(cur_pla)*UAV_RATE(cur_pla)

          ! ##### Available biomass is total biomass minus unavailable biomass
          CELL(y_dim,x_dim)%AV_BIO_SPP(cur_pla)=CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)&
                                                  -CELL(y_dim,x_dim)%UAV_BIO_SPP(cur_pla)

          ! ##### Storage variables
          CELL(y_dim,x_dim)%AV_BIO_SPP_P(cur_pla)=CELL(y_dim,x_dim)%AV_BIO_SPP(cur_pla)

        end do

        ! #### Biomass for all plant species, available, unavailable, and total
        CELL(y_dim,x_dim)%UAV_BIOMASS=sum(CELL(y_dim,x_dim)%UAV_BIO_SPP(:))
        CELL(y_dim,x_dim)%AV_BIOMASS=sum(CELL(y_dim,x_dim)%AV_BIO_SPP(:))
        CELL(y_dim,x_dim)%TOT_BIOMASS=sum(CELL(y_dim,x_dim)%TOT_BIO_SPP(:))

        ! #### Storage variables
        CELL(y_dim,x_dim)%AV_BIOMASS_P=CELL(y_dim,x_dim)%AV_BIOMASS


      ! #### End dimension loop
      end do
    end do

    ! #### Global biomass
    UAV_BIOMASS=sum(CELL(:,:)%UAV_BIOMASS)
    AV_BIOMASS=sum(CELL(:,:)%AV_BIOMASS)
    TOT_BIOMASS=AV_BIOMASS+UAV_BIOMASS

    ! #### Storage vaiables
    AV_BIOMASS_P=AV_BIOMASS


  ! ------------------------
  ! ### When season has chaned
  ! ------------------------

    if (SEA_CH_SW .eq.1) then                                                               ! Check if season is changed

      write(ECHO_NUM,*) ' '
      write(ECHO_NUM,*) 'Season changed! '
      write(ECHO_NUM,*) 'Year: ', year
      write(ECHO_NUM,*) 'Day: ', day
      write(ECHO_NUM,*) 'Season: ', SEASON

      ! ## General grazing configuration initializations. [graz_conf_sea_*.gr]
      call graz_conf_read

      ! ## diet & site selection initialization. [site_conf_sea_*.gr, diet_conf_sea_*.gr]
      call sel_con_read

    end if ! end for SEA_CH_SW

! ================================================  NOW, FOR REAL CALCULATION  ================================================

  ! ------------------------
  ! ## Available biomass modifications
  ! ------------------------

    ! ### if MAX_GR_AMT is less then AV_BIOMASS, then available biomass is the maximum amount of grazing, if not, nothing changed
    if(MAN_SW(2) .eq. 1 .and. MAX_GR_AMT .lt. AV_BIOMASS) then
      AV_BIOMASS=MAX_GR_AMT
    end if

    ! ### If minimum management grazing amount is greater than available biomass, 
    if(MAN_SW(3) .eq. 1 .and. MIN_GR_AMT .gt. AV_BIOMASS) then
      AV_BIOMASS=0
    end if


    ! ### Cell level modifications
    do y_dim=1,MAX_Y_DIM
      do x_dim=1,MAX_X_DIM

        ! ### Cell level total plant available biomass modification
        if (AV_BIOMASS_P .le. 0) then
          CELL(y_dim,x_dim)%AV_BIOMASS=0
        else
          CELL(y_dim,x_dim)%AV_BIOMASS=AV_BIOMASS*(CELL(y_dim,x_dim)%AV_BIOMASS_P/AV_BIOMASS_P)
        end if

        ! ### Available biomass for each plant species modifications
        do cur_pla=1,PLA_SPP_NUM
          if (CELL(y_dim,x_dim)%AV_BIOMASS .le. 0) then
            CELL(y_dim,x_dim)%AV_BIO_SPP(cur_pla)=0
          else
            CELL(y_dim,x_dim)%AV_BIO_SPP(cur_pla)=CELL(y_dim,x_dim)%AV_BIOMASS&
                                                 *(CELL(y_dim,x_dim)%AV_BIO_SPP_P(cur_pla)/CELL(y_dim,x_dim)%AV_BIOMASS_P)
          end if
        end do

      end do
    end do


    ! ### Available biomass for each animal is total available biomass times competition factors
    do cur_ani=1,ANI_SPP_NUM
      ANI_AV_BIO(cur_ani)=AV_BIOMASS*ANI_COM_FAC(cur_ani)
    end do

  ! ------------------------
  ! ## Fixed rate
  ! ------------------------
    if (FR_SW .eq. 1) then

      ! ### Calculate total demand using fixed rate for each animal
      do cur_ani=1,ANI_SPP_NUM
        TOT_DMD(cur_ani)=ANI_AV_BIO(cur_ani)*FIX_GR_R(cur_ani)
      end do

    end if ! end FR_SW checking

  ! ------------------------
  ! ## Stocking rate function
  ! ------------------------
    if (SD_SW .eq. 1) then

      ! ### Loop for animal species
      do cur_ani=1,ANI_SPP_NUM

        ! ### Calculate stocking density for each animal species
        SPP_SD(cur_ani)=(ANI_AV_BIO(cur_ani)/(TOT_K-UAV_BIOMASS))*(MAX_SD(cur_ani)-MIN_SD(cur_ani))+MIN_SD(cur_ani)

        ! ### Calculate total number of animal
        ANI_NUM_SPP(cur_ani)=SPP_SD(cur_ani)*CELLAREA*MAX_Y_DIM*MAX_X_DIM

        ! ### Calculate total demand for each animal species
        TOT_DMD(cur_ani)=ANI_NUM_SPP(cur_ani)*MAX_INT(cur_ani)

      end do

    end if ! end SD_SW checking

  ! ------------------------
  ! ## Total demand modification with minimum management grazing amount
  ! ------------------------

    ! ### When total demand is less than minimum management grazing amount
    if (AV_BIOMASS .gt. sum(TOT_DMD(:)) .and. sum(TOT_DMD(:)) .lt. MIN_GR_AMT .and. MIN_GR_AMT .lt. AV_BIOMASS) then
      do cur_ani=1,ANI_SPP_NUM

        ! #### In case available biomass is zero
        if (sum(TOT_DMD(:)) .le. 0) then
          TOT_DMD(cur_ani)=0
        else
          TOT_DMD(cur_ani)=MIN_GR_AMT*(TOT_DMD(cur_ani)/sum(TOT_DMD(:)))
        end if

      end do
    end if

  ! ------------------------
  ! ## Selective grazing
  ! ------------------------

  ! ! ### Clear class available biomass in each class everyday
  !   SITE_PREF(:)%SPP_AV_BIO(:,:)=0

  ! ### Loop for animal species
    do cur_ani=1,ANI_SPP_NUM

    ! ### Clear class available biomass in each class everyday
      SITE_PREF(cur_ani)%SPP_AV_BIO(:,:)=0

      ! ### Loop for preference classes
      do cur_cla=1,SITE_PREF(cur_ani)%SS_CLA_NUM

        ! ### Different biomass calculation
        ! #### First, calculate total available plant biomass for each site selection preference class
        do y_dim=1,MAX_Y_DIM
          do x_dim=1,MAX_X_DIM

            ! ##### Check which class current cell is for each animal species
            if(CELL(y_dim,x_dim)%SS_PR_CLA(cur_ani) .eq. cur_cla) then

              ! @# Total available plant species biomass for each animal species is total cell available plant biomass time competition factor
              do cur_pla=1,PLA_SPP_NUM
                SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla)=SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla)&
                                                      +CELL(y_dim,x_dim)%AV_BIO_SPP(cur_pla)*ANI_COM_FAC(cur_ani)
              end do

            end if    ! End current cell preference class check

          end do ! end looping x dimension
        end do ! end looping y dimension

        ! #### Second, total availabe biomass in each site preferece class for each animal species
        SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla)=sum(SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,:))

        ! #### When total available biomass is not zero, do the regular diet selection
        if (SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla) .gt. 0) then

          ! ##### When there is plant species preference diet selecion
          if (DS_SW(1) .eq. 1) then

            ! @# Next, demand fraction
            ! @## Initialize temporary variables
            SUB_FRAC=0
            TEMP_BIO=0

            ! @# Plant species loop
            do cur_pla=1,PLA_SPP_NUM

              ! @## Prefered class
              if (any(cur_pla .eq. P_INDEX(:,cur_ani))) then
                SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)=1-exp(-3.65*(SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla)&
                                                                              /SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla)))
                SUB_FRAC=SUB_FRAC+SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)
              end if

              ! @## Less prefered class (here only calculate aggregated biomass)
              if (any(cur_pla .eq. L_INDEX(:,cur_ani))) then
                TEMP_BIO=TEMP_BIO+SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla)
              end if

              ! @## Unprefered class
              if (any(cur_pla .eq. U_INDEX(:,cur_ani))) then
                SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)=0.031917*exp(2.89*(SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla)&
                                                                               /SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla)))
                SUB_FRAC=SUB_FRAC+SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)
              end if

            end do ! end looping for plant species

            ! @# Acutal preference fraction for less preffered plant species
            do cur_pla=1,PLA_SPP_NUM
              if (any(cur_pla .eq. L_INDEX(:,cur_ani))) then
                SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)=(1-SUB_FRAC)*(SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla)&
                                                                                /TEMP_BIO)
              end if
            end do

          else 

            ! ##### If there is no plant species preference, diet fraction is available plant species over total plant species
            do cur_pla=1,PLA_SPP_NUM
                SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)=SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla)&
                                                                      /SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla)
            end do

          end if ! end checking plant species diet preference

        ! #### In case total biomass is zero, all diet fraction is zero
        else

          do cur_pla=1,PLA_SPP_NUM
              SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)=0
          end do

        end if    ! End checking total biomass values

  ! ------------------------------
  ! ## Calculate acutal forage amount
  ! ------------------------------

        ! ### For class number one, set class demand equal to total demand
        if (cur_cla .eq. 1) then
          SITE_PREF(cur_ani)%D_DMD(cur_cla)=TOT_DMD(cur_ani)
        end if

        ! ### If class total available forage is larger than class demand, animal forage amount will be demand times diet preference fractions
        if (SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla) .ge. SITE_PREF(cur_ani)%D_DMD(cur_cla)) then

          do cur_pla=1,PLA_SPP_NUM
            SITE_PREF(cur_ani)%SPP_FORAGE(cur_cla,cur_pla)=SITE_PREF(cur_ani)%D_DMD(cur_cla)&
                                                                     *SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)
          end do ! end looping for plant species

        else

          ! ### when class total available forage is not larger than class demand, animal class forage amount is available biomass times diet preference fraction
          do cur_pla=1,PLA_SPP_NUM
            SITE_PREF(cur_ani)%SPP_FORAGE(cur_cla,cur_pla)=SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla)&
                                                                     *SITE_PREF(cur_ani)%DIET_FRAC(cur_cla,cur_pla)
          end do ! end looping for plant species

          ! ### When class total available gorage is not larger than class demand, and it is not the final class, unsatisfied demand would be pased to the next class demand
          if (cur_cla .lt. SITE_PREF(cur_ani)%SS_CLA_NUM) then
            SITE_PREF(cur_ani)%D_DMD(cur_cla+1)=SITE_PREF(cur_ani)%D_DMD(cur_cla)-SITE_PREF(cur_ani)%TOT_AV_BIO(cur_cla)
          end if

        end if ! end checking whether class total available forage is larger than class demand


      end do ! end looping for preference class

    end do ! End looping animal spcies

  end if  ! end if of checking global grazing switch


  ! write(*,*) ''
  ! write(*,*) 'opt done'
end subroutine grazing_process_opt
