subroutine grazing

  use parameter_var
  use structure
  implicit none


! # Global grazing switch is on 
if (GR_SW .eq. 1) then

  ! Initialize daily total grazed
  TOT_GRAZED_DA = 0

  ! # Spatial dimension loop
  do y_dim=1,MAX_Y_DIM
  do x_dim=1,MAX_X_DIM

  ! ------------------------
  ! ## Calculate actual grazed amount in each cell {{{
  ! ------------------------

    ! ### First, loop for animal species
    do cur_ani=1,ANI_SPP_NUM

      ! ### Second, loop for site preference classes
      do cur_cla=1,SITE_PREF(cur_ani)%SS_CLA_NUM
  
        ! ### Then, loop for plant species
        do cur_pla=1,PLA_SPP_NUM
  
        ! #### LAI selection. Calculate actual diet fraction which is affected by LAI if the switch is on.
          if(DS_SW(2) .eq. 1) then
            LAI_FAC=(CELL(y_dim,x_dim)%SPP_LAI(cur_pla)**3)/((CELL(y_dim,x_dim)%SPP_LAI(cur_pla)**3)+1)
          else
            LAI_FAC=1
          end if ! end LAI switch cheking
  
          ! ##### Check which class the cell is in for current animal species and calculate cell level grazed amount for each plant species, animal species and site preference class levelfor each plant species, animal species and site preference class level.
          if(CELL(y_dim,x_dim)%SS_PR_CLA(cur_ani) .eq. cur_cla) then
  
            ! @# Dominator check
            if (SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla) .gt. 0) then 
              CELL(y_dim,x_dim)%SPP_GRAZED(cur_ani,cur_pla)=SITE_PREF(cur_ani)%SPP_FORAGE(cur_cla,cur_pla)&
                                 *CELL(y_dim,x_dim)%AV_BIO_SPP(cur_pla)/SITE_PREF(cur_ani)%SPP_AV_BIO(cur_cla,cur_pla)&
                                 *LAI_FAC
            else
              CELL(y_dim,x_dim)%SPP_GRAZED(cur_ani,cur_pla)=0
            end if    ! End dominator check
  
            ! ##### Nitrogen concentration modification when the switch is on
            if(DS_SW(3) .eq. 1 .and. CELL(y_dim,x_dim)%SPP_N_CON(cur_pla) .le. 0.0104) then
              CELL(y_dim,x_dim)%SPP_GRAZED(cur_ani,cur_pla)=CELL(y_dim,x_dim)%SPP_GRAZED(cur_ani,cur_pla)*0.964&
                                          *CELL(y_dim,x_dim)%SPP_N_CON(cur_pla)+1.5
            end if ! end nitrogen concentration checking
  
          end if ! end cell preference cheking
  
          ! Calculate daily total grazed amount
          TOT_GRAZED_DA=TOT_GRAZED_DA+CELL(y_dim,x_dim)%SPP_GRAZED(cur_ani,cur_pla)
  
        end do ! end plant looping
      end do ! end site preference class checking
    end do ! end animal species checking }}}
  
  end do ! End spatial looping
  end do ! End spatial looping
  
  ! write(*,*) TOT_GRAZED_DA

  ! # Spatial dimension loop
  do y_dim=1,MAX_Y_DIM
  do x_dim=1,MAX_X_DIM

  ! ------------------------
  ! # First stage {{{
  ! ------------------------
  
    do cur_pla=1,PLA_SPP_NUM
  
    ! ## Defoliation effect {{{
      if (DF_SW .eq. 1) then
  
        CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)=CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)&
                                                -sum(CELL(y_dim,x_dim)%SPP_GRAZED(:,cur_pla))
  
        if (CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla) .le. 0) then
          CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)=0
        end if
  
      end if !}}}
  
    ! ## Detachment {{{
      if (DT_SW .eq. 1) then
        do cur_ani=1,ANI_SPP_NUM
          CELL(y_dim,x_dim)%SPP_DETACH(cur_ani,cur_pla)=CELL(y_dim,x_dim)%SPP_GRAZED(cur_ani,cur_pla)&
                                                        *DET_RATE(cur_ani)
        end do
  
        CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)=CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)&
                                                -sum(CELL(y_dim,x_dim)%SPP_DETACH(:,cur_pla))
  
        if (CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla) .le. 0) then
          CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)=0
        end if
  
      end if !}}}
  
    ! ## Mortality rate {{{
      ! if (MR_SW .eq. 1) then
      !   CELL(y_dim,x_dim)%SPP_MOR(cur_pla)=CELL(y_dim,x_dim)%SPP_MOR(cur_pla)&
      !                                 *MR_VAR_A(cur_pla)*sum(CELL(y_dim,x_dim)%SPP_GRAZED(:,cur_pla))&
      !                                 +MR_VAR_B(cur_pla)*sum(CELL(y_dim,x_dim)%SPP_GRAZED(:,cur_pla))&
      !                                 *CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)/CELLAREA
      ! end if !}}}
  
    end do ! Stop looping for plant species
  
    ! ## Soil compactness {{{
    if (SC_SW .eq. 1) then

      CELL(y_dim,x_dim)%SOIL_DCOM=0
  
      do cur_ani=1,ANI_SPP_NUM
  
        ! SD is modified by cell level grazed amount and total grazed amount
        if (TOT_GRAZED_DA .gt. 0) then
          CELL(y_dim,x_dim)%SOIL_DCOM = CELL(y_dim,x_dim)%SOIL_DCOM+SC_VAR_A(cur_ani)&
                                      *((MAX_SD(cur_ani)*(sum(CELL(y_dim,x_dim)%SPP_GRAZED(cur_ani,:))/TOT_GRAZED_DA)&
                                      +MIN_SD(cur_ani)))
        else
          CELL(y_dim,x_dim)%SOIL_DCOM=0
        end if
  
      end do
  
    else
      CELL(y_dim,x_dim)%SOIL_DCOM=0
    end if
  
    CELL(y_dim,x_dim)%SOIL_COM=SC_FREE+CELL(y_dim,x_dim)%SOIL_DCOM ! }}}
    ! write(*,*) CELL(y_dim,x_dim)%SOIL_DCOM
  
  ! }}}

  end do  ! end looping for x
  end do ! end looping for y 

end if ! end checking GR_SW 

! # Spatial dimension loop
do y_dim=1,MAX_Y_DIM
do x_dim=1,MAX_X_DIM
! ------------------------
! # Second stage {{{
! ------------------------

  ! ### Initialize variables
  CELL(y_dim,x_dim)%AN_POOL_D=0
  CELL(y_dim,x_dim)%LIT_N_D=0

  do cur_pla=1,PLA_SPP_NUM

  ! ## LAI {{{

    if (LA_SW .eq. 1) then
      ! CELL(y_dim,x_dim)%SPP_LAI(cur_pla)=0.01*(128-62*(1-exp(-0.0102*(CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)/CELLAREA))))&
      !                                        *CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)/CELLAREA

      ! CELL(y_dim,x_dim)%SPP_LAI(cur_pla)=0.01*(90-82*(1-exp(-0.0102*(CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)/CELLAREA))))&
      !                                        *CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)/CELLAREA

      ! CELL(y_dim,x_dim)%SPP_LAI(cur_pla) = F_LA(cur_pla) * CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla) * ASLA(cur_pla) / CELLAREA

      CELL(y_dim,x_dim)%SPP_LAI(cur_pla) = F_LA(cur_pla) * CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla) * ASLA(cur_pla) / CELLAREA

      if (CELL(y_dim,x_dim)%SPP_LAI(cur_pla) .le. 0) then
        CELL(y_dim,x_dim)%SPP_LAI(cur_pla)=0
      end if
      ! write(*,*) CELL(y_dim,x_dim)%SPP_LAI(cur_pla)

      ! write(*,*) CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)

    else
      CELL(y_dim,x_dim)%SPP_LAI(cur_pla)=0 ! Default values
    end if !}}}

  ! ## Respiration rate {{{
    if (RS_SW .eq. 1) then

      CELL(y_dim,x_dim)%SPP_RES(cur_pla)=RES_RATE(cur_pla)*CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)

      if (CELL(y_dim,x_dim)%SPP_RES(cur_pla) .le. 0) then
        CELL(y_dim,x_dim)%SPP_RES(cur_pla)=0
      end if

    else
      CELL(y_dim,x_dim)%SPP_RES(cur_pla)=0
    end if !}}}

  ! ## Available N {{{
    if (any(NR_SW(:) .eq. 1)) then

      do cur_ani=1,ANI_SPP_NUM

        N_RET=N_RET_RATE(cur_ani)*CELL(y_dim,x_dim)%SPP_GRAZED(cur_ani,cur_pla)&
                                                   *CELL(y_dim,x_dim)%SPP_N_CON(cur_pla)

        ! ### 1) From urine {{{
        if (NR_SW(1) .eq. 1) then
  
          CELL(y_dim,x_dim)%AN_POOL_D=CELL(y_dim,x_dim)%AN_POOL_D+N_RET&
                                         *(0.8-(0.1*(SPP_CN(cur_pla)-12)/13))&
                                         *1000 ! convert to mgN

          if (CELL(y_dim,x_dim)%AN_POOL_D .le. 0) then
            CELL(y_dim,x_dim)%AN_POOL_D=0
          end if
  
        else
  
          CELL(y_dim,x_dim)%AN_POOL_D=0
  
        end if !}}}
  
        ! ### 2) From feces {{{
        if (NR_SW(2) .eq. 1) then

          CELL(y_dim,x_dim)%LIT_N_D=CELL(y_dim,x_dim)%LIT_N_D+N_RET&
                                         *(1-(0.8-(0.1*(SPP_CN(cur_pla)-12)/13)))&
                                         *1000 ! convert to mgN
  
          if (CELL(y_dim,x_dim)%LIT_N_D .le. 0) then
            CELL(y_dim,x_dim)%LIT_N_D=0
          end if
  
        else
  
          CELL(y_dim,x_dim)%LIT_N_D=0
  
        end if !}}}

      end do ! end looping of animal species

    end if ! end NR_SW check }}}

  end do ! end looping for plant species

  ! ## Litter pool {{{
  if (LP_SW .eq. 1 .and. DT_SW .eq. 1) then
    CELL(y_dim,x_dim)%LIT_POOL_D=0

    CELL(y_dim,x_dim)%LIT_POOL_D=CELL(y_dim,x_dim)%LIT_POOL_D+sum(CELL(y_dim,x_dim)%SPP_DETACH(:,:))

    if (CELL(y_dim,x_dim)%LIT_POOL_D .le. 0) then
      CELL(y_dim,x_dim)%LIT_POOL_D=0
    end if

  else
    CELL(y_dim,x_dim)%LIT_POOL_D=0
  end if !}}}

  !}}}

! ------------------------
! # Third stage {{{
! ------------------------

  do cur_pla=1,PLA_SPP_NUM

  ! ## From Soil compactness {{{

    if (SC_EF_SW .eq. 1 .and. SC_SW .eq. 1) then
      CELL(y_dim,x_dim)%SPP_RDP(cur_pla) = (-1.)*SC_EF_VAR_A(cur_pla)*CELL(y_dim,x_dim)%SOIL_DCOM-SC_EF_VAR_B(cur_pla)
    end if ! }}}

  ! ## From LAI {{{

    ! ### 1) Change in photosysthesis {{{
    if (LA_EF_SW(1) .eq. 1) then
      CELL(y_dim,x_dim)%SPP_PS(cur_pla)=LA_PS_VAR_A(cur_pla)*CELL(y_dim,x_dim)%SPP_LAI(cur_pla)&
                                          /(LA_PS_VAR_B(cur_pla)+CELL(y_dim,x_dim)%SPP_LAI(cur_pla))

      if (CELL(y_dim,x_dim)%SPP_PS(cur_pla) .le. 0) then
        CELL(y_dim,x_dim)%SPP_PS(cur_pla)=0
      end if

    else
      CELL(y_dim,x_dim)%SPP_PS(cur_pla)=0
    end if !}}}

    ! ### 2) Change in rainfall interception {{{
    if (LA_EF_SW(2) .eq. 1) then
      CELL(y_dim,x_dim)%SPP_RI(cur_pla)=1-exp(-LA_RI_VAR_A(cur_pla)*CELL(y_dim,x_dim)%SPP_LAI(cur_pla))

      if (CELL(y_dim,x_dim)%SPP_RI(cur_pla) .le. 0) then
        CELL(y_dim,x_dim)%SPP_RI(cur_pla)=0
      end if

    else
      CELL(y_dim,x_dim)%SPP_RI(cur_pla)=0
    end if !}}}

  end do ! end looping for plant species

    ! ### 3) Chang in evapotranspiration {{{
    ! #### 3.0) Potential evapotranspiration {{{
    if(LA_EF_SW(3) .eq. 1 .or. LA_EF_SW(4) .eq. 1) then
      do cur_sea=1,SEA_NUM
        CELL(y_dim,x_dim)%POT_ETP=0.128*(SOLA_RAD(cur_sea)*(1-(0.23*(1-exp(-0.0000029&
                                        *(CELL(y_dim,x_dim)%TOT_BIOMASS+CELL(y_dim,x_dim)%LIT_POOL_D))&
                                        +SOIL_ALB(cur_sea)*0.24))/58.3))*((5304/(AVG_TEMP(cur_sea)**2))*exp(21.25-(5304/AVG_TEMP(cur_sea)))&
                                        /((5304/(AVG_TEMP(cur_sea)**2))*exp(21.25-(5304/AVG_TEMP(cur_sea)))+0.68))

        if (CELL(y_dim,x_dim)%POT_ETP.le. 0) then
          CELL(y_dim,x_dim)%POT_ETP=0
        end if

      end do
    end if !}}}

    ! #### 3.1) Potential soil evaporation {{{
    if(LA_EF_SW(3) .eq. 1) then
      CELL(y_dim,x_dim)%POT_EVP=CELL(y_dim,x_dim)%POT_ETP*exp(-0.4*sum(CELL(y_dim,x_dim)%SPP_LAI(:)/PLA_SPP_NUM))

      if (CELL(y_dim,x_dim)%POT_EVP .le. 0) then
        CELL(y_dim,x_dim)%POT_EVP=0
      end if

    else
      CELL(y_dim,x_dim)%POT_EVP=0
    end if ! }}}

  do cur_pla=1,PLA_SPP_NUM

    ! #### 3.2) Potential transpiration {{{
    if(LA_EF_SW(4) .eq. 1) then

      if(CELL(y_dim,x_dim)%SPP_LAI(cur_pla) .le. 3) then

        CELL(y_dim,x_dim)%SPP_TRP(cur_pla)=CELL(y_dim,x_dim)%POT_ETP*CELL(y_dim,x_dim)%SPP_LAI(cur_pla)/3

        if (CELL(y_dim,x_dim)%SPP_TRP(cur_pla) .le. 0) then
          CELL(y_dim,x_dim)%SPP_TRP(cur_pla)=0
        end if

      else

        CELL(y_dim,x_dim)%SPP_TRP(cur_pla)=CELL(y_dim,x_dim)%POT_ETP * CELL(y_dim,x_dim)%SPP_LAI(cur_pla) / sum(CELL(y_dim,x_dim)%SPP_LAI(:))

      end if

    else

      CELL(y_dim,x_dim)%SPP_TRP(cur_pla)=0

    end if !}}}

    ! 4) Change in available light for other species
    ! if(LA_EF_SW(5) .eq. 1) then
    ! end if

  !}}}

  !}}}

  ! ## Form available N {{{
    ! ### 1) Change in N uptake {{{
    if (AN_EF_SW(1) .eq. 1) then
      CELL(y_dim,x_dim)%SPP_NU_D(cur_pla)=AN_NU_VAR_A(cur_pla)*CELL(y_dim,x_dim)%AN_POOL_D&
                                        /(AN_NU_VAR_B(cur_pla)+CELL(y_dim,x_dim)%AN_POOL_D)

      if (CELL(y_dim,x_dim)%SPP_NU_D(cur_pla) .le. 0) then
        CELL(y_dim,x_dim)%SPP_NU_D(cur_pla)=0
      end if

    else
      CELL(y_dim,x_dim)%SPP_NU_D(cur_pla)=0
    end if !}}}

    ! #### 1.1) Change in Plant N concentration
    ! write(*,*) CELL(y_dim,x_dim)%SPP_N_CON(cur_pla)
    CELL(y_dim,x_dim)%SPP_N_CON(cur_pla)=SPP_N_CON(cur_pla)+0.5*CELL(y_dim,x_dim)%SPP_NU_D(cur_pla)

    ! ### 2) Change in carbon conversion {{{
    if (AN_EF_SW(2) .eq. 1) then
      CELL(y_dim,x_dim)%SPP_CC(cur_pla)=AN_CC_VAR_A(cur_pla)*CELL(y_dim,x_dim)%SPP_N_CON(cur_pla)&
                                        +AN_CC_VAR_B(cur_pla)

      if (CELL(y_dim,x_dim)%SPP_CC(cur_pla) .le. 0 .or. CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla) .le. 0) then
        CELL(y_dim,x_dim)%SPP_CC(cur_pla)=0
      end if

    else
      ! CELL(y_dim,x_dim)%SPP_CC(cur_pla)=POT_CC(cur_pla)
      CELL(y_dim,x_dim)%SPP_CC(cur_pla)=0
    end if !}}}

    ! ### 3) Change in root to shoot ratio {{{
    ! if (AN_EF_SW(3) .eq. 1) then
    !   CELL(y_dim,x_dim)%SPP_RT(cur_pla)=AN_RT_VAR_A(cur_pla)/(1+AN_RT_VAR_B(cur_pla)*(CELL(y_dim,x_dim)%SPP_CC(cur_pla)&
    !                                                               /POT_CC(cur_pla)))
    !
    !   if (CELL(y_dim,x_dim)%SPP_RT(cur_pla) .le. 0) then
    !     CELL(y_dim,x_dim)%SPP_RT(cur_pla)=0
    !   end if
    !
    ! else
    !   CELL(y_dim,x_dim)%SPP_RT(cur_pla)=0
    ! end if !}}}

    ! ### 4) root to shoot ratio with C and N relationship
    !}}}

  end do ! end looping plant species }}}

end do  ! end looping for x
end do ! end looping for y 


end subroutine grazing
