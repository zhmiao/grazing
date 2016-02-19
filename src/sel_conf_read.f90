subroutine sel_con_read
  
  use parameter_var
  use structure
  implicit none

  integer :: i, j

    ! ------------------------
    ! # Diet selection configuration
    ! ------------------------

      ! ## First, diet selection based on plant species preference
      if (DS_SW(1) .eq. 1) then

        write(ECHO_NUM,*) 'Diet selection exist.'

        ! ### Allocate index arrays
        if(allocated(P_INDEX))deallocate(P_INDEX)
        if(allocated(L_INDEX))deallocate(L_INDEX)
        if(allocated(U_INDEX))deallocate(U_INDEX)
        allocate(P_INDEX(PLA_SPP_NUM, ANI_SPP_NUM))
        allocate(L_INDEX(PLA_SPP_NUM, ANI_SPP_NUM))
        allocate(U_INDEX(PLA_SPP_NUM, ANI_SPP_NUM))

        ! ### Open configuration file
        open(DS_CON, file=CWD(1:len_trim(CWD))//PRE_DIR_DS(1:len_trim(PRE_DIR_DS))//&
                  SEASON(1:len_trim(SEASON))//'.gr', action='read', iostat=ioerr)

        if (ioerr .ne. 0) then
          write(*,*) 'Diet selection configuration file reading error'
          stop
        end if

        ! ### Loop for animal species
        do cur_ani=1,ANI_SPP_NUM

          read(DS_CON,*)
          read(DS_CON,*)
          read(DS_CON,*)

          ! #### Read for prefered plant species for each animal
          read(DS_CON,*,iostat=ioerr) (P_INDEX(cur_pla,cur_ani),cur_pla=1,PLA_SPP_NUM)
          if (ioerr .ne. 0) then
            write(*,*) 'Prefered index reading error, species number', i
            stop
          else
            write(ECHO_NUM,*) 'Animal',cur_ani, 'Prefered plant species:'
            write(ECHO_NUM,*) P_INDEX(:,cur_ani)
          end if

          ! #### Read for less prefered plant spcies for each animal
          read(DS_CON,*,iostat=ioerr) (L_INDEX(cur_pla,cur_ani),cur_pla=1,PLA_SPP_NUM)
          if (ioerr .ne. 0) then
            write(*,*) 'Less prefered index reading error, species number', i
            stop
          else
            write(ECHO_NUM,*) 'Animal',cur_ani, 'Less prefered plant species:'
            write(ECHO_NUM,*) L_INDEX(:,cur_ani)
          end if

          ! #### Read for undesired plant species for each animal
          read(DS_CON,*,iostat=ioerr) (U_INDEX(cur_pla,cur_ani),cur_pla=1,PLA_SPP_NUM)
          if (ioerr .ne. 0) then
            write(*,*) 'Unprefered index reading error, species number', i
            stop
          else
            write(ECHO_NUM,*) 'Animal',cur_ani, 'Unprefered plant species:'
            write(ECHO_NUM,*) U_INDEX(:,cur_ani)
          end if

        end do ! end looping animal species

      end if ! end DS_SW(1) cheking

      close(DS_CON)

    ! ------------------------
    ! # Site selection configuration
    ! ------------------------

      ! ## Allocate preference type
      if(allocated(SITE_PREF)) deallocate(SITE_PREF)
      allocate(SITE_PREF(ANI_SPP_NUM))

      if (any(SS_SW(:) .eq. 1)) then

        ! ## Open configuration file
        open(SS_CON, file=CWD(1:len_trim(CWD))//PRE_DIR_SS(1:len_trim(PRE_DIR_SS))//&
                  SEASON(1:len_trim(SEASON))//'.gr', action='read', iostat=ioerr)

        if (ioerr .ne. 0) then
          write(*,*) 'Diet selection configuration file reading error'
          stop
        end if

        ! ## Loop for animal speices. Start site configuration file readings.
        do cur_ani=1,ANI_SPP_NUM

          ! Split lines
          read(SS_CON,*)
          read(SS_CON,*)
          read(SS_CON,*)

          ! ### Select by plant abundance
          if (SS_SW(1) .eq. 1) then

            ! #### Read abundance preference class number
            read(SS_CON,*,iostat=ioerr) SITE_PREF(cur_ani)%AB_CLA_NUM
            if (ioerr .ne. 0) then 
              write(*,*) 'Abundance selection class number reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'abundance selection class number:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%AB_CLA_NUM
            end if

            ! #### Allocate abundance check points with class number
            if (allocated(SITE_PREF(cur_ani)%AB_CP)) deallocate(SITE_PREF(cur_ani)%AB_CP)
            allocate(SITE_PREF(cur_ani)%AB_CP(SITE_PREF(cur_ani)%AB_CLA_NUM+1))

            ! #### Read class check points
            read(SS_CON,*,iostat=ioerr) (SITE_PREF(cur_ani)%AB_CP(j), j=1,SITE_PREF(cur_ani)%AB_CLA_NUM+1)
            if (ioerr .ne. 0) then
              write(*,*) 'Abundance selection check points reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'abundance selection check points are:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%AB_CP
            end if

          else
            read(SS_CON,*)
            read(SS_CON,*)
          end if    ! End SS_SW(1) switch check

          ! Split line
          read(SS_CON,*)

          ! ### Select by slope
          if (SS_SW(2) .eq. 1) then

            ! #### Read slop preference class number
            read(SS_CON,*,iostat=ioerr) SITE_PREF(cur_ani)%SL_CLA_NUM
            if (ioerr .ne. 0) then 
              write(*,*) 'Slope selection class number reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'slope selection class number:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%SL_CLA_NUM
            end if

            ! #### Allocate slop class check points with class number
            if (allocated(SITE_PREF(cur_ani)%SL_CP)) deallocate(SITE_PREF(cur_ani)%SL_CP)
            allocate(SITE_PREF(cur_ani)%SL_CP(SITE_PREF(cur_ani)%SL_CLA_NUM+1))

            ! #### Read slop class check points
            read(SS_CON,*,iostat=ioerr) (SITE_PREF(cur_ani)%SL_CP(j), j=1,SITE_PREF(cur_ani)%SL_CLA_NUM+1)
            if (ioerr .ne. 0) then
              write(*,*) 'Slope selection check points reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'slope selection check points are:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%SL_CP
            end if

          else
            read(SS_CON,*)
            read(SS_CON,*)

          end if    ! End check SS_SW(2) switch

          ! Split line
          read(SS_CON,*)

          ! ### Select by water resource
          if (SS_SW(3) .eq. 1) then

            ! #### Read water resource preference class number
            read(SS_CON,*,iostat=ioerr) SITE_PREF(cur_ani)%WA_CLA_NUM
            if (ioerr .ne. 0) then 
              write(*,*) 'Water resource selection class number reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'water resource selection class number:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%WA_CLA_NUM
            end if

            ! #### Allocate water resource class check points with class number
            if (allocated(SITE_PREF(cur_ani)%WA_CP)) deallocate(SITE_PREF(cur_ani)%WA_CP)
            allocate(SITE_PREF(cur_ani)%WA_CP(SITE_PREF(cur_ani)%WA_CLA_NUM+1))

            ! #### Read check points
            read(SS_CON,*,iostat=ioerr) (SITE_PREF(cur_ani)%WA_CP(j), j=1,SITE_PREF(cur_ani)%WA_CLA_NUM+1)
            if (ioerr .ne. 0) then
              write(*,*) 'Water resource selection check points reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'water resource selection check points are:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%WA_CP
            end if

          else
            read(SS_CON,*)
            read(SS_CON,*)

          end if    ! End checking SS_SW(3)

          ! Split line
          read(SS_CON,*)

          ! ### Select by snow cover
          if (SS_SW(4) .eq. 1) then

            ! #### Read in class number
            read(SS_CON,*,iostat=ioerr) SITE_PREF(cur_ani)%SN_CLA_NUM
            if (ioerr .ne. 0) then 
              write(*,*) 'Snow cover selection class number reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'snow cover selection class number:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%SN_CLA_NUM
            end if

            ! #### Allocate check points with class number
            if (allocated(SITE_PREF(cur_ani)%SN_CP)) deallocate(SITE_PREF(cur_ani)%SN_CP)
            allocate(SITE_PREF(cur_ani)%SN_CP(SITE_PREF(cur_ani)%SN_CLA_NUM+1))

            ! #### Read in check points
            read(SS_CON,*,iostat=ioerr) (SITE_PREF(cur_ani)%SN_CP(j), j=1,SITE_PREF(cur_ani)%SN_CLA_NUM+1)
            if (ioerr .ne. 0) then
              write(*,*) 'Snow cover selection check points reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'snow cover selection check points are:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%SN_CP
            end if
          else
            read(SS_CON,*)
            read(SS_CON,*)

          end if      ! End checking SS_SW(4)

          ! Split line
          read(SS_CON,*)

          ! ### Total site selection preference class
          if (any(SS_SW(:) .eq. 1)) then

            ! #### Read in general site preference class number
            read(SS_CON,*,iostat=ioerr) SITE_PREF(cur_ani)%SS_CLA_NUM
            if (ioerr .ne. 0) then 
              write(*,*) 'Total site selection class number reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'slope selection class number:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%SL_CLA_NUM
            end if

            ! #### Allocate variables with general site preference class number, including check points
            if (allocated(SITE_PREF(cur_ani)%SS_CP))      deallocate(SITE_PREF(cur_ani)%SS_CP)
            if (allocated(SITE_PREF(cur_ani)%SPP_AV_BIO)) deallocate(SITE_PREF(cur_ani)%SPP_AV_BIO)
            if (allocated(SITE_PREF(cur_ani)%TOT_AV_BIO)) deallocate(SITE_PREF(cur_ani)%TOT_AV_BIO)
            if (allocated(SITE_PREF(cur_ani)%DIET_FRAC))  deallocate(SITE_PREF(cur_ani)%DIET_FRAC)
            if (allocated(SITE_PREF(cur_ani)%D_DMD))      deallocate(SITE_PREF(cur_ani)%D_DMD)
            if (allocated(SITE_PREF(cur_ani)%SPP_FORAGE)) deallocate(SITE_PREF(cur_ani)%SPP_FORAGE)
            allocate(SITE_PREF(cur_ani)%SS_CP(SITE_PREF(cur_ani)%SS_CLA_NUM+1))
            allocate(SITE_PREF(cur_ani)%SPP_AV_BIO(SITE_PREF(cur_ani)%SS_CLA_NUM,PLA_SPP_NUM))
            allocate(SITE_PREF(cur_ani)%TOT_AV_BIO(SITE_PREF(cur_ani)%SS_CLA_NUM))
            allocate(SITE_PREF(cur_ani)%DIET_FRAC(SITE_PREF(cur_ani)%SS_CLA_NUM,PLA_SPP_NUM))
            allocate(SITE_PREF(cur_ani)%D_DMD(SITE_PREF(cur_ani)%SS_CLA_NUM))
            allocate(SITE_PREF(cur_ani)%SPP_FORAGE(SITE_PREF(cur_ani)%SS_CLA_NUM,PLA_SPP_NUM))

            ! #### Read in general site selection preference check points
            read(SS_CON,*,iostat=ioerr) (SITE_PREF(cur_ani)%SS_CP(j), j=1,SITE_PREF(cur_ani)%SS_CLA_NUM+1)
            if (ioerr .ne. 0) then
              write(*,*) 'Total site selection check points reading error.'
              stop
            else
              write(ECHO_NUM,*) 'Animal',cur_ani,'actual site selection check points are:'
              write(ECHO_NUM,*) SITE_PREF(cur_ani)%SS_CP
            end if
          else
            read(SS_CON,*)
            read(SS_CON,*)

          end if    ! End total site prefernce configuration

          ! ## Calculate actual preference class for each cell. (Still in the animal loop)

          ! ### Set initial preference score to zero
          CELL(:,:)%SS_PR_SCR=0

          ! ### Now loop for spatial dimension
          do y_dim=1,MAX_Y_DIM
            do x_dim=1,MAX_X_DIM

              ! ### Preference score calculation
              ! #### 1) Abundance (the higher, the better)
              if(SS_SW(1) .eq. 1) then

                ! ##### Abundance check point one
                if (CELL(y_dim,x_dim)%TOT_BIOMASS/CELLAREA .eq. SITE_PREF(cur_ani)%AB_CP(1)) then
                  CELL(y_dim,x_dim)%SS_PR_SCR=CELL(y_dim,x_dim)%SS_PR_SCR+(SITE_PREF(cur_ani)%AB_CLA_NUM)
                end if

                ! ##### Other check points
                do cur_cla=1,SITE_PREF(cur_ani)%AB_CLA_NUM
                  if (CELL(y_dim,x_dim)%TOT_BIOMASS/CELLAREA .gt. SITE_PREF(cur_ani)%AB_CP(cur_cla) .and. &
                             CELL(y_dim,x_dim)%TOT_BIOMASS/CELLAREA .le. SITE_PREF(cur_ani)%AB_CP(cur_cla+1)) then
                    CELL(y_dim,x_dim)%SS_PR_SCR=CELL(y_dim,x_dim)%SS_PR_SCR+(SITE_PREF(cur_ani)%AB_CLA_NUM-cur_cla+1)
                  end if
                end do

              end if    ! End abundance score calculation

              ! #### 2) Slope (Regular)
              if(SS_SW(2) .eq. 1) then

                ! ##### Check point one
                if (CELL(y_dim,x_dim)%SLOPE .eq. SITE_PREF(cur_ani)%SL_CP(1)) then
                  CELL(y_dim,x_dim)%SS_PR_SCR=CELL(y_dim,x_dim)%SS_PR_SCR+1
                end if

                ! ##### Other check points
                do cur_cla=1,SITE_PREF(cur_ani)%SL_CLA_NUM
                  if (CELL(y_dim,x_dim)%SLOPE .gt. SITE_PREF(cur_ani)%SL_CP(cur_cla) .and. &
                             CELL(y_dim,x_dim)%SLOPE .le. SITE_PREF(cur_ani)%SL_CP(cur_cla+1)) then
                    CELL(y_dim,x_dim)%SS_PR_SCR=CELL(y_dim,x_dim)%SS_PR_SCR+cur_cla
                  end if
                end do

              end if    ! End slope score calculation

              ! #### 3) Water resource
              if(SS_SW(3) .eq. 1) then

                ! ##### Check point one
                if (CELL(y_dim,x_dim)%WATER_DIST .eq. SITE_PREF(cur_ani)%WA_CP(1)) then
                  CELL(y_dim,x_dim)%SS_PR_SCR=CELL(y_dim,x_dim)%SS_PR_SCR+1
                end if

                ! ##### Other check points
                do cur_cla=1,SITE_PREF(cur_ani)%WA_CLA_NUM
                  if (CELL(y_dim,x_dim)%WATER_DIST .gt. SITE_PREF(cur_ani)%WA_CP(cur_cla) .and. &
                             CELL(y_dim,x_dim)%WATER_DIST .le. SITE_PREF(cur_ani)%WA_CP(cur_cla+1)) then
                    CELL(y_dim,x_dim)%SS_PR_SCR=CELL(y_dim,x_dim)%SS_PR_SCR+cur_cla
                  end if
                end do

              end if    ! End water resource score calculation

              ! #### 4) Snow cover
              if(SS_SW(4) .eq. 1) then

                ! ##### Check point one
                if (CELL(y_dim,x_dim)%SNOW_COV .eq. SITE_PREF(cur_ani)%SN_CP(1)) then
                  CELL(y_dim,x_dim)%SS_PR_SCR=CELL(y_dim,x_dim)%SS_PR_SCR+1
                end if

                ! ##### Other check points
                do cur_cla=1,SITE_PREF(cur_ani)%SN_CLA_NUM
                  if (CELL(y_dim,x_dim)%SNOW_COV .gt. SITE_PREF(cur_ani)%SN_CP(cur_cla) .and. &
                             CELL(y_dim,x_dim)%SNOW_COV .le. SITE_PREF(cur_ani)%SN_CP(cur_cla+1)) then
                    CELL(y_dim,x_dim)%SS_PR_SCR=CELL(y_dim,x_dim)%SS_PR_SCR+cur_cla
                  end if
                end do

              end if    ! End snow cover score calculation

              ! ####  Calculate actual preference class for each cell (the higher, the better)
              if (any(SS_SW(:) .eq. 1)) then

                ! ##### Check point one
                if (CELL(y_dim,x_dim)%SS_PR_SCR .eq. SITE_PREF(cur_ani)%SS_CP(1)) then
                  CELL(y_dim,x_dim)%SS_PR_CLA(cur_ani)=1
                end if

                ! ##### Other check points
                do cur_cla=1,SITE_PREF(cur_ani)%SS_CLA_NUM
                  if (CELL(y_dim,x_dim)%SS_PR_SCR .gt. SITE_PREF(cur_ani)%SS_CP(cur_cla) .and. &
                             CELL(y_dim,x_dim)%SS_PR_SCR .le. SITE_PREF(cur_ani)%SS_CP(cur_cla+1)) then
                    CELL(y_dim,x_dim)%SS_PR_CLA=cur_cla
                  end if
                end do

              end if    ! End actual preference class calculation

            end do ! end x dimension loop
          end do ! end y dimension loop

        end do   ! end looping for animal species

      close(SS_CON)

      else

        ! ## When there is no site selection, cell site preference class equals to one
        do y_dim=1,MAX_Y_DIM
          do x_dim=1,MAX_X_DIM
            CELL(y_dim,x_dim)%SS_PR_CLA(:)=1
          end do
        end do

        do cur_ani=1,ANI_SPP_NUM

          SITE_PREF(cur_ani)%SS_CLA_NUM=1

          if (allocated(SITE_PREF(cur_ani)%SPP_AV_BIO))  deallocate(SITE_PREF(cur_ani)%SPP_AV_BIO)
          if (allocated(SITE_PREF(cur_ani)%TOT_AV_BIO))  deallocate(SITE_PREF(cur_ani)%TOT_AV_BIO)
          if (allocated(SITE_PREF(cur_ani)%DIET_FRAC))   deallocate(SITE_PREF(cur_ani)%DIET_FRAC)
          if (allocated(SITE_PREF(cur_ani)%D_DMD))       deallocate(SITE_PREF(cur_ani)%D_DMD)
          if (allocated(SITE_PREF(cur_ani)%SPP_FORAGE))  deallocate(SITE_PREF(cur_ani)%SPP_FORAGE)
          allocate(SITE_PREF(cur_ani)%SPP_AV_BIO(SITE_PREF(cur_ani)%SS_CLA_NUM,PLA_SPP_NUM))
          allocate(SITE_PREF(cur_ani)%TOT_AV_BIO(SITE_PREF(cur_ani)%SS_CLA_NUM))
          allocate(SITE_PREF(cur_ani)%DIET_FRAC(SITE_PREF(cur_ani)%SS_CLA_NUM,PLA_SPP_NUM))
          allocate(SITE_PREF(cur_ani)%D_DMD(SITE_PREF(cur_ani)%SS_CLA_NUM))
          allocate(SITE_PREF(cur_ani)%SPP_FORAGE(SITE_PREF(cur_ani)%SS_CLA_NUM,PLA_SPP_NUM))

        end do ! end animal species looping

      end if ! end cheking SS_SW(:)

      ! Write cell site preference class
      ! if (day .eq. 1) then
      !   do cur_ani=1,ANI_SPP_NUM
      !     do y_dim=1,MAX_Y_DIM
      !       write(ECHO_NUM,*) (CELL(y_dim,x_dim)%SS_PR_CLA(cur_ani), x_dim=1,MAX_X_DIM)
      !     end do
      !   end do
      ! end if

end subroutine
