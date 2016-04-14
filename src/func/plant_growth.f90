subroutine plant_growth

  use parameter_var
  use structure

  implicit none
  
  ! # Loop according to spatial demensions
  do y_dim=1,MAX_Y_DIM
    do x_dim=1,MAX_X_DIM

      ! # Loop according to plant species
      do cur_pla=1,PLA_SPP_NUM

        if (day .le. 121+CELL(y_dim,x_dim)%GROW_DAYS(cur_pla) .and. day .ge. 121) then

          ! if (year .eq. 1998 .and. day .eq. 121) then
          !   CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla) = 0.001
          ! end if
 
          ! ## In growing seasons {{{
          CELL(y_dim,x_dim)%G_R(cur_pla)          = CELL(y_dim,x_dim)%R_MAX_CO(cur_pla)&
                                                    *R_MAX(cur_pla)*(1+0.01*(CELL(y_dim,x_dim)%SPP_RDP(cur_pla)))

          CELL(y_dim, x_dim)%SPP_K(cur_pla)       = CELL(y_dim,x_dim)%SPP_K_CO(cur_pla)*K_CO(cur_pla)*CELL(y_dim, x_dim)%DAY_RAIN*CELLAREA

          ! if (day .eq. 121) then
          !   ! write(*,*) K_CO(cur_pla)
          !   ! write(*,*) CELL(y_dim,x_dim)%SPP_K(cur_pla)
          !   write(*,*) '-------'
          !   write(*,*) CELL(y_dim,x_dim)%G_R(cur_pla)
          !   write(*,*) '++++++'
          ! end if

          if (CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla) .le. CELL(y_dim,x_dim)%SPP_K(cur_pla)) then

            ! cell(y_dim, x_dim)%del_bio_spp(cur_pla) = (cell(y_dim,x_dim)%g_r(cur_pla)*(cell(y_dim, x_dim)%tot_bio_spp(cur_pla)&
            !                                           +cell(y_dim,x_dim)%spp_k(cur_pla))&
            !                                           *(1-(cell(y_dim, x_dim)%tot_bio_spp(cur_pla)&
            !                                           +cell(y_dim, x_dim)%spp_k(cur_pla))&
            !                                           /(2*cell(y_dim, x_dim)%spp_k(cur_pla)))) 

            cell(y_dim, x_dim)%del_bio_spp(cur_pla) = (cell(y_dim,x_dim)%g_r(cur_pla)*(cell(y_dim, x_dim)%tot_bio_spp(cur_pla)&
                                                      +cell(y_dim,x_dim)%spp_k(cur_pla)*0.118)&
                                                      *(1-(cell(y_dim, x_dim)%tot_bio_spp(cur_pla)&
                                                      +cell(y_dim, x_dim)%spp_k(cur_pla)*0.118)&
                                                      /(1.118*cell(y_dim, x_dim)%spp_k(cur_pla)))) 

            ! if (year .eq. 1998) then
            !   write(*,*) '+++++'
            !   write(*,*) CELL(y_dim,x_dim)%DEL_BIO_SPP(cur_pla)
            !   write(*,*) '-----'
            ! end if

            CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla) = CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla) + CELL(y_dim,x_dim)%DEL_BIO_SPP(cur_pla)

            CELL(y_dim, x_dim)%SPP_K_LY(cur_pla)    = CELL(y_dim,x_dim)%SPP_K(cur_pla) 

          else

            ! ## Not in growing season {{{
            CELL(y_dim,x_dim)%D_R(cur_pla)=CELL(y_dim,x_dim)%DECREASE_R_CO(cur_pla)*DECREASE_R(cur_pla)*CELLAREA/(MAX_Y_DIM*MAX_X_DIM)

            if (CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)-CELL(y_dim,x_dim)%D_R(cur_pla) .ge. 0) then 

              ! ### If biomass minus decrease rate is greater than zero
              CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)-CELL(y_dim,x_dim)%D_R(cur_pla)

            else 

              ! ### If biomass is minus decrease rate is smaller or equal to zero
              CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=0

            end if 

          end if

          ! }}}

        else

          ! ## Not in growing season {{{
          CELL(y_dim,x_dim)%D_R(cur_pla)=CELL(y_dim,x_dim)%DECREASE_R_CO(cur_pla)*DECREASE_R(cur_pla)*CELLAREA/(MAX_Y_DIM*MAX_X_DIM)

          if (CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)-CELL(y_dim,x_dim)%D_R(cur_pla) .ge. 0) then 

            ! ### If biomass minus decrease rate is greater than zero
            CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)-CELL(y_dim,x_dim)%D_R(cur_pla)
            ! CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)

          else 

            ! ### If biomass is minus decrease rate is smaller or equal to zero
            CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=0

          end if 

        end if ! end checking plant growing season }}}

      end do ! end looping plant species

      CELL(y_dim,x_dim)%TOT_K=sum(CELL(y_dim,x_dim)%SPP_K(:))


    end do ! end looping x
  end do ! end looping y

  TOT_K=sum(CELL(:,:)%TOT_K)

  ! write(*,*) ' '
  ! write(*,*) 'plant growth done'
end subroutine plant_growth


