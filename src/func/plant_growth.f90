subroutine plant_growth

  use parameter_var
  use structure

  implicit none
  

  ! # Loop according to plant species
  do cur_pla=1,PLA_SPP_NUM

    ! # Loop according to spatial demensions
    do y_dim=1,MAX_Y_DIM
      do x_dim=1,MAX_X_DIM

        if (day .le. CELL(y_dim,x_dim)%GROW_DAYS(cur_pla)) then

          ! ## In growing seasons {{{
          CELL(y_dim,x_dim)%G_R(cur_pla)          = CELL(y_dim,x_dim)%R_MAX_CO(cur_pla)*R_MAX(cur_pla)

          CELL(y_dim, x_dim)%SPP_K(cur_pla)       = CELL(y_dim,x_dim)%SPP_K_CO(cur_pla)*K_CO(cur_pla)*CELL(y_dim, x_dim)%DAY_RAIN 

          CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla) = CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)&
                                                    +(CELL(y_dim,x_dim)%G_R(cur_pla)*(CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)&
                                                    +CELL(y_dim,x_dim)%SPP_K(cur_pla))&
                                                    *(1-(CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)&
                                                    +CELL(y_dim, x_dim)%SPP_K(cur_pla))&
                                                    /(2*CELL(y_dim, x_dim)%SPP_K(cur_pla)))) 

          ! }}}

        else

          ! ## Not in growing season {{{
          CELL(y_dim,x_dim)%D_R(cur_pla)=CELL(y_dim,x_dim)%DECREASE_R_CO(cur_pla)*DECREASE_R(cur_pla)

          if (CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)-CELL(y_dim,x_dim)%D_R(cur_pla) .ge. 0) then 

            ! ### If biomass minus decrease rate is greater than zero
            CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)-CELL(y_dim,x_dim)%D_R(cur_pla)

          else 

            ! ### If biomass is minus decrease rate is smaller or equal to zero
            CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=0

          end if 

        end if ! end checking plant growing season }}}

        ! CELL(y_dim,x_dim)%SPP_N_CON(cur_pla)=0.00103
        ! CELL(y_dim,x_dim)%SPP_CN(cur_pla)=25

      end do ! end looping x
    end do ! end looping y

  end do ! end looping plant species

  ! write(*,*) ' '
  ! write(*,*) 'plant growth done'
end subroutine plant_growth


