subroutine plant_growth

  use parameter_var
  use structure

  implicit none
  

  ! # Loop according to spatial demensions
  do y_dim=1,MAX_Y_DIM
    do x_dim=1,MAX_X_DIM

      ! # Loop according to plant species
      do cur_pla=1,PLA_SPP_NUM

				if (day .le. CELL(y_dim,x_dim)%GROW_DAYS(cur_pla)) then

          ! ## In growing seasons
          ! CELL(y_dim, x_dim)%MAX_DAILY_GROWTH     = -1.79+2.11*CELL(y_dim, x_dim)%DAILY_RAIN

					CELL(y_dim, x_dim)%SPP_K(cur_pla)       = K_CO(cur_pla)*CELL(y_dim, x_dim)%DAY_RAIN ! ***** add function and species different phi

          CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla) = CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)&
                                                    +(R_MAX(cur_pla)*(CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)&
                                                    +CELL(y_dim,x_dim)%SPP_K(cur_pla))&
                                                    *(1-(CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)&
                                                    +CELL(y_dim, x_dim)%SPP_K(cur_pla))&
                                                    /(2*CELL(y_dim, x_dim)%SPP_K(cur_pla)))) 

        else

          ! ## Not in growing season
          if (CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)-DECREASE_R(cur_pla) .ge. 0) then 

            ! ### If biomass minus decrease rate is greater than zero
            CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)-DECREASE_R(cur_pla)

          else 

            ! ### If biomass is minus decrease rate is smaller or equal to zero
            CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla)=0

          end if 

        end if ! end checking plant growing season

				CELL(y_dim,x_dim)%SPP_N_CON(cur_pla)=0.00103

      end do ! end looping plant species

    end do ! end looping x
  end do ! end looping y

  ! write(*,*) ' '
  ! write(*,*) 'plant growth done'
end subroutine plant_growth


