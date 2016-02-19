subroutine graz_eff_func
  
  use parameter_var
  use structure
  implicit none
 
	! A temporary variable used for coefficient calculation
	real :: gx 

	do y_dim=1, MAX_Y_DIM
		do x_dim=1, MAX_X_DIM
			do cur_pla=1, PLA_SPP_NUM

				! # First, GROW_DAYS modifications (Only do this at the end of the year)
       	if (day .eq. 365) then
	      	gx = 2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1&
				      		  	+ (2*2.7183-2)/((2.7183-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL-1)**2))-1&
  				      		 	- 1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1

	      	CELL(y_dim,x_dim)%GROW_DAYS_CO(cur_pla)=0.4/(1+EXP(-gx*1.5))+0.8
       	end if

				! # Second, Carrying capacity modifications.
      	gx = 2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1&
				      		  	+ (2*2.7183-2)/((2.7183-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL-1)**2))-1&
				      		  	- 1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1

      	CELL(y_dim,x_dim)%SPP_K_CO(cur_pla)=3/(2+EXP(-gx))

				! # Third, R_MAX modifications.
				gx = - 1/(CELL(y_dim,x_dim)%SPP_RES(cur_pla)+1)+1&
											- 1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1&
				      		  	+ (2*2.7183-2)/((2.7183-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL-1)**2))-1&
											- 1/(CELL(y_dim,x_dim)%SPP_PS(cur_pla)+1)+1&
											+ 1/(CELL(y_dim,x_dim)%SPP_RI(cur_pla)+1)-1&
											+ 1/(CELL(y_dim,x_dim)%POT_EVP+1)-1&
											- 1/(CELL(y_dim,x_dim)%SPP_TRP(cur_pla)+1)+1&
											- 1/(CELL(y_dim,x_dim)%SPP_NU(cur_pla)+1)+1&
											- 1/(CELL(y_dim,x_dim)%SPP_CC(cur_pla)+1)+1

      	CELL(y_dim,x_dim)%R_MAX_CO(cur_pla)=2/(1+EXP(-gx))

      ! CELL(y_dim,x_dim)%SPP_GRO(cur_pla)=CELL(y_dim,x_dim)%SPP_GRO(cur_pla)*(1+SC_EF_VAR_A(cur_pla)&
      !                                             *CELL(y_dim,x_dim)%SOIL_DCOM/0.15)!&
                                                  ! -SC_EF_VAR_B(cur_ani)


		  	! # Fourth, DECREASE_R modifications
		  	gx = 2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1&
										  - 1/(CELL(y_dim,x_dim)%SPP_RES(cur_pla)+1)+1&
											+ 1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)-1&
											- 1/(CELL(y_dim,x_dim)%LIT_POOL+1)+1&
											+ 1/(CELL(y_dim,x_dim)%SPP_PS(cur_pla)+1)-1&
											- 1/(CELL(y_dim,x_dim)%SPP_RI(cur_pla)+1)+1&
											- 1/(CELL(y_dim,x_dim)%POT_EVP+1)+1&
											- 1/(CELL(y_dim,x_dim)%SPP_TRP(cur_pla)+1)+1&
											+ 1/(CELL(y_dim,x_dim)%SPP_NU(cur_pla)+1)-1&
											+ 1/(CELL(y_dim,x_dim)%SPP_CC(cur_pla)+1)-1

      	CELL(y_dim,x_dim)%DECREASE_R_CO(cur_pla)=2/(1+EXP(-gx))

			end do
    end do
  end do

end subroutine
