subroutine graz_eff_func
  
  use parameter_var
  use structure
  implicit none
 
	! A temporary variable used for coefficient calculation
	real :: gx 

	! If there should be grazing effects
	if (IF_EFF_SW .eq. 1) then

  	do y_dim=1, MAX_Y_DIM
  		do x_dim=1, MAX_X_DIM
  			do cur_pla=1, PLA_SPP_NUM
  
					if (IF_EFF_GD .eq. 1) then
  			  	! # First, GROW_DAYS modifications (Only do this at the end of the year)
           	if (day .eq. 365) then
  	        	gx = 2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1&
  			  	      		  	+ (2*EXP(1.)-2)/((EXP(1.)-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL-1)**2))-1&
    		  		      		 	- 1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1
  
  	        	CELL(y_dim,x_dim)%GROW_DAYS_CO(cur_pla)=0.4/(1+EXP(-gx*1.5))+0.8

							! write(*,*) gx
							! write(*,*) CELL(y_dim,x_dim)%SOIL_COM
							! write(*,*) 2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1
							! write(*,*) CELL(y_dim,x_dim)%LIT_POOL
							! write(*,*) (2*EXP(1.)-2)/((EXP(1.)-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL-1)**2))-1
							! write(*,*) CELL(y_dim,x_dim)%AN_POOL
							! write(*,*) CELL(y_dim,x_dim)%LIT_N
							! write(*,*) -1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1
							write(*,*) CELL(y_dim,x_dim)%GROW_DAYS_CO(cur_pla)

           	end if
				  end if
  
					if (IF_EFF_K .eq. 1) then
  				  ! # Second, Carrying capacity modifications.
        	  gx = 2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1&
  				        		  	+ (2*EXP(1.)-2)/((EXP(1.)-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL-1)**2))-1&
  				        		  	- 1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1
  
        	  CELL(y_dim,x_dim)%SPP_K_CO(cur_pla)=3/(2+EXP(-0.7*gx))

							! write(*,*) gx
							! write(*,*) CELL(y_dim,x_dim)%SPP_K_CO(cur_pla)

					end if
  
					if (IF_EFF_R .eq. 1) then
  				  ! # Third, R_MAX modifications.
  				  gx = - 1/(CELL(y_dim,x_dim)%SPP_RES(cur_pla)+1)+1&
  				  							- 1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1&
  				        		  	+ (2*EXP(1.)-2)/((EXP(1.)-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL-1)**2))-1&
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

							! write(*,*) gx
							! write(*,*) CELL(y_dim,x_dim)%SPP_RES(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%AN_POOL
							! write(*,*) CELL(y_dim,x_dim)%LIT_N
							! write(*,*) CELL(y_dim,x_dim)%LIT_POOL
							! write(*,*) CELL(y_dim,x_dim)%SPP_PS(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%SPP_RI(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%POT_EVP
							! write(*,*) CELL(y_dim,x_dim)%SPP_TRP(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%SPP_NU(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%SPP_CC(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%R_MAX_CO(cur_pla)

					end if
  
					if (IF_EFF_DR .eq. 1) then
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
    
          	CELL(y_dim,x_dim)%DECREASE_R_CO(cur_pla)=2/(1+EXP(-0.5*gx))
    
          ! CELL(y_dim,x_dim)%SPP_MOR(cur_pla)=CELL(y_dim,x_dim)%SPP_MOR(cur_pla)&
          !                               *MR_VAR_A(cur_pla)*sum(CELL(y_dim,x_dim)%SPP_GRAZED(:,cur_pla))&
          !                               +MR_VAR_B(cur_pla)*sum(CELL(y_dim,x_dim)%SPP_GRAZED(:,cur_pla))&
          !                               *CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)/CELLAREA

							! write(*,*) gx
							! write(*,*) CELL(y_dim,x_dim)%SOIL_COM
							! write(*,*) CELL(y_dim,x_dim)%SPP_RES(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%AN_POOL
							! write(*,*) CELL(y_dim,x_dim)%LIT_N
							! write(*,*) CELL(y_dim,x_dim)%LIT_POOL
							! write(*,*) CELL(y_dim,x_dim)%SPP_PS(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%SPP_RI(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%POT_EVP
							! write(*,*) CELL(y_dim,x_dim)%SPP_TRP(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%SPP_NU(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%SPP_CC(cur_pla)
							! write(*,*) CELL(y_dim,x_dim)%DECREASE_R_CO(cur_pla)

  				end if

  			end do
      end do
    end do

	end if ! End checking If Effects switch.
end subroutine
