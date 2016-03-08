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
            gx = (2*EXP(1.)-2)/((EXP(1.)-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL_D-1)**2))-1&
                          -1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1
  
            ! This is for soil compactness effects
            if (SC_SW .eq. 1) gx=gx+2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1

            ! Coefficient calculation
            CELL(y_dim,x_dim)%GROW_DAYS_CO(cur_pla)=0.4/(1+EXP(-gx*1.5))+0.8

            end if
          end if
  
          if (IF_EFF_K .eq. 1) then
            ! # Second, Carrying capacity modifications.
            gx = (2*EXP(1.)-2)/((EXP(1.)-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL_D-1)**2))-1&
                          -1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1

            ! This is for soil compactness effects
            if (SC_SW .eq. 1) gx=gx+2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1

            ! Coefficient calculation
            CELL(y_dim,x_dim)%SPP_K_CO(cur_pla)=3/(2+EXP(-0.7*gx))

          end if
  
          if (IF_EFF_R .eq. 1) then
            ! # Third, R_MAX modifications.
            gx = -1/(CELL(y_dim,x_dim)%SPP_RES(cur_pla)+1)+1&
                          -1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)+1&
                          +(2*EXP(1.)-2)/((EXP(1.)-2)+EXP((2*CELL(y_dim,x_dim)%LIT_POOL_D-1)**2))-1&
                          -1/(CELL(y_dim,x_dim)%SPP_PS(cur_pla)+1)+1&
                          +1/(CELL(y_dim,x_dim)%SPP_RI(cur_pla)+1)-1&
                          +1/(CELL(y_dim,x_dim)%POT_EVP+1)-1&

            ! This is for soil compactness effects
            if (SC_SW .eq. 1) gx=gx+2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1
          
            ! Coefficient calculation
            CELL(y_dim,x_dim)%R_MAX_CO(cur_pla)=2/(1+EXP(-gx))

          if (IF_EFF_DR .eq. 1) then
            ! # Fourth, DECREASE_R modifications
            gx = -1/(CELL(y_dim,x_dim)%SPP_RES(cur_pla)+1)+1&
                          +1/(CELL(y_dim,x_dim)%AN_POOL+CELL(y_dim,x_dim)%LIT_N+1)-1&
                          -1/(CELL(y_dim,x_dim)%LIT_POOL_D+1)+1&
                          +1/(CELL(y_dim,x_dim)%SPP_PS(cur_pla)+1)-1&
                          -1/(CELL(y_dim,x_dim)%SPP_RI(cur_pla)+1)+1&
                          -1/(CELL(y_dim,x_dim)%POT_EVP+1)+1&
                          -1/(CELL(y_dim,x_dim)%SPP_TRP(cur_pla)+1)+1&
                          +1/(CELL(y_dim,x_dim)%SPP_NU(cur_pla)+1)-1&
                          +1/(CELL(y_dim,x_dim)%SPP_CC(cur_pla)+1)-1

            ! This is for soil compactness effects
            if (SC_SW .eq. 1) gx=gx+2/(EXP((0.8*CELL(y_dim,x_dim)%SOIL_COM-1.5)**2))-1

            ! This is used for mortality rate and grazing pressure
            if (MR_SW .eq. 1) gx=gx+1/(CELL(y_dim,x_dim)%SPP_GRAZED(:,cur_pla)+1)-1 
    
            ! Coefficient calculations
            CELL(y_dim,x_dim)%DECREASE_R_CO(cur_pla)=2/(1+EXP(-0.5*gx))
    
          end if

        end do
      end do
    end do

  end if ! End checking If Effects switch.
end subroutine
