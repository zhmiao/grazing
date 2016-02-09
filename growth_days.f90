subroutine growth_days

  use parameter_var
  use structure

  implicit none
  
  integer i
  !integer x, y

  do y_dim=1,MAX_Y_DIM
    do x_dim=1,MAX_X_DIM

			do cur_pla=1,PLA_SPP_NUM
        CELL(y_dim, x_dim)%GROW_DAYS(cur_pla)=65+(300*EXP(0.01*CELL(y_dim,x_dim)%RAINFALL))/(EXP(0.01*CELL(y_dim,x_dim)%RAINFALL)+EXP(6.25))
		  end do

      CELL(y_dim, x_dim)%DAILY_RAIN=CELL(y_dim, x_dim)%RAINFALL/CELL(y_dim, x_dim)%GROW_DAYS
      ! write(*,*) CELL(y_dim, x_dim)%GROW_DAYS
    end do 
  end do

  ! write(*,*) ' '
  ! do y_dim = 1, MAX_Y_DIM
  !   write(*,'(10I)')(CELL(y_dim, i)%GROW_DAYS, i=1,MAX_X_DIM)
  ! end do
  ! write(*,*) ' '
  ! do y_dim = 1, MAX_Y_DIM
  !   write(*,'(10F)')(CELL(y_dim, i)%DAILY_RAIN, i=1,MAX_X_DIM)
  ! end do

end subroutine growth_days
