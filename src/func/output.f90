subroutine model_output
  
  use parameter_var
  use structure

  implicit none

  ! real temp_a, temp_b

!==============================
! Cell level output {{{
!==============================


  ! CELL(y_dim,x_dim)%TOT_BIOMASS=sum(CELL(y_dim,x_dim)%TOT_BIO_SPP(:))
  ! ! Output for species total biomass in each cell
  ! do cur_pla=1,PLA_SPP_NUM
  !   write(OUTPUT_DIR,'(A19,I1)')'/out/totbio_pla_',cur_pla
  !   open(OUTPUT_NUM+cur_pla, FILE=CWD(1:len_trim(CWD))//OUTPUT_DIR(1:len_trim(OUTPUT_DIR))//'.csv',STATUS='REPLACE'&
  !                                                                     ,ACTION='WRITE',IOSTAT=ioerr)
  !     if (ioerr .ne. 0) then
  !       write(*,*) 'outputfile can not be opened'
  !       stop
  !     end if
  !
  !   write(OUTPUT_NUM+cur_pla,*) ' '
  !   write(OUTPUT_NUM+cur_pla,*)  day,','
  !   do y_dim=1,MAX_Y_DIM
  !     write(OUTPUT_NUM+cur_pla,*) (CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla), x_dim=1,MAX_X_DIM)
  !     ! do x_dim=1,MAX_X_DIM
  !     !   write(OUTPUT_NUM+cur_pla,*) CELL(y_dim, x_dim)%TOT_BIO_SPP(cur_pla),','
  !     ! end do ! end looping for x
  !   end do ! end looping for y
  !
  !   ! do y_dim=1,MAX_Y_DIM
  !   !   write(OUTPUT_NUM+cur_pla,*) (CELL(y_dim,x_dim)%AV_BIO_SPP(cur_pla), x_dim=1,MAX_X_DIM)
  !   ! end do ! end looping for y
  !   !
  !   ! do y_dim=1,MAX_Y_DIM
  !   !   write(OUTPUT_NUM+cur_pla,*) (CELL(y_dim,x_dim)%UAV_BIO_SPP(cur_pla), x_dim=1,MAX_X_DIM)
  !   ! end do ! end looping for y
  !
  !   close(OUTPUT_NUM+cur_pla)
  ! end do ! end looping for plant species }}}


!==============================
! Global level output (daily)
!==============================

! Global species biomass

  do cur_pla=1,PLA_SPP_NUM

    SPP_BIOMASS(cur_pla)=0

    do y_dim=1,MAX_Y_DIM
      do x_dim=1,MAX_X_DIM
        SPP_BIOMASS(cur_pla)=SPP_BIOMASS(cur_pla)+CELL(y_dim,x_dim)%TOT_BIO_SPP(cur_pla)
      end do
    end do

    ! inquire(unit=OUTPUT_NUM+cur_pla, opened=isopen)
    ! if (isopen .eq. .false.) then
    !   write(OUTPUT_DIR,'(A23,I1)')'/output/glo_totbio_pla_',cur_pla
    !   open(OUTPUT_NUM+cur_pla, FILE=CWD(1:len_trim(CWD))//OUTPUT_DIR(1:len_trim(OUTPUT_DIR))//'.csv',STATUS='REPLACE'&
    !                                                                   ,ACTION='WRITE',IOSTAT=ioerr)
    !   if (ioerr .ne. 0) then
    !     write(*,*) 'Species global total biomass outputfile can not be opened'
    !     stop
    !   end if
    ! end if
    !
    ! write(OUTPUT_NUM+cur_pla,*) SPP_BIOMASS(cur_pla),','

    ! close(OUTPUT_NUM+cur_pla)

  end do ! end looping for plant species

! Global total biomass

    TOT_BIOMASS=sum(SPP_BIOMASS(:))

    write(OUTPUT_DIR,'(A15)')'/out/glo_totbio'

    inquire(unit=OUTPUT_NUM, opened=isopen)
    if (isopen .eq. .false.) then
    open(OUTPUT_NUM, FILE=CWD(1:len_trim(CWD))//OUTPUT_DIR(1:len_trim(OUTPUT_DIR))//'.csv',STATUS='REPLACE'&
                                                                      ,ACTION='WRITE',IOSTAT=ioerr)
      if (ioerr .ne. 0) then
        write(*,*) 'Global total plant biomass outputfile can not be opened'
        stop
      end if

    end if

    write(OUTPUT_NUM,*) TOT_BIOMASS,','

    ! if (day .eq. 206 .or. day .eq. 258) then
    !   ! write(*,*) (TOT_BIOMASS/CELLAREA)*0.01
    !   do y_dim=1,MAX_Y_DIM
    !     do x_dim=1,MAX_X_DIM
    !       write(*,*) day, '',  CELL(y_dim,x_dim)%GROW_DAYS(:), '', (TOT_BIOMASS/CELLAREA)*0.01, '', F_LA(:), '', CELL(y_dim,x_dim)%TOT_BIO_SPP(:), '', ASLA(:), '', CELLAREA, '', CELL(y_dim,x_dim)%SPP_LAI(:)
    !     end do
    !   end do
    ! end if

    TOT_BIOMASS_Y=TOT_BIOMASS_Y+TOT_BIOMASS

    if (day .eq. 365) then

      write(OUTPUT_DIR,'(A15)')'/out/yearly_glo'
  
      inquire(unit=OUTPUT_NUM_Y, opened=isopen)
      if (isopen .eq. .false.) then
      open(OUTPUT_NUM_Y, FILE=CWD(1:len_trim(CWD))//OUTPUT_DIR(1:len_trim(OUTPUT_DIR))//'.csv',STATUS='REPLACE'&
                                                                        ,ACTION='WRITE',IOSTAT=ioerr)
        if (ioerr .ne. 0) then
          write(*,*) 'Yearly global total plant biomass outputfile can not be opened'
          stop
        end if
  
      end if
  
      write(OUTPUT_NUM_Y,*) TOT_BIOMASS_Y,','

      ! close(OUTPUT_NUM_Y)

    endif

    
    ! write(*,*) TOT_BIOMASS,','

    ! close(OUTPUT_NUM)
    

! if (day .eq. 181 .or. day .eq. 206 .or. day .eq. 232) temp_b=TOT_BIOMASS
if (day .eq. 181 .or. day .eq. 232) temp_b=TOT_BIOMASS

! if (day .eq. 211 .or. day .eq. 236 .or. day .eq. 261) write(*,*) day, ' ', TOT_BIOMASS, ' ', temp_b

! if (GR_SW .eq. 1) then
!
!   ! if (day .eq. 212 .or. day .eq. 237 .or. day .eq. 262) temp_a=0 
!   !
!   ! do y_dim=1,MAX_Y_DIM
!   !   do x_dim=1,MAX_X_DIM
!   !     temp_a=temp_a+sum(CELL(y_dim,x_dim)%SPP_GRAZED(:,:))
!   !   end do
!   ! end do
!
! end if


  do y_dim=1,MAX_Y_DIM
    do x_dim=1,MAX_X_DIM
      temp_c=sum(CELL(y_dim,x_dim)%DEL_BIO_SPP(:))
      temp_k=sum(CELL(y_dim,x_dim)%DEL_BIO_SPP(:))/sum(CELL(y_dim,x_dim)%TOT_BIO_SPP(:))
      ! temp_d=sum(CELL(y_dim,x_dim)%SPP_PS(:))*84.87654
      ! temp_d=sum(CELL(y_dim,x_dim)%SPP_PS(:))*1.0368
      temp_d=sum(CELL(y_dim,x_dim)%SPP_PS(:))
      temp_g=sum(CELL(y_dim,x_dim)%SPP_LAI(:))
      if (temp_h .eq. 1 .and. day .eq. 206) temp_i=sum(CELL(y_dim,x_dim)%DEL_BIO_SPP(:))
      if (temp_h .eq. 1 .and. day .eq. 258) temp_j=sum(CELL(y_dim,x_dim)%DEL_BIO_SPP(:))
      if (temp_h .eq. 1 .and. day .eq. 206) temp_l=sum(CELL(y_dim,x_dim)%DEL_BIO_SPP(:))/sum(CELL(y_dim,x_dim)%TOT_BIO_SPP(:))
      if (temp_h .eq. 1 .and. day .eq. 258) temp_m=sum(CELL(y_dim,x_dim)%DEL_BIO_SPP(:))/sum(CELL(y_dim,x_dim)%TOT_BIO_SPP(:))
    end do
  end do

! if (day .eq. 186 .or. day .eq. 212 .or. day .eq. 237) temp_a=TOT_BIOMASS

if (day .eq. 186 .or. day .eq. 237) temp_a=TOT_BIOMASS


if (day .eq. 206) write(*,'(I3,A8,F6.2,A8,F6.2,A9,F6.2,A9,F6.2,A8,F6.2,A8,F6.2)')  day, ' GI: ',  (temp_b-temp_a)/temp_b,&
                                                                           '   DM: ', (TOT_BIOMASS/CELLAREA)*0.01,&
                                                                           '   LAI: ', temp_g, '  Pnr: ', temp_d,'   CD: ', temp_c/temp_i,'   DR: ', temp_k/temp_l

if (day .eq. 258) write(*,'(I3,A8,F6.2,A8,F6.2,A9,F6.2,A9,F6.2,A8,F6.2,A8,F6.2)')  day, ' GI: ',  (temp_b-temp_a)/temp_b,&
                                                                           '   DM: ', (TOT_BIOMASS/CELLAREA)*0.01,&
                                                                           '   LAI: ', temp_g, '  Pnr: ', temp_d,'   CD: ', temp_c/temp_j,'   DR: ', temp_k/temp_m

! if (day .eq. 235 .or. day .eq. 260) write(*,*)day, '',  temp_a/temp_b, '', (TOT_BIOMASS/CELLAREA)*0.01

! Average variable output globally
! do y_dim=1,MAX_Y_DIM
!   do x_dim=1,MAX_X_DIM
!     CELL(y_dim,x_dim)%VAR_AVG=sum(CELL(y_dim,x_dim)%SPP_LAI(:))&
!                               /(PLA_SPP_NUM)
!   end do
! end do
!
! write(*,*) sum(CELL(:,:)%VAR_AVG)/(MAX_Y_DIM*MAX_X_DIM)
!
!
!     write(*,*) TOT_BIOMASS,','
!     write()

! write(*,*) sum(CELL(:,:)%POT_EVP)/(MAX_Y_DIM*MAX_X_DIM)
! write(*,*) DET_RATE(:)

end subroutine model_output
