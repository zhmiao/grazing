subroutine spwt_join(w,m,n)

implicit none

integer, dimension(:,:) ::  w
integer :: m, n
integer :: i, j

do i=0,m-1
  do j=1,n

    ! rook move
    if (i-1 .gt. -1) then
      w(i*n+j, (i-1)*n+j)=1
    end if
    if (i+1 .lt. m) then
      w(i*n+j, (i+1)*n+j)=1
    end if
    if (j-1 .gt. 0) then
      w(i*n+j, i*n+j-1)=1
    end if
    if (j+1 .lt. n+1) then
      w(i*n+j, i*n+j+1)=1
    end if

    ! bishop move
    if (i .gt. 0 .and. j .gt. 1) then
      w(i*n+j, (i-1)*n+j-1)=1
    end if
    if (i .gt. 0 .and. j .lt. n) then
      w(i*n+j, (i-1)*n+j+1)=1
    end if
    if (i .lt. m-1 .and. j .lt. n) then
      w(i*n+j, (i+1)*n+j+1)=1
    end if
    if (i .lt. m-1 .and. j .gt. 1) then
      w(i*n+j, (i+1)*n+j-1)=1
    end if
  end do
end do

return
end subroutine spwt_join
