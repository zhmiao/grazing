! This module is used for different utility functions and subroutines
module misc

  contains

  ! Subroutine for Check and reallocate dimensional variables {{{
  subroutine check_and_reallocate(DVAR,D) !{{{
    
      implicit none
      real, dimension(:), allocatable, intent(out):: DVAR
      integer, intent(in) :: D
    
      if(allocated(DVAR)) deallocate(DVAR)
      allocate(DVAR(D))
    
  end subroutine check_and_reallocate !}}}
  
  subroutine check_and_reallocate_two(DVAR,D1,D2) !{{{
    
      implicit none
      real, dimension(:,:), allocatable, intent(out):: DVAR
      integer, intent(in) :: D1,D2
    
      if(allocated(DVAR)) deallocate(DVAR)
      allocate(DVAR(D1,D2))
    
  end subroutine check_and_reallocate_two !}}}

  subroutine check_and_reallocate_int(DVAR,D) !{{{
    
      implicit none
      integer, dimension(:), allocatable, intent(out):: DVAR
      integer, intent(in) :: D
    
      if(allocated(DVAR)) deallocate(DVAR)
      allocate(DVAR(D))
    
  end subroutine check_and_reallocate_int !}}}
  
  ! subroutine check_and_reallocate_two_int(DVAR,D1,D2) !{{{
  !   
  !     implicit none
  !     integer, dimension(:,:), allocatable, intent(out):: DVAR
  !     integer, intent(in) :: D1,D2
  !   
  !     if(allocated(DVAR)) deallocate(DVAR)
  !     allocate(DVAR(D1,D2))
  !   
  ! end subroutine check_and_reallocate_two_int !}}}
  ! }}}

  ! ! Subroutine for calculating average values {{{
  !
  !  subroutine cell_avg_1(dir, input)
  !    implicit none
  !    
  !  end subroutine avg_output
  !
  ! ! }}}
  !
  ! ! Subroutine for variable average value output {{{
  !
  !  subroutine avg_output(dir, input)
  !    implicit none
  !    
  !  end subroutine avg_output
  !
  ! !}}}

end module misc
