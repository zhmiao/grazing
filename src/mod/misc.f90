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


end module misc
