subroutine function_process(function_name, res, input_variables) 

  use parameter_var                                                                 ! Read main parameter module, mainly for CWD
  use fparser                                                                       ! Read parser module
  implicit none

  character(len=*) :: function_name                                                 ! Function file name, used for recongnizing function files
  real(rn), dimension(:) :: input_variables                                         ! Input variable array, dynamic length for different number of inputs
  real(rn), dimension(:) :: res                                                     ! Used for storing calculated results, and returns the value

  integer  :: aop
  integer  :: nspp                                                                  ! Stores number of function
  integer  :: nparm                                                                 ! Stores total number of variables, read from function file
  integer  :: nvar                                                                  ! Stores number of input variables which equals to the size(input_variables)

  character(len=10), dimension(:), allocatable :: var
  type Variables                                                                    ! Stores variables names. Since variables can be a string, string reading functin is needed and specific data type needs to be set up
    character(len=:),  allocatable :: str
  end type Variables
  type(Variables), dimension(:), allocatable :: var_type

  real(rn), dimension(:,:), allocatable :: val                                      ! Stores values for each variables

  character(len=:), allocatable :: func

  integer :: i, j                                                                   ! General parameters

  nvar=size(input_variables)                                                        ! First, set variable dimension

  open(150, file=CWD(1:len_trim(CWD))//CON_FUN(1:len_trim(CON_FUN))//&
    SEASON(1:len_trim(SEASON))//function_name, action='read', iostat=ioerr)         ! Open function file

  if (ioerr .ne. 0) then                                                            ! If not opend correctly
    write(*,*) function_name, 'function file not opened'
    stop
  else 

    read(150,*)
    read(150,*)
    read(150,*)

    write(*,*)'Processing function: ', function_name
    write(ECHO_NUM,*) ' '
    write(ECHO_NUM,*) 'Function', function_name

    read(150,*,iostat=ioerr) aop                                                                ! The first thing is function number
    if (ioerr .ne. 0) then 
      write(*,*) function_name, 'reading error'
      stop
    else
      if (aop .eq. 1) then
        nspp=ANI_SPP_NUM
        write(ECHO_NUM,*) 'This is an animal function '
        write(ECHO_NUM,*) 'Number of species is: ', nspp
      else
        nspp=PLA_SPP_NUM
        write(ECHO_NUM,*) 'This is a plant function '
        write(ECHO_NUM,*) 'Number of species is: ', nspp
      end if
    end if

    read(150,*) nparm                                                               ! Read in number of total parameter
    if (ioerr .ne. 0) then 
      write(*,*) function_name, 'reading error'
      stop
    else
      write(ECHO_NUM,*) 'Number of parameters is: ', nparm                                   ! Print it out
      write(ECHO_NUM,*) 'Number of variables is: ', nvar                                     ! Print out the number of input variables too
      allocate(val(nparm+nvar,nspp))                                                  ! Allocate value dimensions with total number of variables
      allocate(var(nparm+nvar))
      allocate(var_type(nparm+nvar))
    end if

    read(150,*)
    read(150,*)
    read(150,*)

    do i=1,nparm                                                                    ! Use the string reading function to reading parameters. (trim is true!)
      if(ReadLine(150,var_type(i)%str)) then
        var(i)=var_type(i)%str
        cycle
      else
        write(*,*) function_name, 'parameters not read.'
        stop
      end if
    end do

    read(150,*)
    read(150,*)
    read(150,*)

    do i=nparm+1,nparm+nvar                                                         ! Use the string reading function to reading variables. (trim is true!)
      if(ReadLine(150,var_type(i)%str)) then
        var(i)=var_type(i)%str
        cycle
      else
        write(*,*) 'variables not read.'
        stop
      end if
    end do

    read(150,*)
    read(150,*)
    read(150,*)

    if(ReadLine(150,func)) then
      write(ECHO_NUM,*) 'Function is: '
      write(ECHO_NUM,*) func
    else
      write(*,*) function_name, 'function string not read.'
      stop
    end if

    write(ECHO_NUM,*) 'Parameters are: ', (trim(var(i)), ' ', i=1,nparm)               ! Print out the name of function variables
    write(ECHO_NUM,*) 'Variables are: ', (trim(var(i)),' ', i=nparm+1,nparm+nvar)      ! Print out the name of input variables

    read(150,*)
    read(150,*)
    read(150,*)

    do j=1,nspp
      write(ECHO_NUM,*) 'For species number ', j
      read(150,*,iostat=ioerr) (val(i,j), i=1,nparm)                                         ! Read in function variable values
      if (ioerr .ne. 0) then
        write(*,*) function_name, 'reading error'
        stop
      else
        write(ECHO_NUM,*) 'Parameter values are: ', (val(i,j), ' ', i=1,nparm)           ! Print them out
        do i=1,nvar                                                               ! Use the input value to give these variables values
          val(nparm+i,j)=input_variables(i)
        end do
        write(ECHO_NUM,*) 'Variable values are: ', (val(i,j),' ', i=nparm+1,nparm+nvar)  ! Print them out
      end if
    end do

    read(150,*,iostat=ioerr)
    if (ioerr .ne. 0) then 
      write(*,*) function_name, 'reading error'
      stop
    end if

  end if
  close(150)

  call initf(1)

  call parsef(1,func,var)

  do i=1,nspp                                                                   ! Evaluate the functions and calculate the result
    write(ECHO_NUM,*) 'Result for species number ', i
    res(i)=evalf(1,val(:,i))
    if (EvalErrType .lt. 0) write(*,*) 'Error: ', EvalErrMsg()
    write(ECHO_NUM,*)res(i)
  end do

contains

function ReadLine(aunit, InLine, trimmed)  result(OK)                           ! This is the function that reads character strings
integer, intent(IN) :: aunit
character(LEN=:), allocatable, optional :: InLine
logical, intent(in), optional :: trimmed
integer, parameter :: line_buf_len= 1024*4
character(LEN=line_buf_len) :: InS
logical :: OK, set
integer status, size, i

OK = .false.
set = .true.
do
    read (aunit,'(a)',advance='NO',iostat=status, size=size) InS
    OK = .not. IS_IOSTAT_END(status)
    if (.not. OK) return
    if (present(InLine)) then
        if (set) then
            InLine = InS(1:size)
            set=.false.
        else
            InLine = InLine // InS(1:size)
        end if
        do i=1,size                                                            ! Stop reading a ! sign so one can write comments
          if (Ins(i:i) .eq. '!') then
            InLine = Ins(1:i-1)
            exit
          end if
        end do
    end if
    if (IS_IOSTAT_EOR(status)) exit
end do
if (present(trimmed) .and. present(InLine)) then
    if (trimmed) InLine = trim(adjustl(InLine))
end if

end function ReadLine

end subroutine function_process
