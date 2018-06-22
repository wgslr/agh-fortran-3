program mult
  use sequential
  use parallel

  integer ( kind = 8) :: i, n, step, parse_result, start
  character(len=10) :: arg

  if (command_argument_count() .NE. 3) then
    n = 10
    step = 1
    start = 10
  else
    call get_command_argument(1, arg)
    read(arg, *, iostat=parse_result) n
    call get_command_argument(2, arg)
    read(arg, *, iostat=parse_result) step
    call get_command_argument(3, arg)
    read(arg, *, iostat=parse_result) start
  end if

  do i = start, n, step
    call measure(i)
  end do

  contains

  subroutine measure(isize)
    implicit none
    integer (kind=4) :: status
    integer (kind=8), intent(in) :: isize
    real (kind=8), allocatable :: first(:,:), second(:, :), multiply(:, :)
    real (kind=8) :: start, stop

    allocate(first(isize, isize))
    allocate(second(isize, isize))
    allocate(multiply(isize, isize))

    first = 1.1
    second = 2.3

  if (THIS_IMAGE() .EQ. 1) then
    call cpu_time(start)
    call mm_seq(first, second, multiply, status)
    call cpu_time(stop)
  end if

    call cpu_time(start)
    call mm_par(first, second, multiply, status)
    call cpu_time(stop)

    print '(i6,";",f15.7,"")', isize,(stop - start)

    deallocate(first)
    deallocate(second)
    deallocate(multiply)
  end subroutine
  
end program

