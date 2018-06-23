program mult
  use sequential
  use parallel

  integer ( kind = 8) :: i, n, step, parse_result, start
  character(len=10) :: arg

  if (command_argument_count() .NE. 3) then
    start = 1
    n = 500
    step = 5
  else
    call get_command_argument(1, arg)
    read(arg, *, iostat=parse_result) start
    call get_command_argument(2, arg)
    read(arg, *, iostat=parse_result) n
    call get_command_argument(3, arg)
    read(arg, *, iostat=parse_result) step
  end if

  do i = start, n, step
    call measure(i)
  end do

  contains

  subroutine measure(isize)
    implicit none
    integer (kind=4) :: status
    integer (kind=8) :: i, j
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

      print '("mm_seq;",i6,";",i6,";",f15.7,"")', isize, NUM_IMAGES(), (stop - start)
    end if

    sync all

    call cpu_time(start)
    call mm_par(first, second, multiply, status)
    call cpu_time(stop)

    if (THIS_IMAGE() .EQ. 1) then
      print '("mm_par;",i6,";",i6,";",f15.7,"")', isize, NUM_IMAGES(), (stop - start)
    end if

    do i=1,isize,1
      do j=1,isize,1
        first(i,j) = (i * isize) * j
      end do
      second(1,i) = i
    end do


    if (this_image() .eq. 1) then
      call cpu_time(start)
      call gauss_seq(first, second(1,:), isize - 1)
      call cpu_time(stop)

      print '("gauss_seq;",i6,";",i6,";",f15.7,"")', isize, NUM_IMAGES(), (stop - start)
    end if

    sync all
    do i=1,isize,1
      do j=1,isize,1
        first(i,j) = (i * isize) * j
      end do
      second(1,i) = i
    end do

    call cpu_time(start)
    call gauss_par(first, second(1,:), isize - 1)
    call cpu_time(stop)

    if (THIS_IMAGE() .EQ. 1) then
      print '("gauss_par;",i6,";",i6,";",f15.7,"")', isize, NUM_IMAGES(), (stop - start)
    end if

    sync all

    deallocate(first)
    deallocate(second)
    deallocate(multiply)
  end subroutine
  
end program

