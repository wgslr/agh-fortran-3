module parallel
  contains
  subroutine mm_par(first, second, multiply, status)
    real ( kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
    real ( kind = 8), intent(in) :: second(: ,:) ! druga macierz
    real ( kind = 8), intent(out) :: multiply(:,:) ! macierz wynikowa
    ! real ( kind = 8), codimension[:], dimension(:,:), allocatable :: buff
    real ( kind = 8), allocatable :: buff(:,:)[:]
    integer ( kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK
    integer ( kind = 4) :: rows1, rows2, cols1, cols2, r, c, i ! kod błędu, 0 gdy OK
    integer ( kind = 4) :: resultshape(2)


    rows1 = size(first, 1)
    cols1 = size(first, 2)
    rows2 = size(second, 1)
    cols2 = size(second, 2)
    resultshape = shape(multiply)

    print *, 123

    allocate(buff[THIS_IMAGE()], SOURCE=multiply)

    ! print *, buff(:,:)[THIS_IMAGE()]

    if (cols1 .NE. rows2) then
      status = 1
      return
    end if

    if (ANY((resultshape - (/rows1, cols2/)) /= 0)) then
      status = 2
      return
    end if

    multiply = 0

    do r = 1, rows1
      do c = 1, cols2
        do i = 1, cols1
          multiply(r, c) = multiply(r, c) + first(r, i) * second(i, c)
        end do
      end do
    end do

    print *, multiply

    status = 0
  end subroutine

end module