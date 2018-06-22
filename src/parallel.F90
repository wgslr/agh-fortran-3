module parallel
  contains
  subroutine mm_par(first, second, multiply, status)
    real ( kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
    real ( kind = 8), intent(in) :: second(: ,:) ! druga macierz
    real ( kind = 8), intent(out) :: multiply(:,:) ! macierz wynikowa
    real ( kind = 8), codimension[:], dimension(:,:), allocatable :: buff
    ! real ( kind = 8) :: buff(size,size)[:]
    integer ( kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK
    integer ( kind = 4) :: rows1, rows2, cols1, cols2, r, c, i, stripe, im
    integer ( kind = 4) :: resultshape(2)
    integer ( kind = 4), codimension[:], allocatable :: minrow, maxrow
    real (kind=8) :: start, stop

    allocate(minrow[*])
    allocate(maxrow[*])

    rows1 = size(first, 1)
    cols1 = size(first, 2)
    rows2 = size(second, 1)
    cols2 = size(second, 2)
    resultshape = shape(multiply)
    stripe = CEILING(real(rows1)/NUM_IMAGES())
    minrow = MIN(rows1, (THIS_IMAGE() - 1) * stripe) + 1
    maxrow = MIN(rows1, THIS_IMAGE() * stripe)

    if (cols1 .NE. rows2) then
      status = 1
      return
    end if

    if (ANY((resultshape - (/rows1, cols2/)) /= 0)) then
      status = 2
      return
    end if

    allocate(buff(stripe, cols2)[*])

    ! if( THIS_IMAGE() .EQ. 1) then
      ! do im = 1, NUM_IMAGES()
        print '("Image ",i6,": ",i6,"..",i6)', THIS_IMAGE(), minrow, maxrow
      ! end do

    ! end if

    ! buff(:,:)[THIS_IMAGE()] = 0 
    call cpu_time(start)

    do r = minrow, maxrow
      do c = 1, cols2
        do i = 1, cols1
          buff(r - minrow + 1, c) = buff(r - minrow + 1, c) + first(r, i) * second(i, c)
        end do
      end do
    end do

    sync all
    call cpu_time(stop)

    print *, "Map: ", (stop - start)

    print *, THIS_IMAGE(), "Multiplied"

    call cpu_time(start)
    ! Collect results
    if(THIS_IMAGE() .EQ. 1) then
      do im = 1, NUM_IMAGES()
        multiply(minrow[im]:maxrow[im], :) = buff(1:(maxrow[im] - minrow[im] + 1), :)[im]
        ! print '("result(", i6,":",i6,", :) = buff(", i6, ":",i6")")', minrow[im], maxrow[im], 1, (maxrow[im] - minrow[im] + 1)
        ! print *, multiply(minrow[im]:maxrow[im], :)
      end do
    end if
    call cpu_time(stop)

    print *, "Reduce: ", (stop - start)



    ! deallocate(buff)
    ! deallocate(minrow)
    ! deallocate(maxrow)

    ! print *, THIS_IMAGE(), "Deallocated"
    
    status = 0
  end subroutine

end module