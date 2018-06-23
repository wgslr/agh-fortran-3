!------------------------------------------------------------------------------
! MODULE: sequential
!
!> @author
!> Wojciech Geisler
!
! DESCRIPTION: 
!> This module contains paralell algorithm implementations.
!
! REVISION HISTORY:
! 22 06 2018 - Initial version
!------------------------------------------------------------------------------
module parallel
  use ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  contains
  !------------------------------------------------------------------------------
  !> @author
  !> Wojciech Geisler
  !
  ! DESCRIPTION: 
  !> Multiplies two matrices and places result in the `multiply` param.
  !> Uses coarrays to leverage parallelism for faster operation.
  !
  !> @param[in] first First operand
  !> @param[in] second Second operand
  !> @param[out] multiply Multiplication result
  !> @param[out] status Exit code, 0 means success
  !------------------------------------------------------------------------------
  subroutine mm_par(first, second, multiply, status)
    real ( kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
    real ( kind = 8), intent(in) :: second(: ,:) ! druga macierz
    real ( kind = 8), intent(out) :: multiply(:,:) ! macierz wynikowa
    real ( kind = 8), codimension[:], dimension(:,:), allocatable :: buff
    integer ( kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK
    integer ( kind = 4) :: rows1, rows2, cols1, cols2, r, c, i, stripe, im
    integer ( kind = 4) :: resultshape(2)
    integer ( kind = 4), codimension[:], allocatable :: minrow, maxrow

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

    !write (ERROR_UNIT, '("Image ",i6,": ",i6,"..",i6)'), THIS_IMAGE(), minrow, maxrow

    do r = minrow, maxrow
      do c = 1, cols2
        do i = 1, cols1
          buff(r - minrow + 1, c) = buff(r - minrow + 1, c) + first(r, i) * second(i, c)
        end do
      end do
    end do

    sync all

    ! Collect results
    if(THIS_IMAGE() .EQ. 1) then
      do im = 1, NUM_IMAGES()
        multiply(minrow[im]:maxrow[im], :) = buff(1:(maxrow[im] - minrow[im] + 1), :)[im]
      end do
    end if

    deallocate(buff)
    deallocate(minrow)
    deallocate(maxrow)
    
    status = 0
  end subroutine

  !------------------------------------------------------------------------------
  !> @author
  !> Wojciech Geisler
  !
  ! DESCRIPTION: 
  !> Performs gaussian elimination on a coefficients and right hand values.
  !
  !> @param[inout] A Rank 2 array of coefficients
  !> @param[inout] X Rank 1 array of right hand values.
  !> @param[in] n Maximum row number for 0-indexec array A and X
  !------------------------------------------------------------------------------
  subroutine gauss_par(A, X, n)
    integer(kind=8), intent(in) :: n
    real(kind = 8), intent(inout) :: A(0:N, 0:N), X(0:N)
    real ( kind = 8), codimension[:], allocatable :: coA(:,:)
    real ( kind = 8), codimension[:], allocatable :: coX(:)
    real(kind = 8) :: ratio
    integer(kind=8) :: i, j

    allocate(coA(0:N, 0:N)[*])
    allocate(coX(0:N)[*])

    if (THIS_IMAGE() .eq. 1) then
      coA(:,:)[1] = A(:,:)
      coX(:)[1] = X(:)
    end if

    do i = 0, N
      ! scale row i to have 1 on the diagonal
      if(THIS_IMAGE() .eq. 1) then
        coX(i)[1] = coX(i)[1] / coA(i, i)[1]
        coA(:, i)[1] = coA(:, i)[1] / coA(i, i)[1]
      end if
      sync all
      do j = THIS_IMAGE() - 1, N, NUM_IMAGES()
        IF ((i .NE. j) .AND. (ABS(coA(i, i)[1] - 0) > 1d-6)) THEN
          ratio = coA(i, j)[1] / coA(i, i)[1]
          coA(:,j)[1] = coA(:,j)[1] - ratio * coA(:, i)[1]
          coX(j)[1] = coX(j)[1] - ratio * coX(i)[1]
        END IF
      END DO
    END DO
    

    if (THIS_IMAGE() .eq. 1) then
      A(:,:) = coA(:,:)[1] 
      X(:) = coX(:)[1] 
    end if

    deallocate(coA)
    deallocate(coX)
  end subroutine


end module
