!------------------------------------------------------------------------------
! MODULE: sequential
!
!> @author
!> Wojciech Geisler
!
! DESCRIPTION: 
!> This module contains sequential algorithm implementations.
!
! REVISION HISTORY:
! 22 06 2018 - Initial version
!------------------------------------------------------------------------------
module sequential
  contains
  !------------------------------------------------------------------------------
  !> @author
  !> Wojciech Geisler
  !
  ! DESCRIPTION: 
  !> Multiplies two matrices and places result in the `multiply` param.
  !> Performs mathematical operation \f$ first \times second \f$
  !
  !> @param[in] first First operand
  !> @param[in] second Second operand
  !> @param[out] multiply Multiplication result
  !> @param[out] status Exit code, 0 means success
  !------------------------------------------------------------------------------
  subroutine mm_seq(first, second, multiply, status)
    real ( kind = 8), intent(in) :: first(:,:) ! pierwsza macierz
    real ( kind = 8), intent(in) :: second(:, :) ! druga macierz
    real ( kind = 8), intent(out) :: multiply(:, :) ! macierz wynikowa
    integer ( kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK
    integer ( kind = 4) :: rows1, rows2, cols1, cols2, r, c, i ! kod błędu, 0 gdy OK
    integer ( kind = 4) :: resultshape(2)

    rows1 = size(first, 1)
    cols1 = size(first, 2)
    rows2 = size(second, 1)
    cols2 = size(second, 2)
    resultshape = shape(multiply)

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

    status = 0
  end subroutine

  !------------------------------------------------------------------------------
  !> @author
  !> Wojciech Geisler
  !
  ! DESCRIPTION: 
  !> Wrapper on mm_seq with explicit size variable
  !> necessary for creating python bindings.
  !> Accepts only square matrices.
  !
  !> @param[in] first First operand
  !> @param[in] second Second operand
  !> @param[out] multiply Multiplication result
  !> @param[out] status Exit code, 0 means success
  !> @param[in] isize Size of the matrix
  !------------------------------------------------------------------------------
  subroutine mm_seq_square(first, second, multiply, status, isize)
    real ( kind = 8), intent(in) :: first(isize,isize) ! pierwsza macierz
    real ( kind = 8), intent(in) :: second(isize, isize) ! druga macierz
    real ( kind = 8), intent(out) :: multiply(isize, isize) ! macierz wynikowa
    integer ( kind = 4), intent(out) :: status ! kod błędu, 0 gdy OK
    integer ( kind = 4), intent(in) :: isize

    call mm_seq(first, second, multiply, status)
  end 

end module