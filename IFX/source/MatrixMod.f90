! $Id: MatrixMod.f90 1.10 2020/05/05 12:45:05EDT 622325 Development  $
module MatrixMod

!****h* source/MatrixMod
!  NAME
!  MatrixMod: OcactFortLib MatrixMod module.
!  REVISION
!  $Revision: 1.10 $
!  MODULE DESCRIPTION
!  The MatrixMod module contains functions that calculate the determinant and
!  inverse of a matrix.
!  MODULE ROUTINES
!  Determinant
!  Inverse
!*****

  implicit none
  save

  private
  public :: Determinant
  public :: Inverse

  ! Allow Determinant to be the generic name for the specific subroutines.
!****f* MatrixMod/Determinant
!  NAME
!  Determinant: This function calculates the determinant of a matrix.
!  DESCRIPTION
!  This function calculates the determinant of a matrix. The matrix must
!  be a square matrix.
!  RETURNS
!  Real or Double Precision: The same type as 'arr'.
!  SYNOPSIS
!  real function Determinant(arr) result(rv)
!  double precision function Determinant(arr) result(rv)
!  ARGUMENTS
!  * arr: 2-dimensional array (of any size) of reals or double precisions.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface Determinant
    module procedure DeterminantReal, DeterminantDoublePrec
  end interface Determinant

  ! Allow Determinant2by2 to be the generic name for the specific subroutines.
  interface Determinant2by2
    module procedure Determinant2by2Real, Determinant2by2DoublePrec
  end interface Determinant2by2

  ! Allow Determinant3by3 to be the generic name for the specific subroutines.
  interface Determinant3by3
    module procedure Determinant3by3Real, Determinant3by3DoublePrec
  end interface Determinant3by3

  ! Allow DeterminantGeneral to be the generic name for the specific
  ! subroutines.
  interface DeterminantGeneral
    module procedure DeterminantGeneralReal, DeterminantGeneralDoublePrec
  end interface DeterminantGeneral

  ! Allow Inverse to be the generic name for the specific subroutines.
  !****f* MatrixMod/Inverse
!  NAME
!  Inverse: This function returns the inverse of a matrix.
!  DESCRIPTION
!  This function takes a matrix, 'arr', and returns a matrix that is the
!  inverse of 'arr'.
!  RETURNS
!  2-dimensional array of Reals or Double Precisions: The same type as 'arr'.
!  SYNOPSIS
!  real function Inverse(arr) result(rv)
!  double precision function Inverse(arr) result(rv)
!  ARGUMENTS
!  * arr: 2-dimensional array (of any size) of reals or double precisions.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface Inverse
    module procedure InverseReal, InverseDoublePrec
  end interface Inverse

  ! Allow Inverse2by2 to be the generic name for the specific subroutines.
  interface Inverse2by2
    module procedure Inverse2by2Real, Inverse2by2DoublePrec
  end interface Inverse2by2

  ! Allow Inverse3by3 to be the generic name for the specific subroutines.
  interface Inverse3by3
    module procedure Inverse3by3Real, Inverse3by3DoublePrec
  end interface Inverse3by3

  ! Allow InverseGeneral to be the generic name for the specific subroutines.
  interface InverseGeneral
    module procedure InverseGeneralReal, InverseGeneralDoublePrec
  end interface InverseGeneral

  ! Allow RowReduce to be the generic name for the specific subroutines.
  interface RowReduce
    module procedure RowReduceReal, RowReduceDoublePrec
  end interface RowReduce

  contains

!******************************************************************************
  recursive real function DeterminantReal(arr) result(rv)

    real, dimension(:, :), intent(in) :: arr

    if (size(arr, 1) /= size(arr, 2)) then
      write(6, '(A)') "Error. Can only find determinant of a square matrix."
      stop
    end if

    select case (size(arr, 1))
    case (1)
      rv = arr(1, 1)
    case (2)
      rv = Determinant2by2(arr)
    case (3)
      rv = Determinant3by3(arr)
    case default
      rv = DeterminantGeneral(arr)
    end select

  end function DeterminantReal
!******************************************************************************
  recursive double precision function DeterminantDoublePrec(arr) result(rv)

    double precision, dimension(:, :), intent(in) :: arr

    if (size(arr, 1) /= size(arr, 2)) then
      write(6, '(A)') "Error. Can only find determinant of a square matrix."
      stop
    end if

    select case (size(arr, 1))
    case (1)
      rv = arr(1, 1)
    case (2)
      rv = Determinant2by2(arr)
    case (3)
      rv = Determinant3by3(arr)
    case default
      rv = DeterminantGeneral(arr)
    end select

  end function DeterminantDoublePrec
!******************************************************************************
  real function Determinant2by2Real(arr) result(rv)

    real, dimension(2, 2), intent(in) :: arr

    rv = arr(1,1) * arr(2,2) - arr(1,2) * arr(2,1)
    
  end function Determinant2by2Real
!******************************************************************************
  double precision function Determinant2by2DoublePrec(arr) result(rv)

    double precision, dimension(2, 2), intent(in) :: arr

    rv = arr(1,1) * arr(2,2) - arr(1,2) * arr(2,1)
    
  end function Determinant2by2DoublePrec
!******************************************************************************
  real function Determinant3by3Real(arr) result(rv)

    real, dimension(3, 3), intent(in) :: arr

    rv = arr(1,1) * arr(2,2) * arr(3,3) - &
         arr(1,1) * arr(2,3) * arr(3,2) - &
         arr(1,2) * arr(2,1) * arr(3,3) + &
         arr(1,2) * arr(2,3) * arr(3,1) + &
         arr(1,3) * arr(2,1) * arr(3,2) - &
         arr(1,3) * arr(2,2) * arr(3,1)
    
  end function Determinant3by3Real
!******************************************************************************
  double precision function Determinant3by3DoublePrec(arr) result(rv)

    double precision, dimension(3, 3), intent(in) :: arr

    rv = arr(1,1) * arr(2,2) * arr(3,3) - &
         arr(1,1) * arr(2,3) * arr(3,2) - &
         arr(1,2) * arr(2,1) * arr(3,3) + &
         arr(1,2) * arr(2,3) * arr(3,1) + &
         arr(1,3) * arr(2,1) * arr(3,2) - &
         arr(1,3) * arr(2,2) * arr(3,1)
    
  end function Determinant3by3DoublePrec
!******************************************************************************
  ! This function was taken (and adapted) from:
  ! https://www.dreamincode.net/forums/topic/366232-FORTRAN-90:-Determinant-of-a-Matrix/
  ! It was created by: Louisda16th a.k.a Ashwith J. Rego
  real function DeterminantGeneralReal(arr) result(rv)

    real, dimension(:, :), intent(in) :: arr
    ! Even though we're dealing with reals, use double precision internally
    ! to increase precision of calculated results.
    double precision, dimension(size(arr,1), size(arr,1)) :: tempArr
    integer :: i, j, k, l
    double precision  :: m, temp
    logical :: detExists = .true.

    l = 1
    tempArr = arr
	!Convert to upper triangular form
	do k = 1, size(arr, 1) - 1
      if (tempArr(k, k) == 0) then
        detExists = .false.
        do i = k + 1, size(arr, 1)
          if (tempArr(i, k) /= 0.0d0) THEN
            do j = 1, size(arr, 1)
              temp = tempArr(i, j)
              tempArr(i, j) = tempArr(k, j)
              tempArr(k, j) = temp
            end do
            detExists = .true.
            l = -l
            exit
          end if
        end do
        if (.not. detExists) then
          rv = 0.0
          return
        end if
      end if
      do j = k + 1, size(arr, 1)
        m = tempArr(j, k) / tempArr(k, k)
        do i = k + 1, size(arr, 1)
          tempArr(j, i) = tempArr(j, i) - m * tempArr(k, i)
        end do
      end do
    end do

	!Calculate determinant by finding product of diagonal elements
    rv = l
    do i = 1, size(arr, 1)
      rv = rv * tempArr(i, i)
    end do

  end function DeterminantGeneralReal
!******************************************************************************
  ! Note: This version works fine for smallish arrays.  But for large arrays
  ! (say, over 13 or 14, or so), it gets unweildy.  It is very ineffecient and
  ! takes way too long to run.
  !
  !real function DeterminantGeneralReal(arr) result(rv)
  !
  !  real, dimension(:, :), intent(in) :: arr
  !  real, dimension(size(arr,1)-1, size(arr,1)-1) :: tempArr
  !  integer :: i
  !
  !  rv = 0.0
  !  do i = 1, size(arr, 1)
  !    tempArr(:, 1:i-1) = arr(2:, 1:i-1)
  !    tempArr(:, i:) = arr(2:, i+1:)
  !    rv = rv + (-1)**(1+i) * arr(1, i) * Determinant(tempArr)
  !  end do
  !
  !end function DeterminantGeneralReal
!******************************************************************************
  ! This function was taken (and adapted) from:
  ! https://www.dreamincode.net/forums/topic/366232-FORTRAN-90:-Determinant-of-a-Matrix/
  ! It was created by: Louisda16th a.k.a Ashwith J. Rego
  double precision function DeterminantGeneralDoublePrec(arr) result(rv)

    double precision, dimension(:, :), intent(in) :: arr
    double precision, dimension(size(arr,1), size(arr,1)) :: tempArr
    integer :: i, j, k, l
    double precision  :: m, temp
    logical :: detExists = .true.

    l = 1
    tempArr = arr
	! Convert to upper triangular form.
	do k = 1, size(arr, 1) - 1
      if (tempArr(k, k) == 0) then
        detExists = .false.
        do i = k + 1, size(arr, 1)
          if (tempArr(i, k) /= 0.0d0) THEN
            do j = 1, size(arr, 1)
              temp = tempArr(i, j)
              tempArr(i, j) = tempArr(k, j)
              tempArr(k, j) = temp
            end do
            detExists = .true.
            l = -l
            exit
          end if
        end do
        if (.not. detExists) then
          rv = 0.0
          return
        end if
      end if
      do j = k + 1, size(arr, 1)
        m = tempArr(j, k) / tempArr(k, k)
        do i = k + 1, size(arr, 1)
          tempArr(j, i) = tempArr(j, i) - m * tempArr(k, i)
        end do
      end do
    end do

	! Calculate determinant by finding product of diagonal elements.
    rv = l
    do i = 1, size(arr, 1)
      rv = rv * tempArr(i, i)
    end do

  end function DeterminantGeneralDoublePrec
!******************************************************************************
  ! Note: This version works fine for smallish arrays.  But for large arrays
  ! (say, over 13 or 14, or so), it gets unweildy.  It is very ineffecient and
  ! takes way too long to run.
  !
  !double precision function DeterminantGeneralDoublePrec(arr) &
  !  result(rv)
  !
  !  double precision, dimension(:, :), intent(in) :: arr
  !  double precision, dimension(size(arr,1)-1, size(arr,1)-1) :: tempArr
  !  integer :: i
  !
  !  rv = 0.0d0
  !  do i = 1, size(arr, 1)
  !    tempArr(:, 1:i-1) = arr(2:, 1:i-1)
  !    tempArr(:, i:) = arr(2:, i+1:)
  !    rv = rv + (-1)**(1+i) * arr(1, i) * Determinant(tempArr)
  !  end do
  !
  !end function DeterminantGeneralDoublePrec
!******************************************************************************
  real function InverseReal(arr) result(rv)

    real, dimension(:, :), intent(in) :: arr
    dimension :: rv(size(arr, 1), size(arr, 1))

    if (size(arr, 1) /= size(arr, 2)) then
      write(6, '(A)') "Error. Can only take inverse of a square matrix."
      stop
    end if

    select case (size(arr, 1))
    case (1)
      rv = 1.0 / arr
    case (2)
      rv = Inverse2by2(arr)
    case (3)
      rv = Inverse3by3(arr)
    case default
      rv = InverseGeneral(arr)
    end select

  end function InverseReal
!******************************************************************************
  double precision function InverseDoublePrec(arr) result(rv)

    double precision, dimension(:, :), intent(in) :: arr
    dimension :: rv(size(arr, 1), size(arr, 1))

    if (size(arr, 1) /= size(arr, 2)) then
      write(6, '(A)') "Error. Can only take inverse of a square matrix."
      stop
    end if

    select case (size(arr, 1))
    case (1)
      rv = 1.0d0 / arr
    case (2)
      rv = Inverse2by2(arr)
    case (3)
      rv = Inverse3by3(arr)
    case default
      rv = InverseGeneral(arr)
    end select

  end function InverseDoublePrec
!******************************************************************************
  real function Inverse2by2Real(arr) result(rv)

    real, dimension(2, 2), intent(in) :: arr
    dimension :: rv(2, 2)

    rv(1, 1) = arr(2, 2)
    rv(1, 2) = -arr(1, 2)
    rv(2, 1) = -arr(2, 1)
    rv(2, 2) = arr(1, 1)
    rv = (1.0d0 / Determinant2by2(arr)) * rv

  end function Inverse2by2Real
!******************************************************************************
  double precision function Inverse2by2DoublePrec(arr) result(rv)

    double precision, dimension(2, 2), intent(in) :: arr
    dimension :: rv(2, 2)

    rv(1, 1) = arr(2, 2)
    rv(1, 2) = -arr(1, 2)
    rv(2, 1) = -arr(2, 1)
    rv(2, 2) = arr(1, 1)
    rv = (1.0d0 / Determinant2by2(arr)) * rv

  end function Inverse2by2DoublePrec
!******************************************************************************
  real function Inverse3by3Real(arr) result(rv)

    real, dimension(3, 3), intent(in) :: arr
    dimension :: rv(3, 3)

    rv(1, 1) = Determinant2by2( &
      reshape((/arr(2,2), arr(3,2), arr(2,3), arr(3,3)/), (/2,2/)))
    rv(1, 2) = Determinant2by2( &
      reshape((/arr(1,3), arr(3,3), arr(1,2), arr(3,2)/), (/2,2/)))
    rv(1, 3) = Determinant2by2( &
      reshape((/arr(1,2), arr(2,2), arr(1,3), arr(2,3)/), (/2,2/)))
    rv(2, 1) = Determinant2by2( &
      reshape((/arr(2,3), arr(3,3), arr(2,1), arr(3,1)/), (/2,2/)))
    rv(2, 2) = Determinant2by2( &
      reshape((/arr(1,1), arr(3,1), arr(1,3), arr(3,3)/), (/2,2/)))
    rv(2, 3) = Determinant2by2( &
      reshape((/arr(1,3), arr(2,3), arr(1,1), arr(2,1)/), (/2,2/)))
    rv(3, 1) = Determinant2by2( &
      reshape((/arr(2,1), arr(3,1), arr(2,2), arr(3,2)/), (/2,2/)))
    rv(3, 2) = Determinant2by2( &
      reshape((/arr(1,2), arr(3,2), arr(1,1), arr(3,1)/), (/2,2/)))
    rv(3, 3) = Determinant2by2( &
      reshape((/arr(1,1), arr(2,1), arr(1,2), arr(2,2)/), (/2,2/)))
    rv = (1.0 / Determinant3by3(arr)) * rv

  end function Inverse3by3Real
!******************************************************************************
  double precision function Inverse3by3DoublePrec(arr) result(rv)

    double precision, dimension(3, 3), intent(in) :: arr
    dimension :: rv(3, 3)

    rv(1, 1) = Determinant2by2( &
      reshape((/arr(2,2), arr(3,2), arr(2,3), arr(3,3)/), (/2,2/)))
    rv(1, 2) = Determinant2by2( &
      reshape((/arr(1,3), arr(3,3), arr(1,2), arr(3,2)/), (/2,2/)))
    rv(1, 3) = Determinant2by2( &
      reshape((/arr(1,2), arr(2,2), arr(1,3), arr(2,3)/), (/2,2/)))
    rv(2, 1) = Determinant2by2( &
      reshape((/arr(2,3), arr(3,3), arr(2,1), arr(3,1)/), (/2,2/)))
    rv(2, 2) = Determinant2by2( &
      reshape((/arr(1,1), arr(3,1), arr(1,3), arr(3,3)/), (/2,2/)))
    rv(2, 3) = Determinant2by2( &
      reshape((/arr(1,3), arr(2,3), arr(1,1), arr(2,1)/), (/2,2/)))
    rv(3, 1) = Determinant2by2( &
      reshape((/arr(2,1), arr(3,1), arr(2,2), arr(3,2)/), (/2,2/)))
    rv(3, 2) = Determinant2by2( &
      reshape((/arr(1,2), arr(3,2), arr(1,1), arr(3,1)/), (/2,2/)))
    rv(3, 3) = Determinant2by2( &
      reshape((/arr(1,1), arr(2,1), arr(1,2), arr(2,2)/), (/2,2/)))
    rv = (1.0d0 / Determinant3by3(arr)) * rv

  end function Inverse3by3DoublePrec
!******************************************************************************
  real function InverseGeneralReal(arr) result(rv)

    real, dimension(:, :), intent(in) :: arr
    dimension :: rv(size(arr, 1), size(arr, 1))
    ! The augmented arrays, has the same number of rows and double the
    ! number of columns as the original matix.
    real, dimension(size(arr, 1), 2 * size(arr, 1)) :: aug
    integer :: rows, cols, i, j

    ! The number of rows and columns should be the same (since this must be
    ! a square matrix). But lets keep them separate for clarity reasons.
    rows = size(arr, 1)
    cols = size(arr, 2)
    ! Set the left hand side equal to the original matrix.
    aug(1:rows, 1:cols) = arr
    ! Set the right hand side equal to the indentity matrix.
    aug(1:rows, cols+1:2*cols) = 0.0
    do i = cols+1, 2*cols
      aug(i - cols, i) = 1.0
    end do

    ! This should zero out the lower left hand portion of the left hand side
    ! of the augmented matrix.
    do i = 1, rows - 1
      do j = i + 1, rows
        aug(j,:) = RowReduce(aug(i,:), aug(j,:), i)
      end do
    end do

    ! This should zero out the upper right hand portion of the left hand side
    ! of the augmented matrix.
    do i = rows, 2, -1
      do j = i - 1, 1, -1
        aug(j,:) = RowReduce(aug(i,:), aug(j,:), i)
      end do
    end do

    ! All that's left should be numbers on the diagonal of the left hand side
    ! of the augmented matrix. We need to normalize them to get the identity
    ! matrix.
    do i = 1, rows
      aug(i,:) = aug(i,:) / aug(i,i)
    end do

    ! The inverse array is the right hand part of the augmented matrix.
    rv = aug(1:rows, cols+1:2*cols)

  end function InverseGeneralReal
!******************************************************************************
  double precision function InverseGeneralDoublePrec(arr) result(rv)

    double precision, dimension(:, :), intent(in) :: arr
    dimension :: rv(size(arr, 1), size(arr, 1))
    ! The augmented arrays, has the same number of rows and double the
    ! number of columns as the original matix.
    double precision, dimension(size(arr, 1), 2 * size(arr, 1)) :: aug
    integer :: rows, cols, i, j

    ! The number of rows and columns should be the same (since this must be
    ! a square matrix). But lets keep them separate for clarity reasons.
    rows = size(arr, 1)
    cols = size(arr, 2)
    ! Set the left hand side equal to the original matrix.
    aug(1:rows, 1:cols) = arr
    ! Set the right hand side equal to the indentity matrix.
    aug(1:rows, cols+1:2*cols) = 0.0d0
    do i = cols+1, 2*cols
      aug(i - cols, i) = 1.0d0
    end do

    ! This should zero out the lower left hand portion of the left hand side
    ! of the augmented matrix.
    do i = 1, rows - 1
      do j = i + 1, rows
        aug(j,:) = RowReduce(aug(i,:), aug(j,:), i)
      end do
    end do

    ! This should zero out the upper right hand portion of the left hand side
    ! of the augmented matrix.
    do i = rows, 2, -1
      do j = i - 1, 1, -1
        aug(j,:) = RowReduce(aug(i,:), aug(j,:), i)
      end do
    end do

    ! All that's left should be numbers on the diagonal of the left hand side
    ! of the augmented matrix. We need to normalize them to get the identity
    ! matrix.
    do i = 1, rows
      aug(i,:) = aug(i,:) / aug(i,i)
    end do

    ! The inverse array is the right hand part of the augmented matrix.
    rv = aug(1:rows, cols+1:2*cols)

  end function InverseGeneralDoublePrec
!******************************************************************************
  real function RowReduceReal(row1, row2, pivotPos) result(rv)

    real, dimension(:), intent(in) :: row1
    real, dimension(:), intent(in) :: row2
    integer, intent(in) :: pivotPos
    dimension :: rv(size(row1))

    rv = row2 - row1 * row2(pivotPos) / row1(pivotPos)

  end function RowReduceReal
!******************************************************************************
  double precision function RowReduceDoublePrec(row1, row2, pivotPos) &
    result(rv)

    double precision, dimension(:), intent(in) :: row1
    double precision, dimension(:), intent(in) :: row2
    integer, intent(in) :: pivotPos
    dimension :: rv(size(row1))

    rv = row2 - row1 * row2(pivotPos) / row1(pivotPos)

  end function RowReduceDoublePrec
!******************************************************************************

end module MatrixMod
