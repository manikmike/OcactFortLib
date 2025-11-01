! $Id: RoundingMod.f90 1.27 2014/03/18 10:25:59EDT 404551 Development  $
module RoundingMod

!****h* source/RoundingMod
!  NAME
!  RoundingMod: OcactFortLib RoundingMod module.
!  REVISION
!  $Revision: 1.27 $
!  MODULE DESCRIPTION
!  The RoundingMod contains functions used to round a single value using
!  classic rounding (RoundCl) or round an array of values, either with
!  classic rounding (RoundCl) or with progressive rounding (RoundPr).
!  The function RoundMult rounds a single value or an array of values to
!  the desired multiple.
!  MODULE ROUTINES
!  RoundCl
!  RoundMult
!  RoundPr
!*****

  implicit none
  save

  private
  public :: RoundCl, RoundPr, RoundMult

!****f* RoundingMod/RoundCl
!  NAME
!  RoundCl: This function rounds a value or an array of values using
!  classic rounding.
!  DESCRIPTION
!  This function rounds a value or an array of values using classic rounding.
!
!  If the argument 'numDigits' is omitted, the function will round to
!  the nearest integer.
!  RETURNS
!  Integer, Real, or Double Precision scalar or array: The same type
!  (and size, if an array) as 'num'.
!  SYNOPSIS
!  integer function RoundCl(num, numDigits) result(numRound)
!  real function RoundCl(num, numDigits) result(numRound)
!  double precision function RoundCl(num, numDigits) result(numRound)
!  ARGUMENTS
!  * num: A value of type integer, real, or double precision, or a 1 to 7
!    dimensional array (of any size) of integers, reals, or double precisions.
!  * numDigits: (optional) Number of digits to the right of the decimal point to
!    which you want to round. A negative number rounds to the left of the
!    decimal point; zero to the nearest integer.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RoundCl
    module procedure RoundClRealScal, RoundClIntScal, RoundClDoublePrecScal, &
      RoundClRealArrDim1, RoundClIntArrDim1,RoundClDoublePrecArrDim1, &
      RoundClRealArrDim2, RoundClIntArrDim2, RoundClDoublePrecArrDim2, &
      RoundClRealArrDim3, RoundClIntArrDim3, RoundClDoublePrecArrDim3, &
      RoundClRealArrDim4, RoundClIntArrDim4, RoundClDoublePrecArrDim4, &
      RoundClRealArrDim5, RoundClIntArrDim5, RoundClDoublePrecArrDim5, &
      RoundClRealArrDim6, RoundClIntArrDim6, RoundClDoublePrecArrDim6, &
      RoundClRealArrDim7, RoundClIntArrDim7, RoundClDoublePrecArrDim7
  end interface RoundCl

!****f* RoundingMod/RoundPr
!  NAME
!  RoundPr: This function rounds the values in an array using progressive
!  rounding.
!  DESCRIPTION
!  This function rounds the values in an array using progressive rounding.
!  Progressive rounding is a procedure used to round a set
!  of ordered numbers which preserves the rounded total.
!
!  If the argument 'numDigits' is omitted, the function will round to
!  the nearest integer.
!
!  If the argument 'targetTotal' is present, the function will multiply
!  all elements of the array by the ratio of ‘targetTotal’ over the sum
!  of the elements of the array. This is done before the progressive rounding,
!  and forces the sum of the rounded array to equal ‘targetTotal’.
!
!  The sum of the elements of the rounded array will match the original sum,
!  or ‘targetTotal’, only to the extent allowed by the rounding specified by
!  ‘numDigits’ (or to the nearest integer, if ‘numDigits’ was omitted).
!
!  Below is an example showing how to progressively round a 4 element
!  array of 1.1, 1.4, 1.2, and 1.3 to the nearest integer.
!       i   - The ith number in a list
!     x(i)  - Array of i values to round, with x(0) defined to be 0
!     X(i)  - The sum of x(i)s from i = 0 to i
!   R[X(i)] - The rounded X(i)s to the nearest integer
!     r(i)  - The progressive rounded result by subtracting R[X(i)] - R[X(i-1)]
!
!                                         Classic
!     i    x(i)    X(i)  R[X(i)]   r(i)   Rounding
!     0    0.0     0.0      0
!     1    1.1     1.1      1       1        1
!     2    1.4     2.5      3       2        1
!     3    1.2     3.7      4       1        1
!     4    1.3     5        5       1        1
!    _____________________________________________
!     Sum    5                      5        4
!
!  The sum of the 4 element array is 5, but classic rounding would round each
!  of the 4 elements to 1, leaving the final sum of 4. Progressive rounding
!  rounds the elements to 1, 2, 1, and 1, preserving the sum of 5.
!  RETURNS
!  Integer, Real, or Double Precision array: The same type as 'num'.
!  SYNOPSIS
!  integer function RoundPr(num, numDigits, targetTotal) result(numRound)
!  real function RoundPr(num, numDigits, targetTotal) result(numRound)
!  double precision function RoundPr(num, numDigits, targetTotal) result(numRound)
!  ARGUMENTS
!  * num: 1 to 7 dimensional array (of any size) of integers, reals,
!    or double precisions.
!  * numDigits: (optional) Number of digits to the right of the decimal point to
!    which you want to round. A negative number rounds to the left of the
!    decimal point; zero to the nearest integer.
!  * targetTotal: (optional) The desired total of the array after
!    progressive rounding. The same type as 'num'.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RoundPr
    module procedure &
      RoundPrRealArrDim1, RoundPrIntArrDim1, RoundPrDoublePrecArrDim1, &
      RoundPrRealArrDim2, RoundPrIntArrDim2, RoundPrDoublePrecArrDim2, &
      RoundPrRealArrDim3, RoundPrIntArrDim3, RoundPrDoublePrecArrDim3, &
      RoundPrRealArrDim4, RoundPrIntArrDim4, RoundPrDoublePrecArrDim4, &
      RoundPrRealArrDim5, RoundPrIntArrDim5, RoundPrDoublePrecArrDim5, &
      RoundPrRealArrDim6, RoundPrIntArrDim6, RoundPrDoublePrecArrDim6, &
      RoundPrRealArrDim7, RoundPrIntArrDim7, RoundPrDoublePrecArrDim7
  end interface RoundPr

!****f* RoundingMod/RoundMult
!  NAME
!  RoundMult: This function rounds a value or an array of values to the
!  desired multiple.
!  DESCRIPTION
!  This function uses classic rounding to round a value or an array of values
!  to the desired multiple. The variable ‘num’ can also be negative (or contain
!  negative values) such that this function may return a negative value. Also,
!  ‘mult’ can be either positive or negative, but this does not affect the
!  return value of this function.
!  RETURNS
!  Integer, Real, or Double Precision scalar or array: The same type
!  (and size, if an array) as 'num'.
!  SYNOPSIS
!  integer function RoundMult(num, mult) result(numRound)
!  real function RoundMult(num, mult) result(numRound)
!  double precision function RoundMult(num, mult) result(numRound)
!  ARGUMENTS
!  * num: A value of type integer, real, or double precision, or a 1 to 7
!    dimensional array (of any size) of integers, reals, or double precisions.
!  * mult: The multiple to which you want to round 'num'.
!    A value of type integer, real, or double precision. If 'num' is an integer,
!    then 'mult' must also be an integer.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RoundMult
    module procedure &
      RoundMultRealIntScal, RoundMultRealDbleScal, &
      RoundMultRealRealScal, RoundMultDbleIntScal, &
      RoundMultDbleDbleScal, RoundMultDbleRealScal, &
      RoundMultIntIntScal, &
      RoundMultRealIntArrDim1, RoundMultRealDbleArrDim1, &
      RoundMultRealRealArrDim1, RoundMultDbleIntArrDim1, &
      RoundMultDbleDbleArrDim1, RoundMultDbleRealArrDim1, &
      RoundMultIntIntArrDim1, &
      RoundMultRealIntArrDim2, RoundMultRealDbleArrDim2, &
      RoundMultRealRealArrDim2, RoundMultDbleIntArrDim2, &
      RoundMultDbleDbleArrDim2, RoundMultDbleRealArrDim2, &
      RoundMultIntIntArrDim2, &
      RoundMultRealIntArrDim3, RoundMultRealDbleArrDim3, &
      RoundMultRealRealArrDim3, RoundMultDbleIntArrDim3, &
      RoundMultDbleDbleArrDim3, RoundMultDbleRealArrDim3, &
      RoundMultIntIntArrDim3, &
      RoundMultRealIntArrDim4, RoundMultRealDbleArrDim4, &
      RoundMultRealRealArrDim4, RoundMultDbleIntArrDim4, &
      RoundMultDbleDbleArrDim4, RoundMultDbleRealArrDim4, &
      RoundMultIntIntArrDim4, &
      RoundMultRealIntArrDim5, RoundMultRealDbleArrDim5, &
      RoundMultRealRealArrDim5, RoundMultDbleIntArrDim5, &
      RoundMultDbleDbleArrDim5, RoundMultDbleRealArrDim5, &
      RoundMultIntIntArrDim5, &
      RoundMultRealIntArrDim6, RoundMultRealDbleArrDim6, &
      RoundMultRealRealArrDim6, RoundMultDbleIntArrDim6, &
      RoundMultDbleDbleArrDim6, RoundMultDbleRealArrDim6, &
      RoundMultIntIntArrDim6, &
      RoundMultRealIntArrDim7, RoundMultRealDbleArrDim7, &
      RoundMultRealRealArrDim7, RoundMultDbleIntArrDim7, &
      RoundMultDbleDbleArrDim7, RoundMultDbleRealArrDim7, &
      RoundMultIntIntArrDim7
  end interface RoundMult

contains

!******************************************************************************
  real function RoundClRealScal(num, numDigits) result(numRound)

   ! Number array
   real, intent(in) :: num
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits

   integer :: numDigitsLocal

   if (present(numDigits)) then
      numDigitsLocal = numDigits
   else
      numDigitsLocal = 0
   end if

   ! Classic rounding
   if (num < 0.0) then
      numRound = (int(num * 10.0**numDigitsLocal - 0.5)) / 10.0**numDigitsLocal
   else
      numRound = (int(num * 10.0**numDigitsLocal + 0.5)) / 10.0**numDigitsLocal
   end if

  end function RoundClRealScal
!******************************************************************************
  double precision function RoundClDoublePrecScal(num, numDigits) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits

   integer :: numDigitsLocal

   if (present(numDigits)) then
      numDigitsLocal = numDigits
   else
      numDigitsLocal = 0
   end if

   ! Classic rounding
   if (num < 0.0d0) then
      numRound = (int(num * 10.0d0**numDigitsLocal - 0.5d0)) / &
            10.0d0**numDigitsLocal
   else
      numRound = (int(num * 10.0d0**numDigitsLocal + 0.5d0)) / &
            10.0d0**numDigitsLocal
   end if

  end function RoundClDoublePrecScal
!******************************************************************************
  integer function RoundClIntScal(num, numDigits) result(numRound)

   ! Number array
   integer, intent(in) :: num
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits

   integer :: numDigitsLocal

   if (present(numDigits)) then
      numDigitsLocal = numDigits
   else
      numDigitsLocal = 0
   end if

   ! Note: Even though this is the "integer" version of this function,
   ! we still use double precision constants. If all the numbers in this
   ! formula would be kept as integers, we'd end up doing integer division,
   ! which would end up truncating at the wrong times.
   if (num < 0) then
      numRound = (int(num * 10.0d0**numDigitsLocal - 0.5d0)) / &
            10.0d0**numDigitsLocal
   else
      numRound = (int(num * 10.0d0**numDigitsLocal + 0.5d0)) / &
            10.0d0**numDigitsLocal
   end if

  end function RoundClIntScal
!******************************************************************************
  real function RoundClRealArrDim1(num, numDigits) result(numRound)

   ! Number array
   real, intent(in) :: num(:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Classic rounding
   do i = 1, n
      numRound(i) = RoundCl(num(i), numDigits)
   end do

  end function RoundClRealArrDim1
!******************************************************************************
  double precision function RoundClDoublePrecArrDim1(num, numDigits) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Classic rounding
   do i = 1, n
      numRound(i) = RoundCl(num(i), numDigits)
   end do

  end function RoundClDoublePrecArrDim1
!******************************************************************************
  integer function RoundClIntArrDim1(num, numDigits) result(numRound)

   ! Number array
   integer, intent(in) :: num(:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Classic rounding
   do i = 1, n
      numRound(i) =  RoundCl(num(i), numDigits)
   end do

  end function RoundClIntArrDim1
!******************************************************************************
  real function RoundClRealArrDim2(num, numDigits) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundCl(num(i, j), numDigits)
      end do
   end do

  end function RoundClRealArrDim2
!******************************************************************************
  double precision function RoundClDoublePrecArrDim2(num, numDigits) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundCl(num(i, j), numDigits)
      end do
   end do

  end function RoundClDoublePrecArrDim2
!******************************************************************************
  integer function RoundClIntArrDim2(num, numDigits) result(numRound)

   ! Number array
   integer, intent(in) :: num(:, :)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundCl(num(i, j), numDigits)
      end do
   end do

  end function RoundClIntArrDim2
!******************************************************************************
  real function RoundClRealArrDim3(num, numDigits) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundCl(num(i, j, k), numDigits)
         end do
      end do
   end do

  end function RoundClRealArrDim3
!******************************************************************************
  double precision function RoundClDoublePrecArrDim3(num, numDigits) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundCl(num(i, j, k), numDigits)
         end do
      end do
   end do

  end function RoundClDoublePrecArrDim3
!******************************************************************************
  integer function RoundClIntArrDim3(num, numDigits) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundCl(num(i, j, k), numDigits)
         end do
      end do
   end do

  end function RoundClIntArrDim3
!******************************************************************************
  real function RoundClRealArrDim4(num, numDigits) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundCl(num(i, j, k, l), numDigits)
            end do
         end do
      end do
   end do

  end function RoundClRealArrDim4
!******************************************************************************
  double precision function RoundClDoublePrecArrDim4(num, numDigits) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundCl(num(i, j, k, l), numDigits)
            end do
         end do
      end do
   end do

  end function RoundClDoublePrecArrDim4
!******************************************************************************
  integer function RoundClIntArrDim4(num, numDigits) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundCl(num(i, j, k, l), numDigits)
            end do
         end do
      end do
   end do

  end function RoundClIntArrDim4
!******************************************************************************
  real function RoundClRealArrDim5(num, numDigits) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundCl(num(i, j, k, l, m), numDigits)
               end do
            end do
         end do
      end do
   end do

  end function RoundClRealArrDim5
!******************************************************************************
  double precision function RoundClDoublePrecArrDim5(num, numDigits) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundCl(num(i, j, k, l, m), numDigits)
               end do
            end do
         end do
      end do
   end do

  end function RoundClDoublePrecArrDim5
!******************************************************************************
  integer function RoundClIntArrDim5(num, numDigits) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundCl(num(i, j, k, l, m), numDigits)
               end do
            end do
         end do
      end do
   end do

  end function RoundClIntArrDim5
!******************************************************************************
  real function RoundClRealArrDim6(num, numDigits) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundCl(num(i, j, k, l, m, n), numDigits)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundClRealArrDim6
!******************************************************************************
  double precision function RoundClDoublePrecArrDim6(num, numDigits) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundCl(num(i, j, k, l, m, n), numDigits)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundClDoublePrecArrDim6
!******************************************************************************
  integer function RoundClIntArrDim6(num, numDigits) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundCl(num(i, j, k, l, m, n), numDigits)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundClIntArrDim6
!******************************************************************************
  real function RoundClRealArrDim7(num, numDigits) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundCl(num(i, j, k, l, m, n, o), numDigits)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundClRealArrDim7
!******************************************************************************
  double precision function RoundClDoublePrecArrDim7(num, numDigits) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundCl(num(i, j, k, l, m, n, o), numDigits)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundClDoublePrecArrDim7
!******************************************************************************
  integer function RoundClIntArrDim7(num, numDigits) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundCl(num(i, j, k, l, m, n, o), numDigits)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundClIntArrDim7
!******************************************************************************
  real function RoundPrRealArrDim1(num, numDigits, targetTotal) result(numRound)

   ! Number array
   real, intent(in) :: num(:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   real, optional, intent(in) :: targetTotal
   dimension :: numRound(size(num))

   ! The prSum array starts at 0 because prSum(0)=0 is needed to sum the array.
   real :: prSum(0:size(num))
   real :: ratio
   integer :: n, i

   prSum = 0.0
   n = size(num)

   ! The sum of num
   do i = 1, n
      prSum(i) = prSum(i-1) + num(i)
   end do

   if(present(targetTotal)) then
      ratio = targetTotal / prSum(n)
      prSum = prSum * ratio
   end if

   ! prSum rounded to numDigits
   prSum = RoundCl(prSum, numDigits)

   ! Progressive Rounding
   do i = 1, n
      numRound(i) = prSum(i) - prSum(i-1)
   end do

  end function RoundPrRealArrDim1
!******************************************************************************
  double precision function RoundPrDoublePrecArrDim1(num, numDigits, targetTotal) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   double precision, optional, intent(in) :: targetTotal
   dimension :: numRound(size(num))

   ! The prSum array starts at 0 because prSum(0)=0 is needed to sum the array.
   double precision :: prSum(0:size(num))
   double precision :: ratio
   integer :: n, i

   prSum = 0.0d0
   n = size(num)

   ! The sum of num
   do i = 1, n
      prSum(i) = prSum(i-1) + num(i)
   end do

   if(present(targetTotal)) then
      ratio = targetTotal / prSum(n)
      prSum = prSum * ratio
   end if

   ! prSum rounded to numDigits
   prSum = RoundCl(prSum, numDigits)

   ! Progressive Rounding
   do i = 1, n
      numRound(i) = prSum(i) - prSum(i-1)
   end do

  end function RoundPrDoublePrecArrDim1
!******************************************************************************
  integer function RoundPrIntArrDim1(num, numDigits, targetTotal) result(numRound)

   ! Number array
   integer, intent(in) :: num(:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   integer, optional, intent(in) :: targetTotal
   dimension :: numRound(size(num))

   ! The prSum array starts at 0 because prSum(0)=0 is needed to sum the array.
   double precision :: prSum(0:size(num))
   double precision :: ratio
   integer :: n, i

   prSum = 0.0d0
   n = size(num)

   ! The sum of num
   do i = 1, n
      prSum(i) = prSum(i-1) + num(i)
   end do

   if(present(targetTotal)) then
      ratio = dble(targetTotal) / prSum(n)
      prSum = prSum * ratio
   end if

   ! prSum rounded to numDigits
   prSum = RoundCl(prSum, numDigits)

   ! Progressive Rounding
   do i = 1, n
      numRound(i) = prSum(i) - prSum(i-1)
   end do

  end function RoundPrIntArrDim1
!******************************************************************************
  real function RoundPrRealArrDim2(num, numDigits, targetTotal) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   real, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   ! Local 1-dimension array
   real, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrRealArrDim2
!******************************************************************************
  double precision function RoundPrDoublePrecArrDim2(num, numDigits, targetTotal) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   double precision, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   ! Local 1-dimension array
   double precision, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrDoublePrecArrDim2
!******************************************************************************
  integer function RoundPrIntArrDim2(num, numDigits, targetTotal) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   integer, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   ! Local 1-dimension array
   integer, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrIntArrDim2
!******************************************************************************
  real function RoundPrRealArrDim3(num, numDigits, targetTotal) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   real, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3))

   ! Local 1-dimension array
   real, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrRealArrDim3
!******************************************************************************
  double precision function RoundPrDoublePrecArrDim3(num, numDigits, targetTotal) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   double precision, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3))

   ! Local 1-dimension array
   double precision, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrDoublePrecArrDim3
!******************************************************************************
  integer function RoundPrIntArrDim3(num, numDigits, targetTotal) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   integer, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3))

   ! Local 1-dimension array
   integer, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrIntArrDim3
!******************************************************************************
  real function RoundPrRealArrDim4(num, numDigits, targetTotal) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   real, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4))

   ! Local 1-dimension array
   real, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrRealArrDim4
!******************************************************************************
  double precision function RoundPrDoublePrecArrDim4(num, numDigits, targetTotal) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   double precision, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4))

   ! Local 1-dimension array
   double precision, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrDoublePrecArrDim4
!******************************************************************************
  integer function RoundPrIntArrDim4(num, numDigits, targetTotal) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   integer, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4))

   ! Local 1-dimension array
   integer, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrIntArrDim4
!******************************************************************************
  real function RoundPrRealArrDim5(num, numDigits, targetTotal) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   real, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   ! Local 1-dimension array
   real, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrRealArrDim5
!******************************************************************************
  double precision function RoundPrDoublePrecArrDim5(num, numDigits, targetTotal) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   double precision, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   ! Local 1-dimension array
   double precision, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrDoublePrecArrDim5
!******************************************************************************
  integer function RoundPrIntArrDim5(num, numDigits, targetTotal) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   integer, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   ! Local 1-dimension array
   integer, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrIntArrDim5
!******************************************************************************
  real function RoundPrRealArrDim6(num, numDigits, targetTotal) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   real, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
         size(num, DIM = 6))

   ! Local 1-dimension array
   real, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrRealArrDim6
!******************************************************************************
  double precision function RoundPrDoublePrecArrDim6(num, numDigits, targetTotal) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   double precision, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
         size(num, DIM = 6))

   ! Local 1-dimension array
   double precision, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrDoublePrecArrDim6
!******************************************************************************
  integer function RoundPrIntArrDim6(num, numDigits, targetTotal) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   integer, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
         size(num, DIM = 6))

   ! Local 1-dimension array
   integer, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrIntArrDim6
!******************************************************************************
  real function RoundPrRealArrDim7(num, numDigits, targetTotal) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   real, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
         size(num, DIM = 6), size(num, DIM = 7))

   ! Local 1-dimension array
   real, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrRealArrDim7
!******************************************************************************
  double precision function RoundPrDoublePrecArrDim7(num, numDigits, targetTotal) &
      result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   double precision, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
         size(num, DIM = 6), size(num, DIM = 7))

   ! Local 1-dimension array
   double precision, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrDoublePrecArrDim7
!******************************************************************************
  integer function RoundPrIntArrDim7(num, numDigits, targetTotal) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:,:,:)
   ! Number of digits to the right of the decimal point to which you want to
   ! round. A negative number rounds to the left of the decimal point;
   ! zero to the nearest integer.
   integer, optional, intent(in) :: numDigits
   integer, optional, intent(in) :: targetTotal
   ! Return value
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
         size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
         size(num, DIM = 6), size(num, DIM = 7))

   ! Local 1-dimension array
   integer, dimension(size(num)) :: dim1Arr

   ! Reshape num to a 1-dimensional array
   dim1Arr = reshape(num, (/size(num)/))
   ! Progressive Rounding
   dim1Arr = RoundPr(dim1Arr, numDigits, targetTotal)
   ! Reshape the progressively rounded 1-dim array to the shape of num
   numRound = reshape(dim1Arr, shape(num))

  end function RoundPrIntArrDim7
!******************************************************************************
  real function RoundMultRealIntScal(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult

   ! Rounds a number to the desired multiple
   numRound = RoundCl(num / mult) * mult

  end function RoundMultRealIntScal
!******************************************************************************
  real function RoundMultRealDbleScal(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult

   ! Rounds a number to the desired multiple
   numRound = RoundCl(num / mult) * mult

  end function RoundMultRealDbleScal
!******************************************************************************
  real function RoundMultRealRealScal(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num
   ! Multiple to which you want to round num.
   real, intent(in) :: mult

   ! Rounds a number to the desired multiple
   numRound = RoundCl(num / mult) * mult

  end function RoundMultRealRealScal
!******************************************************************************
  double precision function RoundMultDbleIntScal(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult

   ! Rounds a number to the desired multiple
   numRound = RoundCl(num / mult) * mult

  end function RoundMultDbleIntScal
!******************************************************************************
  double precision function RoundMultDbleDbleScal(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult

   ! Rounds a number to the desired multiple
   numRound = RoundCl(num / mult) * mult

  end function RoundMultDbleDbleScal
!******************************************************************************
  double precision function RoundMultDbleRealScal(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num
   ! Multiple to which you want to round num.
   real, intent(in) :: mult

   ! Rounds a number to the desired multiple
   numRound = RoundCl(num / mult) * mult

  end function RoundMultDbleRealScal
!******************************************************************************
  integer function RoundMultIntIntScal(num, mult) result(numRound)

   ! Number array
   integer, intent(in) :: num
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult

   ! Note: Even though this is the "integer" version of this function,
   ! we still use a double precision 'mult'. If all the numbers in this
   ! formula would be kept as integers, we'd end up doing integer division,
   ! which would end up truncating at the wrong times.

   ! Rounds a number to the desired multiple
   numRound = RoundCl(num / dble(mult)) * mult

  end function RoundMultIntIntScal
!******************************************************************************
  real function RoundMultRealIntArrDim1(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Multiple rounding
   do i = 1, n
      numRound(i) = RoundMult(num(i), mult)
   end do

  end function RoundMultRealIntArrDim1
!******************************************************************************
  real function RoundMultRealDbleArrDim1(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Multiple rounding
   do i = 1, n
      numRound(i) = RoundMult(num(i), mult)
   end do

  end function RoundMultRealDbleArrDim1
!******************************************************************************
  real function RoundMultRealRealArrDim1(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Multiple rounding
   do i = 1, n
      numRound(i) = RoundMult(num(i), mult)
   end do

  end function RoundMultRealRealArrDim1
!******************************************************************************
  double precision function RoundMultDbleIntArrDim1(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Multiple rounding
   do i = 1, n
      numRound(i) = RoundMult(num(i), mult)
   end do

  end function RoundMultDbleIntArrDim1
!******************************************************************************
  double precision function RoundMultDbleDbleArrDim1(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Multiple rounding
   do i = 1, n
      numRound(i) = RoundMult(num(i), mult)
   end do

  end function RoundMultDbleDbleArrDim1
!******************************************************************************
  double precision function RoundMultDbleRealArrDim1(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Multiple rounding
   do i = 1, n
      numRound(i) = RoundMult(num(i), mult)
   end do

  end function RoundMultDbleRealArrDim1
!******************************************************************************
  integer function RoundMultIntIntArrDim1(num, mult) result(numRound)

   ! Number array
   integer, intent(in) :: num(:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num))

   integer :: n, i

   ! Size of the input number array
   n = size(num)

   ! Multiple rounding
   do i = 1, n
      numRound(i) = RoundMult(num(i), mult)
   end do

  end function RoundMultIntIntArrDim1
!******************************************************************************
  real function RoundMultRealIntArrDim2(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundMult(num(i, j), mult)
      end do
   end do

  end function RoundMultRealIntArrDim2
!******************************************************************************
  real function RoundMultRealDbleArrDim2(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundMult(num(i, j), mult)
      end do
   end do

  end function RoundMultRealDbleArrDim2
!******************************************************************************
  real function RoundMultRealRealArrDim2(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundMult(num(i, j), mult)
      end do
   end do

  end function RoundMultRealRealArrDim2
!******************************************************************************
  double precision function RoundMultDbleIntArrDim2(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundMult(num(i, j), mult)
      end do
   end do

  end function RoundMultDbleIntArrDim2
!******************************************************************************
  double precision function RoundMultDbleDbleArrDim2(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundMult(num(i, j), mult)
      end do
   end do

  end function RoundMultDbleDbleArrDim2
!******************************************************************************
  double precision function RoundMultDbleRealArrDim2(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundMult(num(i, j), mult)
      end do
   end do

  end function RoundMultDbleRealArrDim2
!******************************************************************************
  integer function RoundMultIntIntArrDim2(num, mult) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2))

   integer :: dim1, dim2
   integer :: i, j

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)

   ! Classic rounding
   do j = 1, dim2
      do i = 1, dim1
         numRound(i, j) = RoundMult(num(i, j), mult)
      end do
   end do

  end function RoundMultIntIntArrDim2
!******************************************************************************
  real function RoundMultRealIntArrDim3(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundMult(num(i, j, k), mult)
         end do
      end do
   end do

  end function RoundMultRealIntArrDim3
!******************************************************************************
  real function RoundMultRealDbleArrDim3(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundMult(num(i, j, k), mult)
         end do
      end do
   end do

  end function RoundMultRealDbleArrDim3
!******************************************************************************
  real function RoundMultRealRealArrDim3(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundMult(num(i, j, k), mult)
         end do
      end do
   end do

  end function RoundMultRealRealArrDim3
!******************************************************************************
  double precision function RoundMultDbleIntArrDim3(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundMult(num(i, j, k), mult)
         end do
      end do
   end do

  end function RoundMultDbleIntArrDim3
!******************************************************************************
  double precision function RoundMultDbleDbleArrDim3(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundMult(num(i, j, k), mult)
         end do
      end do
   end do

  end function RoundMultDbleDbleArrDim3
!******************************************************************************
  double precision function RoundMultDbleRealArrDim3(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundMult(num(i, j, k), mult)
         end do
      end do
   end do

  end function RoundMultDbleRealArrDim3
!******************************************************************************
  integer function RoundMultIntIntArrDim3(num, mult) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3))

   integer :: dim1, dim2, dim3
   integer :: i, j, k

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)

   ! Classic rounding
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            numRound(i, j, k) = RoundMult(num(i, j, k), mult)
         end do
      end do
   end do

  end function RoundMultIntIntArrDim3
!******************************************************************************
  real function RoundMultRealIntArrDim4(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundMult(num(i, j, k, l), mult)
            end do
         end do
      end do
   end do

  end function RoundMultRealIntArrDim4
!******************************************************************************
  real function RoundMultRealDbleArrDim4(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundMult(num(i, j, k, l), mult)
            end do
         end do
      end do
   end do

  end function RoundMultRealDbleArrDim4
!******************************************************************************
  real function RoundMultRealRealArrDim4(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundMult(num(i, j, k, l), mult)
            end do
         end do
      end do
   end do

  end function RoundMultRealRealArrDim4
!******************************************************************************
  double precision function RoundMultDbleIntArrDim4(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundMult(num(i, j, k, l), mult)
            end do
         end do
      end do
   end do

  end function RoundMultDbleIntArrDim4
!******************************************************************************
  double precision function RoundMultDbleDbleArrDim4(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundMult(num(i, j, k, l), mult)
            end do
         end do
      end do
   end do

  end function RoundMultDbleDbleArrDim4
!******************************************************************************
  double precision function RoundMultDbleRealArrDim4(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundMult(num(i, j, k, l), mult)
            end do
         end do
      end do
   end do

  end function RoundMultDbleRealArrDim4
!******************************************************************************
  integer function RoundMultIntIntArrDim4(num, mult) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4))

   integer :: dim1, dim2, dim3, dim4
   integer :: i, j, k, l

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)

   ! Classic rounding
   do l = 1, dim4
      do k = 1, dim3
         do j = 1, dim2
            do i = 1, dim1
               numRound(i, j, k, l) = RoundMult(num(i, j, k, l), mult)
            end do
         end do
      end do
   end do

  end function RoundMultIntIntArrDim4
!******************************************************************************
  real function RoundMultRealIntArrDim5(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundMult(num(i, j, k, l, m), mult)
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealIntArrDim5
!******************************************************************************
  real function RoundMultRealDbleArrDim5(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundMult(num(i, j, k, l, m), mult)
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealDbleArrDim5
!******************************************************************************
  real function RoundMultRealRealArrDim5(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundMult(num(i, j, k, l, m), mult)
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealRealArrDim5
!******************************************************************************
  double precision function RoundMultDbleIntArrDim5(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundMult(num(i, j, k, l, m), mult)
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleIntArrDim5
!******************************************************************************
  double precision function RoundMultDbleDbleArrDim5(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundMult(num(i, j, k, l, m), mult)
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleDbleArrDim5
!******************************************************************************
  double precision function RoundMultDbleRealArrDim5(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundMult(num(i, j, k, l, m), mult)
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleRealArrDim5
!******************************************************************************
  integer function RoundMultIntIntArrDim5(num, mult) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5))

   integer :: dim1, dim2, dim3, dim4, dim5
   integer :: i, j, k, l, m

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)

   ! Classic rounding
   do m = 1, dim5
      do l = 1, dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
               numRound(i, j, k, l, m) = &
                     RoundMult(num(i, j, k, l, m), mult)
               end do
            end do
         end do
      end do
   end do

  end function RoundMultIntIntArrDim5
!******************************************************************************
  real function RoundMultRealIntArrDim6(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundMult(num(i, j, k, l, m, n), mult)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealIntArrDim6
!******************************************************************************
  real function RoundMultRealDbleArrDim6(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundMult(num(i, j, k, l, m, n), mult)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealDbleArrDim6
!******************************************************************************
  real function RoundMultRealRealArrDim6(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundMult(num(i, j, k, l, m, n), mult)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealRealArrDim6
!******************************************************************************
  double precision function RoundMultDbleIntArrDim6(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundMult(num(i, j, k, l, m, n), mult)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleIntArrDim6
!******************************************************************************
  double precision function RoundMultDbleDbleArrDim6(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundMult(num(i, j, k, l, m, n), mult)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleDbleArrDim6
!******************************************************************************
  double precision function RoundMultDbleRealArrDim6(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundMult(num(i, j, k, l, m, n), mult)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleRealArrDim6
!******************************************************************************
  integer function RoundMultIntIntArrDim6(num, mult) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6
   integer :: i, j, k, l, m, n

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)

   ! Classic rounding
   do n = 1, dim6
      do m = 1, dim5
         do l = 1, dim4
            do k = 1, dim3
               do j = 1, dim2
                  do i = 1, dim1
                     numRound(i, j, k, l, m, n) = &
                           RoundMult(num(i, j, k, l, m, n), mult)
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultIntIntArrDim6
!******************************************************************************
  real function RoundMultRealIntArrDim7(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundMult(num(i, j, k, l, m, n, o), mult)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealIntArrDim7
!******************************************************************************
  real function RoundMultRealDbleArrDim7(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundMult(num(i, j, k, l, m, n, o), mult)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealDbleArrDim7
!******************************************************************************
  real function RoundMultRealRealArrDim7(num, mult) result(numRound)

   ! Number array
   real, intent(in) :: num(:,:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundMult(num(i, j, k, l, m, n, o), mult)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultRealRealArrDim7
!******************************************************************************
  double precision function RoundMultDbleIntArrDim7(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundMult(num(i, j, k, l, m, n, o), mult)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleIntArrDim7
!******************************************************************************
  double precision function RoundMultDbleDbleArrDim7(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   double precision, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundMult(num(i, j, k, l, m, n, o), mult)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleDbleArrDim7
!******************************************************************************
  double precision function RoundMultDbleRealArrDim7(num, mult) result(numRound)

   ! Number array
   double precision, intent(in) :: num(:,:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   real, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundMult(num(i, j, k, l, m, n, o), mult)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultDbleRealArrDim7
!******************************************************************************
  integer function RoundMultIntIntArrDim7(num, mult) result(numRound)

   ! Number array
   integer, intent(in) :: num(:,:,:,:,:,:,:)
   ! Multiple to which you want to round num.
   integer, intent(in) :: mult
   dimension :: numRound(size(num, DIM = 1), size(num, DIM = 2), &
       size(num, DIM = 3), size(num, DIM = 4), size(num, DIM = 5), &
       size(num, DIM = 6), size(num, DIM = 7))

   integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
   integer :: i, j, k, l, m, n, o

   ! Size of the input number array
   dim1 = size(num, DIM = 1)
   dim2 = size(num, DIM = 2)
   dim3 = size(num, DIM = 3)
   dim4 = size(num, DIM = 4)
   dim5 = size(num, DIM = 5)
   dim6 = size(num, DIM = 6)
   dim7 = size(num, DIM = 7)

   ! Classic rounding
   do o = 1, dim7
      do n = 1, dim6
         do m = 1, dim5
            do l = 1, dim4
               do k = 1, dim3
                  do j = 1, dim2
                     do i = 1, dim1
                        numRound(i, j, k, l, m, n, o) = &
                              RoundMult(num(i, j, k, l, m, n, o), mult)
                     end do
                  end do
               end do
            end do
         end do
      end do
   end do

  end function RoundMultIntIntArrDim7
!******************************************************************************
end module RoundingMod
