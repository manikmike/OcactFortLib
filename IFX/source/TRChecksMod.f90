! $Id: TRChecksMod.f90 1.26 7/29/2021 15:31:34 Morris,M. $
module TRChecksMod

!****h* source/TRChecksMod
!  NAME
!  TRChecksMod: OcactFortLib TRChecksMod module.
!  REVISION
!  $Revision: 1.25 $
!  MODULE DESCRIPTION
!  The TRChecksMod module contains subroutines and functions that are helpful
!  for TR-related checks. For example, MKSCheckSandbox can be used to
!  check the program files against the latest versions in MKS.  Other routines
!  can be used to check randomly selected data in an array. The random input
!  data point can then be checked against the original data file to confirm
!  the program read in the data correctly.
!
!  This module uses the RandomMod. This means that there has to first
!  be a call to InitRandomModule to initialize the RandomMod. This call
!  is not located anywhere within this module. Instead, the user must decide
!  where the call should be made (usually somewhere in the "initialization"
!  part of the program).
!  MODULE ROUTINES
!  CopyOutOfSyncGitFiles
!  CopyOutOfSyncMKSFiles
!  LBoundPart
!  GitCheckRepo
!  MKSCheckSandbox
!  RandDataFromArr
!  RandDataFromArrMess
!  RunLogInit
!  RunLogWrapUp
!  SetRandDataFromArrMessFields
!*****

  use FilesMod
  use PlatformsMod
  use RandomMod
  use StringsMod
  use VersionMod
  implicit none
  save

  private
  public :: RandDataFromArr, RandDataFromArrMess, SetRandDataFromArrMessFields
  public :: LBoundPart
  public :: MKSCheckSandbox, GitCheckRepo
  public :: CopyOutOfSyncMKSFiles, CopyOutOfSyncGitFiles
  public :: RunLogInit, RunLogWrapUp

  ! The maximum length for a charcter string in this module.
  integer, parameter :: MAX_CHAR_LENGTH = 10000
  ! The number of decimal places to print out for the RandDataFromArrMess.
  integer, parameter :: MESS_NUM_DEC = 6

  ! Format string to use for outputting messages from the subroutine
  ! RandDataFromArrMess. This can be changed using the subroutine
  ! SetRandDataFromArrMessFields.
  character (len = MAX_CHAR_LENGTH) :: fmt = "(A20, A40, A20)"

  ! Allow RandDataFromArr to be the generic name for these subroutines.
!****f* TRChecksMod/RandDataFromArr
!  NAME
!  RandDataFromArr: The function returns a random data point from an array.
!  DESCRIPTION
!  This function takes an array of any number of elements, and any rank
!  between 1 and 7. It returns a random data point from that array. The
!  indices of the selected random point are returned in 'indices'. The
!  array 'lbounds' should hold the lower bound of each rank of 'arr'.
!  This will almost always be passed in using the function lbound:
!  lbound(arr). In order for this function to pick a random element, the
!  RandomMod must have been initialized (with a call to
!  InitRandomModule()). Here is an example:
!      use RandomMod
!      use TRChecksMod
!      ! numPeople holds the number of people by sex and age for each year.
!      integer, dimension(2000:2080, 1:2, 0:100) :: numPeople
!      ! Since numPeople is of rank 3 (i.e. a 3-dimension array, by year,
!      ! sex and age), then this variable should be an array to hold 3
!      ! integers.
!      integer, dimension(3) :: indices
!      ! This variable will hold the random number returned.
!      integer :: rand
!      integer :: i, j, k
!      ...  !
!      ...  ! Some code here to load data into numPeople.
!      ...  !
!      ! Use exactly one of the following two lines. The first will give
!      ! a reproducable random number stream -- i.e. you'll get the same
!      ! results every time you run the program. To change the results
!      ! and still get a reproducable stream, just change the seed, which
!      ! is currently set to 1. It can be any positive integer. To get a
!      ! different random number every time you run the program use the
!      ! second line.
!      !call InitRandomModule((/1/))
!      call InitRandomModule(1)
!      rand = RandDataFromArr(numPeople, lbound(numPeople), indices)
!      i = indices(1)
!      j = indices(2)
!      k = indices(3)
!      ! This next line should print the same integer number twice.
!      write(6, '(2I)') numPeople(i, j, k), rand
!  In the above example notice that you must have two "use" statements.
!  Also notice that you must call InitRandomModule before and calls to
!  RandDataFromArr. Finally, notice that 'indices' is used solely for
!  the purpose of returning the values of the indices at which 'arr' was
!  referenced. It is not given any values to be passed into
!  RandDataFromArr, and if it were, they would be erased. In all cases,
!  the second argument passed to RandDataFromArr will probably be
!  lbound(arr).
!  RETURNS
!  Integers, reals, or double precision (the same type as 'arr'): Random data
!  point.
!  SYNOPSIS
!  integer function RandDataFromArr(arr, lbounds, indices) result(randData)
!  real function RandDataFromArr(arr, lbounds, indices) result(randData)
!  double precision function RandDataFromArr(arr, lbounds, indices) &
!    result(randData)
!  ARGUMENTS
!  * arr: Array of integers, reals, or double precisions (of any size, and of
!    rank between 1 and 7).
!  * lbounds: Integer array (of size equal to the rank of 'arr'). Lower bound of
!    each rank of 'arr'.
!  * indices: Integer array (of size equal to the rank of 'arr'). Indices of the
!    selected random point.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RandDataFromArr
    ! These functions get passed 1-dimensional arrays.
    module procedure RandDataFromIntArrDim1, RandDataFromRealArrDim1, &
      RandDataFromDoublePrecArrDim1
    ! These functions get passed 2-dimensional arrays.
    module procedure RandDataFromIntArrDim2, RandDataFromRealArrDim2, &
      RandDataFromDoublePrecArrDim2
    ! These functions get passed 3-dimensional arrays.
    module procedure RandDataFromIntArrDim3, RandDataFromRealArrDim3, &
      RandDataFromDoublePrecArrDim3
    ! These functions get passed 4-dimensional arrays.
    module procedure RandDataFromIntArrDim4, RandDataFromRealArrDim4, &
      RandDataFromDoublePrecArrDim4
    ! These functions get passed 5-dimensional arrays.
    module procedure RandDataFromIntArrDim5, RandDataFromRealArrDim5, &
      RandDataFromDoublePrecArrDim5
    ! These functions get passed 6-dimensional arrays.
    module procedure RandDataFromIntArrDim6, RandDataFromRealArrDim6, &
      RandDataFromDoublePrecArrDim6
    ! These functions get passed 7-dimensional arrays.
    module procedure RandDataFromIntArrDim7, RandDataFromRealArrDim7, &
      RandDataFromDoublePrecArrDim7
  end interface RandDataFromArr

  ! Allow RandDataFromArrMess to be the generic name for these subroutines.
!****f* TRChecksMod/RandDataFromArrMess
!  NAME
!  RandDataFromArrMess: The function returns a random data point from an array
!  printed in a message.
!  DESCRIPTION
!  This subroutine is similar to RandDataFromArr, but instead of returning
!  the indices of the array that were used (in 'indices', as RandDataFromArr) it
!  prints out a nice generic message stating the indices. The array passed
!  in, 'arr', has the same restrictions as RandDataFromArr (rank no bigger
!  than 7) and 'lbounds' is exactly the same as RandDataFromArr. A string that
!  is a description of the data that the array holds is in 'descr'. Often
!  'descr' may just hold the name of the array, 'arr', as a text string.
!  The array 'indexNames' holds strings that describe each dimension of
!  the 'arr'. The output message is printed to the file pointed to by
!  'fileNum'. It is assumed that this file has been previously opened. If
!  'fileNum' is 6, then the message will go to standard output, which is
!  usually the screen (unless it has been redirected).
!  If 'fixedWidth' is false or omitted, the output message will be of
!  the format:
!    descr: indexNames(1) = ???, ..., data = ???
!  where, the question marks are replaced by a number depending on the
!  results of the random selection; and 3 dots are replaced by any more
!  indexNames if the array passed in was bigger than rank 1.
!  If 'fixedWidth' is true, the output message will be the same
!  as above, but the formatting will be different  -- it will be more
!  of a tabular format (with fixed field sizes). The portion "descr:"
!  will be in the first field; the portion "indexNames(1) = ???, ..."
!  will be in the second field; and the portion "data = ???" will
!  be in the third field. The default sizes for these 3 fields are
!  20, 40, and 20, respectively. If you want to change these sizes, use
!  the subroutine SetRandDataFromArrMessFields. See the documentation
!  for SetRandDataFromArrMessFields for more details.
!  If 'prec' is omitted, then the data field will be printed out with 6
!  decimal places of precision. If it is not omitted, it will be printed
!  out with however many decimal places are specified by 'prec'.
!  Here is an example:
!      use RandomMod
!      use TRChecksMod
!      ! numPeople holds the number of people by sex and age for each year.
!      integer, dimension(2000:2080, 1:2, 0:100) :: numPeople
!      ! Since numPeople is of rank 3 (i.e. a 3-dimension array, by year,
!      ! sex and age), then this variable should be an array to hold 3
!      ! integers.
!      integer, dimension(3) :: indices
!      ! This variable will hold the random number returned.
!      integer :: rand
!      integer :: i, j, k
!      ...  !
!      ...  ! Some code here to load data into numPeople. Let's assume
!      ...  ! that numPeople(2043, 2, 14) = 125169.
!      ...  !
!      ! Use exactly one of the following two lines. The first will give
!      ! a reproducable random number stream -- i.e. you'll get the same
!      ! results every time you run the program. To change the results
!      ! and still get a reproducable stream, just change the seed, which
!      ! is currently set to 1. It can be any positive integer. To get a
!      ! different random number every time you run the program use the
!      ! second line.
!      !call InitRandomModule((/1/))
!      call InitRandomModule(1)
!      call RandDataFromArrMess(numPeople, lbound(numPeople), "numPeople", &
!             (/"year", "sex", "age"/), 6)
!      ! This above line should print to the screen a message like this:
!      !   numPeople: year = 2043, sex = 2, age = 14, data = 125169
!      ! Of course, the above message will vary depending on the random
!      ! array element that is actually chosen.
!  SYNOPSIS
!  subroutine RandDataFromArrMess(arr, lbounds, descr, indexNames, fileNum, &
!    fixedWidth, prec)
!  ARGUMENTS
!  * arr: Array of integers, reals, or double precisions (of any size, and of
!    rank between 1 and 7).
!  * lbounds: Integer array (of size equal to the rank of 'arr'). Lower bound of
!    each rank of 'arr'.
!  * descr: Character string (of variable length). Description of the data that
!    the array holds.
!  * indexNames: Character string array (of size equal to the rank of 'arr',
!    and the size of each string is variable). Strings that desribe each
!    dimension of the array.
!  * fileNum: Output file as an integer.
!  * fixedWidth: (optional) Format of output message. False for standard format,
!    and true for fixed field sizes.
!  * prec: (optional) Number of decimal places in the output data file as an
!    integer.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RandDataFromArrMess
    ! These functions get passed 1-dimensional arrays.
    module procedure RandDataFromIntArrDim1Mess, RandDataFromRealArrDim1Mess, &
      RandDataFromDoublePrecArrDim1Mess
    ! These functions get passed 2-dimensional arrays.
    module procedure RandDataFromIntArrDim2Mess, RandDataFromRealArrDim2Mess, &
      RandDataFromDoublePrecArrDim2Mess
    ! These functions get passed 3-dimensional arrays.
    module procedure RandDataFromIntArrDim3Mess, RandDataFromRealArrDim3Mess, &
      RandDataFromDoublePrecArrDim3Mess
    ! These functions get passed 4-dimensional arrays.
    module procedure RandDataFromIntArrDim4Mess, RandDataFromRealArrDim4Mess, &
      RandDataFromDoublePrecArrDim4Mess
    ! These functions get passed 5-dimensional arrays.
    module procedure RandDataFromIntArrDim5Mess, RandDataFromRealArrDim5Mess, &
      RandDataFromDoublePrecArrDim5Mess
    ! These functions get passed 6-dimensional arrays.
    module procedure RandDataFromIntArrDim6Mess, RandDataFromRealArrDim6Mess, &
      RandDataFromDoublePrecArrDim6Mess
    ! These functions get passed 7-dimensional arrays.
    module procedure RandDataFromIntArrDim7Mess, RandDataFromRealArrDim7Mess, &
      RandDataFromDoublePrecArrDim7Mess
  end interface RandDataFromArrMess

contains
!******************************************************************************
!****f* TRChecksMod/SetRandDataFromArrMessFields
!  NAME
!  SetRandDataFromArrMessFields: This subroutine is used to set the field sizes
!  used by the subroutine RandDataFromArrMess.
!  DESCRIPTION
!  This subroutine is used to set the field sizes used by the subroutine
!  RandDataFromArrMess.  The value in 'field1' is the size to be used
!  for the first field of output (the description string). The value in
!  'field2' is the size to be used for the second field of output (the
!  data indices field). The value in 'field3' is the size to be used
!  for the third field of output (the data point field). See the
!  documentation of RandDataFromArrMess for more details.
!  SYNOPSIS

  subroutine SetRandDataFromArrMessFields(field1, field2, field3)

!  ARGUMENTS
!  * field1: Size to be used for the first field of output.
!  * field2: Size to be used for the second field of output.
!  * field3: Size to be used for the third field of output.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: field1
    integer, intent(in) :: field2
    integer, intent(in) :: field3
!*****

    fmt = "(A" // trim(IntToAsc(field1)) // &
         ", A" // trim(IntToAsc(field2)) // &
         ", A" // trim(IntToAsc(field3)) // ")"

  end subroutine SetRandDataFromArrMessFields
!******************************************************************************
  subroutine WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
    fixedWidth)

    integer, intent(in) :: fileNum
    character (len = *), intent(in) :: descr
    character (len = *), dimension(:), intent(in) :: indexNames
    integer, dimension(:), intent(in) :: indices
    character (len = *), intent(in) :: randDataAsc
    logical, intent(in) :: fixedWidth
    character (len = MAX_CHAR_LENGTH) :: mess1, mess2, mess3
    integer :: i

    mess1 = trim(descr) // ":"
    mess2 = trim(indexNames(1)) // " = " // trim(IntToAsc(indices(1)))
    do i = 2, size(indices)
      mess2 = trim(mess2) // ", " // trim(indexNames(i)) // " = " // &
        trim(IntToAsc(indices(i)))
    end do
    mess3 = "data = " // trim(randDataAsc)
    if (fixedWidth) then
      write(fileNum, trim(fmt)) mess1, mess2, mess3
    else
      write(fileNum, '(A)') trim(mess1) // " " // trim(mess2) // &
        ", " // trim(mess3)
    end if

  end subroutine WriteMessage
!******************************************************************************
  subroutine RandDataFromIntArrDim1Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 1
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension(lbounds(1):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromIntArrDim1Mess
!******************************************************************************
  subroutine RandDataFromRealArrDim1Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 1
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension(lbounds(1):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromRealArrDim1Mess
!******************************************************************************
  subroutine RandDataFromDoublePrecArrDim1Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 1
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension(lbounds(1):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromDoublePrecArrDim1Mess
!******************************************************************************
  subroutine RandDataFromIntArrDim2Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 2
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension(lbounds(1):, lbounds(2):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromIntArrDim2Mess
!******************************************************************************
  subroutine RandDataFromRealArrDim2Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 2
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension(lbounds(1):, lbounds(2):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromRealArrDim2Mess
!******************************************************************************
  subroutine RandDataFromDoublePrecArrDim2Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 2
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension(lbounds(1):, lbounds(2):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromDoublePrecArrDim2Mess
!******************************************************************************
  subroutine RandDataFromIntArrDim3Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 3
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromIntArrDim3Mess
!******************************************************************************
  subroutine RandDataFromRealArrDim3Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 3
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromRealArrDim3Mess
!******************************************************************************
  subroutine RandDataFromDoublePrecArrDim3Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 3
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromDoublePrecArrDim3Mess
!******************************************************************************
  subroutine RandDataFromIntArrDim4Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 4
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromIntArrDim4Mess
!******************************************************************************
  subroutine RandDataFromRealArrDim4Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 4
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromRealArrDim4Mess
!******************************************************************************
  subroutine RandDataFromDoublePrecArrDim4Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 4
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromDoublePrecArrDim4Mess
!******************************************************************************
  subroutine RandDataFromIntArrDim5Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 5
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension(lbounds(1):, lbounds(2):, lbounds(3):, &
      lbounds(4):, lbounds(5):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromIntArrDim5Mess
!******************************************************************************
  subroutine RandDataFromRealArrDim5Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 5
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension(lbounds(1):, lbounds(2):, lbounds(3):, &
      lbounds(4):, lbounds(5):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromRealArrDim5Mess
!******************************************************************************
  subroutine RandDataFromDoublePrecArrDim5Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 5
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension(lbounds(1):, lbounds(2):, lbounds(3):, &
      lbounds(4):, lbounds(5):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromDoublePrecArrDim5Mess
!******************************************************************************
  subroutine RandDataFromIntArrDim6Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 6
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromIntArrDim6Mess
!******************************************************************************
  subroutine RandDataFromRealArrDim6Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 6
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromRealArrDim6Mess
!******************************************************************************
  subroutine RandDataFromDoublePrecArrDim6Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 6
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromDoublePrecArrDim6Mess
!******************************************************************************
  subroutine RandDataFromIntArrDim7Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 7
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):, lbounds(7):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromIntArrDim7Mess
!******************************************************************************
  subroutine RandDataFromRealArrDim7Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 7
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):, lbounds(7):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromRealArrDim7Mess
!******************************************************************************
  subroutine RandDataFromDoublePrecArrDim7Mess(arr, lbounds, descr, &
    indexNames, fileNum, fixedWidth, prec)

    integer, parameter :: NUM_DIMS = 7
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):, lbounds(7):), intent(in) :: arr
    character (len = *), intent(in) :: descr
    character (len = *), dimension(NUM_DIMS), intent(in) :: indexNames
    integer, intent(in) :: fileNum
    logical, optional, intent(in) :: fixedWidth
    integer, optional, intent(in) :: prec
    integer, dimension(NUM_DIMS) :: indices
    character (len = MAX_CHAR_LENGTH) :: randDataAsc

    if (present(prec)) then
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), prec)
    else
      randDataAsc = &
        NumToAsc(RandDataFromArr(arr, lbound(arr), indices), MESS_NUM_DEC)
    end if
    if (present(fixedWidth)) then
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        fixedWidth)
    else
      call WriteMessage(fileNum, descr, indexNames, indices, randDataAsc, &
        .false.)
    end if

  end subroutine RandDataFromDoublePrecArrDim7Mess
!******************************************************************************
  integer function RandDataFromIntArrDim1(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 1
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension(lbounds(1):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1))

  end function RandDataFromIntArrDim1
!******************************************************************************
  real function RandDataFromRealArrDim1(arr, lbounds, indices) result(randData)

    integer, parameter :: NUM_DIMS = 1
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension(lbounds(1):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1))

  end function RandDataFromRealArrDim1
!******************************************************************************
  double precision function RandDataFromDoublePrecArrDim1(arr, lbounds, &
    indices) result(randData)

    integer, parameter :: NUM_DIMS = 1
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension(lbounds(1):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1))

  end function RandDataFromDoublePrecArrDim1
!******************************************************************************
  integer function RandDataFromIntArrDim2(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 2
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension(lbounds(1):, lbounds(2):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2))

  end function RandDataFromIntArrDim2
!******************************************************************************
  real function RandDataFromRealArrDim2(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 2
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension(lbounds(1):, lbounds(2):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2))

  end function RandDataFromRealArrDim2
!******************************************************************************
  double precision function RandDataFromDoublePrecArrDim2(arr, lbounds, &
    indices) result(randData)

    integer, parameter :: NUM_DIMS = 2
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension(lbounds(1):, lbounds(2):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2))

  end function RandDataFromDoublePrecArrDim2
!******************************************************************************
  integer function RandDataFromIntArrDim3(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 3
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3))

  end function RandDataFromIntArrDim3
!******************************************************************************
  real function RandDataFromRealArrDim3(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 3
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3))

  end function RandDataFromRealArrDim3
!******************************************************************************
  double precision function RandDataFromDoublePrecArrDim3(arr, lbounds, &
    indices) result(randData)

    integer, parameter :: NUM_DIMS = 3
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3))

  end function RandDataFromDoublePrecArrDim3
!******************************************************************************
  integer function RandDataFromIntArrDim4(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 4
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4))

  end function RandDataFromIntArrDim4
!******************************************************************************
  real function RandDataFromRealArrDim4(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 4
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4))

  end function RandDataFromRealArrDim4
!******************************************************************************
  double precision function RandDataFromDoublePrecArrDim4(arr, lbounds, &
    indices) result(randData)

    integer, parameter :: NUM_DIMS = 4
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4))

  end function RandDataFromDoublePrecArrDim4
!******************************************************************************
  integer function RandDataFromIntArrDim5(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 5
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension(lbounds(1):, lbounds(2):, lbounds(3):, &
      lbounds(4):, lbounds(5):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), indices(5))

  end function RandDataFromIntArrDim5
!******************************************************************************
  real function RandDataFromRealArrDim5(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 5
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension(lbounds(1):, lbounds(2):, lbounds(3):, &
      lbounds(4):, lbounds(5):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), indices(5))

  end function RandDataFromRealArrDim5
!******************************************************************************
  double precision function RandDataFromDoublePrecArrDim5(arr, lbounds, &
    indices) result(randData)

    integer, parameter :: NUM_DIMS = 5
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension(lbounds(1):, lbounds(2):, lbounds(3):, &
      lbounds(4):, lbounds(5):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), indices(5))

  end function RandDataFromDoublePrecArrDim5
!******************************************************************************
  integer function RandDataFromIntArrDim6(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 6
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):) , intent(in):: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), &
      indices(5), indices(6))

  end function RandDataFromIntArrDim6
!******************************************************************************
  real function RandDataFromRealArrDim6(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 6
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), &
      indices(5), indices(6))

  end function RandDataFromRealArrDim6
!******************************************************************************
  double precision function RandDataFromDoublePrecArrDim6(arr, lbounds, &
    indices) result(randData)

    integer, parameter :: NUM_DIMS = 6
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), &
      indices(5), indices(6))

  end function RandDataFromDoublePrecArrDim6
!******************************************************************************
  integer function RandDataFromIntArrDim7(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 7
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    integer, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):, lbounds(7):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), &
      indices(5), indices(6), indices(7))

  end function RandDataFromIntArrDim7
!******************************************************************************
  real function RandDataFromRealArrDim7(arr, lbounds, indices) &
    result(randData)

    integer, parameter :: NUM_DIMS = 7
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    real, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):, lbounds(7):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), &
      indices(5), indices(6), indices(7))

  end function RandDataFromRealArrDim7
!******************************************************************************
  double precision function RandDataFromDoublePrecArrDim7(arr, lbounds, &
    indices) result(randData)

    integer, parameter :: NUM_DIMS = 7
    integer, dimension(NUM_DIMS), intent(in) :: lbounds
    double precision, dimension( &
      lbounds(1):, lbounds(2):, lbounds(3):, lbounds(4):, lbounds(5):, &
      lbounds(6):, lbounds(7):), intent(in) :: arr
    integer, dimension(NUM_DIMS), intent(out) :: indices

    indices = GetRandIndices(lBounds, ubound(arr))
    randData = arr(indices(1), indices(2), indices(3), indices(4), &
      indices(5), indices(6), indices(7))

  end function RandDataFromDoublePrecArrDim7
!******************************************************************************
  integer function GetRandIndices(lBounds, uBounds) result(rv)

    integer, dimension(:), intent(in) :: lBounds
    integer, dimension(:), intent(in) :: uBounds
    dimension :: rv(size(lBounds))
    integer :: i

    do i = 1, size(lBounds)
      rv(i) = RandFromUnif(lBounds(i), uBounds(i))
    end do

  end function GetRandIndices
!******************************************************************************
!****f* TRChecksMod/LBoundPart
!  NAME
!  LBoundPart: This function returns the lower bounds of an array.
!  DESCRIPTION
!  This function returns the lower bounds of an array along the dimensions
!  specified in 'str'. The array itself is not passed to the function;
!  instead the lower bounds of the array are passed in as 'lBounds'.
!  These are easily obtained using the Fortran instrinsic function,
!  lbound, on the array.
!
!  The string 'str' is up to 7 characters long and
!  should consist of only the letters 't' or 'f' (upper or lower case).
!  Each 't' means that the lower bound of that dimension should be
!  returned and each 'f' means that it should be skipped. The dimension
!  corresponds to the placement of the character in the string.
!
!  For example, if the array 'arr' is dimensioned by year (2006 - 2085),
!  age (20 - 64), and sex (1 - 2), and you want the lower bounds of the
!  last two dimension, then use this function call:
!    LBoundPart(lbound(arr), "FTT"). 
!  This will return the following array
!  of 7 integers containing a 20, then a 1, and then 5 undefined values.
!  When passing the results of this function as the second argument to
!  the function 'RandDataFromArrMess', then 'RandDataFromArrMess' is
!  careful to make sure not to try to access any of the undefined values
!  of the array.
!  RETURNS
!  1-dimensional array of integers (of size 7): Lower bounds of an arrary
!  along the dimensions specified in 'str'.
!  SYNOPSIS

  integer function LBoundPart(lBounds, str) result(rv)

!  ARGUMENTS
!  * lBounds: Integer array (of size equal to the rank of 'arr'). Lower bounds
!    of the array.
!  * str: Up to 7 characters long and should consist of only the letters 't'
!    or 'f'.
!  ARGUMENT DECLARATIONS
    integer, dimension(:), intent(in) :: lBounds
    character (len = *), intent(in) :: str
!*****

    allocatable :: rv(:)
    integer :: i, rvInd, trueCount

    trueCount = 0
    do i = 1, len(str)
      select case(str(i:i))
      case ("T", "t")
        trueCount = trueCount + 1
      case ("F", "f")
        ! Skip False because only counting the number of True.
      case default
        write(6, '(A)') "Error in LBoundPart()."
        write(6, '(A)') "  Invalid character found in string str: " // str
        stop
      end select
    end do

    allocate(rv(trueCount))

    rvInd = 0
    do i = 1, len(str)
      select case(str(i:i))
      case ("T", "t")
        rvInd = rvInd + 1
        rv(rvInd) = lbounds(i)
      case ("F", "f")
        ! Nothing to do for this case. We're skipping this lbound.
      case default
        write(6, '(A)') "Error in LBoundPart()."
        write(6, '(A)') "  Invalid character found in string str: " // str
        stop
      end select
    end do

  end function LBoundPart
!******************************************************************************
!****f* TRChecksMod/MKSCheckSandbox
!  NAME
!  MKSCheckSandbox: This function calls the CheckSandbox program from the OCACT
!  Utilities Library (OUL).
!  DESCRIPTION
!  This function calls the CheckSandbox program from the OCACT Utilities
!  Library (OUL). It passes 'sandboxLocation' as the input directory to
!  CheckSandbox, and passes 'outFile' as the name of the output file
!  that will be created. See the CheckSandbox documentation from the
!  OUL for more details on how this program works.
!
!  The return value for this function is the value that gets returned by the
!  'CheckSandbox' program. This value should be negative if no sandbox
!  was checked (possibly due to an error occuring). If the sandbox was
!  checked, then the return value (0 or greater) represents the number
!  of files that differed from the files on the server.
!
!  If the 'warn' argument is passed in and it is false, then no warning message
!  will be printed. Otherwise, a warning meassage will be printed to
!  standard-out if the return value is at least 1. A list of files that
!  differ will be printed as part of this warning message.
!  RETURNS
!  Integer: Number of files with differences.
!  SYNOPSIS

  integer function MKSCheckSandbox(sandboxLocation, outFile, warn) result(rv)

!  ARGUMENTS
!  * sandboxLocation: Input directory to CheckSandbox.
!  * outFile: Name of the output file.
!  * warn: (optional) Warning message if files differ.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: sandboxLocation
    character (len = *), intent(in) :: outFile
    logical, optional, intent(in) :: warn
!*****
    logical :: writeWarning

    ! The variables "sandboxLocation" and "outFile" can contain spaces in them
    ! and Fortran will just pass them around as a regular variable. However,
    ! when sending them to the command line, a space will cause them to be
    ! broken up. So enclose them in double quotes to make sure the command
    ! line intepreter keeps them together.
    rv = SystemCall("CheckSandbox """ // trim(sandboxLocation) // """" // &
      " /v /h /s /o """ // trim(outFile) // """")

    if (rv > 0) then
      if (present(warn)) then
        writeWarning = warn
      else
        writeWarning = .true.
      end if
    else
      writeWarning = .false.
    end if
    
    if (writeWarning) then
      if (rv == 1) then
        write(6, '(A)') "WARNING: The following " // trim(IntToAsc(rv)) // &
          " file does not match the latest version on the MKS server:"
      else if (rv > 1) then
        write(6, '(A)') "WARNING: The following " // trim(IntToAsc(rv)) // &
          " files do not match the latest version on the MKS server:"
      end if
      rv = SystemCall("CheckSandbox " // trim(sandboxLocation) // " /d")
    end if

  end function MKSCheckSandbox
!******************************************************************************
!****f* TRChecksMod/CopyOutOfSyncMKSFiles
!  NAME
!  CopyOutOfSyncMKSFiles: This subroutine is used to copy over any MKS/PTC
!  Integrity files that are out-of-sync into a specified location.
!  DESCRIPTION
!  This subroutine is used to make a copy of files that are out-of-sync with
!  the MKS/PTC Integrity system. The files are copied into the 'outLocation'
!  directory, as specified by the user. If that directory doesn't already
!  exist, then it will be created if there are any out-of-sync files.
!
!  To determine which files are out of sync, there should be a call to the
!  MKSCheckSandbox function. If the user has already called this function,
!  and the results are in an output file, then the name of that output file
!  should be passed in to this subroutine as 'mksCheckLocation'.
!
!  If the user has not already called MKSCheckSandbox, then this subroutine
!  will call it. In that case, the user should pass in the directory containing
!  the sandbox as 'mksCheckLocation'.
!  SYNOPSIS

  subroutine CopyOutOfSyncMKSFiles(outLocation, mksCheckLocation)

!  ARGUMENTS
!  * outLocation: Location where any out-of-sync files will be copied.
!  * mksCheckLocation: This can either be a directory name or a file name.
!    If it is a file name, then it should be the name of the output of a
!    previous call to MKSCheckSandbox. If it is a directory name then it
!    should be the location of the MKS/PTC sandbox to use (and an internal
!    call to MKSCheckSandbox will be made). 
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: outLocation
    character (len = *), intent(in) :: mksCheckLocation
!*****
    logical :: doesFileExist, doesDirExist
    integer :: tempUnitNum
    logical :: isOpen
    integer :: rvInt
    logical :: rvLogical
    integer :: numOOSFiles
    character (len = 1000) :: tempFileName
    integer :: fileSize
    integer :: i

    ! Use the OS's TMP folder to create a name for the file that is output
    ! from the MKSCheckSandbox function.
    rvInt = GetEnviron("TMP", tempFileName)
    tempFileName = trim(tempfileName) // "\OFL_OOSFiles.txt"
    ! First determine if mksCheckLocation is a file or a directory.
    inquire(file = trim(mksCheckLocation), exist = doesFileExist)
    doesDirExist = CheckDir(trim(mksCheckLocation))
    ! Based on what it is, make sure we have a file located in tempFileName
    ! that contains the output of a call to MKSCheckSambox.
    if (doesFileExist) then
      ! If the file already exists, then we just need to copy it over to a
      ! temp. location to be ready to process. We don't want to open and read
      ! the actual file itself, because if the calling program still has the
      ! file open, we don't want to re-open it.
      rvInt = SystemCall("copy """ // trim(mksCheckLocation) // """ " // &
        trim(tempFileName) // " > nul")
    else if (doesDirExist) then
      ! If we're dealing with a directory, then that means that it should be
      ! a sandbox directory. So we first need to check the sandbox to see
      ! what files are out of sync, before proceeding with copying them over.
      numOOSFiles = MKSCheckSandbox(trim(mksCheckLocation), trim(tempFileName))
    else
      write(6, '(A)') "Error in CopyOutOfSyncMKSFiles. The following " // &
        "mksCheckLocation does not exist:"
      write(6, '(A)') "  " // trim(mksCheckLocation)
      stop
    end if

    ! Now, we wil need to read the contents of the tempfileName file to see
    ! which files are out-of-sync.
    ! But we first must find an available unit number for temporary use.
    do tempUnitNum = 100, 10000
      inquire(tempUnitNum, opened = isOpen)
      if (.not. isOpen) then
        exit
      end if
    end do

    ! Check the size of the temp. file to make sure it's a valid file.
    ! (Note, if the program was run from somewhere other than where the
    ! sandbox is located, then MKSCheckSandbox will not work, but will
    ! instead leave us with an empty file. We don't want the program to
    ! crash in such a case, so just print a warning message and continue.)
    inquire(file = trim(tempFileName), size = fileSize)
    if (fileSize == 0) then
      ! If the file is empty (size = 0), then there must have been an issue
      ! with the CheckSandbox call. Print a message (but don't terminate).
      write(6, '(A)') &
        "WARNING: Empty file detected in 'CopyOutOfSyncMKSFiles': " // &
        "File created by call to 'MKSCheckSandbox' is empty."
      write(6, '(A)') "  (called with 'mksCheckLocation' = '" // &
        trim(mksCheckLocation) // "')"
    else
      ! If it's not an empty file, then open the file, process it, and close it.
      open(tempUnitNum, file = trim(tempfileName), status = "old")
      ! Skip the first 3 lines.
      do i = 1, 3
        read(tempUnitNum, *)
      end do
      ! Read the 4th line to see if there are any out-of-sync files.
      ! Note: We don't rely on the possibly already set numOOSFiles
      ! variable, as its value will only sometimes have already been
      ! set, i.e., when this subroutine called MKSCheckSandbox. But in
      ! the scenario where it didn't call it, then the variable won't
      ! yet be set.  So we'll just set it here in any case.
      read(tempUnitNum, '(44X, I)') numOOSFiles
      ! If there is at least 1 out-of-sync file, then create the
      ! outLocation. It's OK if the outLocation already exists - the call
      ! to MakeDir won't hurt that.
      if (numOOSFiles > 0) then
        rvLogical = MakeDir(trim(outLocation))
      end if
      ! Skip the remaining 6 lines of the header.
      do i = 1, 6
        read(tempUnitNum, *)
      end do
      ! Read through the body of the file, processing each line.
      call ProcessMKSCheckSandboxFileBody(trim(outLocation), tempUnitNum)
      ! Close the file.
      close(tempUnitNum)
    end if

    ! Delete the temporary file that was used.
    rvLogical = DelFile(trim(tempfileName))

  end subroutine CopyOutOfSyncMKSFiles
!******************************************************************************
  subroutine ProcessMKSCheckSandboxFileBody(outLocation, unitNum)

    character (len = *), intent(in) :: outLocation
    integer, intent(in) :: unitNum

    character (len = 1000) :: line
    character (len = 1000) :: fileName
    character (len = 1000) :: nextToken
    ! First token in the file containing the MKS/PTC check results, indicating
    ! whether or not the file was in sync. Should be either "YES" or "NO".
    character (len = 1000) :: inSyncYesNo
    integer :: ios
    integer :: rv

    do
      read(unitNum, '(A)', iostat = ios) line
      ! Check if we're reached the end of the file.
      if(ios /= 0) then
        exit
      end if
      ! Parse the line read in. The first item should either be "YES" or "NO"
      ! indicating whether or not the file is in sync.
      inSyncYesNo = ParseLine(line)
      ! The next portion of the line contains the file name. It could have
      ! spaces in it, so don't use ParseLine. Instead, use the rest of the line
      ! until the revision number. Use 'adjustl' to remove leading spaces.
      fileName = adjustl(line(1:index(line, "(Rev.") - 1))
      ! Decide what to do, depending on whether or not the file was in sync.
      if (trim(inSyncYesNo) == "YES") then
        ! File is in sync. Make sure to delete the file from outLocation
        ! in case a version of it was sitting in there from a previous run.
        ! If it wasn't there, the DelFile function will do nothing.
        ! First, however, we just need fileName to be the actual name of the
        ! file, not the full path of where the original file is location.
        ! To do this, search for the last slash delimitter, add one, and take
        ! from there till the end of the string.
        fileName = fileName(index(fileName, SlashString(), .true.) + 1:)
        rv = DelFile(trim(outLocation) // SlashString() // trim(fileName))
      else if (trim(inSyncYesNo) == "NO") then
        ! File is out of sync. Copy it to fileLocation. Put quotes around
        ! the file name, in case there are spaces in the name. Use /y switch
        ! to automatically overwrite any file that may be there.
        rv = SystemCall("copy /y """ // trim(fileName) // """ " // &
          trim(outLocation) // " > nul")
      else
        write(6, '(A)') &
          "Error reading CheckMKSSandbox() output. Invalid inSyncYesNo string"
        write(6, '(A)') "      inSyncYesNo: " // trim(inSyncYesNo)
        write(6, '(A)') "Remainder of line: " // trim(line)
        stop
      end if
    end do

  end subroutine ProcessMKSCheckSandboxFileBody
!******************************************************************************
!****f* TRChecksMod/GitCheckRepo
!  NAME
!  GitCheckRepo: This function calls the CheckRepo program from the OCACT
!  Utilities Library (OUL).
!  DESCRIPTION
!  This function calls the CheckRepo program from the OCACT Utilities
!  Library (OUL). It passes 'repoLocation' as the input directory to
!  CheckRepo, and passes 'outFile' and optionally, 'outFileDetailed' as the
!  names of the output files that will be created. See the CheckRepo
!  documentation from the OUL for more details on how this program works.
!
!  The return value for this function is the value that gets returned by the
!  'CheckRepo' program. This value should be negative if no repository
!  was checked (possibly due to an error occuring). If the repository was
!  checked, then the return value (0 or greater) represents the number
!  of files that differed from the files on the server.
!
!  If the 'warn' argument is passed in and it is false, then no warning message
!  will be printed. Otherwise, a warning meassage will be printed to
!  standard-out if the return value is at least 1. A list of files that
!  differ will be printed as part of this warning message.
!  RETURNS
!  Integer: Number of files with differences.
!  SYNOPSIS

  integer function GitCheckRepo(repoLocation, outFile, outFileDetailed, &
    warn) result(rv)

!  ARGUMENTS
!  * repoLocation: Input directory to CheckRepo.
!  * outFile: Name of the output file.
!  * outFileDetailed (optional): Name of the detailed output file.
!  * warn: (optional) Warning message if files differ.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: repoLocation
    character (len = *), intent(in) :: outFile
    character (len = *), optional, intent(in) :: outFileDetailed
    logical, optional, intent(in) :: warn
!*****
    logical :: writeWarning

    ! The variables "repoLocation", "outFile", and "outFileDetailed" can
    ! contain spaces in them and Fortran will just pass them around as a 
    ! regular variable. However, when sending them to the command line, a 
    ! space will cause them to be broken up. So enclose them in double
    ! quotes to make sure the command line intepreter keeps them together.
    if (present(outFileDetailed)) then
      rv = SystemCall("CheckRepo """ // trim(repoLocation) // """" // &
        " /o """ // trim(outFileDetailed) // """ > """ // trim(outFile) &
        // """")
    else
      rv = SystemCall("CheckRepo """ // trim(repoLocation) // """" // &
        " > """ // trim(outFile) // """")
    end if

    if (rv > 0) then
      if (present(warn)) then
        writeWarning = warn
      else
        writeWarning = .true.
      end if
    else
      writeWarning = .false.
    end if

    if (writeWarning) then
      if (rv == 1) then
        write(6, '(A)') "WARNING: The following " // trim(IntToAsc(rv)) // &
          " file does not match the latest version in the Git repository:"
      else if (rv > 1) then
        write(6, '(A)') "WARNING: The following " // trim(IntToAsc(rv)) // &
          " files do not match the latest version in the Git repository:"
      end if
      rv = SystemCall("CheckRepo """ // trim(repoLocation) // """ /d")
    end if

  end function GitCheckRepo
!******************************************************************************
!****f* TRChecksMod/CopyOutOfSyncGitFiles
!  NAME
!  CopyOutOfSyncGitFiles: This subroutine is used to copy over any Git
!  files that are out-of-sync into a specified location.
!  DESCRIPTION
!  This subroutine is used to make a copy of files that are out-of-sync with
!  the Git system. The files are copied into a subdirectory of the program's
!  output folder, passed in via 'outFolder'. If this subdirectory doesn't
!  already exist, then it will be created if there are any out-of-sync files.
!
!  To determine which files are out of sync, there should be a call to the
!  GitCheckRepo function. If the user has already called this function,
!  and the results are in an output file, then the name of that output file
!  should be passed in to this subroutine as 'gitCheckLocation'.
!
!  If the user has not already called GitCheckRepo, then this subroutine
!  will call it. In that case, the user should pass in the directory containing
!  the repository as 'gitCheckLocation'.
!  SYNOPSIS

  subroutine CopyOutOfSyncGitFiles(outFolder, gitCheckLocation)

!  ARGUMENTS
!  * outFolder: Location of output folder.
!  * gitCheckLocation: This can either be a directory name or a file name.
!    If it is a file name, then it should be the name of the output of a
!    previous call to GitCheckRepo. If it is a directory name then it
!    should be the location of the Git repository to use (and an internal
!    call to GitCheckRepo will be made).
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: outFolder
    character (len = *), intent(in) :: gitCheckLocation
!*****
    logical :: doesFileExist, doesDirExist
    integer :: tempUnitNum, tempUnitNum3, tempUnitNum4
    logical :: isOpen
    integer :: rvInt
    logical :: rvLogical
    integer :: numOOSFiles
    character (len = 1000) :: tempFileName
    character (len = 1000) :: tempFileName2
    character (len = 1000) :: tempFileName3
    character (len = 1000) :: tempFileName4
    integer :: fileSize
    integer :: i
    character (len = 5) :: tempRead
    character (len = 1000) :: tempString
    character (len = 1000) :: choppedTempString
    character (len = 1000) :: line
    integer :: ios
    integer :: slashLoc
    character (len = 1000) :: checkFolderToUse
    character (len = 24) :: readArray
    character (len = 1000) :: repoRootLocation
    integer :: iChar

    ! Use the OS's TMP folder to create a name for the file that is output
    ! from the GitCheckRepo function.
    rvInt = GetEnviron("TMP", tempFileName)
    tempFileName = trim(tempfileName) // "\OFL_OOSFiles.txt"
    rvInt = GetEnviron("TMP", tempFileName2)
    tempFileName2 = trim(tempfileName2) // "\OFL_OOSFiles2.txt"
    ! First determine if gitCheckLocation is a file or a directory.
    inquire(file = trim(gitCheckLocation), exist = doesFileExist)
    doesDirExist = CheckDir(trim(gitCheckLocation))
    ! Based on what it is, make sure we have a file located in tempFileName
    ! that contains the output of a call to GitCheckRepo.
    if (doesFileExist) then
      ! If the file already exists, then we just need to copy it over to a
      ! temp. location to be ready to process. We don't want to open and read
      ! the actual file itself, because if the calling program still has the
      ! file open, we don't want to re-open it.
      rvInt = SystemCall("copy """ // trim(gitCheckLocation) // """ " // &
        trim(tempFileName) // " > nul")
    else if (doesDirExist) then
      ! If we're dealing with a directory, then that means that it should be
      ! a repository directory. So we first need to check the repository to see
      ! what files are out of sync, before proceeding with copying them over.
      numOOSFiles = GitCheckRepo(trim(gitCheckLocation), trim(tempFileName), &
        trim(tempFileName2))
    else
      write(6, '(A)') "Error in CopyOutOfSyncGitFiles. The following " // &
        "gitCheckLocation does not exist:"
      write(6, '(A)') "  " // trim(gitCheckLocation)
      stop
    end if

    ! Now, we wil need to read the contents of the tempfileName file to see
    ! which files are out-of-sync.
    ! But we first must find an available unit number for temporary use.
    do tempUnitNum = 100, 10000
      inquire(tempUnitNum, opened = isOpen)
      if (.not. isOpen) then
        exit
      end if
    end do

    ! Check the size of the temp. file to make sure it's a valid file.
    ! (Note, if the program was run from somewhere other than where the
    ! repository is located, then GitCheckRepo will not work, but will
    ! instead leave us with an empty file. We don't want the program to
    ! crash in such a case, so just print a warning message and continue.)
    inquire(file = trim(tempFileName), size = fileSize)
    if (fileSize == 0) then
      ! If the file is empty (size = 0), then there must have been an issue
      ! with the GitCheckRepo call. Print a message (but don't terminate).
      write(6, '(A)') &
        "WARNING: Empty file detected in 'CopyOutOfSyncGitFiles': " // &
        "File created by call to 'GitCheckRepo' is empty."
      write(6, '(A)') "  (called with 'gitCheckLocation' = '" // &
        trim(gitCheckLocation) // "')"
    else
      ! Step 1: Let's read the directory we are checking from the
      ! "OFL_OOSFiles.txt" file line four.
      open(tempUnitNum, file = trim(tempFileName), status = "old")
      read(tempUnitNum, *)
      read(tempUnitNum, *)
      read(tempUnitNum, *)
      ! The location of the repo or sub-repo starts at column 25
      read(tempUnitNum, '(a24, a)') readArray, checkFolderToUse
      close(tempUnitNum)

      ! Step 2: Let's delete all the files in the out of sync directory
      ! that come from this repository.
      ! We must find another available unit number for temporary use.
      do tempUnitNum3 = 100, 10000
        inquire(tempUnitNum3, opened = isOpen)
        if (.not. isOpen) then
          exit
        end if
      end do
      rvInt = GetEnviron("TMP", tempFileName3)
      ! This call lists all files currently in the root repo
      tempFileName3 = trim(tempfileName3) // "\OFL_OOSFiles3.txt"
      rvInt = SystemCall("git -C " // trim(checkFolderToUse) // &
        " ls-tree --full-tree -r --name-only HEAD > " // trim(tempFileName3))
      ! Now, we can delete the files already in the out of sync from
      ! this repo.
      open(tempUnitNum3, file = trim(tempFileName3), status = "old")
      do
        read(tempUnitNum3, '(A)', iostat = ios) line
        if (ios /= 0) then
          exit
        end if
        ! The files listed in tempfileName3 has the sub-directories as well
        ! So, we need to get just the file name.
        slashLoc = -1
        do while (slashLoc /= 0)
          slashLoc = scan(line, "/")
          line = line(slashLoc + 1:)
        end do
        ! If this file exists in the out of sync folder, delete it.
        inquire(file = trim(outFolder) // SlashString() // trim(line), &
          exist = doesFileExist)
        if (doesFileExist) then
          rvLogical = DelFile(trim(trim(outFolder) // SlashString() // &
            trim(line)))
        end if
      end do

      ! Step 3: We have to check if this is a sub-repo or not.  The
      ! GitCheckRepo call lists out of sync files but only relative
      ! to the root location.  For instance, if the root directory is
      ! Fer, the "OFL_OOSFiles.txt" file will list out of sync files like:
      ! "dat\NchsFertilityRates.txt"
      ! and
      ! "pro\FerMod.f90".
      ! But what if we only want to check the "dat" directory?  We need
      ! to see if this is a sub-repo or the root directory for that.
      ! Otherwise, the copying code below will try to find a file in the
      ! dat\dat directory.
      ! We must find yet another available unit number for temporary use.
      ! This is just in case we are not in the root of a repository.
      do tempUnitNum4 = 100, 10000
        inquire(tempUnitNum4, opened = isOpen)
        if (.not. isOpen) then
          exit
        end if
      end do
      rvInt = GetEnviron("TMP", tempFileName4)
      tempFileName4 = trim(tempfileName4) // "\OFL_OOSFiles4.txt"
      ! This call determines the root location of the repository
      rvInt = SystemCall("git -C " // trim(checkFolderToUse) // &
        " rev-parse --show-toplevel > " // trim(tempFileName4))
      open(tempUnitNum4, file = trim(tempfileName4), status = "old")
      ! This is the actual repo
      read(tempUnitNum4, '(A)') repoRootLocation
      close(tempUnitNum4)
      ! We need to convert forward slashes in 'repoRootLocation' to backwards
      do iChar = 1, len(trim(repoRootLocation))
        if (repoRootLocation(iChar:iChar) == "/") then
          repoRootLocation(iChar:iChar) = "\"
        end if
      end do

      ! Reopen tempUnitNum (file with standard GitCheckRepo output)
      ! process it, and close it.
      open(tempUnitNum, file = trim(tempfileName), status = "old")
      ! Skip the first 8 lines.
      do i = 1, 8
        read(tempUnitNum, *)
      end do
      ! Read the 9th line to see if there are any out-of-sync files.
      ! Note: We don't rely on the possibly already set numOOSFiles
      ! variable, as its value will only sometimes have already been
      ! set, i.e., when this subroutine called GitCheckRepo. But in
      ! the scenario where it didn't call it, then the variable won't
      ! yet be set.  So we'll just set it here in any case.
      read(tempUnitNum, '(A)') tempString
      choppedTempString = ParseLine(tempString, ":")
      numOOSFiles = AscToInt(trim(tempString))
      ! Check if there are any files out of sync.
      if (numOOSFiles == 0) then
        ! If there are no out-of-sync files, then delete the directory
        ! made for storing them (it could exist there from a previous run).
        ! If there are no files left in the out-of-sync directory, then delete
        ! that entire directory.
        rvLogical = DelDir(trim(outFolder))
      else if (numOOSFiles > 0) then
        ! If there is at least 1 out-of-sync file, then create the
        ! directory to hold the out-of-sync files. It's OK if that directory
        ! already exists - the call to MakeDir won't hurt that.
        rvLogical = MakeDir(trim(outFolder))
        call ProcessGitCheckRepoFileBody(trim(repoRootLocation), &
          trim(outFolder), tempUnitNum)
      end if

      ! Close the file.
      close(tempUnitNum)
    end if

    ! Delete the temporary files that were used.
    rvLogical = DelFile(trim(tempfileName))
    rvLogical = DelFile(trim(tempfileName2))
    rvLogical = DelFile(trim(tempfileName3))
    rvLogical = DelFile(trim(tempfileName4))

  end subroutine CopyOutOfSyncGitFiles
!******************************************************************************
  subroutine ProcessGitCheckRepoFileBody(checkFolder, outFolder, unitNum)

    character (len = *), intent(in) :: checkFolder
    character (len = *), intent(in) :: outFolder
    integer, intent(in) :: unitNum

    character (len = 1000) :: line
    character (len = 1000) :: fileName
    character (len = 1000) :: nextToken
    ! First token in the file containing the Git check results, indicating
    ! in what way the file is out of sync.
    character (len = 1000) :: changeType
    integer :: ios
    integer :: rv
    integer :: iRead
    logical :: listReached

    listReached = .false.
    do
      read(unitNum, '(A)', iostat = ios) line
      ! Check if we're reached the end of the file.
      if(ios /= 0) then
        exit
      ! If this is the section showing differences from working files
      ! and Bitbucket, skip next 5 lines and then read line containing
      ! the name of the first file that is out-of-sync.
      else if (Left(line, 49) == &
        "Files that differ from Working Files to Bitbucket" .or. &
        Left(line, 32) == "Files that differ from Bitbucket") then
        listReached = .true.
        do iRead = 1, 5
          read(unitNum, '(A)', iostat = ios) line
        end do
        read(unitNum, '(A)', iostat = ios) line
      end if

      ! If we know we've reached the list of out of sync working files
      ! from Bitbucket, we can read the next file out of sync.
      if (listReached) then
        ! Parse the line read in. The first item should either be "A",
        ! "D", "M", or "R" indicating whether the file was added,
        ! deleted, modified, or renamed, respectively.
        changeType = ParseLine(line)
        ! The next portion of the line contains the file name. It could have
        ! spaces in it, so don't use ParseLine. Also, there are six spaces
        ! at the beginning of the name so elimate those.
        fileName = line(7:sizeof(line))
        ! Construct the full path to the file.
        fileName = trim(checkFolder) // SlashString() // trim(fileName)
        ! Copy over files in this list
        if (trim(changeType) == "A" .or. trim(changeType) == "D" .or. &
          trim(changeType) == "M" .or. trim(changeType) == "R") then
          ! File is out of sync. Copy it to fileLocation. Put quotes around
          ! the file name, in case there are spaces in the name. Use /y switch
          ! to automatically overwrite any file that may be there.
          rv = SystemCall("copy /y " // trim(fileName) // " " // &
            trim(outFolder) // " > nul")
        ! Once we get through all files that are different, the next line has
        !  73 asterisks so we know we are done.
        else if (trim(changeType) == repeat("*", 73)) then
          exit
        else
          write(6, '(A)') &
            "Error reading GitCheckRepo() output. Invalid changeType string"
          write(6, '(A)') "      changeType: " // trim(changeType)
          write(6, '(A)') "Remainder of line: " // trim(line)
          stop
        end if
      end if
    end do

  end subroutine ProcessGitCheckRepoFileBody
!******************************************************************************
!****f* TRChecksMod/RunLogInit
!  NAME
!  RunLogInit: This subroutine is used to populate the contents of a run log
!  file with general log information including the date/time that the run was
!  started.
!  DESCRIPTION
!  This subroutine is used to populate the contents of a run log file with
!  general log information including the date/time that the run was started.
!  The run log file should be a file that is already open. The associated unit
!  number is passed in as the first argument to this subroutine. This subroutine
!  should typically be called near the beginning of the program and will create
!  a "header" for the run log file. The header contains information about the
!  run. The second argument, "comments", is optional. If passed in, then the
!  text contained in that variable will be put in the run log file, preceeded
!  by "Comments: ". However, if it is passed in and is empty, then the run log
!  file will contain a line like this: "Comments: [None]".
!
!  These are the lines that are written to the run log file:
!  * The full path of the program executable.
!  * The user's PIN (and name, if it can be obtained from the PIN with "WhoIs").
!  * The start date of the run.
!  * The start time of the run.
!  * The compiler version used for compiling the OFL.
!  * The version number of the OFL.
!  * Comments (optional).
!
!  In programs that call this subroutine, there is typically also a call to
!  'RunLogWrapUp' somewhere near the end of the program.
!  SYNOPSIS

  subroutine RunLogInit(runLogUnitNum, comments)

!  ARGUMENTS
!  * runLogUnitNum: Unit number of the run log file. This file should already
!    be open.
!  * comments: (optional) A description, notes, or comments that will be put
!    in the run log file.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: runLogUnitNum
    character (len = *), optional, intent(in) :: comments
!*****
    integer, dimension(8) :: dateTime
    integer :: rv
    character (len = 1000) :: file
    character (len = 1000) :: path
    character (len = 1000) :: userPIN
    character (len = 1000) :: userName
    character (len = 1000) :: tempFile
    integer :: tempUnitNum
    logical :: isOpen

    call date_and_time(values = dateTime)
    call GetArg(0, file)
    rv = GetFullPath(file, path)

    call GetLoginName(userPIN)
    ! In addition to writing out the user's PIN, we also want to write
    ! out the user's name. We'll get this using the "WhoIs" utility,
    ! if it's available.
    ! Try calling WhoIs, and put the results in a temporary file.
    rv = GetEnviron("TMP", tempFile)
    tempFile = trim(tempFile) // "\TempFile_OFL_WhoIs.txt"
    rv = SystemCall("WhoIs " // trim(userPIN) // " > " // trim(tempFile))
    ! A return value of 0 means the call to WhoIs worked. Otherwise, it didn't.
    if (rv == 0) then
      ! We must find an available unit number for temporary use.
      do tempUnitNum = 100, 10000
        inquire(tempUnitNum, opened = isOpen)
        if (.not. isOpen) then
          exit
        end if
      end do
      ! Open the temporary file to read the result.
      open(tempUnitNum, file = trim(tempfile), status = "old", action = "read")
      read(tempUnitNum, '(A)') userName
      ! Now, close and delete the temporary file.
      close(tempUnitNum)
      rv = DelFile(trim(tempFile))
    else
      ! The call to WhoIs didn't work. So set the userName to be "unknown".
      userName = " [UNKNOWN] "
    end if

    write(runLogUnitNum, '(A)') "Program: " // trim(path)
    write(runLogUnitNum, '(A)') "User: " // trim(userPIN) // " (" // &
      trim(userName) // ")"
    write(runLogUnitNum, '(A, I2.2, A, I2.2, A, I4)') "Start date: ", &
      dateTime(2), "/", dateTime(3), "/", dateTime(1)
    write(runLogUnitNum, '(A, I2.2, A, I2.2, A, I2.2, A, I3.3)') &
      "Start time: ", dateTime(5), ":", dateTime(6), ":", dateTime(7), ".", &
      dateTime(8)
    write(runLogUnitNum, '(A)') "OFL compiler version: " // &
      trim(OcactFortLibCompVer())
    write(runLogUnitNum, '(A)') "OFL version: " // &
      trim(OcactFortLibVer(.true.))
    ! Output comments, if passed in.
    if (present(comments)) then
      if (len_trim(comments) == 0) then
        write(runLogUnitNum, '(A)') "Comments: [None]"
      else
        write(runLogUnitNum, '(A)') "Comments: " // trim(comments)
      end if
    end if

  end subroutine RunLogInit
!******************************************************************************
!****f* TRChecksMod/RunLogWrapUp
!  NAME
!  RunLogWrapUp: This subroutine is used to populate the contents of a run log
!  file with information about the date/time that the run was completed.
!  DESCRIPTION
!  This subroutine is used to populate the contents of a run log file with
!  information about the date/time that the run was completed. The run log file
!  should be a file that is already open. The associated unit number is passed
!  in as the argument to this subrouinte. This subroutine should typically be
!  called near the end of the program and will create a "footer" for the run
!  log file. The footer contains information about when the run ended.
!
!  These are the lines that are written to the run log file:
!  * The completion date of the run.
!  * The completion time of the run.
!
!  In programs that call this subroutine, there is typically also a call to
!  'RunLogInit' somewhere near the beginning of the program.
!  SYNOPSIS

  subroutine RunLogWrapUp(runLogUnitNum)

!  ARGUMENTS
!  * runLogUnitNum: Unit number of the run log file. This file should already
!    be open.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: runLogUnitNum
!*****

    integer, dimension(8) :: dateTime

    call date_and_time(values = dateTime)

    write(runLogUnitNum, '(A, I2.2, A, I2.2, A, I4)') "Finish date: ", &
      dateTime(2), "/", dateTime(3), "/", dateTime(1)
    write(runLogUnitNum, '(A, I2.2, A, I2.2, A, I2.2, A, I3.3)') &
      "Finish time: ", dateTime(5), ":", dateTime(6), ":", dateTime(7), ".", &
      dateTime(8)

  end subroutine RunLogWrapUp
!******************************************************************************

end module TRChecksMod
