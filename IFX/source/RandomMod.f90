! $Id: RandomMod.f90 1.15 2012/06/22 10:45:42EDT 622325 Development  $
module RandomMod

!****h* source/RandomMod
!  NAME
!  RandomMod: OcactFortLib RandomMod module.
!  REVISION
!  $Revision: 1.15 $
!  MODULE DESCRIPTION
!  The RandomMod module contains subroutines and functions used to return random
!  numbers from normal, standard normal, and uniform distributions. Before using
!  this module, call the InitRandomModule subroutine to initialize RandomMod.
!  MODULE ROUTINES
!  InitRandomModule
!  RandFromNorm
!  RandFromStandNorm
!  RandFromUnif
!  SelectStream
!*****

  implicit none
  save

  private
  public :: InitRandomModule, SelectStream
  public :: RandFromUnif, RandFromStandNorm, RandFromNorm

  ! Allow InitRandomModule to be the generic name for the specific subroutines.
!****f* RandomMod/InitRandomModule
!  NAME
!  InitRandomModule: This subroutine is used to initialize the RandomMod module.
!  It should be called before any other routines in this module.
!  DESCRIPTION
!  This subroutine should be called before any other subroutines or functions in
!  this module are called. The maximum number of random number streams this
!  module can handle is MAX_RAND_STREAMS (which is currently set to 100). This
!  subroutine is used to initialize the streams with seeds that are
!  positive integers.
!
!  If 'randSeeds' (an integer array) is passed in,  then the number of streams
!  being initialized is the size of the array. Each stream is initialized with
!  the corresponding value in 'randSeeds'.
!
!  If 'numberOfStreams' (a single integer value) is passed
!  in, then that number is used as the number of streams to be
!  initialized. Since, in this case, no seeds are passed in, the streams
!  are initliazed with "random" seeds based on the date and the time
!  that the program was started. This will give different results every
!  time the program is run since the seeds will change from run to run.
!
!  After the call to 'InitRandomModule', the current stream will be set
!  to 1 (i.e. the first stream). Every subsequent call to a subroutine
!  or function in this module that draws from the random number stream
!  will draw from this first stream, until a 'SelectStream' is called.
!  (See the documentation on 'SelectStream' below.)
!
!  The InitRandomModule subroutine should not be called more than once
!  in a program.
!  SYNOPSIS
!  subroutine InitRandomModule(randSeeds)
!  subroutine InitRandomModule(numberOfStreams)
!  ARGUMENTS
!  * randSeeds: Random number streams array.
!  * numberOfStreams: Random number streams.
!  ARGUMENT DECLARATIONS
!  integer, dimension(:), intent(in) :: randSeeds
!  integer, intent(in) :: numberOfStreams
!*****
  interface InitRandomModule
    module procedure InitRandomModuleNumStreams, InitRandomModuleSeeds
  end interface InitRandomModule

  ! Allow RandFromUnif to be the generic name for the specific subroutines.
  !****f* RandomMod/RandFromUnif
!  NAME
!  RandFromUnif: This function returns a random number from a uniform
!  distribution.
!  DESCRIPTION
!  This function returns a random number drawn from a Uniform distribution
!  over a specified interval. This function can take 0, 1, or 2 arguments.
!  If the number of arguments is:
!  * 0, then the interval is the unit interval, from 0 to 1.
!  * 1, then it is the higher end of the interval, with 0 being the lower end.
!  * 2, then the first is the lower end of the interval, and the second is the
!    higher end.
!  If 2 arguments are passed in, then they must both be of the same type. The
!  return type is the same as that of the arguments. If no arguments are passed
!  in, then the return type is double precision.
!  RETURNS
!  Integer, Real, or Double Precision: Random number from a uniform
!  distribution.
!  SYNOPSIS
!  integer function RandFromUnif(arg1, arg2) result(rand)
!  real function RandFromUnif(arg1, arg2) result(rand)
!  double precision function RandFromUnif(arg1, arg2) result(rand)
!  ARGUMENTS
!  * arg1: (optional) Can be an integer, real, or double precison.
!  * arg2: (optional) Can be an integer, real, or double precison.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RandFromUnif
    module procedure RandFromUnifInt, RandFromUnifReal, RandFromUnifDoublePrec
  end interface RandFromUnif

  ! Allow RandFromNorm to be the generic name for the specific subroutines.
  !****f* RandomMod/RandFromNorm
!  NAME
!  RandFromNorm: This function returns a random number from a normal
!  distribution.
!  DESCRIPTION
!  This function returns a random number from a normal distribution with a
!  mean equal to the variable 'mean' and a standard deviation equal to
!  the variable 'sd'. Both arguments must be of the same type (either
!  both real or both double precision). The return type is the same as
!  the argument types.
!  RETURNS
!  Real or Double Precision: Random number from a normal distribution.
!  SYNOPSIS
!  real function RandFromNorm(mean, sd) result(rand)
!  double precision function RandFromNorm(mean, sd) result(rand)
!  ARGUMENTS
!  * mean: Can be either real or double precison, must be the same type as sd.
!  * sd: Can be either real or double precison, must be the same type as mean.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RandFromNorm
    module procedure RandFromNormReal, RandFromNormDoublePrec
  end interface RandFromNorm

  ! Maximum number of random number streams.
  integer, parameter :: MAX_RAND_STREAMS = 100
  ! Used for the Ran1 random number generator.
  integer, parameter :: NTAB = 32

  ! These 3 arrays are used for the Ran1 random number generator.
  integer, dimension(MAX_RAND_STREAMS, NTAB) :: iv
  integer, dimension(MAX_RAND_STREAMS) :: iy
  integer, dimension(MAX_RAND_STREAMS) :: currentSeeds

  ! Inititalizing these next two variables to 0 will help assure that the
  ! program will blow up if one of the functions is called before the
  ! InitRandomModule subroutine is called!

  ! Number of random number streams being accessed.
  integer :: numStreams = 0
  ! Current random number streams being accessed
  integer :: currentStream = 0

  contains

!******************************************************************************
  ! This subroutine is used to initialize the random number generator used
  ! in this module. It is not a public subroutine, so it is access through
  ! the name InitRandomModule() (which is public and will choose which
  ! initialization subroutine to run). This subroutine takes an array of
  ! positive integers. These integers are used to seed the random number
  ! streams. (This module can handle up to MAX_RAND_STREAMS number of
  ! different streams.) The number of streams that this module will handle
  ! is the size of the array passed in.
  subroutine InitRandomModuleSeeds(randSeeds)

    integer, dimension(:), intent(in) :: randSeeds
    integer :: i

    ! Initialize the current stream to be the first one.
    currentStream = 1

    ! Set the number of streams and make sure there aren't too many.
    numStreams = size(randSeeds)
    if (numStreams > MAX_RAND_STREAMS) then
      write(6, '(A, 2(/, A, I5))') "Error in InitRandomModuleSeeds!", &
        "  MAX_RAND_STREAMS: ", MAX_RAND_STREAMS, &
        "        numStreams: ", numStreams
      stop
    end if
    ! Seed the Ran1 random number generator for each stream (while
    ! checking to make sure all the seeds are positive).
    do i = 1, numStreams
      if (randSeeds(i) <= 0) then
        write(6, '(A, /, A, I, A, I)') "Error in InitRandomModule(). ", &
          "All the seeds must be positive but found seed number ", i, &
          " is ", randSeeds(i)
        stop
      end if
      currentSeeds(i) = randSeeds(i)
      call InitRan1(currentSeeds(i), iv(i,:), iy(i))
    end do

  end subroutine InitRandomModuleSeeds
!******************************************************************************
  ! This subroutine is used to initialize the random number generator used
  ! in this module. It is not a public subroutine, so it is access through
  ! the name InitRandomModule() (which is public and will choose which
  ! initialization subroutine to run). This subroutine takes an integer, which
  ! will be the number of streams used by this module. (This module can handle
  ! up to MAX_RAND_STREAMS number of different streams.) The seeds for each of
  ! the streams will be set "randomly" and will be different for each run.
  subroutine InitRandomModuleNumStreams(numberOfStreams)

    integer, intent(in) :: numberOfStreams
    integer :: i
    real :: thisSeed

    ! Initialize the current stream to be the first one.
    currentStream = 1

    ! Set the number of streams and make sure there aren't too many.
    numStreams = numberOfStreams
    if (numStreams > MAX_RAND_STREAMS) then
      write(6, '(A, 2(/, A, I5))') "Error in InitRandomModuleNumStreams!", &
        "  MAX_RAND_STREAMS: ", MAX_RAND_STREAMS, &
        "        numStreams: ", numStreams
      stop
    end if

    call random_seed()
    do i = 1, numStreams
      call random_number(thisSeed)
      currentSeeds(i) = nint(10000.0 * thisSeed)
      call InitRan1(currentSeeds(i), iv(i,:), iy(i))
    end do

  end subroutine InitRandomModuleNumStreams
!******************************************************************************
  !****f* RandomMod/SelectStream
!  NAME
!  SelectStream: This subroutine selects which random number stream will be the
!  "current" stream when drawing random numbers.
!  DESCRIPTION
!  After the initial call to 'InitRandomModule', the "current" stream is set to
!  1 (i.e the first stream). See the documentation to 'InitRandomModule'. To
!  begin drawing numbers from a different stream, just call 'SelectStream' and
!  pass the number of the stream you want to set as the "current" one. The
!  current position for all the streams is stored in memory, so returning to a
!  stream will continue selecting numbers from the middle of that stream.
!  The "current" stream will remain the "current" one until another call
!  is made to 'SelectStream'.
!  SYNOPSIS

  subroutine SelectStream(newStreamNum)

!  ARGUMENTS
!  * newStreamNum: Number of the stream you want to set as the "current" stream.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: newStreamNum
!*****

    if (newStreamNum > numStreams) then
      write(6, '(A, 2(/, A, I5))') "Error in SelectStream!", &
        "  newStreamNum: ", newStreamNum, &
        "    numStreams: ", numStreams
      stop
    end if

    currentStream = newStreamNum

  end subroutine SelectStream
!******************************************************************************
  ! This function returns a random number drawn from a Normal distribution
  ! with mean "mean" and standard deviation "sd", where mean and sd are
  ! passed in to the function.
  real function RandFromNormReal(mean, sd)

    real, intent(in) :: mean, sd

    RandFromNormReal = sd * RandFromStandNorm() + mean

  end function RandFromNormReal
!******************************************************************************
  ! This function returns a random number drawn from a Normal distribution
  ! with mean "mean" and standard deviation "sd", where mean and sd are
  ! passed in to the function.
  real function RandFromNormDoublePrec(mean, sd)

    double precision, intent(in) :: mean, sd

    RandFromNormDoublePrec = sd * RandFromStandNorm() + mean

  end function RandFromNormDoublePrec
!******************************************************************************
! This code was adapted from http://www.geocities.com/WallStreet/9245/vba5.htm
  ! This function returns a random number drawn from a Standard Normal
  ! distribution (i.e. a Normal distribution with a mean of 0 and standard
  ! deviation of 1).

  !****f* RandomMod/RandFromStandNorm
!  NAME
!  RandFromStandNorm: This function returns a random number from a standard
!  normal distribution
!  DESCRIPTION
!  This function returns a random number from a standard normal distribution
!  (i.e. a normal distribution with a mean of 0 and a standard deviation
!  of 1).
!  RETURNS
!  Double Precision: Random number from a standard normal distribution.
!  SYNOPSIS

  double precision function RandFromStandNorm() result(randNum)

!  ARGUMENTS
!  [None]
!*****
    logical, save :: haveSavedRandNum = .false.
    double precision, save :: savedRandNum
    double precision :: fac, r, V1, V2

    ! Turn this off for now...
    haveSavedRandNum = .false.
    if (haveSavedRandNum) then
      ! If we have a saved random number from the last call to this
      ! function, then just use it.
      randNum = savedRandNum
      haveSavedRandNum = .false.
    else
      ! Otherwise, we need to generate 2 new random numbers; one for
      ! this time, and one for the next time this function is called.
      do
        ! This picks a random point (V1, V2) in a 2 by 2 square centered
        ! on the origin.
        V1 = 2.0d0 * RandFromUnifDoublePrec() - 1.0d0
        V2 = 2.0d0 * RandFromUnifDoublePrec() - 1.0d0
        ! Check if the point falls within a unit circle centered on
        ! the origin.
        r = V1**2.0d0 + V2**2.0d0
        if (r .le. 1.0d0 .and. r .gt. 0.0d0) then
          exit
        end if
      end do
      fac = sqrt(-2.0d0 * log(r) / r)
      ! Set the return value and save the the other value.
      randNum = V1 * fac
      savedRandNum = V2 * fac
      haveSavedRandNum = .true.
    end if

  end function RandFromStandNorm
!******************************************************************************
  integer function RandFromUnifInt(arg1, arg2) result(rand)
    ! From a technical point of view, this function can't have both of
    ! its arguments being optional, because if it did, then when this function
    ! is called by its generic name (RandFromUnif) with no arguments, the
    ! compiler would not know if it's calling this function or
    ! RandFromUnifReal() (The compiler can't tell from the return type alone).
    ! From a practical point of view, it's not likely that this function would
    ! be called with 0 arguments (representing the interval (0,1) as in the
    ! function RandFromUnifReal()). But if the user really wnated this, he
    ! could call the function explicitely passing it the arguments 0 and 1.

    integer, intent(in) :: arg1
    integer, optional, intent(in) :: arg2
    real :: low
    real :: high
    real :: randFromUnifZeroOne

    if (present(arg2)) then
      ! If both arguments are passed in, then the first one is low
      ! and the second one is high.
      high = arg2
      low = arg1
    else
      ! If only one argument was passed in, then that number is
      ! considered high and the defualt value for low is 0.0.
      high = arg1
      low = 0.0
    end if
    ! Since we are dealing with integers, we will want to convert the 
    ! final random number to an integer. We will do this by truncating.
    ! But since the random number generated will be between low and high,
    ! then a truncation means we'll never get the high result. So we'll
    ! add 1 to high, so that truncaition may end up giving us the high
    ! result. Note: we don't need to worry about possibly getting a
    ! result of high + 1, because the unit uniform random number
    ! generator should not return exactly 1. This is true of both Ran1()
    ! and of random().
    high = high + 1

    randFromUnifZeroOne = &
      Ran1(currentSeeds(currentStream), iv(currentStream,:), iy(currentStream))
    ! This is to use the system-supplied generator instead of Ran1.
    !call random(randFromUnifZeroOne)
    ! This line will truncate the final value to convert it to an integer.
    ! Alternatively, we could have called int(), but this is an extra
    ! function call, and my experience has been that it is actually a
    ! very time-consuming function call.
    rand = low + randFromUnifZeroOne * (high - low)

  end function RandFromUnifInt
!******************************************************************************
  ! This function returns a random number drawn from a Uniform distribution
  ! over a specified interval. This function can take 1 or 2 arguments.
  ! If 2 arguments are passed in, then the first is the lower end of the
  ! interval, and the second is the higher end. If 1 argument is passed in,
  ! then it is the higher end of the interval, with 0 being the lower end.
  ! Theoretically, it would be nice to have this function be able to take
  ! 0 arguments (like RandFromUnifDoublePrec). However, this isn't allowed
  ! since, when using the generic name RandFromUnif, the compiler wouldn't
  ! know which function to use when no arguments are passed in (it can't
  ! tell solely based on the return type).
  real function RandFromUnifReal(arg1, arg2) result(rand)

    real, intent(in) :: arg1
    real, optional, intent(in) :: arg2
    real :: low
    real :: high
    real :: randFromUnifZeroOne

    if (present(arg2)) then
      ! If both arguments are passed in, then the first one is low
      ! and the second one is high.
      high = arg2
      low = arg1
    else
      ! If exactly one argument was passed in, then that number is
      ! considered high and the defualt value for low is 0.0.
      high = arg1
      low = 0.0
    end if

    randFromUnifZeroOne = &
      Ran1(currentSeeds(currentStream), iv(currentStream,:), iy(currentStream))
    ! This is to use the system-supplied generator instead of Ran1.
    !call random(randFromUnifZeroOne)
    rand = low + randFromUnifZeroOne * (high - low)

  end function RandFromUnifReal
!******************************************************************************
  ! This function returns a random number drawn from a Uniform distribution
  ! over a specified interval. This function can take 0, 1, or 2 arguments.
  ! If 2 arguments are passed in, then the first is the lower end of the
  ! interval, and the second is the higher end. If 1 argument is passed in,
  ! then it is the higher end of the interval, with 0 being the lower end.
  ! If no arguments are passed in, then the interval is the unit interval,
  ! from 0 to 1.
  double precision function RandFromUnifDoublePrec(arg1, arg2) result(rand)

    double precision, optional, intent(in) :: arg1
    double precision, optional, intent(in) :: arg2
    double precision :: low
    double precision :: high
    double precision :: randFromUnifZeroOne

    if (present(arg2)) then
      ! If both arguments are passed in, then the first one is low
      ! and the second one is high.
      high = arg2
      low = arg1
    else if (present(arg1)) then
      ! If exactly one argument was passed in, then that number is
      ! considered high and the defualt value for low is 0.0.
      high = arg1
      low = 0.0d0
    else
      ! If neither argument is passed in, then use the default values
      ! of 0.0 and 1.0 for low and high, respectively.
      high = 1.0d0
      low = 0.0d0
    end if

    randFromUnifZeroOne = &
      Ran1(currentSeeds(currentStream), iv(currentStream,:), iy(currentStream))
    ! This is to use the system-supplied generator instead of Ran1.
    !call random(randFromUnifZeroOne)
    rand = low + randFromUnifZeroOne * (high - low)

  end function RandFromUnifDoublePrec
!******************************************************************************
  subroutine InitRan1(idum, iv, iy)

    integer, intent(inout) :: idum
    integer, intent(inout) :: iv(NTAB)
    integer, intent(inout) :: iy
    integer IA,IM,IQ,IR,NDIV
    real AM,EPS,RNMX
    parameter (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,&
           NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
    ! Minimal random number generator of Park and Miller with Bays-Durham
    ! shufflee and added safeguards. Returns a uniform random deviate between
    ! 0.0 and 1.0 (exclusive of the endpoint values). Call with idum a
    ! negative integer to initialize; thereafter,do not alter idum between
    ! successive deviates in a sequence. RNMX should approximate the largest
    ! floating value that is less than 1.
    integer j,k
    iv = 0
    iy = 0
    if (idum <= 0) then
      write(6, '(2A, /, A, I)') "Error in InitRan1(), idum must be ", &
        "a positive number.", "  idum: ", idum
      stop
    end if
    do j=NTAB+8,1,-1
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      if (j.le.NTAB) iv(j)=idum
    end do
    iy=iv(1)
    
  end subroutine InitRan1
!******************************************************************************
  ! From the book "Numerical Recipes in Fortran", see:
  ! http://www.library.cornell.edu/nr/bookfpdf/f7-1.pdf
  real function Ran1(idum, iv, iy)
    integer, intent(inout) :: idum
    integer, intent(inout) :: iv(NTAB)
    integer, intent(inout) :: iy
    integer IA,IM,IQ,IR,NDIV
    real AM,EPS,RNMX
    parameter (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,&
           NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
    ! Minimal random number generator of Park and Miller with Bays-Durham
    ! shufflee and added safeguards.Returns a uniform random deviate between
    ! 0.0 and 1.0 (exclusive of the endpoint values).Call with idum a negative
    ! integer to initialize;thereafter,do not alter idum between successive
    ! deviates in a sequence.RNMX should approximate the largest floating
    ! value that is less than 1.
    integer j,k

    k=idum/IQ                    ! Start here.
    idum=IA*(idum-k*IQ)-IR*k     ! Compute idum=mod(IA*idum,IM) without
    if (idum.lt.0) idum=idum+IM  !    overflows by Schrage’s method.
    j=1+iy/NDIV                  ! Will be in the range 1:NTAB.
    iy=iv(j)                     ! Output previously stored value and refill
    iv(j)=idum                   !    the shuffle table.
    Ran1=min(AM*iy,RNMX)         ! Because users don’t expect endpoint values.

  end function Ran1
!******************************************************************************

end module RandomMod
