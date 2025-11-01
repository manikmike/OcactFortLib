! $Id: GraduationMod.f90 1.4 8/4/2021 15:04:09 Morris,M. $
module GraduationMod

!****h* source/GraduationMod
!  NAME
!  GraduationMod: OcactFortLib GraduationMod module.
!  REVISION
!  $Revision: 1.3 $
!  MODULE DESCRIPTION
!  The GraduationMod module contains the routintes used for
!  graduating/smoothing data.
!  MODULE ROUTINES
!  Beers
!  BeersInterpolate
!!  Gomp ! Not yet incorporated.
!  WhitHend
!  WhitHend2D
!*****

  use MatrixMod

  implicit none
  save

  private
  !public :: Gomp
  public :: Beers, BeersInterpolate
  public :: WhitHend
  public :: WhitHend2D

  ! We need to set up the arrays of the Beers coefficients.
  ! There are 4 sets:
  ! - Standard Beers.
  ! - Modified Beers.
  ! - Standard interpolated Beers.
  ! - Modified Interpolated Beers.

  ! This section is for the standard Beers coefficients.
  double precision, parameter, dimension(5, 5) :: SUB_COEFF1 = reshape( &
    (/  0.3333d0, -0.1636d0, -0.0210d0,  0.0796d0, -0.0283d0, &
        0.2595d0, -0.0780d0,  0.0130d0,  0.0100d0, -0.0045d0, &
        0.1924d0,  0.0064d0,  0.0184d0, -0.0256d0,  0.0084d0, &
        0.1329d0,  0.0844d0,  0.0054d0, -0.0356d0,  0.0129d0, &
        0.0819d0,  0.1508d0, -0.0158d0, -0.0284d0,  0.0115d0/), &
    shape(SUB_COEFF1), order = (/2, 1/))

  double precision, parameter, dimension(5, 5) :: SUB_COEFF2 = reshape( &
    (/  0.0404d0,  0.2000d0, -0.0344d0, -0.0128d0,  0.0068d0, &
        0.0093d0,  0.2268d0, -0.0402d0,  0.0028d0,  0.0013d0, &
       -0.0108d0,  0.2272d0, -0.0248d0,  0.0112d0, -0.0028d0, &
       -0.0198d0,  0.1992d0,  0.0172d0,  0.0072d0, -0.0038d0, &
       -0.0191d0,  0.1468d0,  0.0822d0, -0.0084d0, -0.0015d0/), &
    shape(SUB_COEFF2), order = (/2, 1/))

  double precision, parameter, dimension(5, 5) :: SUB_COEFF3 = reshape( &
    (/ -0.0117d0,  0.0804d0,  0.1570d0, -0.0284d0,  0.0027d0, &
       -0.0020d0,  0.0160d0,  0.2200d0, -0.0400d0,  0.0060d0, &
        0.0050d0, -0.0280d0,  0.2460d0, -0.0280d0,  0.0050d0, &
        0.0060d0, -0.0400d0,  0.2200d0,  0.0160d0, -0.0020d0, &
        0.0027d0, -0.0284d0,  0.1570d0,  0.0804d0, -0.0117d0/), &
    shape(SUB_COEFF3), order = (/2, 1/))

  double precision, parameter, dimension(5, 5) :: SUB_COEFF4 = reshape( &
    (/ -0.0015d0, -0.0084d0,  0.0822d0,  0.1468d0, -0.0191d0, &
       -0.0038d0,  0.0072d0,  0.0172d0,  0.1992d0, -0.0198d0, &
       -0.0028d0,  0.0112d0, -0.0248d0,  0.2272d0, -0.0108d0, &
        0.0013d0,  0.0028d0, -0.0402d0,  0.2268d0,  0.0093d0, &
        0.0068d0, -0.0128d0, -0.0344d0,  0.2000d0,  0.0404d0/), &
    shape(SUB_COEFF4) ,order = (/2, 1/))

  double precision, parameter, dimension(5, 5) :: SUB_COEFF5 = reshape( &
    (/  0.0115d0, -0.0284d0, -0.0158d0,  0.1508d0,  0.0819d0, &
        0.0129d0, -0.0356d0,  0.0054d0,  0.0844d0,  0.1329d0, &
        0.0084d0, -0.0256d0,  0.0184d0,  0.0064d0,  0.1924d0, &
       -0.0045d0,  0.0100d0,  0.0130d0, -0.0780d0,  0.2595d0, &
       -0.0283d0,  0.0796d0, -0.0210d0, -0.1636d0,  0.3333d0/), &
    shape(SUB_COEFF5), order = (/2, 1/))

  double precision, parameter, dimension(5, 5, 5) :: BEERS_SUB_COEFF = &
    (/ SUB_COEFF1, SUB_COEFF2, SUB_COEFF3, SUB_COEFF4, SUB_COEFF5/)
  ! This is the end of the standard Beers coefficients.

  ! This set is for the modified Beers coefficients.
  double precision, parameter, dimension(5, 5) :: MOD_SUB_COEFF1 = reshape( &
    (/  0.3332d0, -0.1938d0,  0.0702d0, -0.0118d0,  0.0022d0, &
        0.2569d0, -0.0753d0,  0.0205d0, -0.0027d0,  0.0006d0, &
        0.1903d0,  0.0216d0, -0.0146d0,  0.0032d0, -0.0005d0, &
        0.1334d0,  0.0969d0, -0.0351d0,  0.0059d0, -0.0011d0, &
        0.0862d0,  0.1506d0, -0.0410d0,  0.0054d0, -0.0012d0/), &
    shape(MOD_SUB_COEFF1), order = (/2, 1/))

  double precision, parameter, dimension(5, 5) :: MOD_SUB_COEFF2 = reshape( &
    (/  0.0486d0,  0.1831d0, -0.0329d0,  0.0021d0, -0.0009d0, &
        0.0203d0,  0.1955d0, -0.0123d0, -0.0031d0, -0.0004d0, &
        0.0008d0,  0.1893d0,  0.0193d0, -0.0097d0,  0.0003d0, &
       -0.0108d0,  0.1677d0,  0.0577d0, -0.0153d0,  0.0007d0, &
       -0.0159d0,  0.1354d0,  0.0972d0, -0.0170d0,  0.0003d0/), &
    shape(MOD_SUB_COEFF2), order = (/2, 1/))

  double precision, parameter, dimension(5, 5) :: MOD_SUB_COEFF3 = reshape( &
    (/ -0.0160d0,  0.0973d0,  0.1321d0, -0.0121d0, -0.0013d0, &
       -0.0129d0,  0.0590d0,  0.1564d0,  0.0018d0, -0.0043d0, &
       -0.0085d0,  0.0260d0,  0.1650d0,  0.0260d0, -0.0085d0, &
       -0.0043d0,  0.0018d0,  0.1564d0,  0.0590d0, -0.0129d0, &
       -0.0013d0, -0.0121d0,  0.1321d0,  0.0973d0, -0.0160d0/), &
    shape(MOD_SUB_COEFF3), order = (/2, 1/))

  double precision, parameter, dimension(5, 5) :: MOD_SUB_COEFF4 = reshape( &
    (/  0.0003d0, -0.0170d0,  0.0972d0,  0.1354d0, -0.0159d0, &
        0.0007d0, -0.0153d0,  0.0577d0,  0.1677d0, -0.0108d0, &
        0.0003d0, -0.0097d0,  0.0193d0,  0.1893d0,  0.0008d0, &
       -0.0004d0, -0.0031d0, -0.0123d0,  0.1955d0,  0.0203d0, &
       -0.0009d0,  0.0021d0, -0.0329d0,  0.1831d0,  0.0486d0/), &
    shape(MOD_SUB_COEFF4) ,order = (/2, 1/))

  double precision, parameter, dimension(5, 5) :: MOD_SUB_COEFF5 = reshape( &
    (/  -0.0012d0,  0.0054d0, -0.0410d0,  0.1506d0,  0.0862d0, &
        -0.0011d0,  0.0059d0, -0.0351d0,  0.0969d0,  0.1334d0, &
        -0.0005d0,  0.0032d0, -0.0146d0,  0.0216d0,  0.1903d0, &
         0.0006d0, -0.0027d0,  0.0205d0, -0.0753d0,  0.2569d0, &
         0.0022d0, -0.0118d0,  0.0702d0, -0.1938d0,  0.3332d0/), &
    shape(MOD_SUB_COEFF5), order = (/2, 1/))

  double precision, parameter, dimension(5, 5, 5) :: MOD_BEERS_SUB_COEFF = &
    (/ MOD_SUB_COEFF1, MOD_SUB_COEFF2, MOD_SUB_COEFF3, MOD_SUB_COEFF4, &
    MOD_SUB_COEFF5/)
  ! This is the end of the set of modified Beers coefficients.

  ! This set is for the standard interpolated Beers coefficients.
  double precision, parameter, dimension(5, 6) :: INT_COEFF1 = reshape( &
    (/  1.0000d0,  0.0000d0,  0.0000d0,  0.0000d0,  0.0000d0,  0.0000d0, &
        0.6667d0,  0.4969d0, -0.1426d0, -0.1006d0,  0.1079d0, -0.0283d0, &
        0.4072d0,  0.8344d0, -0.2336d0, -0.0976d0,  0.1224d0, -0.0328d0, &
        0.2148d0,  1.0204d0, -0.2456d0, -0.0536d0,  0.0884d0, -0.0244d0, &
        0.0819d0,  1.0689d0, -0.1666d0, -0.0126d0,  0.0399d0, -0.0115d0/), &
    shape(INT_COEFF1), order = (/2, 1/))

  double precision, parameter, dimension(5, 6) :: INT_COEFF2 = reshape( &
    (/  0.0000d0,  1.0000d0,  0.0000d0,  0.0000d0,  0.0000d0,  0.0000d0, &
       -0.0404d0,  0.8404d0,  0.2344d0, -0.0216d0, -0.0196d0,  0.0068d0, &
       -0.0497d0,  0.6229d0,  0.5014d0, -0.0646d0, -0.0181d0,  0.0081d0, &
       -0.0389d0,  0.3849d0,  0.7534d0, -0.1006d0, -0.0041d0,  0.0053d0, &
       -0.0191d0,  0.1659d0,  0.9354d0, -0.0906d0,  0.0069d0,  0.0015d0/), &
    shape(INT_COEFF2), order = (/2, 1/))

  double precision, parameter, dimension(5, 6) :: INT_COEFF3 = reshape( &
    (/ 0.0000d0,  0.0000d0,  1.0000d0,  0.0000d0,  0.0000d0,  0.0000d0, &
       0.0117d0, -0.0921d0,  0.9234d0,  0.1854d0, -0.0311d0,  0.0027d0, &
       0.0137d0, -0.1101d0,  0.7194d0,  0.4454d0, -0.0771d0,  0.0087d0, &
       0.0087d0, -0.0771d0,  0.4454d0,  0.7194d0, -0.1101d0,  0.0137d0, &
       0.0027d0, -0.0311d0,  0.1854d0,  0.9234d0, -0.0921d0,  0.0117d0/), &
    shape(INT_COEFF3), order = (/2, 1/))

  double precision, parameter, dimension(5, 6) :: INT_COEFF4 = reshape( &
    (/ 0.0000d0,  0.0000d0,  0.0000d0,  1.0000d0,  0.0000d0,  0.0000d0, &
       0.0015d0,  0.0069d0, -0.0906d0,  0.9354d0,  0.1659d0, -0.0191d0, &
       0.0053d0, -0.0041d0, -0.1006d0,  0.7534d0,  0.3849d0, -0.0389d0, &
       0.0081d0, -0.0181d0, -0.0646d0,  0.5014d0,  0.6229d0, -0.0497d0, &
       0.0068d0, -0.0196d0, -0.0216d0,  0.2344d0,  0.8404d0, -0.0404d0/), &
    shape(INT_COEFF4) ,order = (/2, 1/))

  double precision, parameter, dimension(5, 6) :: INT_COEFF5 = reshape( &
    (/  0.0000d0,  0.0000d0,  0.0000d0,  0.0000d0,  1.0000d0,  0.0000d0, &
       -0.0115d0,  0.0399d0, -0.0126d0, -0.1666d0,  1.0689d0,  0.0819d0, &
       -0.0244d0,  0.0884d0, -0.0536d0, -0.2456d0,  1.0204d0,  0.2148d0, &
       -0.0328d0,  0.1224d0, -0.0976d0, -0.2336d0,  0.8344d0,  0.4072d0, &
       -0.0283d0,  0.1079d0, -0.1006d0, -0.1426d0,  0.4969d0,  0.6667d0/), &
    shape(INT_COEFF5), order = (/2, 1/))

  double precision, parameter, dimension(5, 6, 5) :: BEERS_INT_COEFF = &
    (/ INT_COEFF1, INT_COEFF2, INT_COEFF3, INT_COEFF4, INT_COEFF5/)
  ! This is the end of the set of standard interpolated Beers coefficients.

  ! This set is for the modified interpolated Beers coefficients.
  double precision, parameter, dimension(5, 6) :: MOD_INT_COEFF1 = reshape( &
    (/  1.0000d0,  0.0000d0,  0.0000d0,  0.0000d0,  0.0000d0,  0.0000d0, &
        0.6668d0,  0.5270d0, -0.2640d0,  0.0820d0, -0.0140d0,  0.0022d0, &
        0.4099d0,  0.8592d0, -0.3598d0,  0.1052d0, -0.0173d0,  0.0028d0, &
        0.2196d0,  1.0279d0, -0.3236d0,  0.0874d0, -0.0136d0,  0.0023d0, &
        0.0862d0,  1.0644d0, -0.1916d0,  0.0464d0, -0.0066d0,  0.0012d0/), &
    shape(MOD_INT_COEFF1), order = (/2, 1/))

  double precision, parameter, dimension(5, 6) :: MOD_INT_COEFF2 = reshape( &
    (/  0.0000d0,  1.0000d0,  0.0000d0,  0.0000d0,  0.0000d0,  0.0000d0, &
       -0.0486d0,  0.8655d0,  0.2160d0, -0.0350d0,  0.0030d0, -0.0009d0, &
       -0.0689d0,  0.6903d0,  0.4238d0, -0.0442d0,  0.0003d0, -0.0013d0, &
       -0.0697d0,  0.5018d0,  0.5938d0, -0.0152d0, -0.0097d0, -0.0010d0, &
       -0.0589d0,  0.3233d0,  0.7038d0,  0.0578d0, -0.0257d0, -0.0003d0/), &
    shape(MOD_INT_COEFF2), order = (/2, 1/))

  double precision, parameter, dimension(5, 6) :: MOD_INT_COEFF3 = reshape( &
    (/ -0.0430d0,  0.1720d0,  0.7420d0,  0.1720d0, -0.0430d0,  0.0000d0, &
       -0.0270d0,  0.0587d0,  0.7072d0,  0.3162d0, -0.0538d0, -0.0013d0, &
       -0.0141d0, -0.0132d0,  0.6098d0,  0.4708d0, -0.0477d0, -0.0056d0, &
       -0.0056d0, -0.0477d0,  0.4708d0,  0.6098d0, -0.0132d0, -0.0141d0, &
       -0.0013d0, -0.0538d0,  0.3162d0,  0.7072d0,  0.0587d0, -0.0270d0/), &
    shape(MOD_INT_COEFF3), order = (/2, 1/))

  double precision, parameter, dimension(5, 6) :: MOD_INT_COEFF4 = reshape( &
    (/  0.0000d0, -0.0430d0,  0.1720d0,  0.7420d0,  0.1720d0, -0.0430d0, &
       -0.0003d0, -0.0257d0,  0.0578d0,  0.7038d0,  0.3233d0, -0.0589d0, &
       -0.0010d0, -0.0097d0, -0.0152d0,  0.5938d0,  0.5018d0, -0.0697d0, &
       -0.0013d0,  0.0003d0, -0.0442d0,  0.4238d0,  0.6903d0, -0.0689d0, &
       -0.0009d0,  0.0030d0, -0.0350d0,  0.2160d0,  0.8655d0, -0.0486d0/), &
    shape(MOD_INT_COEFF4) ,order = (/2, 1/))

  double precision, parameter, dimension(5, 6) :: MOD_INT_COEFF5 = reshape( &
    (/  0.0000d0,  0.0000d0,  0.0000d0,  0.0000d0,  1.0000d0,  0.0000d0, &
        0.0012d0, -0.0066d0,  0.0464d0, -0.1916d0,  1.0644d0,  0.0862d0, &
        0.0023d0, -0.0136d0,  0.0874d0, -0.3236d0,  1.0279d0,  0.2196d0, &
        0.0028d0, -0.0173d0,  0.1052d0, -0.3598d0,  0.8592d0,  0.4099d0, &
        0.0022d0, -0.0140d0,  0.0820d0, -0.2640d0,  0.5270d0,  0.6668d0/), &
    shape(MOD_INT_COEFF5), order = (/2, 1/))

  double precision, parameter, dimension(5, 6, 5) :: MOD_BEERS_INT_COEFF = &
    (/ MOD_INT_COEFF1, MOD_INT_COEFF2, MOD_INT_COEFF3, MOD_INT_COEFF4, &
    MOD_INT_COEFF5/)
  ! This is the end of the set of modified interpolated Beers coefficients.

  ! Allow Beers to be the generic name for the specific subroutines.
  !****f* GraduationMod/Beers
  !  NAME
  !  Beers: This subroutine does H.S. Beers graduating.
  !  DESCRIPTION
  !  The array to be graduated is passed in as the variable 'arrFiveYear'.
  !  It should contain data in 5-year age groups. The 'arrSingleYear'
  !  variable is the output variable containing the graduated
  !  single-year-of-age values. By default, the resulting single-year-of-age
  !  data will add up to the original 5-year age group data (within each age
  !  group). However, if the optional argument 'modCoeff' is passed in as true,
  !  then the resulting single-year-of-age data will be smoother but will not
  !  add up to the original 5-year age group data. Also, if the optional
  !  'codeMod' argument is passed in as true, then the algorithm is modified to
  !  avoid having negative values in the graduated single-year-of age results.
  !  SYNOPSIS
  !  subroutine Beers(arrFiveYear, arrSingleYear, modCoeff, codeMod)
  !  ARGUMENTS
  !  * arrFiveYear: 1-dimensional array (of any size) of reals or double
  !    precisions. This is the array of 5-year age group values needing to be
  !    graduated.
  !  * arrSingleYear: 1-dimensional array (of any size) of reals or double
  !    precisions. This contains the output (the graduated values) by single
  !    year of age.
  !  * modCoeff (optional): If false or omitted, the subroutine uses the
  !    standard coefficients. If true, the subroutine uses modified coefficients
  !    which allow for additional smoothness by not requiring the smoothed
  !    single age results to add up to the corresponding 5-year age group input.
  !  * codeMod (optional): If false or omitted, the subroutine uses the
  !    standard version of Beers. If true, it uses some modifications to help
  !    avoid the possibility of some negatives values in the graduated results.
  !  ARGUMENT DECLARATIONS
  !  See ARGUMENTS section.
  !*****
  interface Beers
    module procedure BeersReal, BeersDoublePrec
  end interface Beers

  ! Allow BeersInterpolate to be the generic name for the specific subroutines.
  !****f* GraduationMod/BeersInterpolate
  !  NAME
  !  BeersInterpolate: This subroutine does H.S. Beers interpolating graduation.
  !  DESCRIPTION
  !  The array to be interpolated is passed in as the variable 'arrFiveYear'.
  !  It should contain data for every 5th year of age. The 'arrSingleYear'
  !  variable is the output variable containing the interpolated graduated
  !  single-year-of-age values. By default, the resulting interpolated
  !  single-year-of-age data will line up so that every fifth age matches the
  !  original data that was passed in. However, if the optional argument
  !  'modCoeff' is passed in as true, then the resulting single-year-of-age
  !  data will be smoother but every fifth age will not be required to match
  !  the original data.
  !  SYNOPSIS
  !  subroutine BeersInterpolate(arrFiveYear, arrSingleYear, modCoeff)
  !  ARGUMENTS
  !  * arrFiveYear: 1-dimensional array (of any size) of reals or double
  !    precisions. This is the array of values for every fifth age.
  !  * arrSingleYear: 1-dimensional array (of any size) of reals or double
  !    precisions. This contains the output (the interpolated graduated values)
  !    by single year of age.
  !  * modCoeff (optional): If false or omitted, the subroutine uses the
  !    standard coefficients. If true, the subroutine uses modified coefficients
  !    which allow for additional smoothness by not requiring the
  !    single age results to match the original input data at every fifth age.
  !  ARGUMENT DECLARATIONS
  !  See ARGUMENTS section.
  !*****
  interface BeersInterpolate
    module procedure BeersInterpolateReal, BeersInterpolateDoublePrec
  end interface BeersInterpolate

  ! Allow WhitHend to be the generic name for the specific subroutines.
  !****f* GraduationMod/WhitHend
  !  NAME
  !  WhitHend: This subroutine does Whittaker-Henderson graduating.
  !  DESCRIPTION
  !  The array to be graduated is passes in as the variable 'arr'.  The variables
  !  'degree', 'smooth', and 'weights', are parameters for the Whittaker-Henderson
  !  smoothing.  The 'grad' variable is an output variable containing the
  !  graduated values of 'arr'.
  !  SYNOPSIS
  !  subroutine WhitHend(arr, degree, smooth, weights, grad)
  !  ARGUMENTS
  !  * arr: 1-dimensional array (of any size) of reals or double precisions.
  !    This is the array of values needing to be graduated. 
  !  * degree: The degree for smoothing, as an integer.
  !  * smooth: The smoothness parameter, as a real or double precision.
  !  * weights: 1-dimensional array (of the same size as 'arr') of reals or
  !    double precisions. These are the weights used for the graduation.
  !  * grad: 1-dimensional array (of the same size as 'arr') of reals or
  !    double precisions. These are the graduated results.
  !  ARGUMENT DECLARATIONS
  !  See ARGUMENTS section.
  !*****
  interface WhitHend
    module procedure WhitHendReal, WhitHendDoublePrec
  end interface WhitHend

  ! Allow WhitHend2D to be the generic name for the specific subroutines.
  !****f* GraduationMod/WhitHend2D
  !  NAME
  !  WhitHend2D: This subroutine does Whittaker-Henderson 2-dimensional graduating.
  !  (It's actually Whittaker-Henderson-Lowrie graduating.)
  !  DESCRIPTION
  !  The 2-dimensional array to be graduated is passes in as the variable
  !  'raw2D'. The variables 'horder' and 'vorder' are the orders of differences
  !  to minimize (1,2,3 or 4). The variables 'h' and 'v' are the smoothness
  !  factors. The variable 'weight2D' contains the weights for each raw value.
  !  The 'grad2D' variable is an output variable containing the graduated values
  !  of 'raw2D'.
  !  SYNOPSIS
  !  subroutine WhitHend2D(raw2D, horder, vorder, h, v, weight2D, grad2D)
  !  ARGUMENTS
  !  * raw2D: 2-dimensional array (of any size) of reals or double precisions.
  !    This is the array of values needing to be graduated. 
  !  * horder: The horizontal order of differences to minimize (1, 2, 3, or 4).
  !  * vorder: The vertical order of differences to minimize (1, 2, 3, or 4).
  !  * h: The horizontal smoothness factor.
  !  * v: The vertical smoothness factor.
  !  * weights2D: 2-dimensional array (of the same size as 'raw2D') of reals or
  !    double precisions. These are the weights used for the graduation.
  !  * grad2D: 2-dimensional array (of the same size as 'raw2D') of reals or
  !    double precisions. These are the graduated results.
  !  ARGUMENT DECLARATIONS
  !  See ARGUMENTS section.
  !*****
  interface WhitHend2D
    module procedure WhitHend2DReal, WhitHend2DDoublePrec
  end interface WhitHend2D

  contains

!******************************************************************************
  subroutine BeersReal(arrFiveYear, arrSingleYear, modCoeff, codeMod)

    real, dimension(:), intent(in) :: arrFiveYear
    real, dimension(:), intent(out) :: arrSingleYear
    logical, optional, intent(in) :: modCoeff
    logical, optional, intent(in) :: codeMod

    ! Have all the calculations be done with double precisions so as to increase
    ! accuracy. To do this, we'll need a temporary array of double precisions
    ! to hold the results.
    double precision, dimension(size(arrSingleYear)) :: tempArrDoublePrec

    call BeersDoublePrec(dble(arrFiveYear), tempArrDoublePrec, modCoeff, &
      codeMod)
    arrSingleYear = tempArrDoublePrec

  end subroutine BeersReal
!******************************************************************************
  subroutine BeersDoublePrec(arrFiveYear, arrSingleYear, modCoeff, codeMod)

    double precision, dimension(:), intent(in) :: arrFiveYear
    double precision, dimension(:), intent(out) :: arrSingleYear
    logical, optional, intent(in) :: modCoeff
    logical, optional, intent(in) :: codeMod

    double precision, dimension(5, 5, 5) :: beersSubCoeffs

    if (present(modCoeff)) then
      if (modCoeff) then
        beersSubCoeffs = MOD_BEERS_SUB_COEFF
      else
        beersSubCoeffs = BEERS_SUB_COEFF
      end if
    else
      beersSubCoeffs = BEERS_SUB_COEFF
    end if

    if (present(codeMod)) then
      if (codeMod) then
        call BeersModified(arrFiveYear, arrSingleYear, &
          beersSubCoeffs)
      else
        call BeersStandard(arrFiveYear, arrSingleYear, &
          beersSubCoeffs)
      end if
    else
      call BeersStandard(arrFiveYear, arrSingleYear, &
        beersSubCoeffs)
    end if

  end subroutine BeersDoublePrec
!******************************************************************************
  subroutine BeersStandard(fiveYearGroups, singleYearAges, beersSubCoeffs)
    ! This is the standard Beers routine. For various reasons, there are
    ! other modified versions of the Beers subroutines.

    double precision, dimension(1:), intent(in) :: fiveYearGroups
    double precision, dimension(0:), intent(out) :: singleYearAges
    double precision, dimension(5, 5, 5), intent(in) :: beersSubCoeffs

    integer :: numGroups
    integer :: numAges
    integer :: mult
    integer :: lowAge
    integer :: lowGroup

    numGroups = size(fiveYearGroups)
    numAges = size(singleYearAges)

    do mult = 1, numGroups
      if (mult == 1) then
        singleYearAges(0:4) = matmul(beersSubCoeffs(:, :, 1), &
          fiveYearGroups(1:5))
      else if (mult == 2) then
        singleYearAges(5:9) = matmul(beersSubCoeffs(:, :, 2), &
          fiveYearGroups(1:5))
      else if (mult <= numGroups - 2) then
        lowAge = 5 * (mult - 1)
        lowGroup = mult - 2
        singleYearAges(lowAge:lowAge + 4) = &
          matmul(beersSubCoeffs(:, :, 3), &
          fiveYearGroups(lowGroup:lowGroup + 4))
      else if (mult == numGroups-1) then
        lowAge = 5 * (mult - 1)
        singleYearAges(lowAge:lowAge + 4) = &
          matmul(beersSubCoeffs(:, :, 4), &
          fiveYearGroups(lowGroup:lowGroup + 4))
      else
        lowAge = 5 * (mult - 1)
        singleYearAges(lowAge:lowAge + 4) = &
          matmul(beersSubCoeffs(:, :, 5), &
          fiveYearGroups(lowGroup:lowGroup + 4))
      end if
    end do

  end subroutine BeersStandard
!******************************************************************************
  subroutine BeersModified(fiveYearGroups, singleYearAges, beersSubCoeffs)
    ! The Census program has some age group data where there are 0s in certain
    !   categories until a certain age.  Thus, to avoid odd bumps and having
    !   non zero numbers in ages where there should be, we adjust Beers
    !   slightly.  This used to be called "BeersCen".

    double precision, dimension(1:), intent(in) :: fiveYearGroups
    double precision, dimension(0:), intent(out) :: singleYearAges
    double precision, dimension(5, 5, 5), intent(in) :: beersSubCoeffs

    double precision, dimension(1:200) :: revisedGroups = 0.d0
    integer :: numGroups, numAges
    integer :: mult, lowAge, lowGroup, group, age
    integer :: midGroup, startGroup, endGroup

    numGroups = size(fiveYearGroups)
    numAges = size(singleYearAges)

    do mult = 1, numGroups

      lowAge = 5 * (mult - 1)
      if (mult <= 2) then
        startGroup = 1
      else if (mult <= numGroups - 2) then
        startGroup = mult - 2
      else
        startGroup = numGroups - 4
      end if
      endGroup = startGroup + 4

      if (mult == 1) then
        call LoadRevisedGroups(revisedGroups, fiveYearGroups, startGroup, 1)
        singleYearAges(lowAge:lowAge + 4) = &
          matmul(beersSubCoeffs(:, :, 1), revisedGroups(startGroup:endGroup))
      else if (mult == 2) then
        call LoadRevisedGroups(revisedGroups, fiveYearGroups, startGroup, 2)
        singleYearAges(lowAge:lowAge + 4) = &
          matmul(beersSubCoeffs(:, :, 2), revisedGroups(startGroup:endGroup))
      else if (mult <= numGroups - 2) then
        call LoadRevisedGroups(revisedGroups, fiveYearGroups, startGroup, 3)
        singleYearAges(lowAge:lowAge + 4) = &
          matmul(beersSubCoeffs(:, :, 3), revisedGroups(startGroup:endGroup))
      else if (mult == numGroups - 1) then
        call LoadRevisedGroups(revisedGroups, fiveYearGroups, startGroup, 4)
        singleYearAges(lowAge:lowAge + 4) = &
          matmul(beersSubCoeffs(:, :, 4), revisedGroups(startGroup:endGroup))
      else
        call LoadRevisedGroups(revisedGroups, fiveYearGroups, startGroup, 5)
        singleYearAges(lowAge:lowAge + 4) = &
          matmul(beersSubCoeffs(:, :, 5), revisedGroups(startGroup:endGroup))
      end if
    end do

  end subroutine BeersModified
!******************************************************************************
  subroutine LoadRevisedGroups(revisedGroups, fiveYearGroups, startGroup, pow)
    ! This subroutine is used by the BeersModified subroutine to assist in the
    !   modified version of Beers to produce realistic estimates of data
    !   that may have zeros until a certain age group.

    double precision, dimension(1:200), intent(out) :: revisedGroups
    double precision, dimension(1:), intent(in) :: fiveYearGroups
    integer, intent(in) :: startGroup
    integer, intent(in) :: pow

    integer :: baseGroup
    integer :: endGroup
    integer :: midGroup
    integer :: group
    integer :: numGroups
    integer, parameter, dimension(5, 1:5) :: POWER = (/ &
      0, 1, 2, 3, 4 , &
      1, 0, 1, 2, 3 , &
      2, 1, 0, 1, 2 , &
      3, 2, 1, 0, 1 , &
      4, 3, 2, 1, 0 /)

    midGroup = startGroup + 2
    endGroup = startGroup + 4
    select case (pow)
      case (1)
        baseGroup = 1
      case (2)
        baseGroup = 2
      case (4)
        baseGroup = size(fiveYearGroups) - 1
      case (5)
        baseGroup = size(fiveYearGroups)
      case default
        baseGroup = midGroup
    end select

    do group = startGroup, endGroup ! five consecutive groups
      revisedGroups(group) = min(fiveYearGroups(group), &
        fiveYearGroups(baseGroup) * 2 ** POWER(pow, group - startGroup + 1))
    end do

  end subroutine LoadRevisedGroups
!******************************************************************************
  subroutine BeersInterpolateReal(arrFiveYear, arrSingleYear, modCoeff)

    real, dimension(:), intent(in) :: arrFiveYear
    real, dimension(:), intent(out) :: arrSingleYear
    logical, optional, intent(in) :: modCoeff

    ! Have all the calculations be done with double precisions so as to increase
    ! accuracy. To do this, we'll need a temporary array of double precisions
    ! to hold the results.
    double precision, dimension(size(arrSingleYear)) :: tempArrDoublePrec

    call BeersInterpolateDoublePrec(dble(arrFiveYear), tempArrDoublePrec, &
      modCoeff)
    arrSingleYear = tempArrDoublePrec

  end subroutine BeersInterpolateReal
!******************************************************************************
  subroutine BeersInterpolateDoublePrec(arrFiveYear, arrSingleYear, modCoeff)

    double precision, dimension(:), intent(in) :: arrFiveYear
    double precision, dimension(:), intent(out) :: arrSingleYear
    logical, optional, intent(in) :: modCoeff

    double precision, dimension(5, 6, 5) :: beersIntCoeffs
    integer :: numGroups
    integer :: numAges
    integer :: mult
    integer :: lowAge
    integer :: lowGroup

    if (present(modCoeff)) then
      if (modCoeff) then
        beersIntCoeffs = MOD_BEERS_INT_COEFF
      else
        beersIntCoeffs = BEERS_INT_COEFF
      end if
    else
      beersIntCoeffs = BEERS_INT_COEFF
    end if

    numGroups = size(arrFiveYear)
    numAges = size(arrSingleYear)

    do mult = 1, numGroups
      if (mult == 1) then
        arrSingleYear(0:4) = matmul(beersIntCoeffs(:, :, 1), arrFiveYear(1:6))
      else if (mult == 2) then
        arrSingleYear(5:9) = matmul(beersIntCoeffs(:, :, 2), arrFiveYear(1:6))
      else if (mult <= numGroups - 2) then
        lowAge = 5 * (mult - 1)
        lowGroup = mult - 2
        arrSingleYear(lowAge:lowAge + 4) = &
          matmul(beersIntCoeffs(:, :, 3), arrFiveYear(lowGroup:lowGroup + 5))
      else if (mult == numGroups-1) then
        lowAge = 5 * (mult - 1)
        arrSingleYear(lowAge:lowAge + 4) = &
          matmul(beersIntCoeffs(:, :, 4), arrFiveYear(lowGroup:lowGroup + 5))
      else
        lowAge = 5 * (mult - 1)
        arrSingleYear(lowAge:lowAge + 4) = &
          matmul(beersIntCoeffs(:, :, 5), arrFiveYear(lowGroup:lowGroup + 5))
      end if
    end do
    arrSingleYear(numAges - 1) = arrFiveYear(numGroups)

  end subroutine BeersInterpolateDoublePrec
!******************************************************************************
  subroutine WhitHendReal(arr, degree, smooth, weights, grad)

    real, dimension(:), intent(in) :: arr
    integer, intent(in) :: degree
    real, intent(in) :: smooth
    real, dimension(size(arr)), intent(in) :: weights
    real, dimension(size(arr)), intent(out) :: grad

    ! The weights passed in are set to the diagonal of this square matrix
    double precision, dimension(size(arr), size(arr)) :: weightsDiag
    ! The difference matrix.
    double precision, dimension(size(arr), size(arr)) :: diffMatrix
    ! An array of factors to be used to compute the final, graduated array.
    double precision, dimension(size(arr), size(arr)) :: factArr

    integer :: iRow
    integer :: iCol
    integer :: iDiff
    integer :: arrSize
    ! A single row of Pascal's Triangle. Used for setting values in diffMatrix.
    integer, dimension(degree + 1) :: pt

    arrSize = size(arr)

    ! Weights need to add up to 1.0.
    if (abs(sum(weights) - 1.0d0) <= 0.000001d0) then
      write(6, '(A)') "Error in WhitHend: Weights do not add up to 1.0!"
      write(6, '(A)') "Ending program!"
      stop
    end if

    ! Initialize the weightsDiag array to 0, and then set the values on
    ! the diagonal to be the weights.
    weightsDiag = 0.0d0
    do iRow = 1, arrSize
      weightsDiag(iRow, iRow) = weights(iRow)
    end do

    ! Set the values for the row that we'll need from the Pascal's Triangle.
    ! We need values for the row the contains "degree + 1" elements.
    ! Normally, all values are postive.  But for this, we need all the even
    ! elements to be negative.
    ! The first value on a line is always 1. Subsequent values can be
    ! calculated from the prior value.
    pt(1) = 1
    do iRow = 2, degree + 1
      pt(iRow) = -1 * pt(iRow - 1) * (degree + 2 - iRow) / (iRow - 1)
    end do

    ! Initialize all values to 0, then set some of the values in the
    ! first few rows using the 'pt' array.  The rest of the values in the
    ! row remain 0.  And all the bottom rows (after 'arrSize - degree')
    ! remain 0.
    diffMatrix = 0.0d0
    do iRow = 1, arrSize - degree
      diffMatrix(iRow, iRow:iRow + degree) = pt
    end do

    factArr = &
      Inverse(weightsDiag + smooth * matmul(transpose(diffMatrix), diffMatrix))

    grad = matmul(factArr, matmul(weightsDiag, arr))

  end subroutine WhitHendReal
!******************************************************************************
  subroutine WhitHendDoublePrec(arr, degree, smooth, weights, grad)

    double precision, dimension(:), intent(in) :: arr
    integer, intent(in) :: degree
    double precision, intent(in) :: smooth
    double precision, dimension(size(arr)), intent(in) :: weights
    double precision, dimension(size(arr)), intent(out) :: grad

    ! The weights passed in are set to the diagonal of this square matrix
    double precision, dimension(size(arr), size(arr)) :: weightsDiag
    ! The difference matrix.
    double precision, dimension(size(arr), size(arr)) :: diffMatrix
    ! An array of factors to be used to compute the final, graduated array.
    double precision, dimension(size(arr), size(arr)) :: factArr

    integer :: iRow
    integer :: iCol
    integer :: iDiff
    integer :: arrSize
    ! A single row of Pascal's Triangle. Used for setting values in diffMatrix.
    integer, dimension(degree + 1) :: pt

    arrSize = size(arr)

    ! Weights need to add up to 1.0.
    if (abs(sum(weights) - 1.0d0) > 0.000001d0) then
      write(6, '(A)') "Error in WhitHend: Weights do not add up to 1.0!"
      write(6, '(A)') "Ending program!"
      stop
    end if

    ! Initialize the weightsDiag array to 0, and then set the values on
    ! the diagonal to be the weights.
    weightsDiag = 0.0d0
    do iRow = 1, arrSize
      weightsDiag(iRow, iRow) = weights(iRow)
    end do

    ! Set the values for the row that we'll need from the Pascal's Triangle.
    ! We need values for the row the contains "degree + 1" elements.
    ! Normally, all values are postive.  But for this, we need all the even
    ! elements to be negative.
    ! The first value on a line is always 1. Subsequent values can be
    ! calculated from the prior value.
    pt(1) = 1
    do iRow = 2, degree + 1
      pt(iRow) = -1 * pt(iRow - 1) * (degree + 2 - iRow) / (iRow - 1)
    end do

    ! Initialize all values to 0, then set some of the values in the
    ! first few rows using the 'pt' array.  The rest of the values in the
    ! row remain 0.  And all the bottom rows (after 'arrSize - degree')
    ! remain 0.
    diffMatrix = 0.0d0
    do iRow = 1, arrSize - degree
      diffMatrix(iRow, iRow:iRow + degree) = pt
    end do

    factArr = &
      Inverse(weightsDiag + smooth * matmul(transpose(diffMatrix), diffMatrix))

    grad = matmul(factArr, matmul(weightsDiag, arr))

  end subroutine WhitHendDoublePrec
!******************************************************************************
  subroutine WhitHend2DReal(raw2D, horder, vorder, h, v, weight2D, grad2D)
    ! Graduate using Whittaker-Henderson-Lowrie in 2 dimensions.
    ! raw2D: ungraduated values
    ! horder, vorder: orders of differences to minimize (1,2,3 or 4)
    ! h,v : smoothness factors.
    ! weight2D: the weights for each raw value
    ! grad2D: graduated values
    real, dimension (:, :), intent(in) :: raw2D
    integer, intent(in) :: horder
    integer, intent(in) :: vorder
    real, intent(in) :: h
    real, intent(in) :: v
    real, dimension (size(raw2D, 1), size(raw2D, 2)), &
      intent(in) :: weight2D
    real, dimension (size(raw2D, 1), size(raw2D, 2)), &
      intent(out) :: grad2D
    ! These final five parameters are only used if you want to use
    !   a standard grid and only give some credibility to the
    !   specific data.
    double precision :: hrate
    double precision :: vrate
    double precision :: l
    double precision, dimension(0:size(raw2D) - 1) :: grad
    double precision, dimension(0:size(raw2D) - 1) :: orig
    double precision, dimension(0:size(raw2D) - 1) :: weight
    double precision, dimension(0:4) :: stat
    integer :: row
    integer :: k
    integer :: col
    integer :: rv
    integer :: rowSize
    integer :: colSize

    rowSize = size(raw2D, 1)
    colSize = size(raw2D, 2)
    ! The WHL2Grad subroutine code, originally in C, is set up to have
    !   the data in just a 1D array so set that up.  Also, C likes
    !   to start at index 0 to confuse us even more.
    do row = 1, rowSize
      do col = 1, colSize
        k = (row - 1) * (colSize) + col - 1
        weight(k) = weight2D(row, col)
        orig(k) = raw2D(row, col)
      end do
    end do
    ! The WHL2Grad allows the user to weight your graduation with
    !   a standard table but we will just say there is no standard
    !   table and thus just pass 0's (or arrays of 0s)
    !   for the last five parameters.
    rv = WHL2Grad(rowSize, colSize, &
      horder, vorder, dble(h), dble(v), orig, weight, grad, stat, &
      (/ 0.0d0 /), (/ 0.0d0 /), 0.0d0, 0.0d0, 0.0d0)
    if (rv /= 0) then
      write(6, *) "Error in WHL2Grad!  Ending program!"
      stop
    end if
    do row = 1, rowSize
      do col = 1, colSize
        grad2D(row, col) = grad((row - 1) * (colSize) + col - 1)
      end do
    end do

  end subroutine WhitHend2DReal
!******************************************************************************
  subroutine WhitHend2DDoublePrec(raw2D, horder, vorder, h, v, weight2D, grad2D)
    ! Graduate using Whittaker-Henderson-Lowrie in 2 dimensions.
    ! raw2D: ungraduated values
    ! horder, vorder: orders of differences to minimize (1,2,3 or 4)
    ! h,v : smoothness factors.
    ! weight2D: the weights for each raw value
    ! grad2D: graduated values
    double precision, dimension (:, :), intent(in) :: raw2D
    integer, intent(in) :: horder
    integer, intent(in) :: vorder
    double precision, intent(in) :: h
    double precision, intent(in) :: v
    double precision, dimension (size(raw2D, 1), size(raw2D, 2)), &
      intent(in) :: weight2D
    double precision, dimension (size(raw2D, 1), size(raw2D, 2)), &
      intent(out) :: grad2D
    ! These final five parameters are only used if you want to use
    !   a standard grid and only give some credibility to the
    !   specific data.
    double precision :: hrate
    double precision :: vrate
    double precision :: l
    double precision, dimension(0:size(raw2D) - 1) :: grad
    double precision, dimension(0:size(raw2D) - 1) :: orig
    double precision, dimension(0:size(raw2D) - 1) :: weight
    double precision, dimension(0:4) :: stat
    integer :: row
    integer :: k
    integer :: col
    integer :: rv
    integer :: rowSize
    integer :: colSize

    rowSize = size(raw2D, 1)
    colSize = size(raw2D, 2)
    ! The WHL2Grad subroutine code, originally in C, is set up to have
    !   the data in just a 1D array so set that up.  Also, C likes
    !   to start at index 0 to confuse us even more.
    do row = 1, rowSize
      do col = 1, colSize
        k = (row - 1) * (colSize) + col - 1
        weight(k) = weight2D(row, col)
        orig(k) = raw2D(row, col)
      end do
    end do
    ! The WHL2Grad allows the user to weight your graduation with
    !   a standard table but we will just say there is no standard
    !   table and thus just pass 0's (or arrays of 0s)
    !   for the last five parameters.
    rv = WHL2Grad(rowSize, colSize, &
      horder, vorder, h, v, orig, weight, grad, stat, &
      (/ 0.0d0 /), (/ 0.0d0 /), 0.0d0, 0.0d0, 0.0d0)
    if (rv /= 0) then
      write(6, *) "Error in WHL2Grad!  Ending program!"
      stop
    end if
    do row = 1, rowSize
      do col = 1, colSize
        grad2D(row, col) = grad((row - 1) * (colSize) + col - 1)
      end do
    end do

  end subroutine WhitHend2DDoublePrec
!******************************************************************************
  function WHL2Grad(row, col, horder, vorder, h, v, raw, &
    weight, grad, stat, wt_std, std, hrate, vrate, l) result(functionResult)

    !***************************************************************************
    ! This code and associated functions and subroutines originally came from
    !  C code emailed from Bob Howard.  The email from Bob with the original
    !  code is located at: 
    !  \\lrserv1\usr\dem.21\Common\pro\Re EXTERNAL Re Two Dimensional Whittaker-Henderson Graduation.msg
    ! Note that this is what the SOA uses for their improvement tool at:
    !  https://www.soa.org/globalassets/assets/files/research/exp-study/research-2014-mp-report.pdf
    !***************************************************************************

    ! Graduate using Whittaker-Henderson-Lowrie in 2 dimensions.
    ! row, col: the number of elements to graduate in each dimension
    ! horder, vorder: orders of differences to minimize (1,2,3 or 4)
    ! h,v : smoothness factors.
    ! l: balance between fit to raw data and standard table
    !    The expression to minimize is F*(1-l) + Tsum*l + h*HS + v*VS
    !    where Tsum is the weighted sum of squared error between graduated and standard table
    !          F is the weighted sum of squared error between graduated and raw data
    !     HS, VS are horizontal and vertical smoothness
    ! rate: the rate of growth of an exponential as the standard of smoothness
    ! raw: ungraduated values
    ! weight: the weights for each raw value
    ! grad: graduated values
    ! stat: F*(1-l) + Tsum*l + h*HS + v*VS, F, HS, VS, Tsum
    ! wt_std: the weights for the standard table
    ! std: the rates for the standard table
    integer, intent(in) :: row
    integer, intent(in) :: col
    integer, intent(in) :: horder
    integer, intent(in) :: vorder
    double precision, intent(in) :: h
    double precision, intent(in) :: v
    double precision, dimension (0:row * col - 1), intent(in) :: raw
    double precision, dimension (0:row * col - 1) :: weight
    double precision, dimension (0:row * col - 1), intent(out) :: grad
    double precision, dimension(0:4), intent(out) :: stat
    ! These final five parameters are only used if you want to use
    !   a standard grid and only give some credibility to the
    !   specific data.
    double precision, dimension (0:row * col - 1), intent(in) :: wt_std
    double precision, dimension (0:row * col - 1), intent(in) :: std
    double precision, intent(in) :: hrate
    double precision, intent(in) :: vrate
    double precision, intent(in) :: l
    integer :: functionResult

    double precision, allocatable, dimension(:) :: a ! the coefficient matrix
    double precision, dimension(0:row * (vorder + 1) - 1) :: a1 ! 1st block of vertical coefficient matrix
    double precision, dimension(0:row * col - 1) :: val ! values of equations
    double precision :: w, F, HS, VS
    ! Note that "Tsum" was renamed from the original C code "T" because there is also a "t" variable
    !   and, unlike C, fortran is not case sensitive.
    double precision :: Tsum
    double precision, dimension(0:48) :: t ! template matrix for difference equations
    integer :: i, j, k
    integer :: n, width, r, c
    integer :: rc
    integer :: alloc_stat

    ! Dimension now
    allocate(a(0:row * col * (1 + col * vorder) - 1), stat = alloc_stat)

    n = row * col
    functionResult = 0

    ! Normalize weights to sum to total number of data points
    weight = weight * dble(n) / sum(weight)

    ! Checks to see if it is possible to do graduation with received parameters
    ! need at least 2order+1 elements
    if (col < 2 * horder + 1) then
      functionResult = 11
      return
    end if
    ! need at least 2order+1 elements
    if (row < 2 * vorder + 1) then
      functionResult = 11
      return
    end if
    ! will handle order 1,2,3,4, but 3 is most common
    if (horder < 1 .or. horder > 4) then
      functionResult = 12
      return
    end if
    ! will handle order 1,2,3,4, but 3 is most common
    if (vorder < 1 .or. vorder > 4) then
      functionResult = 12
      return
    end if
    do i = 0, n - 1
      ! weights must be non-negative
      if (weight(i) < 0) then
        functionResult = 13
        return
      end if
    end do
    ! smoothness factor must be positive
    if (h <= 0) then
      functionResult = 14
      return
    end if
    ! smoothness factor must be positive
    if (v <= 0) then
      functionResult = 14
      return
    end if
    ! fit to table must be between 0 and 1.
    if (l < 0 .or. l > 1) then
      functionResult = 15
      return
    end if
    if (l /= 0) then
      do i = 0, n - 1
        ! weights must be non-negative
        if (wt_std(i) < 0) then
          functionResult = 16
          return
        end if
      end do
    end if

    width = 1 + col * vorder ! width of banded matrix

    ! Add horizontal difference formula
    t = 0.0d0
    r = DFtemplate(horder, col, 1, h, hrate, t) ! template matrix for horizontal differences
    c = horder + 1 ! width of band for horizontal template
    call MakeCoeff(horder, r, c, col, width, t, a) ! coefficient matrix for 1st block

    do k = col, n - 1, col
      do i = 0, col - 1
        do j = 0, c - 1
          a((i + k) * width + j) = a(i * width + j)
        end do
      end do
    end do

    ! add vertical difference formulae
    r = DFtemplate(vorder, row, 1, v, vrate, t) ! template matrix for vertical differences
    c = vorder + 1 ! width of band for vertical smoothness
    call MakeCoeff(vorder, r, c, row, c, t, a1) ! coefficient matrix for a vertical block
    do i = 0, row - 1
      do k = 0, col - 1
        a((i * col + k) * width) = &
          a((i * col + k) * width) + a1(i * c) ! add to horizontal
      end do
      do j = 1, c - 1
        do k = 0, col - 1 ! no overlap in farther columns
          a((i * col + k) * width + j * col) = a1(i * c + j)
        end do
      end do
    end do

    ! add weights and determine values of equations
    do i = 0, n - 1
      if (l == 0) then
        a(i * width) = a(i * width) + weight(i) ! add weights to "main diagonal"
        val(i) = raw(i) * weight(i) ! value of equations
      else
        a(i * width) = a(i * width) + weight(i) * (1 - l) & ! add weights to "main diagonal"
          + wt_std(i) * l
        val(i) = raw(i) * weight(i) * (1 - l) &
          + std(i) * wt_std(i) * l ! value of equations
      end if
    end do

    rc = SolveBand(width, n, grad, val, a)

    if (rc /= 0) then
      functionResult = rc
      return
    end if

    ! calculate stats for graduation
    F = 0
    do i = 0, n - 1
      w = raw(i) - grad(i)
      F = F + weight(i) * (w * w) ! value for Fit
    end do
    if (l /= 0) then
      Tsum = 0
      do i = 0, n - 1
        w = grad(i) - std(i)
        Tsum = Tsum + w * w * wt_std(i) ! fit to standard table
      end do
    else
      Tsum = 0
    end if
    HS = 0
    do i = 0, row - 1
      HS = HS + DiffSq(horder, col, hrate, (grad + i * col)) ! value for horizontal smoothness
    end do
    do i = 0, row - 1
      do j = 0, col - 1
        val(j * row + i) = grad(i * col + j) ! transpose before calculating smoothness
      end do
    end do
    VS = 0
    do i = 0, col - 1
      VS = VS + DiffSq(vorder, row, vrate, (val + i * row)) ! value for vertical smoothness
    end do
    stat(0) = F + h * HS + v * VS + l * Tsum ! minimized expression
    stat(1) = F
    stat(2) = HS
    stat(3) = VS
    stat(4) = Tsum

    deallocate(a)

  end function WHL2Grad
!******************************************************************************
  function SolveBand(b, n, soln, val, matrix) result(rc)
    ! Solve a system of equations with the matrix expressed in banded form 
    ! n: the number of rows and columns in the real square matrix
    ! soln: vector of solutions to equations
    ! val: vector of values of equations
    ! matrix: coefficient matrix in b-banded form
    integer, intent(in) :: b
    integer, intent(in) :: n
    double precision, dimension(0:n - 1), intent(out) :: soln
    double precision, dimension(0:n - 1), intent(in) :: val
    double precision, dimension(0:) :: matrix

    double precision, allocatable, dimension(:) :: ltri ! lower triangle matrix, in b-banded form
    integer :: rc
    integer :: alloc_stat

    allocate(ltri(0:size(matrix) - 1), stat = alloc_stat)

    rc = CholeskyBand(b, n, ltri, matrix)
    if (rc /= 0) then
      return
    end if
    rc = ForBackBand(b, n, soln, val, ltri)
    rc = 0

    deallocate (ltri)

  end function SolveBand
!******************************************************************************
  function CholeskyBand(b, n, z, a) result(functionResult)
    ! perform Cholesky decomposition to obtain a lower matrix
    ! both matrices are in b-banded format
    ! n: the number of rows and columns in the real square matrix
    ! a: matrix to decompose, in b-banded form
    ! z: the lower triangular matrix in b-banded form

    integer, intent(in) :: b
    integer, intent(in) :: n
    double precision, dimension(0:n * b - 1), intent(out) :: z
    double precision, dimension(0:n * b - 1) :: a
    integer :: i, j, k, i1
    double precision :: w
    integer :: functionResult

    do i = 0, n - 1
      w = a(i * b)
      do k = 1, min(i + 1, b) - 1
        w = w - z(i * b + k) * z(i * b + k)
      end do

      if (w >= 0) then
        i1 = i * b
        z(i1) = sqrt(w)
      else
        ! matrix not positive definite!
        functionResult = -21
        return
      end if
      do j = 1, min(n - i, b) - 1
        w = a(i * b + j)
        do k = 1, min(i + 1, b - j) - 1
          w = w - z(i * b + k) * z((i + j) * b + j + k)
        end do

        i1 = (i + j) * b + j
        z(i1) = w / z(i * b)

      end do
    end do

    functionResult = 0

  end function CholeskyBand
!******************************************************************************
  function ForBackBand(b, n, z, val, a) result (functionResult)
    ! perform forward substitution, followed by backward substitution
    ! b: number of bands (columns) in band matrix
    ! n: number of rows in band matrix
    ! z: the solution vector
    ! val: the value vector
    ! a: the lower triangular matrix, in reversed b-banded form
    integer, intent(in) :: b
    integer, intent(in) :: n
    double precision, dimension(0:n - 1), intent(out) :: z
    double precision, dimension(0:n - 1) :: val
    double precision, dimension(0:n * b - 1) :: a

    double precision, dimension(0:n - 1) :: y ! intermediate solution
    double precision :: w
    integer i, j
    integer :: functionResult

    ! forward substitution
    do i = 0, n - 1
      w = val(i)
      do j = 1, min(i, b - 1)
        w = w - a(i * b + j) * y(i - j)
      end do
      y(i) = w / a(i * b)
    end do

    ! backward substitution
    do i = n - 1, 0, -1
      w = y(i)
      do j = 1, min(n - i -1, b - 1)
        w = w - a((i + j) * b + j) * z(i  + j)
      end do
      z(i) = w / a(i * b)
    end do

    functionResult = 0

  end function ForBackBand
!******************************************************************************
  function DiffSq(order, n, rate, grad) result (S)
    ! sum of squared differences.  Do exponential if rate /= 0.
    integer, intent(in) :: order
    integer, intent(in) :: n
    double precision, intent(in) :: rate
    double precision, dimension(0:n - 1), intent(in) :: grad

    double precision, dimension(0:4) :: w, v
    integer :: i, j
    double precision :: S, d

    call DFormula(order, w)
    if (rate /= 0) then
      call DFormula(order - 1, v)
      do i = 0, 4
        w(i) = w(i) - v(i) * rate
      end do
    end if
    S = 0
    do i = 0, n - order - 1
      d = 0
      do j = 0, order
        d = d + w(j) * grad(i + j)
      end do
      S = S + d * d
    end do

  end function DiffSq
!******************************************************************************
  subroutine DFormula(order, formula)

    integer, intent(in) :: order
    double precision, dimension(0:4), intent(out) :: formula

    ! difference formula
    select case (order)
      case (3)
        formula(0) = -1
        formula(1) = 3
        formula(2) = -3
        formula(3) = 1
        formula(4) = 0
      case (2)
        formula(0) = 1
        formula(1) = -2
        formula(2) = 1
        formula(3) = 0
        formula(4) = 0
      case (4)
        formula(0) = 1
        formula(1) = -4
        formula(2) = 6
        formula(3) = -4
        formula(4) = 1
      case (1)
        formula(0) = -1
        formula(1) = 1
        formula(2) = 0
        formula(3) = 0
        formula(4) = 0
      case default
        write(6, *) "Error in DFormula!  'order' must be from 1 - 4!"
        write(6, *) "Ending program!"
        stop
    end select

  end subroutine DFormula
!******************************************************************************
  subroutine MakeCoeff(order, r, c, row, col, t, a)
    ! make a coefficient matrix, a, from template matrix, t.
    ! r,c = number of rows and columns in template
    ! row,col = number of rows and columns in coefficient matrix
    integer, intent(in) :: order
    integer, intent(in) :: r
    integer, intent(in) :: c
    integer, intent(in) :: row
    integer, intent(in) :: col
    double precision, dimension(0:48), intent(in) :: t
    double precision, dimension(0:), intent(out) :: a

    integer :: i, i1, j

    a = 0.0d0
    if (r == row) then ! then the template is the whole block
      do i = 0, row - 1
        do j = 0, c - 1
          a(i * col + j) = t(i * c + j)
        end do
      end do
    else ! then the middle row is replicated to fit the block
      do i = 0, order - 1
        do j = 0, c - 1
          a(i * col + j) = t(i * c + j)
        end do
      end do
      do i = order, row - order - 1
        do j = 0, c - 1
          a(i * col + j) = t(order * c + j)
        end do
      end do
      i1 = order + 1
      do i = row - order, row - 1
        do j = 0, c - 1
          a(i * col + j) = t(i1 * c + j)
        end do
        i1 = i1 + 1
      end do
    end if

  end subroutine MakeCoeff
!******************************************************************************
  function DFtemplate(order, n, banded, h, rate, t) result(c)
    ! develop template for difference formulae, either square or banded
    ! returns the number of rows of the template matrix.
    ! if rate = 0, use normal Whittaker-Henderson
    ! if rate != 0, use Lowrie's exponential form, at rate "rate".
    integer, intent(in) :: order
    integer, intent(in) :: n
    integer, intent(in) :: banded
    double precision, intent(in) :: h
    double precision, intent(in) :: rate
    double precision, dimension(0:48), intent(out) :: t

    double precision, dimension(0:4) :: d ! difference formula
    double precision, dimension(0:4) :: de ! difference formula for exponential, if needed
    double precision, dimension(0:4, 0:8) :: e ! difference matrix
    double precision, dimension(0:8, 0:8) :: f
    double precision :: w
    integer :: i, j, k, r, c, b

    call DFormula(order, d)
    if (rate /= 0) then
      call DFormula(order - 1, de)
      do i = 0, 4
        d(i) = d(i) - de(i) * rate
      end do
      ! then smoothness is exponential at rate plus polynomial of degree order-2
      ! if rate is zero, smoothness is exponential of degree order-1
    end if
    r = min(order + 1, n - order)
    c = min(2 * order + 1, n)
    do i = 0, r - 1
      ! make difference matrix
      do j = 0, i - 1
        e(i, j) = 0.0d0
      end do
      do j = i, r + i - 1
        e(i, j) = d(j - i)
      end do
      do j = r + i, c - 1
        e(i, j) = 0.0d0
      end do
    end do
    do i = 0, c - 1 ! matrix multiply by transpose - squared difference formula
      do j = 0, c - 1
        w = 0.0d0
        do k = 0, r - 1
          w = w + e(k, i) * e(k, j)
        end do
        f(i, j) = w
      end do
    end do
    if (banded) then
      b = order + 1 ! convert to a vector of b-band form
      do i = 0, c - 1
        do j = 0, min(r, c - i) - 1
          t(i * b + j) = h * f(i, i + j)
        end do
        t(i * b + min(r, c - i):48) = 0 ! zero rest of row
      end do
    else ! convert to vector form
      do i = 0, c - 1
        do j = 0, c - 1
          t(i * c + j) = h * f(i, j)
        end do
      end do
    end if

  end function DFtemplate
!******************************************************************************
!******************************************************************************

end module GraduationMod
