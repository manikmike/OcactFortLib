! $Id: RegressionMod.f90 1.9 2017/06/22 15:04:27EDT 622325 Development  $
module RegressionMod

!****h* source/RegressionMod
!  NAME
!  RegressionMod: OcactFortLib RegressionMod module.
!  REVISION
!  $Revision: 1.9 $
!  MODULE DESCRIPTION
!  The RegressionMod contains subroutines and functions that are used to
!  calculate various regression statistics.
!  MODULE ROUTINES
!  AdjRSquared
!  Correlation
!  Covariance
!  ErrSS
!  FStat
!  RegIntercept
!  RegSlope
!  RegSS
!  RegStdDev
!  RegStdError
!  RegVariance
!  RSquared
!  StdDev
!  StdErrorIntercept
!  StdErrorSlope
!  TotSS
!  TStatIntercept
!  TStatSlope
!  Variance
!*****

  use StatTablesMod
  use MatrixMod

  implicit none
  save

  private
  public :: Variance, StdDev, Covariance, Correlation
  public :: RegSlope, RegIntercept
  public :: RegSS, ErrSS, TotSS
  public :: RSquared, AdjRSquared
  public :: RegVariance, RegStdDev, RegStdError
  public :: StdErrorSlope, StdErrorIntercept
  public :: ConfIntDevSlope, ConfIntDevIntercept
  public :: TStatSlope, TStatIntercept, FStat

  ! Allow Variance to be the generic name for the specific subroutines.
!****f* RegressionMod/Variance
!  NAME
!  Variance: This function returns the variance of the data.
!  DESCRIPTION
!  This function takes an array of data, in 'arr', and returns the
!  variance of the data. If 'isSample' is true, then the data is assumed
!  to be sample data, so a denominator of n-1 is used. If 'isSample' is
!  false or omitted, then the data is assumed to be population data, so a
!  denominator of n is used.
!  RETURNS
!  Real or Double Precision (same type as 'arr'): Variance of the data.
!  SYNOPSIS
!  real function Variance(arr, isSample) result(rv)
!  double precision function Variance(arr, isSample) result(rv)
!  ARGUMENTS
!  * arr: 1-dimensional array (of any size) of reals or double precisions.
!  * isSample: (optional) Logical. If true, the data is assumed to be sample
!    data. If false or omitted, the data is assumed to be population data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface Variance
    module procedure VarianceReal, VarianceDoublePrec
  end interface Variance

  ! Allow StdDev to be the generic name for the specific subroutines.
!****f* RegressionMod/StdDev
!  NAME
!  StdDev: This function returns the standard deviation of the data.
!  DESCRIPTION
!  This function takes an array of data, in 'arr', and returns the standard
!  deviation of the data. If 'isSample' is true, then the data is assumed
!  to be sample data, so a denominator of n-1 is used. If 'isSample' is
!  false or omitted, then the data is assumed to be population data, so a
!  denominator of n is used.
!  RETURNS
!  Real or Double Precision (same type as 'arr'): Standard Deviation of the
!  data.
!  SYNOPSIS
!  real function StdDev(arr, isSample) result(rv)
!  double precision function StdDev(arr, isSample) result(rv)
!  ARGUMENTS
!  * arr: 1-dimensional array (of any size) of reals or double precisions.
!  * isSample: (optional) Logical. If true, the data is assumed to be sample
!    data. If false or omitted, the data is assumed to be population data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface StdDev
    module procedure StdDevReal, StdDevDoublePrec
  end interface StdDev

  ! Allow Covariance to be the generic name for the specific subroutines.
!****f* RegressionMod/Covariance
!  NAME
!  Covariance: This function returns the covariance of two arrays of data.
!  DESCRIPTION
!  This function returns the covariance of two arrays of data, 'arr1',
!  and 'arr2'. Although 'arr1' and 'arr2' can be either reals or double
!  precisions, they must be of the same type (i.e., both reals or both
!  double precisions). If 'isSample' is true, then the data is assumed
!  to be sample data, so a denominator of n-1 is used. If 'isSample' is
!  false or omitted, then the data is assumed to be population data, so a
!  denominator of n is used.
!  RETURNS
!  Real or Double Precision (same type as 'arr'): Covariance of two arrays
!  of data.
!  SYNOPSIS
!  real function Covariance(arr1, arr2, isSample) result(rv)
!  double precision function Covariance(arr1, arr2, isSample) result(rv)
!  ARGUMENTS
!  * arr1: 1-dimensional array (of any size) of reals or double precisions (same
!    type as 'arr2').
!  * arr2: 1-dimensional array (of any size) of reals or double precisions (same
!    type as 'arr1').
!  * isSample: (optional) Logical. If true, the data is assumed to be sample
!    data. If false or omitted, the data is assumed to be population data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface Covariance
    module procedure CovarianceReal, CovarianceDoublePrec
  end interface Covariance

  ! Allow Correlation to be the generic name for the specific subroutines.
!****f* RegressionMod/Correlation
!  NAME
!  Correlation: This function returns the correlation of two arrays of data.
!  DESCRIPTION
!  This function returns the correlation of two arrays of data, 'arr1',
!  and 'arr2'. Although 'arr1' and 'arr2' can be either reals or double
!  precisions, they must be of the same type (i.e., both reals or both
!  double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arr'): Correlation of two arrays
!  of data.
!  SYNOPSIS
!  real function Correlation(arr1, arr2) result(rv)
!  double precision function Correlation(arr1, arr2) result(rv)
!  ARGUMENTS
!  * arr1: 1-dimensional array (of any size) of reals or double precisions (same
!    type as 'arr2').
!  * arr2: 1-dimensional array (of any size) of reals or double precisions (same
!    type as 'arr1').
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface Correlation
    module procedure CorrelationReal, CorrelationDoublePrec
  end interface Correlation

  ! Allow RegSlope to be the generic name for the specific subroutines.
!****f* RegressionMod/RegSlope
!  NAME
!  RegSlope: This function returns the slope coefficient.
!  DESCRIPTION
!  This function returns the slope coefficient for a regression equation
!  based on the passed in data. The dependent variable data is passed in
!  through the array 'arrY'. The independent variable data is  passed in
!  through the array 'arrX1'. If 'arrX1' is omitted, then it is assumed
!  to be an array containing consecutive integers (starting with 1) that
!  is the same size as 'arrY'. If there are 2 independent variables, the
!  second set of data is passed in through 'arrX2'. If 'arrX2' is passed
!  in, then you must specify which slope coefficient should be returned.
!  This is specified in the 'slopeNum' argument. The value of 'slopeNum'
!  should be 1 or 2 (correcsponding to 'arrX1' and 'arrX2', respectively).
!
!  Either 'arrX2' and 'slopeNum' should both be passed in, or they should
!  both be omitted. The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be
!  of the same type (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Slope coefficient.
!  SYNOPSIS
!  real function RegSlope(arrY, arrX1, arrX2, slopeNum) result(rv)
!  double precision function RegSlope(arrY, arrX1, arrX2, slopeNum) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * slopeNum: (optional) Identifies which slope coefficient to return. The
!    value of 'slopeNum' should be 1 or 2 (corresponding to 'arrX1' and 'arrX2',
!    respectively).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RegSlope
    module procedure RegSlopeReal, RegSlopeDoublePrec
  end interface RegSlope

  ! Allow RegIntercept to be the generic name for the specific subroutines.
!****f* RegressionMod/RegIntercept
!  NAME
!  RegIntercept: This function returns the intercept for a regression equation.
!  DESCRIPTION
!  This function returns the intercept for a regression equation based on
!  the passed in data.
!  The dependent variable data is passed in through
!  the array 'arrY'. The independent variable data is  passed in through
!  the array 'arrX1'. If 'arrX1' is omitted, then it is assumed to be
!  an array containing consecutive integers (starting with 1) that is the
!  same size as 'arrY'. If there are 2 independent variables, the second
!  set of data is passed in through 'arrX2'. The slopes, 'slope1' and
!  'slope2', can be omitted and the intercept will still be calculated
!  correctly. However, if they known already, they can be passed in.
!  This will increase the efficiency of the calculation of the intercept.
!  Of course, 'slope2' should only be passed in if 'arrX2' was passed in.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of the same
!  type (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Intercept of a regressions
!  equation.
!  SYNOPSIS
!  real function RegIntercept(arrY, arrX1, arrX2, slope1, slope2) result(rv)
!  double precision function RegIntercept(arrY, arrX1, arrX2, slope1, slope2) &
!    result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * slope1: (optional) Slope as an integer.
!  * slope2: (optional) Slope as an integer.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RegIntercept
    module procedure RegInterceptReal, RegInterceptDoublePrec
  end interface RegIntercept

  ! Allow RegSS to be the generic name for the specific subroutines.
!****f* RegressionMod/RegSS
!  NAME
!  RegSS: This function returns the regression sum of squares.
!  DESCRIPTION
!  This function returns the regression sum of squares for the passed in
!  data. The dependent variable data is passed in through the array
!  'arrY'. The independent variable data is passed in through the array
!  'arrX1'. If 'arrX1' is omitted, then it is assumed to be an array
!  containing consecutive integers (starting with 1) that is the same size
!  as 'arrY'. If there are 2 independent variables, the second set of data
!  is passed in through 'arrX2'.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of the same type
!  (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Regression sum of squares.
!  SYNOPSIS
!  real function RegSS(arrY, arrX1, arrX2) result(rv)
!  double precision function RegSS(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RegSS
    module procedure RegSSReal, RegSSDoublePrec
  end interface RegSS

  ! Allow ErrSS to be the generic name for the specific subroutines.
!****f* RegressionMod/ErrSS
!  NAME
!  ErrSS: This function returns the error sum of squares.
!  DESCRIPTION
!  This function returns the error sum of squares for the passed in data.
!  The dependent variable data is passed in through the array 'arrY'.
!  The independent variable data is  passed in through the array 'arrX1'.
!  If 'arrX1' is omitted, then it is assumed to be an array containing
!  consecutive integers (starting with 1) that is the same size as 'arrY'.
!  If there are 2 independent variables, the second set of data is passed
!  in through 'arrX2'.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must
!  all be of the same type (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Error sum of squares.
!  SYNOPSIS
!  real function ErrSS(arrY, arrX1, arrX2) result(rv)
!  double precision function ErrSS(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface ErrSS
    module procedure ErrSSReal, ErrSSDoublePrec
  end interface ErrSS

  ! Allow TotSS to be the generic name for the specific subroutines.
!****f* RegressionMod/TotSS
!  NAME
!  TotSS: This function returns the total sum of squares.
!  DESCRIPTION
!  This function returns the total sum of squares for the passed in data.
!  RETURNS
!  Real or Double Precision (same type as 'arr'): Total sum of squares.
!  SYNOPSIS
!  real function TotSS(arr) result(rv)
!  double precision function TotSS(arr) result(rv)
!  ARGUMENTS
!  * arr: 1-dimensional array (of any size) of reals or double precisions.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface TotSS
    module procedure TotSSReal, TotSSDoublePrec
  end interface TotSS

  ! Allow RSquared to be the generic name for the specific subroutines.
!****f* RegressionMod/RSquared
!  NAME
!  RSquared: This function returns the R-squared value of the regression.
!  DESCRIPTION
!  This function returns the R-squared value of the regression with the
!  passed in data. The dependent variable data is passed in through the
!  array 'arrY'. The independent variable data is  passed in through the
!  array 'arrX1'. If 'arrX1' is omitted, then it is assumed to be an array
!  containing consecutive integers (starting with 1) that is the same size
!  as 'arrY'. If there are 2 independent variables, the second set of
!  data is passed in through 'arrX2'.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of the same type
!  (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): R-sqaured value.
!  SYNOPSIS
!  real function RSquared(arrY, arrX1, arrX2) result(rv)
!  double precision function RSquared(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RSquared
    module procedure RSquaredReal, RSquaredDoublePrec
  end interface RSquared

  ! Allow AdjRSquared to be the generic name for the specific subroutines.
!****f* RegressionMod/AdjRSquared
!  NAME
!  AdjRSquared: This function returns the adjusted R-squared value.
!  DESCRIPTION
!  This function returns the adjusted R-squared value (sometimes called the
!  corrected R-squared value) of the regression with the passed in data.
!  The dependent variable data is passed in through the array 'arrY'. The
!  independent variable data is  passed in through the array 'arrX1'. If
!  'arrX1' is omitted, then it is assumed to be an array containing
!  consecutive integers (starting with 1) that is the same size as 'arrY'.
!  If there are 2 independent variables, the second set of data is passed
!  in through 'arrX2'.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must
!  all be of the same type (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Adjusted R-sqaured value.
!  SYNOPSIS
!  real function AdjRSquared(arrY, arrX1, arrX2) result(rv)
!  double precision function AdjRSquared(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface AdjRSquared
    module procedure AdjRSquaredReal, AdjRSquaredDoublePrec
  end interface AdjRSquared

  ! Allow RegVariance to be the generic name for the specific subroutines.
!****f* RegressionMod/RegVariance
!  NAME
!  RegVariance: This function returns the variance of the entire regression for
!  the passed in data.
!  DESCRIPTION
!  This function returns the variance of the entire regression for the
!  passed in data. The dependent variable data is passed in through the
!  array 'arrY'. The independent variable data is  passed in through the
!  array 'arrX1'. If 'arrX1' is omitted, then it is assumed to be an array
!  containing consecutive integers (starting with 1) that is the same size
!  as 'arrY'. If there are 2 independent variables, the second set
!  of data is passed in through 'arrX2'.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of the same type
!  (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Variance of the
!  entire regression for the data passed in.
!  SYNOPSIS
!  real function RegVariance(arrY, arrX1, arrX2) result(rv)
!  double precision function RegVariance(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RegVariance
    module procedure RegVarianceReal, RegVarianceDoublePrec
  end interface RegVariance

  ! Allow RegStdDev to be the generic name for the specific subroutines.
!****f* RegressionMod/RegStdDev
!  NAME
!  RegStdDev: This function returns the standard deviation of the entire
!  regression for the passed in data.
!  DESCRIPTION
!  This function returns the standard deviation (i.e., standard error) of
!  the entire regression for the passed in data. The dependent variable
!  data is passed in through the array 'arrY'. The independent variable
!  data is  passed in through the array 'arrX1'. If 'arrX1' is omitted,
!  then it is assumed to be an array containing consecutive integers
!  (starting with 1) that is the same size as 'arrY'. If there are 2
!  independent variables, the second set of data is passed in through
!  'arrX2'.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of
!  the same type (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Standard deviation of the
!  entire regression for the data passed in.
!  SYNOPSIS
!  real function RegStdDev(arrY, arrX1, arrX2) result(rv)
!  double precision function RegStdDev(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RegStdDev
    module procedure RegStdDevReal, RegStdDevDoublePrec
  end interface RegStdDev

  ! Allow RegStdError to be the generic name for the specific subroutines.
  ! This is supposed to be an alternate to using RegStdDev. So the
  ! module procedure list should be identical.
!****f* RegressionMod/RegStdError
!  NAME
!  RegStdError: This function returns the standard error of the entire
!  regression for the passed in data.
!  DESCRIPTION
!  This function returns the standard deviation (i.e., standard error) of
!  the entire regression for the passed in data. The dependent variable
!  data is passed in through the array 'arrY'. The independent variable
!  data is  passed in through the array 'arrX1'. If 'arrX1' is omitted,
!  then it is assumed to be an array containing consecutive integers
!  (starting with 1) that is the same size as 'arrY'. If there are 2
!  independent variables, the second set of data is passed in through
!  'arrX2'.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of
!  the same type (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Standard error of the
!  entire regression for the data passed in.
!  SYNOPSIS
!  real function RegStdError(arrY, arrX1, arrX2) result(rv)
!  double precision function RegStdError(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RegStdError
    module procedure RegStdDevReal, RegStdDevDoublePrec
  end interface RegStdError

  ! Allow StdErrorSlope to be the generic name for the specific subroutines.
!****f* RegressionMod/StdErrorSlope
!  NAME
!  StdErrorSlope: This function returns the standard deviation of
!  the slope coefficient of the regression for the passed in data.
!  DESCRIPTION
!  This function returns the standard deviation (i.e., standard error) of
!  the slope coefficient of the regression for the passed in data.
!  The dependent variable data is passed in through the array 'arrY'.
!  The independent variable data is  passed in through the array
!  'arrX1'. If 'arrX1' is omitted, then it is assumed to be an array
!  containing consecutive integers (starting with 1) that is the same size
!  as 'arrY'. If there are 2 independent variables, the second set of
!  data is passed in through 'arrX2'. If 'arrX2' is passed in, then
!  you must specify which slope coefficient standard deviation should
!  be returned. This is specified in the 'slopeNum' argument. The value
!  of 'slopeNum' should be 1 or 2 (correcsponding to 'arrX1' and 'arrX2',
!  respectively).
!
!  Either 'arrX2' and 'slopeNum' should both be passed in,
!  or they should both be omitted. Also, the 3 arrays, 'arrY', 'arrX1'
!  and 'arrX2', must all be of the same type (i.e., all reals or all
!  double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Standard deviation of
!  the slope coefficient of the regression for the passed in data.
!  SYNOPSIS
!  real function StdErrorSlope(arrY, arrX1, arrX2, slopeNum) result(rv)
!  double precision function StdErrorSlope(arrY, arrX1, arrX2, slopeNum)
!    result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * slopeNum: (optional) Integer value should be 1 or 2 correcsponding to
!    'arrX1' and 'arrX2', respectively.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface StdErrorSlope
    module procedure StdErrorSlopeReal, StdErrorSlopeDoublePrec
  end interface StdErrorSlope

  ! Allow StdErrorIntercept to be the generic name for the specific subroutines.
!****f* RegressionMod/StdErrorIntercept
!  NAME
!  StdErrorIntercept: This function returns the standard deviation of
!  the intercept coefficient of the regression for the passed in data.
!  DESCRIPTION
!  This function returns the standard deviation (i.e., standard error) of
!  the intercept coefficient of the regression for the passed in data.
!  The dependent variable data is passed in through the array 'arrY'.
!  The independent variable data is  passed in through the array
!  'arrX1'. If 'arrX1' is omitted, then it is assumed to be an array
!  containing consecutive integers (starting with 1) that is the same
!  size as 'arrY'. If there are 2 independent variables, the second set of
!  data is passed in through 'arrX2'.
!
!  The 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of the same type
!  (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): Standard deviation of
!  the intercept coefficient of the regression for the passed in data.
!  SYNOPSIS
!  real function StdErrorIntercept(arrY, arrX1, arrX2) result(rv)
!  double precision function StdErrorIntercept(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface StdErrorIntercept
    module procedure StdErrorInterceptReal, StdErrorInterceptDoublePrec
  end interface StdErrorIntercept

  ! Allow ConfIntDevSlope to be the generic name for the specific subroutines.
  interface ConfIntDevSlope
    module procedure ConfIntDevSlopeReal, ConfIntDevSlopeDoublePrec
  end interface ConfIntDevSlope

  ! Allow ConfIntDevIntercept to be the generic name for the specific
  ! subroutines.
  interface ConfIntDevIntercept
    module procedure ConfIntDevInterceptReal, ConfIntDevInterceptDoublePrec
  end interface ConfIntDevIntercept

  ! Allow TStatSlope to be the generic name for the specific subroutines.
!****f* RegressionMod/TStatSlope
!  NAME
!  TStatSlope: This function returns the t-statistic for the slope coefficient
!  of the regression for the passed in data.
!  DESCRIPTION
!  This function returns the t-statistic for the slope coefficient of the
!  regression for the passed in data. The dependent variable data is
!  passed in through the array 'arrY'. The independent variable data
!  is  passed in through the array 'arrX1'. If 'arrX1' is omitted, then
!  it is assumed to be an array containing consecutive integers (starting
!  with 1) that is the same size as 'arrY'. If there are 2 independent
!  variables, the second set of data is passed in through 'arrX2'. If
!  'arrX2' is passed in, then you must specify which slope coefficient
!  t-statistic should be returned. This is specified in the 'slopeNum'
!  argument. The value of 'slopeNum' should be 1 or 2 (correcsponding
!  to 'arrX1' and 'arrX2', respectively).
!
!  Either 'arrX2' and 'slopeNum' should both be passed in, or they should both
!  be omitted. Also, the 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of
!  the same type (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): t-statistic for the slope
!  coefficient of the regression for the passed in data.
!  SYNOPSIS
!  real function TStatSlope(arrY, arrX1, arrX2, slopeNum) result(rv)
!  double precision function TStatSlope(arrY, arrX1, arrX2, slopeNum) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * slopeNum: (optional) Integer value should be 1 or 2 correcsponding to
!    'arrX1' and 'arrX2', respectively.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface TStatSlope
    module procedure TStatSlopeReal, TStatSlopeDoublePrec
  end interface TStatSlope

  ! Allow TStatIntercept to be the generic name for the specific subroutines.
!****f* RegressionMod/TStatIntercept
!  NAME
!  TStatIntercept: This function returns the t-statistic for the intercept
!  coefficient of the regression for the passed in data.
!  DESCRIPTION
!  This function returns the t-statistic for the intercept coefficient of
!  the regression for the passed in data. The dependent variable data is
!  passed in through the array 'arrY'. The independent variable data
!  is  passed in through the array 'arrX1'. If 'arrX1' is omitted, then
!  it is assumed to be an array containing consecutive integers (starting
!  with 1) that is the same size as 'arrY'. If there are 2 independent
!  variables, the second set of data is passed in through 'arrX2'.
!
!  Also, the 3 arrays, 'arrY', 'arrX1' and 'arrX2', must all be of the same
!  type (i.e., all reals or all double precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): t-statistic for the intercept
!  coefficient of the regression for the passed in data.
!  SYNOPSIS
!  real function TStatIntercept(arrY, arrX1, arrX2) result(rv)
!  double precision function TStatIntercept(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface TStatIntercept
    module procedure TStatInterceptReal, TStatInterceptDoublePrec
  end interface TStatIntercept

  ! Allow FStat to be the generic name for the specific subroutines.
!****f* RegressionMod/FStat
!  NAME
!  FStat: This function returns the F-statistic.
!  DESCRIPTION
!  This function returns the F-statistic for the regression for the passed
!  in data.
!  The dependent variable data is passed in through the array
!  'arrY'. The independent variable data is  passed in through the array
!  'arrX1'. If 'arrX1' is omitted, then it is assumed to be an array
!  containing consecutive integers (starting with 1) that is the same size
!  as 'arrY'. If there are 2 independent variables, the second set of data
!  is passed in through 'arrX2'.
!
!  Also, the 3 arrays, 'arrY', 'arrX1' and
!  'arrX2', must all be of the same type (i.e., all reals or all double
!  precisions).
!  RETURNS
!  Real or Double Precision (same type as 'arrY'): F-statistic for the
!  regression for the passed in data.
!  SYNOPSIS
!  real function FStat(arrY, arrX1, arrX2) result(rv)
!  double precision function FStat(arrY, arrX1, arrX2) result(rv)
!  ARGUMENTS
!  * arrY: 1-dimensional array (of any size) of reals or double precisions.
!    Dependent variable data.
!  * arrX1: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  * arrX2: (optional) 1-dimensional array (of any size) of reals or double
!    precisions. Independent variable data.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface FStat
    module procedure FStatReal, FStatDoublePrec
  end interface FStat

  contains

!******************************************************************************
  real function VarianceReal(arr, isSample) result(rv)

    real, dimension(:), intent(in) :: arr
    logical, optional, intent(in) :: isSample

    rv = Covariance(arr, arr, isSample)

  end function VarianceReal
!******************************************************************************
  double precision function VarianceDoublePrec(arr, isSample) result(rv)

    double precision, dimension(:), intent(in) :: arr
    logical, optional, intent(in) :: isSample

    rv = Covariance(arr, arr, isSample)

  end function VarianceDoublePrec
!******************************************************************************
  real function StdDevReal(arr, isSample) result(rv)

    real, dimension(:), intent(in) :: arr
    logical, optional, intent(in) :: isSample

    rv = sqrt(Variance(arr, isSample))

  end function StdDevReal
!******************************************************************************
  double precision function StdDevDoublePrec(arr, isSample) result(rv)

    double precision, dimension(:), intent(in) :: arr
    logical, optional, intent(in) :: isSample

    rv = sqrt(Variance(arr, isSample))

  end function StdDevDoublePrec
!******************************************************************************
  real function CovarianceReal(arr1, arr2, isSample) result(rv)

    real, dimension(:), intent(in) :: arr1, arr2
    logical, optional, intent(in) :: isSample
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision :: avg1, avg2, denom

    denom = size(arr1)
    if (present(isSample)) then
      if (isSample) denom = denom - 1.0
    end if
    avg1 = sum(arr1) / size(arr1)
    avg2 = sum(arr2) / size(arr2)
    rv = dot_product(arr1 - avg1, arr2 - avg2) / denom

  end function CovarianceReal
!******************************************************************************
  double precision function CovarianceDoublePrec(arr1, arr2, isSample) &
    result(rv)

    double precision, dimension(:), intent(in) :: arr1, arr2
    logical, optional, intent(in) :: isSample
    double precision :: avg1, avg2, denom

    denom = size(arr1)
    if (present(isSample)) then
      if (isSample) denom = denom - 1.0d0
    end if
    avg1 = sum(arr1) / size(arr1)
    avg2 = sum(arr2) / size(arr2)
    rv = dot_product(arr1 - avg1, arr2 - avg2) / denom

  end function CovarianceDoublePrec
!******************************************************************************
  real function CorrelationReal(arr1, arr2) result(rv)

    real, dimension(:), intent(in) :: arr1, arr2
    rv = Covariance(arr1, arr2) / (StdDev(arr1) * StdDev(arr2))

  end function CorrelationReal
!******************************************************************************
  double precision function CorrelationDoublePrec(arr1, arr2) result(rv)

    double precision, dimension(:), intent(in) :: arr1, arr2
    rv = Covariance(arr1, arr2) / (StdDev(arr1) * StdDev(arr2))

  end function CorrelationDoublePrec
!******************************************************************************
  real function RegSlopeReal(arrY, arrX1, arrX2, slopeNum) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    integer, optional, intent(in) :: slopeNum
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision, dimension(size(arrY)) :: devY, devX1, devX2
    double precision :: numer, denom

    devY = arrY - sum(arrY) / size(arrY)
    devX1 = CalcDevReal(size(arrY), arrX1)
    if (present(arrX2)) then
      devX2 = arrX2 - sum(arrX2) / size(arrX2)
      if (.not. present(slopeNum)) then
        write(6, '(A)') &
          "Error in RegSlope. Must specify which slope (1 or 2)."
        stop
      end if
      select case (slopeNum)
      case (1)
        numer = dot_product(devX1, devY) * dot_product(devX2, devX2) - &
                dot_product(devX2, devY) * dot_product(devX1, devX2)
        denom = dot_product(devX1, devX1) * dot_product(devX2, devX2) - &
                dot_product(devX1, devX2) * dot_product(devX1, devX2)
        rv = numer / denom
      case (2)
        numer = dot_product(devX2, devY) * dot_product(devX1, devX1) - &
                dot_product(devX1, devY) * dot_product(devX1, devX2)
        denom = dot_product(devX1, devX1) * dot_product(devX2, devX2) - &
                dot_product(devX1, devX2) * dot_product(devX1, devX2)
        rv = numer / denom
      case default
        write(6, '(A)') &
          "Error in RegSlope. The slopeNum must be either 1 or 2."
        stop
      end select
    else
      rv = dot_product(devX1, devY) / dot_product(devX1, devX1)
    end if

  end function RegSlopeReal
!******************************************************************************
  double precision function RegSlopeDoublePrec(arrY, arrX1, arrX2, &
    slopeNum) result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    integer, optional, intent(in) :: slopeNum
    double precision, dimension(size(arrY)) :: devY, devX1, devX2
    double precision :: numer, denom

    devY = arrY - sum(arrY) / size(arrY)
    devX1 = CalcDevDoublePrec(size(arrY), arrX1)
    if (present(arrX2)) then
      devX2 = arrX2 - sum(arrX2) / size(arrX2)
      if (.not. present(slopeNum)) then
        write(6, '(A)') &
          "Error in RegSlope. Must specify which slope (1 or 2)."
        stop
      end if
      select case (slopeNum)
      case (1)
        numer = dot_product(devX1, devY) * dot_product(devX2, devX2) - &
                dot_product(devX2, devY) * dot_product(devX1, devX2)
        denom = dot_product(devX1, devX1) * dot_product(devX2, devX2) - &
                dot_product(devX1, devX2) * dot_product(devX1, devX2)
        rv = numer / denom
      case (2)
        numer = dot_product(devX2, devY) * dot_product(devX1, devX1) - &
                dot_product(devX1, devY) * dot_product(devX1, devX2)
        denom = dot_product(devX1, devX1) * dot_product(devX2, devX2) - &
                dot_product(devX1, devX2) * dot_product(devX1, devX2)
        rv = numer / denom
      case default
        write(6, '(A)') &
          "Error in RegSlope. The slopeNum must be either 1 or 2."
        stop
      end select
    else
      rv = dot_product(devX1, devY) / dot_product(devX1, devX1)
    end if

  end function RegSlopeDoublePrec
!******************************************************************************
  real function RegInterceptReal(arrY, arrX1, arrX2, slope1, slope2) &
    result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    real, optional, intent(in) :: slope1, slope2
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision :: avgY, avgX1, avgX2

    avgY = sum(arrY) / size(arrY)
    if (present(arrX1)) then
      avgX1 = sum(arrX1) / size(arrX1)
    else
      avgX1 = (size(arrY) + 1.0) / 2.0
    end if
    if (present(slope1)) then
      rv = avgY - slope1 * avgX1
    else
      rv = avgY - RegSlope(arrY, arrX1, arrX2, 1) * avgX1
    end if

    if (present(arrX2)) then
      avgX2 = sum(arrX2) / size(arrX2)
      if (present(slope2)) then
        rv = rv - slope2 * avgX2
      else
        rv = rv - RegSlope(arrY, arrX1, arrX2, 2) * avgX2
      end if
    end if

  end function RegInterceptReal
!******************************************************************************
  double precision function RegInterceptDoublePrec(arrY, arrX1, arrX2, &
    slope1, slope2) result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    double precision, optional, intent(in) :: slope1, slope2
    double precision :: avgY, avgX1, avgX2

    avgY = sum(arrY) / size(arrY)
    if (present(arrX1)) then
      avgX1 = sum(arrX1) / size(arrX1)
    else
      avgX1 = (size(arrY) + 1.0) / 2.0
    end if
    if (present(slope1)) then
      rv = avgY - slope1 * avgX1
    else
      rv = avgY - RegSlope(arrY, arrX1, arrX2, 1) * avgX1
    end if

    if (present(arrX2)) then
      avgX2 = sum(arrX2) / size(arrX2)
      if (present(slope2)) then
        rv = rv - slope2 * avgX2
      else
        rv = rv - RegSlope(arrY, arrX1, arrX2, 2) * avgX2
      end if
    end if

  end function RegInterceptDoublePrec
!******************************************************************************
  real function RegSSReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision :: slope1, slope2, intercept
    double precision :: avgY, estY
    double precision :: rv_dp
    integer :: i

    avgY = sum(arrY) / size(arrY)
    slope1 = RegSlope(arrY, arrX1, arrX2, 1)
    if (present(arrX2)) then
      slope2 = RegSlope(arrY, arrX1, arrX2, 2)
    end if
    intercept = RegIntercept(arrY, arrX1, arrX2)

    rv_dp = 0.0d0
    do i = 1, size(arrY)
      if (present(arrX1)) then
        estY = intercept + slope1 * arrX1(i) - avgY
      else
        estY = intercept + slope1 * i - avgY
      end if
      if (present(arrX2)) then
        estY = estY + slope2 * arrX2(i)
      end if
      rv_dp = rv_dp + estY * estY
    end do

    rv = rv_dp

  end function RegSSReal
!******************************************************************************
  double precision function RegSSDoublePrec(arrY, arrX1, arrX2) result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    double precision :: slope1, slope2, intercept
    double precision :: avgY, estY
    integer :: i

    avgY = sum(arrY) / size(arrY)
    slope1 = RegSlope(arrY, arrX1, arrX2, 1)
    if (present(arrX2)) then
      slope2 = RegSlope(arrY, arrX1, arrX2, 2)
    end if
    intercept = RegIntercept(arrY, arrX1, arrX2)

    rv = 0.0
    do i = 1, size(arrY)
      if (present(arrX1)) then
        estY = intercept + slope1 * arrX1(i) - avgY
      else
        estY = intercept + slope1 * i - avgY
      end if
      if (present(arrX2)) then
        estY = estY + slope2 * arrX2(i)
      end if
      rv = rv + estY * estY
    end do

  end function RegSSDoublePrec
!******************************************************************************
  real function ErrSSReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2

    rv = TotSS(arrY) - RegSS(arrY, arrX1, arrX2)

  end function ErrSSReal
!******************************************************************************
  double precision function ErrSSDoublePrec(arrY, arrX1, arrX2) result(rv)

    double precision, dimension(:) :: arrY
    double precision, dimension(:), optional :: arrX1
    double precision, dimension(:), optional :: arrX2

    rv = TotSS(arrY) - RegSS(arrY, arrX1, arrX2)

  end function ErrSSDoublePrec
!******************************************************************************
  real function TotSSReal(arr) result(rv)

    real, dimension(:), intent(in) :: arr
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision :: avg, dev
    integer :: i

    avg = sum(arr) / size(arr)
    rv = 0.0
    do i = 1, size(arr)
      dev = arr(i) - avg
      rv = rv + dev * dev
    end do

  end function TotSSReal
!******************************************************************************
  double precision function TotSSDoublePrec(arr) result(rv)

    double precision, dimension(:), intent(in) :: arr
    double precision :: avg, dev
    integer :: i

    avg = sum(arr) / size(arr)
    rv = 0.0d0
    do i = 1, size(arr)
      dev = arr(i) - avg
      rv = rv + dev * dev
    end do

  end function TotSSDoublePrec
!******************************************************************************
  real function RSquaredReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2

    rv = RegSS(arrY, arrX1, arrX2) / TotSS(arrY)

  end function RSquaredReal
!******************************************************************************
  double precision function RSquaredDoublePrec(arrY, arrX1, arrX2) result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2

    rv = RegSS(arrY, arrX1, arrX2) / TotSS(arrY)

  end function RSquaredDoublePrec
!******************************************************************************
  real function AdjRSquaredReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision :: degFreedom

    if (present(arrX2)) then
      degFreedom = size(arrY) - 3.0d0
    else
      degFreedom = size(arrY) - 2.0d0
    end if

    rv = 1.0d0 - (1.0d0 - RSquared(arrY, arrX1, arrX2)) * (size(arrY) - 1) / &
         degFreedom

  end function AdjRSquaredReal
!******************************************************************************
  double precision function AdjRSquaredDoublePrec(arrY, arrX1, arrX2) result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    double precision :: degFreedom

    if (present(arrX2)) then
      degFreedom = size(arrY) - 3.0d0
    else
      degFreedom = size(arrY) - 2.0d0
    end if

    rv = 1.0d0 - (1.0d0 - RSquared(arrY, arrX1, arrX2)) * (size(arrY) - 1) / &
         degFreedom

  end function AdjRSquaredDoublePrec
!******************************************************************************
  real function RegVarianceReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision :: degFreedom

    if (present(arrX2)) then
      degFreedom = size(arrY) - 3.0d0
    else
      degFreedom = size(arrY) - 2.0d0
    end if
    rv = ErrSS(arrY, arrX1, arrX2) / degFreedom

  end function RegVarianceReal
!******************************************************************************
  double precision function RegVarianceDoublePrec(arrY, arrX1, arrX2) &
    result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    double precision :: degFreedom

    if (present(arrX2)) then
      degFreedom = size(arrY) - 3.0d0
    else
      degFreedom = size(arrY) - 2.0d0
    end if
    rv = ErrSS(arrY, arrX1, arrX2) / degFreedom

  end function RegVarianceDoublePrec
!******************************************************************************
  real function RegStdDevReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2

    rv = sqrt(RegVariance(arrY, arrX1, arrX2))

  end function RegStdDevReal
!******************************************************************************
  double precision function RegStdDevDoublePrec(arrY, arrX1, arrX2) &
    result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2

    rv = sqrt(RegVariance(arrY, arrX1, arrX2))

  end function RegStdDevDoublePrec
!******************************************************************************
  real function StdErrorSlopeReal(arrY, arrX1, arrX2, slopeNum) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    integer, optional, intent(in) :: slopeNum
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision, dimension(size(arrY)) :: devX
    double precision, dimension(size(arrY)) :: tempX1
    double precision :: degFreedom, divFactor
    integer :: i

    if (present(arrX2)) then
      degFreedom = size(arrY) - 3.0
      if (present(arrX1)) then
        divFactor = 1.0 - correlation(arrX1, arrX2) * correlation(arrX1, arrX2)
      else
        do i = 1, size(arrY)
          tempX1(i) = dble(i)
        end do
        divFactor = 1.0 - &
          correlation(tempX1, dble(arrX2)) * correlation(tempX1, dble(arrX2))
      end if
      if (.not. present(slopeNum)) then
        write(6, '(A)') "Error in StdErrorSlope. Must define which slope!"
        stop
      end if
      if (slopeNum == 1) then
        devX = CalcDevReal(size(arrY), arrX1)
      else if (slopeNum == 2) then
        devX = arrX2 - sum(arrX2) / size(arrX2)
      else
        write(6, '(A)') "Error in StdErrorSlope, slopeNum must be 1 or 2."
        stop
      end if
    else
      degFreedom = size(arrY) - 2.0
      divFactor = 1.0
      devX = CalcDevReal(size(arrY), arrX1)
    end if
    rv = ErrSS(arrY, arrX1, arrX2) / degFreedom
    rv = rv / sum(devX * devX)
    rv = rv / divFactor
    rv = sqrt(rv)

  end function StdErrorSlopeReal
!******************************************************************************
  double precision function StdErrorSlopeDoublePrec(arrY, arrX1, arrX2, &
    slopeNum) result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    integer, optional, intent(in) :: slopeNum
    double precision, dimension(size(arrY)) :: devX
    double precision, dimension(size(arrY)) :: tempX1
    double precision :: degFreedom, divFactor
    integer :: i

    if (present(arrX2)) then
      degFreedom = size(arrY) - 3.0d0
      if (present(arrX1)) then
        divFactor = &
          1.0d0 - correlation(arrX1, arrX2) * correlation(arrX1, arrX2)
      else
        do i = 1, size(arrY)
          tempX1(i) = dble(i)
        end do
        divFactor = &
          1.0d0 - correlation(tempX1, arrX2) * correlation(tempX1, arrX2)
      end if
      if (.not. present(slopeNum)) then
        write(6, '(A)') "Error in StdErrorSlope. Must define which slope!"
        stop
      end if
      if (slopeNum == 1) then
        devX = CalcDevDoublePrec(size(arrY), arrX1)
      else if (slopeNum == 2) then
        devX = arrX2 - sum(arrX2) / size(arrX2)
      else
        write(6, '(A)') "Error in StdErrorSlope, slopeNum must be 1 or 2."
        stop
      end if
    else
      degFreedom = size(arrY) - 2.0d0
      divFactor = 1.0d0
      devX = CalcDevDoublePrec(size(arrY), arrX1)
    end if
    rv = ErrSS(arrY, arrX1, arrX2) / degFreedom
    rv = rv / sum(devX * devX)
    rv = rv / divFactor
    rv = sqrt(rv)

  end function StdErrorSlopeDoublePrec
!******************************************************************************
  real function StdErrorInterceptReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision, allocatable, dimension(:, :) :: tempX
    double precision, allocatable, dimension(:, :) :: tempArr
    integer :: numVars
    integer :: i

    if (present(arrX2)) then
      numVars = 3
    else
      numVars = 2
    end if
    allocate(tempX(numVars, size(arrY)))
    allocate(tempArr(numVars, numVars))

    tempX(1, :) = 1.0
    if (present(arrX1)) then
      tempX(2, :) = arrX1
    else
      do i = 1, size(arrY)
        tempX(2, i) = dble(i)
      end do
    end if
    if (present(arrX2)) then
      tempX(3, :) = arrX2
    end if
    tempArr = matmul(tempX, transpose(tempX))
    tempArr = Inverse(tempArr)
    rv = sqrt(tempArr(1, 1)) * RegStdError(arrY, arrX1, arrX2)

    deallocate(tempX)
    deallocate(tempArr)

  end function StdErrorInterceptReal
!******************************************************************************
  double precision function StdErrorInterceptDoublePrec(arrY, arrX1, arrX2) &
    result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    double precision, allocatable, dimension(:, :) :: tempX
    double precision, allocatable, dimension(:, :) :: tempArr
    integer :: numVars
    integer :: i

    if (present(arrX2)) then
      numVars = 3
    else
      numVars = 2
    end if
    allocate(tempX(numVars, size(arrY)))
    allocate(tempArr(numVars, numVars))

    tempX(1, :) = 1.0d0
    if (present(arrX1)) then
      tempX(2, :) = arrX1
    else
      do i = 1, size(arrY)
        tempX(2, i) = dble(i)
      end do
    end if
    if (present(arrX2)) then
      tempX(3, :) = arrX2
    end if
    tempArr = matmul(tempX, transpose(tempX))
    tempArr = Inverse(tempArr)
    rv = sqrt(tempArr(1, 1)) * RegStdError(arrY, arrX1, arrX2)

    deallocate(tempX)
    deallocate(tempArr)

  end function StdErrorInterceptDoublePrec
!******************************************************************************
  real function ConfIntDevSlopeReal(confInt, arrY, arrX1, arrX2, slopeNum) &
    result(rv)

    real, intent(in) :: confInt
    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    integer, optional, intent(in) :: slopeNum
    integer :: df

    if (present(arrX2)) then
      df = size(arrY) - 3
    else
      df = size(arrY) - 2
    end if
    rv = StdErrorSlope(arrY, arrX1, arrX2, slopeNum) * &
         CumDistInvT(df, 1.0 - confInt, 2)

  end function ConfIntDevSlopeReal
!******************************************************************************
  double precision function ConfIntDevSlopeDoublePrec(confInt, arrY, arrX1, &
    arrX2, slopeNum) result(rv)

    double precision, intent(in) :: confInt
    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    integer, optional, intent(in) :: slopeNum
    integer :: df

    if (present(arrX2)) then
      df = size(arrY) - 3
    else
      df = size(arrY) - 2
    end if
    rv = StdErrorSlope(arrY, arrX1, arrX2, slopeNum) * &
         CumDistInvT(df, 1.0d0 - confInt, 2)

  end function ConfIntDevSlopeDoublePrec
!******************************************************************************
  real function ConfIntDevInterceptReal(confInt, arrY, arrX1, arrX2) result(rv)

    real, intent(in) :: confInt
    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    integer :: df

    if (present(arrX2)) then
      df = size(arrY) - 3
    else
      df = size(arrY) - 2
    end if
    rv = StdErrorIntercept(arrY, arrX1, arrX2) * &
         CumDistInvT(df, 1.0 - confInt, 2)

  end function ConfIntDevInterceptReal
!******************************************************************************
  double precision function ConfIntDevInterceptDoublePrec(confInt, arrY, &
    arrX1, arrX2) result(rv)

    double precision, intent(in) :: confInt
    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    integer :: df

    if (present(arrX2)) then
      df = size(arrY) - 3
    else
      df = size(arrY) - 2
    end if
    rv = StdErrorIntercept(arrY, arrX1, arrX2) * &
         CumDistInvT(df, 1.0d0 - confInt, 2)

  end function ConfIntDevInterceptDoublePrec
!******************************************************************************
  real function TStatSlopeReal(arrY, arrX1, arrX2, slopeNum) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    integer, optional, intent(in) :: slopeNum

    rv = RegSlope(arrY, arrX1, arrX2, slopeNum) / &
         StdErrorSlope(arrY, arrX1, arrX2, slopeNum)

  end function TStatSlopeReal
!******************************************************************************
  double precision function TStatSlopeDoublePrec(arrY, arrX1, arrX2, slopeNum) &
    result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    integer, optional, intent(in) :: slopeNum

    rv = RegSlope(arrY, arrX1, arrX2, slopeNum) / &
         StdErrorSlope(arrY, arrX1, arrX2, slopeNum)

  end function TStatSlopeDoublePrec
!******************************************************************************
  real function TStatInterceptReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2

    rv = RegIntercept(arrY, arrX1, arrX2) / &
         StdErrorIntercept(arrY, arrX1, arrX2)

  end function TStatInterceptReal
!******************************************************************************
  double precision function TStatInterceptDoublePrec(arrY, arrX1, arrX2) &
    result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2

    rv = RegIntercept(arrY, arrX1, arrX2) / &
         StdErrorIntercept(arrY, arrX1, arrX2)

  end function TStatInterceptDoublePrec
!******************************************************************************
  real function FStatReal(arrY, arrX1, arrX2) result(rv)

    real, dimension(:), intent(in) :: arrY
    real, dimension(:), optional, intent(in) :: arrX1
    real, dimension(:), optional, intent(in) :: arrX2
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision :: tempMult

    if (present(arrX2)) then
      tempMult = (size(arrY) - 3.0d0) / 2.0d0
    else
      tempMult = size(arrY) - 2.0d0
    end if
    rv = tempMult * RegSS(arrY, arrX1, arrX2) / ErrSS(arrY, arrX1, arrX2)

  end function FStatReal
!******************************************************************************
  double precision function FStatDoublePrec(arrY, arrX1, arrX2) result(rv)

    double precision, dimension(:), intent(in) :: arrY
    double precision, dimension(:), optional, intent(in) :: arrX1
    double precision, dimension(:), optional, intent(in) :: arrX2
    double precision :: tempMult

    if (present(arrX2)) then
      tempMult = (size(arrY) - 3.0d0) / 2.0d0
    else
      tempMult = size(arrY) - 2.0d0
    end if
    rv = tempMult * RegSS(arrY, arrX1, arrX2) / ErrSS(arrY, arrX1, arrX2)

  end function FStatDoublePrec
!******************************************************************************
  real function CalcDevReal(sz, arr) result(rv)

    integer, intent(in) :: sz
    real, dimension(:), optional, intent(in) :: arr
    dimension rv(sz)
    integer :: i
    ! Even though this is the "Real" version of the function, we'll use
    ! double precision for internal calculations to increase accuracy
    ! on the final results.
    double precision, dimension(sz) :: tempArr


    if (present(arr)) then
      ! First verify that this function was called properly (i.e. with
      ! consistent "sizes").
      if (sz /= size(arr)) then 
        write(6, '(A, I, A, I, A)') &
          "Error in CalcDevReal, array size = ", &
          size(arr), ", specified size = ", sz, ". These 2 sizes must match."
        stop
      end if 
      rv = arr - sum(arr) / sz
    else
      do i = 1, sz
        tempArr(i) = dble(i)
      end do
      rv = tempArr - (dble(sz) + 1.0d0) / 2.0d0
    end if

  end function CalcDevReal
!******************************************************************************
  double precision function CalcDevDoublePrec(sz, arr) result(rv)

    integer, intent(in) :: sz
    double precision, dimension(:), optional, intent(in) :: arr
    dimension rv(sz)
    integer :: i
    double precision, dimension(sz) :: tempArr


    if (present(arr)) then
      ! First verify that this function was called properly (i.e. with
      ! consistent "sizes").
      if (sz /= size(arr)) then 
        write(6, '(A, I, A, I, A)') &
          "Error in CalcDevDoublePrec, array size = ", &
          size(arr), ", specified size = ", sz, ". These 2 sizes must match."
        stop
      end if 
      rv = arr - sum(arr) / sz
    else
      do i = 1, sz
        tempArr(i) = dble(i)
      end do
      rv = tempArr - (dble(sz) + 1.0d0) / 2.0d0
    end if

  end function CalcDevDoublePrec
!******************************************************************************

end module RegressionMod

