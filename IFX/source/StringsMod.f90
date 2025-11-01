! $Id: StringsMod.f90 1.20 2018/07/26 16:57:02EDT 622325 Development  $
module StringsMod

!****h* source/StringsMod
!  NAME
!  StringsMod: OcactFortLib StringsMod module.
!  REVISION
!  $Revision: 1.20 $
!  MODULE DESCRIPTION
!  The StringsMod module contains subroutines and functions that are used to
!  manipulate character strings. The StringsMod can be used to:
!  * Convert Ascii to Int/Real/DoublePrec or convert Int/Real/DoublePrec to
!    Ascii.
!  * Identify a character string as an alphabetical character or a digit.
!  * Parse a character string.
!  * Convert a character string to upper-case or lower-case letters.
!  * Create a string from the first or last characters (or digits) of another
!    string (or number).
!  MODULE ROUTINES
!  AscToDoublePrec
!  AscToInt
!  AscToLogical
!  AscToReal
!  DoublePrecToAsc
!  IntToAsc
!  IsAlpha
!  IsDigit
!  IsNumeric
!  Left
!  LogicalToAsc
!  NumToAsc
!  ParseLine
!  RealToAsc
!  Right
!  ToLower
!  ToUpper
!*****

  implicit none
  save

  private
  public :: ParseLine
  public :: AscToInt, AscToReal, AscToDoublePrec, AscToLogical
  public :: IntToAsc, RealToAsc, DoublePrecToAsc, NumToAsc, LogicalToAsc
  public :: ToLower, ToUpper
  public :: IsAlpha, IsDigit, IsNumeric
  public :: Left, Right

  ! The maximum length for a charcter string in this module.
  integer, parameter :: MAX_CHAR_LENGTH = 10000

  ! Allow NumToAsc to be the generic name for these subroutines.
!****f* StringsMod/NumToAsc
!  NAME
!  NumToAsc: This function is a generic function for 'IntToAsc', 'RealToAsc',
!  and 'DoublePrecToAsc'.
!  DESCRIPTION
!  This function is a generic function for 'IntToAsc', 'RealToAsc',
!  and 'DoublePrecToAsc'. If 'num' is an integer then 'numDec'
!  is optional (as it is for 'IntToAsc'). If 'num' is a real or
!  double precision then 'numDec' is required (as it is for
!  'RealToAsc' and 'DoublePrecToAsc'). The argument 'useCommas' is
!  always optional. See the documentation for 'IntToAsc',
!  'RealToAsc', and 'DoublePrecToAsc' for more details and examples.
!  RETURNS
!  Character string (of length 10000): An ascii character string.
!  SYNOPSIS
!  MAX_CHAR_LENGTH is set equal to 10,000.
!  character (len = MAX_CHAR_LENGTH) function NumToAsc(num, numDec, useCommas) &
!    result(asc)
!  ARGUMENTS
!  * num: Integer, real, or double precision to be returned as a character
!    string value.
!  * numDec: (may be optional) Number of decimal places. Optional if 'num' is
!    an integer, otherwise required.
!  * useCommas: (optional) If true, commas will be inserted in the returned
!    string.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface NumToAsc
    module procedure IntToAsc, RealToAsc, DoublePrecToAsc
  end interface NumToAsc

  ! Allow Left to be the generic name for these subroutines.
!****f* StringsMod/Left
!  NAME
!  Left: This function returns the left-most characters (or digits) of
!  a string (or number).
!  DESCRIPTION
!  This function takes a character string or a number (integer, real,
!  or double precision), and the number of left-most characters (or digits)
!  it should return.
!
!  The returned string somewhat depends on the variable type of 'arg':
!  * If 'arg' is a character string, then it returns a substring of 'arg'
!    with the first (i.e., left-most) 'numChars' characters. If 'numChars'
!    is larger than the number of characters in 'arg' then the string
!    returned is padded with trailing spaces.
!  * If 'arg' is an integer, then it returns a string representing the first
!    (i.e., left-most) 'numChars' digits of 'arg'. If 'numChars' is larger
!    than the number of digits in 'arg', then the string returned is padded
!    with trailing spaces.
!  * If 'arg' is a real or double precision, then it returns a string
!    representing the first (i.e., left-most) 'numChars' digits of 'arg',
!    with the possible decimal point being counted as one of the characters.
!    Note that if 'numChars' is larger than the amount of precision that can
!    be stored in the number, then the trailing digits returned will not be
!    meaningful. Also note that there will be no padding with trailing spaces.
!  RETURNS
!  Character string (of length numChars): An ascii character string.
!  SYNOPSIS
!  character (len = numChars) function Left(arg, numChars) result(asc)
!  ARGUMENTS
!  * arg: Character string or number (integer, real, or double precision),
!    part of which is to be returned as a character string.
!  * numChars: Number of left-most characters to return.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface Left
    module procedure LeftFromAsc, LeftFromInt, LeftFromReal, LeftFromDoublePrec
  end interface Left

  ! Allow Right to be the generic name for these subroutines.
!****f* StringsMod/Right
!  NAME
!  Right: This function returns the right-most characters (or digits) of
!  a string (or an integer).
!  DESCRIPTION
!  This function takes a character string or an integer, and the
!  number of right-most characters (or digits) it should return.
!
!  The returned string somewhat depends on the variable type of 'arg':
!  * If 'arg' is a character string, then it returns a substring of 'arg'
!    with the last (i.e., right-most) 'numChars' characters. If 'numChars'
!    is larger than the number of characters in 'arg' then the string
!    returned is padded with leading spaces.
!  * If 'arg' is an integer, then it returns a string representing the last
!    (i.e., right-most) 'numChars' digits of 'arg'. If 'numChars' is larger
!    than the number of digits in 'arg', then the string returned is padded
!    with leading spaces.
!
!  Note that this function can not take a real or double precision as 'arg'.
!  This is because the "right-most" characters of a floating point number
!  would not be clearly defined, or would be meaningless.
!  RETURNS
!  Character string (of length numChars): An ascii character string.
!  SYNOPSIS
!  character (len = numChars) function Right(arg, numChars) result(asc)
!  ARGUMENTS
!  * arg: Character string or integer, part of which is to be returned
!    as a character string.
!  * numChars: Number of right-most characters (or digits) to return.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface Right
    module procedure RightFromAsc, RightFromInt
  end interface Right

contains

!******************************************************************************
!****f* StringsMod/AscToInt
!  NAME
!  AscToInt: This function takes a character variable holding an integer and
!  returns the integer.
!  DESCRIPTION
!  This function takes a character variable holding an integer and returns
!  the integer. For example, if the variable 'char1' is a character
!  string and holds the value "135", then the function call
!  AscToInt(char1) will return 135 as an integer. If 'char1' holds
!  "-135", then AscToInt(char1) will return the integer value -135. If
!  'asc' is an empty string (i.e. trim(asc) = "") and 'extraCheck' is
!  true or omitted, then an error message is printed out and the program
!  terminates. But if 'errorCheck' is false then the function returns 0.
!  If 'asc' is not an empty string, then 'errorCheck' has no affect.
!  RETURNS
!  Integer: Integer value of an ascii character string.
!  SYNOPSIS

  integer function AscToInt(asc, extraCheck) result(int)

!  ARGUMENTS
!  * asc: Character variable holding an integer.
!  * extraCheck: (optional) If an error occurs and extraCheck is true or
!    omitted, an error message is printed and the program terminates. If
!    extraCheck is false, the function returns a 0.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: asc
    logical, optional, intent(in) :: extraCheck
!*****
    integer :: ios
    logical :: checkEmptyString

    if (present(extraCheck)) then
      checkEmptyString = extraCheck
    else
      checkEmptyString = .true.
    end if

    if (checkEmptyString .and. trim(asc) == "") then
      write(6, '(2A)') "Error in AscToInt(). Can not convert an empty ", &
        "string to an integer."
      stop
    end if
    read(asc, '(I40)', iostat = ios) int
    if (ios /= 0) then
      write(6, '(3A)') "Error in AscToInt(). Could not read ", trim(asc), &
        " as an integer."
      stop
    end if

  end function AscToInt
!******************************************************************************
!****f* StringsMod/AscToReal
!  NAME
!  AscToReal: This function takes a character variable holding a real and
!  returns the real.
!  DESCRIPTION
!  This function takes a character variable holding a real (or integer)
!  and returns it as a real. For example, if the variable 'char1' is a
!  character string and holds the value "135", then the function call
!  AscToReal(char1) will return 135 as a real. If 'char1' holds "135.7"
!  then the call 'AscToReal' will return the real 135.7. If 'char1'
!  holds "-135.7" then AscToReal will return the real -135.7. If 'asc'
!  is an empty string (i.e. trim(asc) = "") and 'extraCheck' is true
!  or omitted, then an error message is printed out and the program
!  terminates. But if 'errorCheck' is false then the funtion returns 0.0.
!  If 'asc' is not an empty string, then 'errorCheck' has no affect.
!  RETURNS
!  Real: Real value of an ascii character string.
!  SYNOPSIS

  real function AscToReal(asc, extraCheck) result(float)

!  ARGUMENTS
!  * asc: Character variable holding a real (or integer).
!  * extraCheck: (optional) If an error occurs and extraCheck is true or
!    omitted, an error message is printed and the program terminates. If
!    extraCheck is false, the function returns a 0.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: asc
    logical, optional, intent(in) :: extraCheck
!*****
    integer :: ios
    logical :: checkEmptyString

    if (present(extraCheck)) then
      checkEmptyString = extraCheck
    else
      checkEmptyString = .true.
    end if

    if (checkEmptyString .and. trim(asc) == "") then
      write(6, '(2A)') "Error in AscToReal(). Can not convert an empty ", &
        "string to a real."
      stop
    end if
    ! If there is no decimal point in the number, then reading with an "F"
    ! format character will not convert the number properly.
    if (index(asc, ".") > 0) then
      read(asc, '(F40.10)', iostat = ios) float
      if (ios /= 0) then
        write(6, '(3A)') "Error in AscToReal(). Could not read ", trim(asc), &
          " as a real."
        stop
      end if
    else
      float = real(AscToInt(asc, checkEmptyString))
    end if

  end function AscToReal
!******************************************************************************
!****f* StringsMod/AscToDoublePrec
!  NAME
!  AscToDoublePrec: This function takes a character variable holding a double
!  precision and returns the double precision.
!  DESCRIPTION
!  This function takes a character variable holding a double precision
!  (or integer) and returns it as a double precision. For example, if
!  the variable 'char1' is a character string and holds the value
!  "135", then the function call AscToDoublePrec(char1) will return
!  135 as a double precision. If 'char1' holds "135.7" then the call
!  AscToDoublePrec(char1) will return the double precision 135.7. If
!  'char1' holds "-135.7" then AscToDoublePrec will return the double
!  precision number -135.7.
!
!  If 'asc' is an empty string (i.e.
!  trim(asc) = "") and 'extraCheck' is true or omitted, then an error
!  message is printed out and the program terminates. But if
!  'errorCheck' is false then the funtion returns 0.0d0. If 'asc' is
!  not an empty string, then 'errorCheck' has no affect.
!  RETURNS
!  Double Precision: Double precision value of an ascii character string.
!  SYNOPSIS

  double precision function AscToDoublePrec(asc, extraCheck) result(float)

!  ARGUMENTS
!  * asc: Character variable holding a double precision (or integer).
!  * extraCheck: (optional) If an error occurs and extraCheck is true or
!    omitted, an error message is printed and the program terminates. If
!    extraCheck is false, the function returns a 0.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: asc
    logical, optional, intent(in) :: extraCheck
!*****
    integer :: ios
    logical :: checkEmptyString

    if (present(extraCheck)) then
      checkEmptyString = extraCheck
    else
      checkEmptyString = .true.
    end if

    if (checkEmptyString .and. trim(asc) == "") then
      write(6, '(2A)') "Error in AscToDoublePrec(). Can not convert an ", &
        "empty string to a double precision."
      stop
    end if
    ! If there is no decimal point in the number, then reading with an "F"
    ! format character will not convert the number properly.
    if (index(asc, ".") > 0) then
      read(asc, '(F40.10)', iostat = ios) float
      if (ios /= 0) then
        write(6, '(3A)') "Error in AscToDoublePrec(). Could not read ", &
          trim(asc), " as a double precision."
        stop
      end if
    else
      float = dble(AscToInt(asc, checkEmptyString))
    end if

  end function AscToDoublePrec
!******************************************************************************
    ! Note this function works (at last test) for integers in the range
    ! (-2147483648, 2147483647)
!****f* StringsMod/IntToAsc
!  NAME
!  IntToAsc: This function takes an integer and returns the integer as an ascii
!  character string.
!  DESCRIPTION
!  This function takes an integer that can be positive or negative and
!  returns the integer as an ascii character string. The integer must
!  be at least -2147483648 and at most 2147483647. This function can
!  be thought of as the inverse of the function 'AscToInt'.
!
!  For example, if the variable 'i' is an integer and it holds the value
!  75, then IntToAsc(i) will return a character string containing "75"
!  followed by 9998 spaces. Also, if 'i' is as above, then
!  AscToInt(IntToAsc(i)) will return the integer 75.
!
!  If 'numDec' is passed in, then the resulting string will have a decimal
!  point in it. This decimal point will be followed by however many zeros are
!  specified by numDec. For example, 'i' is as above, then
!  IntToAsc(i, 0) will return the string containing "75." followed
!  by 9997 spaces. Also, IntToAsc(i, 4) will return the string
!  containing "75.0000" followed by 9993 spaces.
!
!  If 'useCommas' is passed in, and is .true., then commas will be inserted in
!  the returned string. Otherwise, commas will be omitted. For example,
!  IntToAsc(1234567, useCommas = .true.) will return the string
!  "1,234,567" followed by 9991 spaces.
!  RETURNS
!  Character string (of length 10000): Ascii character string value of an
!  integer.
!  SYNOPSIS
!  MAX_CHAR_LENGTH is set equal to 10,000.
  character (len = MAX_CHAR_LENGTH) function IntToAsc(integ, numDec, &
    useCommas) result(asc)

!  ARGUMENTS
!  * integ: Integer to be returned as a character string value.
!  * numDec: (optional) Number of decimal places.
!  * useCommas: (optional) If true, commas will be inserted in the returned
!    string.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: integ
    integer, optional, intent(in) :: numDec
    logical, optional, intent(in) :: useCommas
!*****
    integer :: i

    write(asc, '(I11)') integ
    ! The above "write" will right justify the number, so we need to adjust
    ! it to the left.
    asc = adjustl(asc)

    if (present(numDec)) then
      asc = trim(asc) // "." // repeat("0", numDec)
    end if

    if (present(useCommas)) then
      if (useCommas) then
         asc = trim(AddCommas(trim(asc)))
      end if
    end if

  end function IntToAsc
!******************************************************************************
!****f* StringsMod/RealToAsc
!  NAME
!  RealToAsc: This function returns a real as an ascii character string.
!  DESCRIPTION
!  This function takes a real (in the argument 'float') that can be positive
!  or negative and returns the real as an ascii character string. The
!  argument 'numDec' is the number of decimal places to be displayed in
!  the string. This function can be thought of as the inverse of the
!  function 'AscToReal' as long as the appropriate number of decimal places
!  is passed in.
!
!  For example, if the variable 'r' is a real and it holds
!  the value 75.123, then RealToAsc(r, 3) will return a character
!  string containing "75.123" filled with spaces to the right. Also,
!  RealToAsc(r, 4) will return "75.1230" (filled with spaces to the
!  right), and RealToAsc(r, 0) will return "75." (filled with spaces to
!  the right).
!
!  If 'useCommas' is passed in, and is .true., then commas
!  will be inserted in the returned string. Otherwise, commas will be
!  omitted. For example, RealToAsc(1234.5, 1, .true.) will return the
!  string "1,234.5" (filled with spaces to the right).
!  RETURNS
!  Character string (of length 10000): Ascii character string value of a
!  real.
!  SYNOPSIS
!  MAX_CHAR_LENGTH is set equal to 10,000.
  character (len = MAX_CHAR_LENGTH) function RealToAsc(float, numDec, &
    useCommas) result(asc)

!  ARGUMENTS
!  * float: Real to be returned as a character string value.
!  * numDec: Number of decimal places.
!  * useCommas: (optional) If true, commas will be inserted in the returned
!    string.
!  ARGUMENT DECLARATIONS
    real, intent(in) :: float
    integer, intent(in) :: numDec
    logical, optional, intent(in) :: useCommas
!*****

    asc = DoublePrecToAsc(dble(float), numDec, useCommas)

  end function RealToAsc
!******************************************************************************
!****f* StringsMod/DoublePrecToAsc
!  NAME
!  DoublePrecToAsc: This function returns a double precision value as an ascii
!  character string.
!  DESCRIPTION
!  This function takes a double precision (in the argument 'float') that can
!  be positive or negative and returns the double precision as an ascii
!  character string. The argument 'numDec' is number of decimal places
!  to be displayed in the string. This function can be thought of as the
!  inverse of the function 'AscToDoublePrec' as long as the appropriate
!  number of decimal places is passed in.
!
!  For example, if the variable 'd' is a double precision and it holds the value
!  75.123, then DoublePrecToAsc(d, 3) will return a character string containing
!  "75.123" filled with spaces to the right. Also, DoublePrecToAsc(d, 4)
!  will return "75.1230" (filled with spaces to the right), and
!  DoublePrecToAsc(d, 0) will return "75." (filled with spaces to the
!  right).
!
!  If 'useCommas' is passed in, and is .true., then commas
!  will be inserted in the returned string. Otherwise, commas will be
!  omitted. For example, DoublePrecToAsc(1234.5, 1, .true.) will return
!  the string "1,234.5" (filled with spaces to the right).
!  RETURNS
!  Character string (of length 10000): Ascii character string value of a
!  double precision.
!  SYNOPSIS
!  MAX_CHAR_LENGTH is set equal to 10,000.
  character (len = MAX_CHAR_LENGTH) function DoublePrecToAsc(float, numDec, &
    useCommas) result(asc)

!  ARGUMENTS
!  * float: Double precision to be returned as a character string value.
!  * numDec: Number of decimal places.
!  * useCommas: (optional) If true, commas will be inserted in the returned
!    string.
!  ARGUMENT DECLARATIONS
    double precision, intent(in) :: float
    integer, intent(in) :: numDec
    logical, optional, intent(in) :: useCommas
!*****
    integer :: length, i
    ! First make sure that the number of decimal places is not negative.
    if (numDec < 0) then
      write(6, '(2A, /, A, /, A, I4)') "Error in RealToAsc(). Can not ", &
        "pass in the number of decimal places ", "being less than zero.", &
        "numDec: ", numDec
      stop
    end if

    write(asc, '(F40.10)') float
    asc = adjustl(asc)
    ! Calculate the length that the final result should be.
    length = index(asc, ".") + numDec
    ! If the next digit (after length) is >= 5, round up, otherwise
    ! just leave it and it will be truncated.
    if (asc(length + 1:length + 1) >= '5') then
      ! To round up, we need to increase the previous digit by one. If that
      ! digit is 9, change it to a zero and carry over the rounding to the
      ! next previous digit. If the "digit" we're up to is the decimal point,
      ! skip it!
      do i = length, 1, -1
        if (asc(i:i) == ".") cycle
        if (asc(i:i) == '9') then
          asc(i:i) = '0'
        else
          asc(i:i) = char(ichar(asc(i:i)) + 1)
          exit
        end if
      end do
    end if
    ! Truncate the number at length.
    asc(length + 1:MAX_CHAR_LENGTH) = " "

    if (present(useCommas)) then
      if (useCommas) then
         asc = trim(AddCommas(trim(asc)))
      end if
    end if

  end function DoublePrecToAsc
!******************************************************************************
!****f* StringsMod/ParseLine
!  NAME
!  ParseLine: This function is used to parse a line where the tokens are
!  separated by spaces or some other specified character or set of characters.
!  DESCRIPTION
!  This function is used to parse a line where the tokens (items to be
!  parsed) are separated by spaces or some other specified character or set of
!  characters. The character set to be used as separators are specified in
!  'sep', if passed in. If it is not passed in, the default character is the
!  space character. This function will remove any leading separators from 'line'
!  and then search for the next separator in 'line'. The returned character
!  string will be the characters found until the first separator (ignoring
!  leading sseparators) is encountered. The returned character string can
!  easily be converted into an integer, real, or double precision, using the
!  'AscToInt', 'AscToReal', or 'AscToDoublePrec' functions in this module.
!  If a separator is not found within the first 10000 characters of 'line',
!  then the first 10000 characters are returned as the token.
!
!  IMPORTANT NOTE: In addition to this function returning the first token from
!  'line', it also modifies 'line' so that the first token is removed.
!  This allows a character string to be easily broken into its tokens
!  by succesive calls to ParseLine (perhaps in a loop), without the
!  calling module needing to modify 'line'.
!  RETURNS
!  Character string (of length 10000): Single token of a parsed line.
!  SYNOPSIS
!  MAX_CHAR_LENGTH is set equal to 10,000.
  character (len = MAX_CHAR_LENGTH) function ParseLine(line, sep) result(token)

!  ARGUMENTS
!  * line: Character string line to be parsed.
!  * sep: (optional)
!  ARGUMENT DECLARATIONS
    character (len = *), intent(inout) :: line
    character (len = *), optional, intent(in) :: sep
!*****
    integer :: ind
    integer :: startPos

    if (present(sep)) then
      startPos = max(verify(line, sep), 1)
      ind = scan(line(startPos:), sep)
    else
      startPos = max(verify(line, " "), 1)
      ind = scan(line(startPos:), " ")
    end if

    if (ind == 0 .or. ind == 1 .or. ind > MAX_CHAR_LENGTH) then
      token = line(1:len(line))
      line(1:len(line)) = " "
    else
      ! Set the token return value.
      token = line(startPos:startPos + ind - 2)
      ! Remove the token that is being returned from the beginning of line.
      line = line(startPos + ind:len(line))
    end if

  end function ParseLine
!******************************************************************************
!****f* StringsMod/ToLower
!  NAME
!  ToLower: This function takes the passed in string and converts all of the
!  letters to lower-case letters.
!  DESCRIPTION
!  This function takes the passed in string and converts all of the
!  upper-case letters to lower-case letters. Any lower-case letters
!  are left as lower-case. Any characters in the string that are not
!  letters are left unchanged.
!  RETURNS
!  Character string (of length 10000): Character string with all lower-case
!  letters.
!  SYNOPSIS
!  MAX_CHAR_LENGTH is set equal to 10,000.
  character (len = MAX_CHAR_LENGTH) function ToLower(str) result(lowerStr)

!  ARGUMENTS
!  * str: Character string to be converted to lower-case letters.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: str
!*****
    integer :: strLength, i


    lowerStr = str
    strLength = len_trim(str)
    do i = 1, strLength
      select case(str(i:i))
        case ('A' : 'Z')
          lowerStr(i:i) = char(ichar(str(i:i)) + 32)
      end select
    end do

  end function ToLower

!******************************************************************************
!****f* StringsMod/ToUpper
!  NAME
!  ToUpper: This function takes the passed in string and converts all of the
!  letters to upper-case letters.
!  DESCRIPTION
!  This function takes the passed in string and converts all of the
!  lower-case letters to upper-case letters. Any upper-case letters
!  are left as upper-case. Any characters in the string that are not
!  letters are left unchanged.
!  RETURNS
!  Character string (of length 10000): Character string with all upper-case
!  letters.
!  SYNOPSIS
!  MAX_CHAR_LENGTH is set equal to 10,000.
  character (len = MAX_CHAR_LENGTH) function ToUpper(str) result(upperStr)

!  ARGUMENTS
!  * str: Character string to be converted to upper-case letters.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: str
!*****
    integer :: strLength, i


    upperStr = str
    strLength = len_trim(str)
    do i = 1, strLength
      select case(str(i:i))
        case ('a' : 'z')
          upperStr(i:i) = char(ichar(str(i:i)) - 32)
      end select
    end do

  end function ToUpper

!******************************************************************************
!****f* StringsMod/IsAlpha
!  NAME
!  IsAlpha: This function determines if the first character of the passed in
!  string is an alphabetical character.
!  DESCRIPTION
!  This function takes the passed in character string and only looks at
!  the first character of it. If this character is an alphabetical
!  character (i.e. a letter, either lower-case or upper-case), then it
!  returns true, otherwise it returns false.
!  RETURNS
!  Logical: Returns true if the first character is alphabetical, otherwise
!  returns false.
!  SYNOPSIS

  logical function IsAlpha(c) result(res)

!  ARGUMENTS
!  * c: Character string to be determined if it is alphabetical.
!  ARGUMENT DECLARATIONS
    character (len = 1), intent(in) :: c
!*****

    select case(c)
      case ('A' : 'Z', 'a' : 'z')
        res = .true.
      case default
        res = .false.
    end select

  end function IsAlpha

!******************************************************************************
!****f* StringsMod/IsDigit
!  NAME
!  IsDigit: This function determines if the passed in character is a digit.
!  DESCRIPTION
!  This function takes the passed in character string (of length 1, i.e., a
!  single character) and determines if this character is a digit (i.e. 0
!  through 9), then it returns true, otherwise it returns false.
!  RETURNS
!  Logical: Returns true if the character is a digit, otherwise returns false.
!  SYNOPSIS

  logical function IsDigit(c) result(res)

!  ARGUMENTS
!  * c: Character to be determined if it is a digit.
!  ARGUMENT DECLARATIONS
    character (len = 1), intent(in) :: c
!*****

    select case(c)
      case ('0' : '9')
        res = .true.
      case default
        res = .false.
    end select

  end function IsDigit

!******************************************************************************
!****f* StringsMod/IsNumeric
!  NAME
!  IsNumeric: This function determines if all characters of the passed in
!  string are digits.
!  DESCRIPTION
!  This function takes the passed in character string and looks at
!  each character of it. If each character is a digit (i.e. 0
!  through 9), then it returns true, otherwise it returns false.
!  If the string is an empty string, then it returns false.
!  RETURNS
!  Logical: Returns true if all characters are digits, otherwise
!  returns false.
!  SYNOPSIS

  logical function IsNumeric(str) result(res)

!  ARGUMENTS
!  * str: Character string to be determined if all characters are digits.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: str
!*****
    integer :: length
    integer :: i

    res = .true.
    length = len_trim(str)

    if (length == 0) then
      res = .false.
    end if

    do i = 1, length
      if (.not. IsDigit(str(i:i))) then
        res = .false.
        exit
      end if
    end do

  end function IsNumeric

!******************************************************************************

  character (len = MAX_CHAR_LENGTH) function AddCommas(inStr) result(outStr)

    character (len = *), intent(in) :: inStr
    logical :: isNeg
    integer :: start
    integer :: i

    ! First, remove any negative sign. We'll add it back at the end.
    if (inStr(1:1) == "-") then
      outStr = inStr(2:)
      isNeg = .true.
    else
      outStr = inStr
      isNeg = .false.
    end if
    ! Start with the last character in the ones place. This is the character
    ! to the left of the decimal point, or the last character if there is
    ! no decimal point. Move 3 more characters to the left since we're not
    ! going to put a comma immediately next to the decimal point, or at
    ! the end of the number.
    start = index(outStr, ".") - 4
    if (start == -4) then
      start = len_trim(outStr) - 3
    end if
    ! Now, loop through every 3 characters and insert a comma.
    do i = start, 1, -3
      outStr = outStr(1:i) // "," // trim(outStr(i + 1:))
    end do
    ! Finally, add back in the negative sign (if necessary).
    if (isNeg) then
      outStr = "-" // trim(outStr)
    end if

  end function AddCommas

!******************************************************************************

  character (len = numChars) function LeftFromAsc(arg, numChars) result(asc)

    character(len =  *), intent(in) :: arg
    integer, intent(in) :: numChars

    asc = arg(1:numChars)

  end function LeftFromAsc

!******************************************************************************

  character (len = numChars) function LeftFromInt(arg, numChars) result(asc)  

    integer, intent(in) :: arg
    integer, intent(in) :: numChars

    asc = LeftFromAsc(NumToAsc(arg), numChars)

  end function LeftFromInt

!******************************************************************************

  character (len = numChars) function LeftFromReal(arg, numChars) result(asc)

    real, intent(in) :: arg
    integer, intent(in) :: numChars

    ! Depending on the actual number in 'arg', we may not to pass NumToAsc
    ! the full 'numChars' as the number of digits of precision (after the
    ! decimal point). But passing numChars is certainly big enough, and
    ! any extra precision that this is passing will get lopped off by taking
    ! the left-most digits, so this works.
    asc = LeftFromAsc(NumToAsc(arg, numChars), numChars)

  end function LeftFromReal

!******************************************************************************

  character (len = numChars) function LeftFromDoublePrec(arg, numChars) &
    result(asc)

    double precision, intent(in) :: arg
    integer, intent(in) :: numChars

    ! Depending on the actual number in 'arg', we may not to pass NumToAsc
    ! the full 'numChars' as the number of digits of precision (after the
    ! decimal point). But passing numChars is certainly big enough, and
    ! any extra precision that this is passing will get lopped off by taking
    ! the left-most digits, so this works.
    asc = LeftFromAsc(NumToAsc(arg, numChars), numChars)

  end function LeftFromDoublePrec

!******************************************************************************

  character (len = numChars) function RightFromAsc(arg, numChars) result(asc)  

    character(len =  *), intent(in) :: arg
    integer, intent(in) :: numChars
    integer :: length

    length = len(arg)
    if (numChars > length) then
      ! Pad with leading spaces. Don't use adjustr, because that will remove
      ! any trailing blanks, which we don't want to do. (If the user wants
      ! that, then the user should trim() the string before passing it in.)
      asc = repeat(" ", numChars - length) // arg
    else
      asc = arg(length - numChars + 1:length)
    end if

  end function RightFromAsc

!******************************************************************************

  character (len = numChars) function RightFromInt(arg, numChars) result(asc)  

    integer, intent(in) :: arg
    integer, intent(in) :: numChars

    asc = RightFromAsc(trim(NumToAsc(arg)), numChars)

  end function RightFromInt

!******************************************************************************
!****f* StringsMod/LogicalToAsc
!  NAME
!  LogicalToAsc: This function takes a logical value as an argument 
!  and returns a character string.
!  DESCRIPTION
!  This function takes a logical value as an argument (i.e., .true. or .false.),
!  and an optional second logical argument called “fullWord”.   It returns a
!  five character string.  If “fullWord” was omitted or false, then the routine
!  returns either “T” for true or “F” for false (based on the first argument
!  passed in).  If “fullWord” is passed in and is true, then the routine
!  returns either “True” or “False” (based on the first argument passed in).
!  RETURNS
!  An ASCII character string from a logical input.
!  SYNOPSIS
  
  character (len = 5) function LogicalToAsc(lg, fullWord) result(asc)

!  ARGUMENTS
!  * lg: Logical to be returned as a character string value.  
!  * fullWord: (optional) If passed in, the routine will return either “True ”
!    or “False”. If omitted or false, the routine will return either “T    ”
!    or “F    ”.


!  ARGUMENT DECLARATIONS
    logical, intent(in)  :: lg
    logical, optional  :: fullWord
!*****    

    if (present(fullWord)) then    
      if (fullWord .and. lg) then
        asc ="True"          
      else if (fullWord .and. .not. lg) then
        asc ="False"       
      else if (.not. fullWord .and. lg) then
        asc ="T"           
      else if (.not. fullWord .and. .not. lg) then
        asc ="F"       
      else 
        write(6, '(2(A, L2))') "Error in LogicalToAsc(). Can not evaluate: ", &
          lg, ", with fullWord: ", fullWord
      end if 
    else
      if (lg) then
        asc= "T" 
      else  if (.not. lg) then
        asc= "F"
      else 
        write(6, '(A, L2)') &
          "Error in LogicalToAsc(). Can not evaluate: ", lg
      end if
    end if    

  end function LogicalToAsc
!******************************************************************************
!****f* StringsMod/AscToLogical
!  NAME
!  AscToLogical: This function takes a character string as an argument 
!  and returns a logical value.
!  DESCRIPTION
!  This function takes a character string as an argument and returns a logical
!  value, either .true. (if the string "True", "true", "T", or "t" was passed
!  in) or .false. (if the string "False", "false", "F", or "f" was passed in)
!  RETURNS
!  Logical value of an ascii character string.
!  SYNOPSIS

  logical function AscToLogical(asc) result(tf)

!  ARGUMENTS
!  * asc: Character variable holding an logical.
!  
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: asc
    
!*****  
    if (asc == "True" .or. asc == "true" .or. asc == "T" .or. asc == "t") then
      tf = .true.
    else if (asc == "False" .or. asc == "false" .or. &
             asc == "F" .or. asc == "f") then
      tf = .false.
    else
      write(6, '(A)') "Error in AscToLogical(). Can not evaluate: " // trim(asc)
      stop
    end if

  end function AscToLogical
!******************************************************************************  
end module StringsMod
