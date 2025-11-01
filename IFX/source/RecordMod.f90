! $Id: RecordMod.f90 1.11 2012/06/22 10:45:42EDT 622325 Development  $
module RecordMod

!****h* source/RecordMod
!  NAME
!  RecordMod: OcactFortLib RecordMod module.
!  REVISION
!  $Revision: 1.11 $
!  MODULE DESCRIPTION
!  The RecordMod contains subroutines and functions used to get, set, and write
!  fields to a record. The RecordLoadFileDesc subroutine reads a "File
!  Description" file used to describe the record layout in a "Record File". The
!  RecordLoadFileDesc subroutine should be called before using other routines in
!  RecordMod. RecordGet is used to get a field from a Record File. The value can
!  be returned as a character string, integer, real, double precision, or date
!  (which is really a character string, but with special date-like formatting).
!  RecordSet is similar to RecordGet. The difference is it sets the value of a
!  field instead of retreiving a value.
!  MODULE ROUTINES
!  RecordGet
!  RecordGetAsChar
!  RecordGetAsDoublePrec
!  RecordGetAsInt
!  RecordGetAsMonthDay
!  RecordGetAsMonthDayYear
!  RecordGetAsMonthYear
!  RecordGetAsReal
!  RecordGetFieldID
!  RecordLoadFileDesc
!  RecordRead
!  RecordSet
!  RecordWrite
!  RecordWriteFileDesc
!  RecordWriteFull
!  RecordWriteHeader
!*****

  use StringsMod
  use FilesMod

  implicit none
  save

  private
  public :: RecordLoadFileDesc, RecordWriteFileDesc
  public :: RecordGet, RecordGetAsChar
  public :: RecordGetAsInt, RecordGetAsReal, RecordGetAsDoublePrec
  public :: RecordGetAsMonthDayYear, RecordGetAsMonthYear , RecordGetAsMonthDay
  public :: RecordSet
  public :: RecordGetFieldID
  public :: RecordWriteFull, RecordWriteHeader, RecordWriteForm
  public :: RecordRead, RecordWrite

  ! Allow RecordGet to be the generic name for the specific subroutines.
!****f* RecordMod/RecordGet
!  NAME
!  RecordGet: This function is used to get a field from a record.
!  DESCRIPTION
!  This function is used to get a field from a record. The record is
!  passed in through 'rec'. It should be the entire line from the file
!  that contains the record in it. If 'field' is a character string,
!  then the function looks for the name of the field that matches
!  'field' and returns the characters in that field. If 'field' is an
!  integer, then it should be an ID associated with a field. To get
!  this ID, use the function 'RecordGetFieldID'. (See the documentation
!  of 'RecordGetFieldID'.) Using the field ID is significantly more
!  efficient than using the field name. This function always returns a
!  character string of length 10000, regardless of how many characters
!  are actually in the field itself. If the field is only 5 characters
!  long, then the last 9995 characters returned by this function will
!  be blank. If a different size return value is needed, see the
!  documentation for the function 'RecordGetAsChar'. If another format
!  is desired for the return value (such as an integer, real, double
!  precision, or date) see the documentation for the other functions in
!  this module: 'RecordGetAsInt', 'RecordGetAsReal',
!  'RecordGetAsDoublePrec', 'RecordGetAsMonthDayYear',
!  'RecordGetAsMonthYear', and 'RecordGetAsMonthDay'.
!  RETURNS
!  Character string of length 1000: Field value from a record.
!  SYNOPSIS
!  character (len = MAX_CHAR_LENGTH) function RecordGet(rec, field) result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RecordGet
    module procedure RecordGetUsingFieldNum, RecordGetUsingFieldName
  end interface RecordGet

  ! Allow RecordGetAsChar to be the generic name for the specific subroutines.
  !****f* RecordMod/RecordGetAsChar
!  NAME
!  RecordGetAsChar: This function is used to get a field from a record.
!  DESCRIPTION
!  This function is similar to 'RecordGet'. (See the documentation for
!  'RecordGet'.) The only difference is that this function takes a
!  third argument, 'length', which defines the number of characters
!  that will be returned by this function.
!  RETURNS
!  Character string of length 'length': Field value from a record.
!  SYNOPSIS
!  character (len = length) function RecordGetAsChar(rec, field, length) &
!    result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  * length: Integer length of the return value character string.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RecordGetAsChar
    module procedure RecordGetAsCharUsingFieldNum, &
      RecordGetAsCharUsingFieldName
  end interface RecordGetAsChar

  ! Allow RecordGetAsInt to be the generic name for the specific subroutines.
  !****f* RecordMod/RecordGetAsInt
!  NAME
!  RecordGetAsInt: This function is used to get a field from a record.
!  DESCRIPTION
!  This function is similar to 'RecordGet'. (See the documentation for
!  'RecordGet'.) The only difference is that this function returns
!  the value in the specified field as an integer instead of as a
!  character string. If the field contains blanks, the integer 0 is
!  returned.
!  RETURNS
!  Integer: Field value from a record.
!  SYNOPSIS
!  integer function RecordGetAsInt(rec, field) result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RecordGetAsInt
    module procedure RecordGetAsIntUsingFieldNum, &
      RecordGetAsIntUsingFieldName
  end interface RecordGetAsInt

  ! Allow RecordGetAsReal to be the generic name for the specific subroutines.
!****f* RecordMod/RecordGetAsReal
!  NAME
!  RecordGetAsReal: This function is used to get a field from a record.
!  DESCRIPTION
!  This function is similar to 'RecordGet'. (See the documentation for
!  'RecordGet'.) The only difference is that this function returns
!  the value in the specified field as a real instead of as a
!  character string. If the field contains blanks, the real 0.0 is
!  returned.
!  RETURNS
!  Real: Field value from a record.
!  SYNOPSIS
!  real function RecordGetAsReal(rec, field) result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RecordGetAsReal
    module procedure RecordGetAsRealUsingFieldNum, &
      RecordGetAsRealUsingFieldName
  end interface RecordGetAsReal

  ! The maximum length for a charcter string in this module.
  integer, parameter :: MAX_CHAR_LENGTH = 10000
  ! The maximum number of fields (not characters) in a record.
  integer, parameter :: MAX_FIELDS = 1000

  ! Allow RecordGetAsDoublePrec to be the generic name for the specific
  ! subroutines.
!****f* RecordMod/RecordGetAsDoublePrec
!  NAME
!  RecordGetAsDoublePrec: This function is used to get a field from a record.
!  DESCRIPTION
!  This function is similar to 'RecordGet'. (See the documentation for
!  'RecordGet'.) The only difference is that this function returns
!  the value in the specified field as a double precision instead of
!  as a character string. If the field contains blanks, the double
!  precision 0.0d0 is returned.
!  RETURNS
!  Double Precision: Field value from a record.
!  SYNOPSIS
!  double precision function RecordGetAsDoublePrec(rec, field) result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RecordGetAsDoublePrec
    module procedure RecordGetAsDoublePrecUsingFieldNum, &
      RecordGetAsDoublePrecUsingFieldName
  end interface RecordGetAsDoublePrec

  ! Allow RecordGetAsMonthDayYear to be the generic name for the specific
  ! subroutines.
!****f* RecordMod/RecordGetAsMonthDayYear
!  NAME
!  RecordGetAsMonthDayYear: This function is used to get a field from a record.
!  DESCRIPTION
!  This function is similar to 'RecordGet'. (See the documentation for
!  'RecordGet'.) It is used on fields which are exactly 8 characters
!  and are meant to represent a date where the first 2 characters are
!  the month, then next 2 are the day, and the last 4 are the year.
!  It returns these characters as a date in the form mm/dd/yyyy, with
!  the slashes ("/") in the appropriate places. For example, if the
!  field is the 8 characters "01151950", then this function will return
!  the character string "01/15/1950".
!  RETURNS
!  Character string (of length 10): Field value from a record.
!  SYNOPSIS
!  character (len = 10) function RecordGetAsMonthDayYear(rec, field) result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RecordGetAsMonthDayYear
    module procedure RecordGetAsMonthDayYearUsingFieldNum, &
      RecordGetAsMonthDayYearUsingFieldName
  end interface RecordGetAsMonthDayYear

  ! Allow RecordGetAsMonthYear to be the generic name for the specific
  ! subroutines.
!****f* RecordMod/RecordGetAsMonthYear
!  NAME
!  RecordGetAsMonthYear: This function is used to get a field from a record.
!  DESCRIPTION
!  This function is similar to 'RecordGetAsMonthDayYear'. (See the
!  documentation for 'RecordGetAsMonthDayYear'.) It is used on fields
!  which are exactly 6 characters and are meant to represent a date
!  where the first 2 characters are the month and the last 4 are the
!  year. It returns these characters as a date in the form mm/yyyy,
!  with the slashes ("/") in the appropriate places. For example, if
!  the field is the 8 characters "011950", then this function will
!  return the character string "01/1950".
!  RETURNS
!  Character string (of length 7): Field value from a record.
!  SYNOPSIS
!  character (len = 7) function RecordGetAsMonthYear(rec, field) result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RecordGetAsMonthYear
    module procedure RecordGetAsMonthYearUsingFieldNum, &
      RecordGetAsMonthYearUsingFieldName
  end interface RecordGetAsMonthYear

  ! Allow RecordGetAsMonthDay to be the generic name for the specific
  ! subroutines.
!****f* RecordMod/RecordGetAsMonthDay
!  NAME
!  RecordGetAsMonthDay: This function is used to get a field from a record.
!  DESCRIPTION
!  This function is similar to 'RecordGetAsMonthDayYear'. (See the
!  documentation for 'RecordGetAsMonthDayYear'.) It is used on fields
!  which are exactly 4 characters and are meant to represent a date
!  where the first 2 characters are the month and the last 2 are the
!  day. It returns these characters as a date in the form mm/dd,
!  with the slashes ("/") in the appropriate places. For example, if
!  the field is the 4 characters "0115", then this function will
!  return the character string "01/15".
!  RETURNS
!  Character string (of length 5): Field value from a record.
!  SYNOPSIS
!  character (len = 5) function RecordGetAsMonthDay(rec, field) result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****

  interface RecordGetAsMonthDay
    module procedure RecordGetAsMonthDayUsingFieldNum, &
      RecordGetAsMonthDayUsingFieldName
  end interface RecordGetAsMonthDay

  ! Allow RecordSet to be the generic name for the specific subroutines.
!****f* RecordMod/RecordSet
!  NAME
!  RecordSet: This function is used to set a value of a field in the record.
!  DESCRIPTION
!  This function is similar to 'RecordGet'. (See the documentation for
!  'RecordGet'.) The only difference is that it is used to set the
!  value of a field in the record instead of to retrieve a value. The
!  new value to be used is 'newVal'. Note that this only changes the
!  characters of 'rec' -- it does not write them back out to the file
!  containing the records. So these changes are not permanent (from one
!  program run to the next), unless the user explicitly writes out the
!  record after it has been changed. This function returns 0 if it was
!  successful; otherwise non-zero.
!  RETURNS
!  Integer: Returns a 0 if successful; otherwise non-zero.
!  SYNOPSIS
!  integer function RecordSet(rec, field, newVal) result(rv)
!  ARGUMENTS
!  * rec: Entire record line as a character string (of variable length).
!  * field: Field can either be an integer or a character string (of variable
!    length).
!  * newVal: New value used to set the value of a field, character string (of
!    variable length).
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface RecordSet
    module procedure RecordSetUsingFieldNum, RecordSetUsingFieldName
  end interface RecordSet

  ! Data for each field (name, starting and ending positions, etc...).
  type :: FieldT
    private
    character (len = MAX_CHAR_LENGTH) :: name
    integer :: startPos
    integer :: endPos
  end type FieldT

  ! The description of the record read in from the File Description.
  type, public :: RecordDescT
    private
    type (FieldT), dimension(MAX_FIELDS) :: fields
    integer :: numFields
    character(len = MAX_CHAR_LENGTH) :: fmt
  end type RecordDescT

  !
  type (RecordDescT) :: recDesc

  contains

!******************************************************************************
!****f* RecordMod/RecordLoadFileDesc
!  NAME
!  RecordLoadFileDesc: This subroutine reads a "File Description" file that is
!  used to describe the record layout in a "Record File".
!  DESCRIPTION
!  This subroutine reads a "File Description" (FD) file that is used to
!  describe the record layout in a "Record File" (RF). It should be
!  called before using the other routines in this module. 
!
!  The RF contains one record per line and each record has multiple fixed-width
!  fields in it. The FD describes the RF by giving the name of each field and
!  how many characters are contained in each field. So the FD file
!  should, essentially, have two columns. The first column contains the
!  names of the fields in the RF, and the second column contains the
!  size of the corresponding field. The first and second columns should
!  be separated by spaces (not tabs). The number of rows in the FD file
!  should correspond to the number of fields in each record in the RF.
!  Comment lines and blank lines are allowed in the FD file and are
!  ignored. A comment line must begin with an exclamation mark (!) as
!  the first character on the line. The name of the FD file is passed
!  to this subroutine in the variable 'fileName'. Additionally, the
!  user must pass in any available unit number (in 'unitNum') that this
!  subroutine can use to open the FD file and read it in. It will close
!  this file at the end of the subroutine so the user can re-use that
!  unit number later in the program.
!
!  For example, say you have a file
!  that contains people's Social Security number, sex, and age. And the
!  file is fixed-width so that the first nine characters are the SSN,
!  the next 1 is the person's sex ("M" or "F") and the last 3 are the
!  person's age. The contents of the RF may look like this:
!     000000001M 51
!     000000002M  3
!     000000003F102
!     000000004F 47
!     000000005M 22
!  This module is used to read the file and have easy access to the
!  fields of each record. You would create a FD file that looks like
!  this:
!     ! This is a file description for a file where the first 9 characters
!     ! are the SSN, then next 1 is the sex of the person, and the last 3
!     ! are person's age.
!     ! Field Name        Number Of Characters
!     ssn                 10
!     sex                  1
!     Age                  3
!  Near the beginning of your program you would have a call to this
!  subroutine that would look something like this:
!     call RecordLoadFileDesc("myFileDesc", 10)
!  Then, at later points in the program you could call 'RecordGet' (or
!  'RecordSet') to get (or set) different fields of a record. See the
!  documentation for these two functions.
!  SYNOPSIS

  subroutine RecordLoadFileDesc(fileName, unitNum)

!  ARGUMENTS
!  * fileName: Name of the "File Description" file.
!  * unitNum: Unit number of the "File Description" file.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: fileName
    integer, intent(in) :: unitNum
!*****
    integer :: fieldNum, startPos, endPos, fieldSize
    character (len = MAX_CHAR_LENGTH) :: line

    recDesc.fields.name = ""
    recDesc.fields.startPos = 0
    recDesc.fields.endPos = 0


    call OpenFile(unitNum, fileName, "old")

    recDesc.fmt = "(A"
    fieldNum = 0
    startPos = 1
    do
      line = GetLine(unitNum)
      if (trim(line) == "") then
        ! Exit the loop if we got to end-of-file.
        exit
      end if
      fieldNum = fieldNum + 1
      recDesc.fields(fieldNum).name = trim(ParseLine(line))
      fieldSize = AscToInt(ParseLine(line))
      endPos = startPos + fieldSize - 1
      recDesc.fields(fieldNum).startPos = startPos
      recDesc.fields(fieldNum).endPos = endPos
      recDesc.fmt = trim(recDesc.fmt) // trim(IntToAsc(1 + &
        max(fieldSize, len_trim(recDesc.fields(fieldNum).name)))) // ", A"
      startPos = endPos + 1
    end do

    call CloseFile(unitNum)


    recDesc.numFields = fieldNum
    ! Remove the last 3 characters (", A") before putting the closing paren.
    recDesc.fmt = trim(recDesc.fmt(1:len_trim(recDesc.fmt)-3)) // ")"

  end subroutine RecordLoadFileDesc
!******************************************************************************
!****f* RecordMod/RecordWriteFileDesc
!  NAME
!  RecordWriteFileDesc: This subroutine is mainly used for debugging to make
!  sure the "File Descrption" file was read in correctly.
!  DESCRIPTION
!  This subroutine is mainly used for debugging to make sure the "File
!  Descrption" file (see documentation of 'RecordLoadFileDesc') was
!  read in correctly. It prints out (to 'unitNum') some details about
!  the "File Description" file that was previously read in through the
!  subroutine 'RecordLoadFileDesc'. The user is in charge of making
!  sure that the file associated with 'unitNum' is already open. The
!  number 6 can be passed in as the 'unitNum' if the output should go
!  to standard out (usually the screen).
!  SYNOPSIS

  subroutine RecordWriteFileDesc(unitNum)

!  ARGUMENTS
!  * unitNum: Unit number of the file.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: unitNum
!*****
    integer :: fieldNum, numChars

    write(unitNum, '(A)') "The currently loaded File Description has " // &
      trim(IntToAsc(recDesc.numFields)) // " fields in it."
    write(unitNum, '(A)') &
      "It has the following format associated with its output functions:"
    write(unitNum, '(A)') trim(recDesc.fmt)
    write(unitNum, '(A)') ""
    write(unitNum, '(A)') "Here are the details about each field:"
    write(unitNum, '(A)') ""
    write(unitNum, '(A5, A20, 3A12)') &
      "Field", "", "Number of", "Starting", "Ending"
    write(unitNum, '(A5, A20, 3A12)') &
      "Num.", "Field Name", "Characters", "Position", "Position"
    write(unitNum, '(A5, A20, 3A12)') repeat("-", 5), repeat("-", 20), &
      repeat("-", 12), repeat("-", 12), repeat("-", 12)

    do fieldNum = 1, recDesc.numFields
      numChars = &
        recDesc.fields(fieldNum).endPos - recDesc.fields(fieldNum).startPos + 1
      write(unitNum, '(I5, A20, 3I12)') &
        fieldNum, trim(recDesc.fields(fieldNum).name), numChars, &
        recDesc.fields(fieldNum).startPos, recDesc.fields(fieldNum).endPos
    end do

  end subroutine RecordWriteFileDesc
!******************************************************************************
!****f* RecordMod/RecordRead
!  NAME
!  RecordRead: This function is used for reading a record.
!  DESCRIPTION
!  This function is used for reading a record. The record being read is from the
!  file associated with 'unitNum'. It does not open or close this file;
!  it just reads it. The entire record is read on one line and
!  will have spacing so that it will line up with the headings if
!  'RecordWriteHeader' was used to write out the headings. This
!  function returns 0 if it was successful; otherwise non-zero.
!  RETURNS
!  Integer: Returns 0 if successful; otherwise non-zero.
!  SYNOPSIS

  integer function RecordRead(rec, unitNum) result(rv)

!  ARGUMENTS
!  * rec: Record being read.
!  * unitNum: Unit number of the file to be opened.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(out) :: rec
    integer, intent(in) :: unitNum
!*****

    read(unitNum, '(A)', iostat = rv) rec

  end function RecordRead
!******************************************************************************
!****f* RecordMod/RecordWrite
!  NAME
!  RecordWrite: This function is used for writing out a record.
!  DESCRIPTION
!  This function is used for writing out a record. It is written to the
!  file associated with 'unitNum'. It does not open or close this file;
!  it just writes to it. The entire record is written on one line and
!  will have spacing so that it will line up with the headings if
!  'RecordWriteHeader' was used to write out the headings. This
!  function returns 0 if it was successful; otherwise non-zero.
!  RETURNS
!  Integer: Returns 0 if successful; otherwise non-zero.
!  SYNOPSIS

  integer function RecordWrite(rec, unitNum) result(rv)

!  ARGUMENTS
!  * rec: Record to write to the file.
!  * unitNum: Unit number of the file to be opened.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: rec
    integer, intent(in) :: unitNum
!*****

    write(unitNum, '(A)', iostat = rv) rec

  end function RecordWrite
!******************************************************************************
  character (len = MAX_CHAR_LENGTH) function RecordGetUsingFieldNum(rec, &
    fieldNum) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: fieldNum
    integer :: startPos, endPos

    startPos = recDesc.fields(fieldNum).startPos
    endPos = recDesc.fields(fieldNum).endPos
    rv = rec(startPos:endPos)

  end function RecordGetUsingFieldNum
!******************************************************************************
  character (len = MAX_CHAR_LENGTH) function RecordGetUsingFieldName(rec, &
    fieldName) result(rv)

    character (len = *), intent(in) :: rec
    character (len = *), intent(in) :: fieldName
    integer :: fieldNum
    logical :: found

    rv = ""

    found = .false.
    do fieldNum = 1, recDesc.numFields
      if (trim(recDesc.fields(fieldNum).name) == fieldName) then
        found = .true.
        rv = RecordGetUsingFieldNum(rec, fieldNum)
        exit
      end if
    end do

    if (.not. found) then
      write(6, '(A)') "Error in RecordGetUsingFieldName(). " // &
        "Could not find field: " // trim(fieldName)
      stop
    end if

  end function RecordGetUsingFieldName
!******************************************************************************
  character (len = length) function RecordGetAsCharUsingFieldNum(rec, &
    fieldNum, length) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: fieldNum
    integer, intent(in) :: length

    rv = RecordGet(rec, fieldNum)

  end function RecordGetAsCharUsingFieldNum
!******************************************************************************
  character (len = length) function RecordGetAsCharUsingFieldName(rec, &
    fieldName, length) result(rv)

    character (len = *), intent(in) :: rec
    character (len = *), intent(in) :: fieldName
    integer, intent(in) :: length

    rv = RecordGet(rec, fieldName)

  end function RecordGetAsCharUsingFieldName
!******************************************************************************
  integer function RecordGetAsIntUsingFieldNum(rec, fieldNum) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: fieldNum

    rv = AscToInt(RecordGet(rec, fieldNum), .false.)

  end function RecordGetAsIntUsingFieldNum
!******************************************************************************
  integer function RecordGetAsIntUsingFieldName(rec, fieldName) result(rv)

    character (len = *), intent(in) :: rec
    character (len = *), intent(in) :: fieldName

    rv = AscToInt(RecordGet(rec, fieldName), .false.)

  end function RecordGetAsIntUsingFieldName
!******************************************************************************
  real function RecordGetAsRealUsingFieldNum(rec, fieldNum) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: fieldNum

    rv = AscToReal(RecordGet(rec, fieldNum), .false.)

  end function RecordGetAsRealUsingFieldNum
!******************************************************************************
  real function RecordGetAsRealUsingFieldName(rec, fieldName) result(rv)

    character (len = *), intent(in) :: rec
    character (len = *), intent(in) :: fieldName

    rv = AscToReal(RecordGet(rec, fieldName), .false.)

  end function RecordGetAsRealUsingFieldName
!******************************************************************************
  double precision function RecordGetAsDoublePrecUsingFieldNum(rec, &
    fieldNum) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: fieldNum

    rv = AscToDoublePrec(RecordGet(rec, fieldNum), .false.)

  end function RecordGetAsDoublePrecUsingFieldNum
!******************************************************************************
  double precision function RecordGetAsDoublePrecUsingFieldName(rec, &
    fieldName) result(rv)

    character (len = *), intent(in) :: rec
    character (len = *), intent(in) :: fieldName

    rv = AscToDoublePrec(RecordGet(rec, fieldName), .false.)

  end function RecordGetAsDoublePrecUsingFieldName
!******************************************************************************
  character(len = 10) function RecordGetAsMonthDayYearUsingFieldNum(rec, &
    fieldNum) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: fieldNum
    character (len = 8) :: monthDayYear

    monthDayYear = RecordGet(rec, fieldNum)
    rv = monthDayYear(1:2) // "/" // monthDayYear(3:4) // "/" // &
      monthDayYear(5:8)

  end function RecordGetAsMonthDayYearUsingFieldNum
!******************************************************************************
  character(len = 10) function RecordGetAsMonthDayYearUsingFieldName(rec, &
    fieldName) result(rv)

    character (len = *), intent(in) :: rec
    character (len = *), intent(in) :: fieldName
    character (len = 8) :: monthDayYear

    monthDayYear = RecordGet(rec, fieldName)
    rv = monthDayYear(1:2) // "/" // monthDayYear(3:4) // "/" // &
      monthDayYear(5:8)

  end function RecordGetAsMonthDayYearUsingFieldName
!******************************************************************************
  character(len = 7) function RecordGetAsMonthYearUsingFieldNum(rec, &
    fieldNum) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: fieldNum
    character (len = 6) :: monthYear

    monthYear = RecordGet(rec, fieldNum)
    rv = monthYear(1:2) // "/" // monthYear(3:6)

  end function RecordGetAsMonthYearUsingFieldNum
!******************************************************************************
  character(len = 7) function RecordGetAsMonthYearUsingFieldName(rec, &
    fieldName) result(rv)

    character (len = *), intent(in) :: rec
    character (len = *), intent(in) :: fieldName
    character (len = 6) :: monthYear

    monthYear = RecordGet(rec, fieldName)
    rv = monthYear(1:2) // "/" // monthYear(3:6)

  end function RecordGetAsMonthYearUsingFieldName
!******************************************************************************
  character(len = 5) function RecordGetAsMonthDayUsingFieldNum(rec, &
    fieldNum) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: fieldNum
    character (len = 4) :: monthDay

    monthDay = RecordGet(rec, fieldNum)
    rv = monthDay(1:2) // "/" // monthDay(3:4)

  end function RecordGetAsMonthDayUsingFieldNum
!******************************************************************************
  character(len = 5) function RecordGetAsMonthDayUsingFieldName(rec, &
    fieldName) result(rv)

    character (len = *), intent(in) :: rec
    character (len = *), intent(in) :: fieldName
    character (len = 4) :: monthDay

    monthDay = RecordGet(rec, fieldName)
    rv = monthDay(1:2) // "/" // monthDay(3:4)

  end function RecordGetAsMonthDayUsingFieldName
!******************************************************************************
  integer function RecordSetUsingFieldNum(rec, fieldNum, newVal) result(rv)

    character (len = *), intent(inout) :: rec
    integer, intent(in) :: fieldNum
    character (len = *), intent(in) :: newVal
    integer :: startPos, endPos

    rv = 0

    startPos = recDesc.fields(fieldNum).startPos
    endPos = recDesc.fields(fieldNum).endPos
    rec(startPos:endPos) = newVal

  end function RecordSetUsingFieldNum
!******************************************************************************
  integer function RecordSetUsingFieldName(rec, fieldName, newVal) result(rv)

    character (len = *), intent(inout) :: rec
    character (len = *), intent(in) :: fieldName
    character (len = *), intent(in) :: newVal
    integer :: fieldNum, startPos, endPos
    logical :: found

    rv = 0

    found = .false.
    do fieldNum = 1, recDesc.numFields
      if (trim(recDesc.fields(fieldNum).name) == fieldName) then
        found = .true.
        rv = RecordSetUsingFieldNum(rec, fieldNum, newVal)
        exit
      end if
    end do

    if (.not. found) then
      write(6, '(A)') "Error in RecordSetUsingFieldName(). " // &
        "Could not find field: " // trim(fieldName)
      stop
    end if

  end function RecordSetUsingFieldName
!******************************************************************************
!****f* RecordMod/RecordGetFieldID
!  NAME
!  RecordGetFieldID: This function returns an ID associated with the field name.
!  DESCRIPTION
!  This function takes the field name in 'fieldName' and returns an ID
!  associated with the field. This ID can be used as a faster way to
!  access variables in a record when using the 'RecordGet' function
!  (or any of the similar functions in this module.)  Passing in the
!  ID is faster than passing in the fieldName to every 'RecordGet'
!  function call. An ID of -1 is returned if 'fieldName' is not valid.
!  RETURNS
!  Integer: The ID associated with the field name.
!  SYNOPSIS

  integer function RecordGetFieldID(fieldName) result(rv)

!  ARGUMENTS
!  * fieldName: Field name in a record.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: fieldName
!*****
    integer :: fieldNum

    rv = -1
    do fieldNum = 1, recDesc.numFields
      if (trim(recDesc.fields(fieldNum).name) == fieldName) then
        rv = fieldNum
        exit
      end if
    end do

  end function RecordGetFieldID
!******************************************************************************
!****f* RecordMod/RecordWriteFull
!  NAME
!  RecordWriteFull: This function is used for writing out a record.
!  DESCRIPTION
!  This function is used for writing out a record. It calls
!  'RecordWriteHeader' first, to write out the field names. Then it
!  call 'RecordWrite' to write out that value of each field. The
!  spacing will be such that the field names will line up with the
!  field values. This function returns 0 if it was successful;
!  otherwise non-zero.
!  RETURNS
!  Integer: Returns 0 if successful; otherwise non-zero.
!  SYNOPSIS

  integer function RecordWriteFull(rec, unitNum) result(rv)

!  ARGUMENTS
!  * rec: Record to write to the file.
!  * unitNum: Unit number of the file.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: rec
    integer, intent(in) :: unitNum
!*****

    rv = RecordWriteHeader(unitNum)
    rv = RecordWriteForm(rec, unitNum)

  end function RecordWriteFull
!******************************************************************************
!****f* RecordMod/RecordWriteHeader
!  NAME
!  RecordWriteHeader: This function writes out the field names on one line.
!  DESCRIPTION
!  This function writes out the field names on one line. They are written
!  to the file associated with 'unitNum'. It does not open or close the
!  file; it just writes to it. All of the field names will be written
!  on one line and will have spacing so that it will line up with the
!  records written out subsequently with 'RecordWrite'. This function
!  returns 0 if it was successful; otherwise non-zero.
!  RETURNS
!  Integer: Returns 0 if successful; otherwise non-zero.
!  SYNOPSIS

  integer function RecordWriteHeader(unitNum) result(rv)

!  ARGUMENTS
!  * unitNum: Unit number of the file.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: unitNum
!*****
    integer :: fieldNum

    write(unitNum, recDesc.fmt) &
      (trim(recDesc.fields(fieldNum).name), fieldNum = 1, recDesc.numFields)

    rv = 0

  end function RecordWriteHeader
!******************************************************************************
  integer function RecordWriteForm(rec, unitNum) result(rv)

    character (len = *), intent(in) :: rec
    integer, intent(in) :: unitNum
    integer :: fieldNum

    write(unitNum, recDesc.fmt) &
      (trim(RecordGet(rec, trim(recDesc.fields(fieldNum).name))), &
      fieldNum = 1, recDesc.numFields)

    rv = 0

  end function RecordWriteForm
!******************************************************************************

end module RecordMod
