! $Id: DateTimeMod.f90 1.4 2017/06/22 12:03:44EDT 622325 Development  $
! this module is not currently ready to be released with the next updated of
! the library. So it is explicitly excluded from robodoc using the options in
! robodoc.rc. See the "ignore files" section in robodoc.rc.
module DateTimeMod

!****h* source/DateTimeMod
!  NAME
!  DateTimeMod: OcactFortLib DateTimeMod module.
!  REVISION
!  $Revision: 1.4 $
!  MODULE DESCRIPTION
!  The DateTimeMod module contains a function that is used to
!  manipulate date strings.
!  MODULE ROUTINES
!  DateFormatter
!*****

  use StringsMod
  implicit none
  save

  private
  public :: DateFormatter

  ! The maximum length for a charcter string in this module.
  integer, parameter :: MAX_CHAR_LENGTH = 10000
  ! The maximum number of dimensions in this module.
  integer, parameter :: MAX_DIM = 100

   contains

!******************************************************************************
!****f* DateTimeMod/DateFormatter
!  NAME
!  DateFormatter: This function takes a character variable holding a date
!  format and returns either the current date or the optional argument date in
!  the requested date format.
!  DESCRIPTION
!  This function takes a character variable holding a date
!  format and returns either the current date or the optional argument date in
!  the requested date format. For example, if the variable 'dateFmt' is a
!  character string holding the value "mm/dd/yyyy", then the current date
!  (ex. April 30, 2015)will return '04/30/2015' as a character string.
!  
!  The DataFormatter functions looks for the following month, day, year inputs:
!      m: 1, 2, 3, ...
!     mm: 01, 02, 03, ...
!    mmm: Jan, Feb, Mar, ...
!   mmmm: January, February, March, ...
!      d: 1, 2, 3, ...
!     dd: 01, 02, 03, ...
!     yy: 10, 11, 12, ...
!   yyyy: 2010, 2011, 2012, ...
!  The dateFmt and optDateFmt inputs must be lower case letters.  Inputs that
!  are not recognized will print out as entered, and '\' is used as an escape
!  character. For example,'DateFormatter("\month: mmmm", '6-15-10', 'm-dd-yy')'
!  will return 'month: June'.
!  RETURNS
!  Character string: Character sting value of the requested date in the
!  requested format.
!  SYNOPSIS

  character (len = MAX_CHAR_LENGTH) &
      function DateFormatter(dateFmt, optDate, optDateFmt) result(date)

!  ARGUMENTS
!  * dateFmt: Character variable holding the date format.
!  * optDate: (optional) The date to be formatted in dateFmt. 
!    If optDate is present, optDateFmt must also be present.
!  * optDateFmt: (optional) Format of optDate. If optDateFmt is present,
!    optDate must also be present.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: dateFmt
    character (len = *), optional, intent(in) :: optDate
    character (len = *), optional, intent(in) :: optDateFmt
!*****
    ! Date/Time variable
    !1: year, 2: month, 3: day, 5: hour, 6: minute, 7:seconds, 8: milliseconds
    integer, dimension(8) :: dateTime
    ! Separates the individual pieces of the dateFmt character string
    character (len = MAX_CHAR_LENGTH), dimension(MAX_DIM) :: parseDateFmt
    ! Separates the individual pieces of the optDateFmt character string
    character (len = MAX_CHAR_LENGTH), dimension(MAX_DIM) :: parseOptDateFmt
    character (len = MAX_CHAR_LENGTH) :: reqDateFmt
    ! Looping variables
    integer :: i, iend, istart, numDimDateFmt, numDimOptDateFmt, n
    ! Character string length variables
    integer :: length, dateLength, dtFmtLength

    dateTime = 0

    if (.not. present(optDate) .and. present(optDateFmt)) then
      write(6,'(A)') &
          "If optDateFmt is present, then optDate must also be present"
      stop
    else if (.not. present(optDateFmt) .and. present(optDate)) then
      write(6,'(A)') &
          "If optDate is present, then optDateFmt must also be present."
      stop
    end if

    if (present(optDate) .and. present(optDateFmt)) then

      call ReadDateFormat(optDateFmt, parseOptDateFmt, numDimOptDateFmt)
      n = 1
      do i = 1, numDimOptDateFmt
        length = len_trim(parseOptDateFmt(i)) - 1
        if(length < 0) then
          length = 0
        end if
        call WriteOptDate(parseOptDateFmt(i), optDate(n:n+length), dateTime)
        n = n + length + 1
      end do

    else

      call date_and_time(values = dateTime)

    end if

    date = ""
    call ReadDateFormat(dateFmt, parseDateFmt, numDimDateFmt)

    dateLength = 0
    do i = 1, numDimDateFmt
      call WriteDate(parseDateFmt(i), dateTime, reqDateFmt, dtFmtLength)
      date = date(1:dateLength) // reqDateFmt(1:dtFmtLength)
      dateLength = dateLength + dtFmtLength
    end do

  end function DateFormatter
!******************************************************************************
  subroutine ReadDateFormat(dateFmt, parseDateFmt, numDimDateFmt)

    character (len = *), intent(in) :: dateFmt
    character (len = MAX_CHAR_LENGTH), dimension(MAX_DIM), &
        intent(out) :: parseDateFmt
    integer, intent(out) :: numDimDateFmt
    logical :: escapeFlag

    integer :: iend, istart, length, i

    parseDateFmt = ""
    length = len(dateFmt)
    iend = 1
    istart = 1
    numDimDateFmt = 1
    escapeFlag = .false.

    do i = 1, length
      if(.not. escapeFlag) then
        if(dateFmt(i:i) == "m" .or. dateFmt(i:i) == "d" .or. &
            dateFmt(i:i) == "y") then
          if(iend > 1) then
            if(dateFmt(i:i) == dateFmt(i-1:i-1)) then
             parseDateFmt(numDimDateFmt) = dateFmt(istart:i)
              iend = iend + 1
            else
              iend = iend + 1
              istart = i
              numDimDateFmt = numDimDateFmt + 1
              parseDateFmt(numDimDateFmt) = dateFmt(istart:i)
            end if
          else
            iend = iend + 1
            parseDateFmt(numDimDateFmt) = dateFmt(istart:i)
          end if
        else if (dateFmt(i:i) == "\") then
          escapeFlag = .true.
          if(i > 1) then
            numDimDateFmt = numDimDateFmt + 1
          end if
          istart = i
          iend = istart + 1
          cycle
        else
          if(i > 1) then
            numDimDateFmt = numDimDateFmt + 1
          end if
          parseDateFmt(numDimDateFmt) = dateFmt(i:i)
          istart = i
          iend = istart + 1
        end if
      else
        parseDateFmt(numDimDateFmt) = dateFmt(i-1:i)
        istart = i
        iend = istart + 1
        escapeFlag = .false.
      end if
    end do

  end subroutine ReadDateFormat
!******************************************************************************
  subroutine WriteDate(parseDateFmt, dateTime, reqDateFmt, dtFmtLength)
    character (len = *), intent(in) :: parseDateFmt
    integer, dimension(8), intent(in) :: dateTime
    character (len = *), intent(out) :: reqDateFmt
    integer, intent(out) :: dtFmtLength

    select case(trim(parseDateFmt))

      case("m")
        reqDateFmt = IntToAsc(dateTime(2))
      case("mm")
        if(dateTime(2) == 1) then
          reqDateFmt = '01'
        else if(dateTime(2) == 2) then
          reqDateFmt = '02'
        else if(dateTime(2) == 3) then
          reqDateFmt = '03'
        else if(dateTime(2) == 4) then
          reqDateFmt = '04'
        else if(dateTime(2) == 5) then
          reqDateFmt = '05'
        else if(dateTime(2) == 6) then
          reqDateFmt = '06'
        else if(dateTime(2) == 7) then
          reqDateFmt = '07'
        else if(dateTime(2) == 8) then
          reqDateFmt = '08'
        else if(dateTime(2) == 9) then
          reqDateFmt = '09'
        else if(dateTime(2) == 10) then
          reqDateFmt = '10'
        else if(dateTime(2) == 11) then
          reqDateFmt = '11'
        else if(dateTime(2) == 12) then
          reqDateFmt = '12'
        end if
      case("mmm")
        if(dateTime(2) == 1) then
          reqDateFmt = 'Jan'
        else if(dateTime(2) == 2) then
          reqDateFmt = 'Feb'
        else if(dateTime(2) == 3) then
          reqDateFmt = 'Mar'
        else if(dateTime(2) == 4) then
          reqDateFmt = 'Apr'
        else if(dateTime(2) == 5) then
          reqDateFmt = 'May'
        else if(dateTime(2) == 6) then
          reqDateFmt = 'Jun'
        else if(dateTime(2) == 7) then
          reqDateFmt = 'Jul'
        else if(dateTime(2) == 8) then
          reqDateFmt = 'Aug'
        else if(dateTime(2) == 9) then
          reqDateFmt = 'Sep'
        else if(dateTime(2) == 10) then
          reqDateFmt = 'Oct'
        else if(dateTime(2) == 11) then
          reqDateFmt = 'Nov'
        else if(dateTime(2) == 12) then
          reqDateFmt = 'Dec'
        end if
      case("mmmm")
        if(dateTime(2) == 1) then
          reqDateFmt = 'January'
        else if(dateTime(2) == 2) then
          reqDateFmt = 'February'
        else if(dateTime(2) == 3) then
          reqDateFmt = 'March'
        else if(dateTime(2) == 4) then
          reqDateFmt = 'April'
        else if(dateTime(2) == 5) then
          reqDateFmt = 'May'
        else if(dateTime(2) == 6) then
          reqDateFmt = 'June'
        else if(dateTime(2) == 7) then
          reqDateFmt = 'July'
        else if(dateTime(2) == 8) then
          reqDateFmt = 'August'
        else if(dateTime(2) == 9) then
          reqDateFmt = 'September'
        else if(dateTime(2) == 10) then
          reqDateFmt = 'October'
        else if(dateTime(2) == 11) then
          reqDateFmt = 'November'
        else if(dateTime(2) == 12) then
          reqDateFmt = 'December'
        end if
      case("d")
        reqDateFmt = IntToAsc(dateTime(3))
      case("dd")
        if(dateTime(3) == 1) then
          reqDateFmt = '01'
        else if(dateTime(3) == 2) then
          reqDateFmt = '02'
        else if(dateTime(3) == 3) then
          reqDateFmt = '03'
        else if(dateTime(3) == 4) then
          reqDateFmt = '04'
        else if(dateTime(3) == 5) then
          reqDateFmt = '05'
        else if(dateTime(3) == 6) then
          reqDateFmt = '06'
        else if(dateTime(3) == 7) then
          reqDateFmt = '07'
        else if(dateTime(3) == 8) then
          reqDateFmt = '08'
        else if(dateTime(3) == 9) then
          reqDateFmt = '09'
        else if(dateTime(3) > 9) then
          reqDateFmt = IntToAsc(dateTime(3))
        end if
      case("yy")
        reqDateFmt = Right(dateTime(1), 2)
      case("yyyy")
        reqDateFmt = IntToAsc(dateTime(1))
        if(left(reqDateFmt,2) == "99") then
          write(6,'(A)') "Error: Entered optional 2 digit year and " &
              // "requested 4 digit year."
          stop
        end if
      case default
         if(left(parseDateFmt,1) == "\") then
          reqDateFmt = parseDateFmt(2:2)
        else
          reqDateFmt = trim(parseDateFmt)
        end if

    end select

    dtFmtLength = len_trim(reqDateFmt)
    if (dtFmtLength == 0) then
      dtFmtLength = 1
    end if

  end subroutine WriteDate
!******************************************************************************
  subroutine WriteOptDate(parseOptDateFmt, optDate, dateTime)

    character (len = *), intent(in) :: parseOptDateFmt
    character (len = *), intent(in) :: optDate
    integer, dimension(8), intent(out) :: dateTime

    select case(trim(parseOptDateFmt))

      case("m")
        if(optDate == "1") then
          dateTime(2) = 1
        else if(optDate == "2") then
          dateTime(2) = 2
        else if(optDate == "3") then
          dateTime(2) = 3
        else if(optDate == "4") then
          dateTime(2) = 4
        else if(optDate == "5") then
          dateTime(2) = 5
        else if(optDate == "6") then
          dateTime(2) = 6
        else if(optDate == "7") then
          dateTime(2) = 7
        else if(optDate == "8") then
          dateTime(2) = 8
        else if(optDate == "9") then
          dateTime(2) = 9
        else if(optDate == "10") then
          dateTime(2) = 10
        else if(optDate == "11") then
          dateTime(2) = 11
        else if(optDate == "12") then
          dateTime(2) = 12
        end if
      case("mm")
        if(optDate == "01") then
          dateTime(2) = 1
        else if(optDate == "02") then
          dateTime(2) = 2
        else if(optDate == "03") then
          dateTime(2) = 3
        else if(optDate == "04") then
          dateTime(2) = 4
        else if(optDate == "05") then
          dateTime(2) = 5
        else if(optDate == "06") then
          dateTime(2) = 6
        else if(optDate == "07") then
          dateTime(2) = 7
        else if(optDate == "08") then
          dateTime(2) = 8
        else if(optDate == "09") then
          dateTime(2) = 9
        else if(optDate == "10") then
          dateTime(2) = 10
        else if(optDate == "11") then
          dateTime(2) = 11
        else if(optDate == "12") then
          dateTime(2) = 12
        end if
      case("mmm")
        if(optDate == "Jan") then
          dateTime(2) = 1
        else if(optDate == "Feb") then
          dateTime(2) = 2
        else if(optDate == "Mar") then
          dateTime(2) = 3
        else if(optDate == "Apr") then
          dateTime(2) = 4
        else if(optDate == "May") then
          dateTime(2) = 5
        else if(optDate == "Jun") then
          dateTime(2) = 6
        else if(optDate == "Jul") then
          dateTime(2) = 7
        else if(optDate == "Aug") then
          dateTime(2) = 8
        else if(optDate == "Sept") then
          dateTime(2) = 9
        else if(optDate == "Oct") then
          dateTime(2) = 10
        else if(optDate == "Nov") then
          dateTime(2) = 11
        else if(optDate == "Dec") then
          dateTime(2) = 12
        end if
      case("mmmm")
        if(optDate == "January") then
          dateTime(2) = 1
        else if(optDate == "February") then
          dateTime(2) = 2
        else if(optDate == "March") then
          dateTime(2) = 3
        else if(optDate == "April") then
          dateTime(2) = 4
        else if(optDate == "May") then
          dateTime(2) = 5
        else if(optDate == "June") then
          dateTime(2) = 6
        else if(optDate == "July") then
          dateTime(2) = 7
        else if(optDate == "August") then
          dateTime(2) = 8
        else if(optDate == "September") then
          dateTime(2) = 9
        else if(optDate == "October") then
          dateTime(2) = 10
        else if(optDate == "November") then
          dateTime(2) = 11
        else if(optDate == "December") then
          dateTime(2) = 12
        end if
      case("d")
        dateTime(3) = AscToInt(optDate)
      case("dd")
        dateTime(3) = AscToInt(optDate)
      case("yy")
        dateTime(1) = AscToInt("99" // optDate)
      case("yyyy")
        dateTime(1) = AscToInt(optDate)
      case default

    end select

  end subroutine WriteOptDate
!******************************************************************************
end module DateTimeMod
