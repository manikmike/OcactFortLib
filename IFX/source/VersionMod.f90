! $Id: VersionMod.f90 1.108 8/4/2021 15:05:02 Morris,M. $
module VersionMod

!****h* source/VersionMod
!  NAME
!  VersionMod: OcactFortLib VersionMod module.
!  REVISION
!  $Revision: 1.107 $
!  MODULE DESCRIPTION
!  The VersionMod module contains the OcactFortLibVer function. This function
!  returns the version of the library.
!  MODULE ROUTINES
!  OcactFortLibCompVer
!  OcactFortLibVer
!*****

  use PlatformsMod

  implicit none

  private
  public :: OcactFortLibVer
  public :: OcactFortLibCompVer

  integer, parameter :: MAX_CHAR = 100
  ! This is the version number of the library. The main version number
  ! is everything up till the last decimal point (not inclusive). The part
  ! after the last decimal point is just the date (in MMDDYYYY format) of
  ! the last change for this version of the library.
  ! After the date, if there is a "B", then this version is for Beta testing.
  character (len = MAX_CHAR), parameter :: &
    OCACT_FORT_LIB_VER = "1.23.1.05092023"

contains

!******************************************************************************
!****f* VersionMod/OcactFortLibVer
!  NAME
!  OcactFortLibVer: This function returns the version of the library.
!  DESCRIPTION
!  This function simply returns a character string (of length 100) stating
!  the version of the library. This string should typically be "trimmed"
!  since it will usually contain less than 25 characters (followed by
!  trailing blanks). Optionally, the string may contain the "architecture"
!  on which the library was built (e.g., "32-bit", "64-bit"). If the
!  "dispArchType" is true, then the architecture type will be displayed
!  after the library version number. If it is false or omitted, then the
!  architecture type information will not be displayed.
!
!  The version will be of the form A.B.C.MMDDYYYY,
!  where "A" is the major version number, "B" is the minor version number,
!  and "C" is the revision number. The "MMDDYYYY" is the 2-digit month,
!  2-digit day, and 4-digit year of the release of this version of the
!  library. The architecture will be of the form "XX-bit", where "XX" is
!  either 32 or 64, and it will be enclosed in brackets.
!
!  An example version number may be "1.0.0.09212006". This means
!  that it is the first release of the library and was on Sep. 21, 2006.
!  Another example version number, with architecture type, may be
!  "1.0.0.09212006 [32-bit]". This means that it is the first release of
!  the library and was on Sep. 21, 2006, and was a 32-bit version.
!  RETURNS
!  Character string (of length 100): Version of the library.
!  SYNOPSIS

  character (len = MAX_CHAR) function OcactFortLibVer(dispArchType) result(rv)

!  ARGUMENTS
!  * dispArchType (optional): If false or omitted, the function will not
!    display the architecture of the library (e.g., "[32-bit]", "[64-bit]").
!    If true the function will display the architecture after displaying the
!    library version.
!  ARGUMENT DECLARATIONS
  logical, optional, intent(in) :: dispArchType
!*****

    rv = OCACT_FORT_LIB_VER
    if (present(dispArchType)) then
      if (dispArchType) then
        rv = trim(rv) // " [" // trim(GetArchitectureType()) // "]"
      end if
    end if

  end function OcactFortLibVer
!******************************************************************************
!****f* VersionMod/OcactFortLibCompVer
!  NAME
!  OcactFortLibVer: This function returns the compiler version used to build
!  the library.
!  DESCRIPTION
!  This function simply returns a character string (of length 100) stating
!  the compiler version used to build the library. This string should typically
!  be "trimmed" since it will usually contain less than 25 characters (followed
!  by trailing blanks).
!
!  The version will be of the form "CompName AAAA.B.YYYYMMDD", where "CompName"
!  is the compiler name (e.g., "Intel"), AAAA" is the major version number,
!  "B" is the minor version number, and "YYYYMMDD" is the build date (4-digit
!  year, 2-digit month, and 2-digit day). 
!
!  An example compiler version may be "Intel 1700.4.20170411". This means
!  that this version of the library was built with the Intel compiler, Composer
!  XE 2017 (i.e., "1700"), Update 4 (i.e., "4"), and with a build date of April
!  11, 2017 (i.e., 20170411).
!  RETURNS
!  Character string (of length 100): Version of the compiler that was used to
!  build the library.
!  SYNOPSIS

  character (len = MAX_CHAR) function OcactFortLibCompVer() result(rv)

    rv = GetFullCompVer()

  end function OcactFortLibCompVer
!******************************************************************************

end module VersionMod
