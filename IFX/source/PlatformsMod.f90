! $Id: PlatformsMod.f90 1.25 2017/07/17 09:51:04EDT 622325 Development  $
! Note: Since this code supports 2 compilers, there are two sets of code.
! If any routines are added, remember to add a version of the code for each
! supported compiler.
! Note: For some reason, the Intel Fortran Compiler, insists that the
! preprocessor lines be at the beginning of each line (with no indenting).
! To use this module, the compiler needs to define which platform is
! being used: either PLATFORM_DIGITAL_FORTRAN, PLATFORM_INTEL_FORTRAN
! or PLATFORM_LAHEY.
module PlatformsMod

!****h* source/PlatformsMod
!  NAME
!  PlatformsMod: OcactFortLib PlatformsMod module.
!  REVISION
!  $Revision: 1.25 $
!  MODULE DESCRIPTION
!  The PlatformsMod module contains an interface for system-specific routines.
!  It is an attempt to allow the user to use one function or subroutine
!  call without needing to change depending on the compiler and/or operating.
!  system. Currently, two compilers are supported: the Intel Fortran
!  (previously, Digital Fortran) compiler and the Lahey compiler.
!
!  Many of the subroutines and functions in this module deal with file
!  manipulations, are used to assist in communicating with the command line,
!  etc..., such as:
!  * Reading arguments from the command line.
!  * Checking, making, deleting, and changing directories.
!  * Retrieving the full path of a file.
!  * Changing the current drive.
!  * Returning the user's login name.
!  * Returning the slash used in paths.
!  * Making calls to the operating system.
!  MODULE ROUTINES
!  ChangeDir
!  ChangeDrive
!  CheckDir
!  DelDir
!  DelFile
!  GetArchitectureType
!  GetArgument
!  GetChar
!i NOTE: Some of the next few lines have an "i" immediately following the
!i exclamation mark. This is because, for now, they are internal functions
!i that should not be picked up by Robodoc. Although they can be called,
!i we are hiding them from the user right now, since they don't work as
!i intended.
!i  GetCompBuildDate
!i  GetCompVerMajor
!i  GetCompVerMinor
!i  GetCompName
!  GetEnviron
!i  GetFullCompVer
!  GetFullPath
!  GetLoginName
!  IsReadOnly
!  MakeDir
!  NumArguments
!  SetEnviron
!  SlashString
!  SystemCall
!  ValidateCreateSubDirs
!*****
#if defined (PLATFORM_LAHEY)
  use service_routines
#elif defined (PLATFORM_DIGITAL_FORTRAN)
  use dflib
#elif defined (PLATFORM_INTEL_FORTRAN)
  use iflport
  use ifport
#endif

  implicit none

  private
  public :: ChangeDir, CheckDir, MakeDir, DelDir, DelFile
  public :: ChangeDrive, GetFullPath
  public :: GetLoginName
  public :: GetEnviron, SetEnviron
  public :: NumArguments, GetArgument
  public :: GetChar
  public :: SlashString
  public :: SystemCall
  public :: IsReadOnly
  public :: GetArchitectureType
  public :: GetCompName
  public :: GetCompVerMajor
  public :: GetCompVerMinor
  public :: GetCompBuildDate
  public :: GetFullCompVer
  public :: ValidateCreateSubDirs

contains

#if defined (PLATFORM_LAHEY)

!******************************************************************************
!****f* PlatformsMod/ChangeDir
!  NAME
!  ChangeDir: This function is used to make the specified directory the current
!  directory.
!  DESCRIPTION
!  This function is used to make the specified directory the current, default
!  directory for the program. This function returns .true. if it was
!  successful, .false. otherwise.
!  RETURNS
!  Logical: True or False returned to show function success.
!  SYNOPSIS

  logical function ChangeDir(dir) result(res)

!  ARGUMENTS
!  * dir: Directory to make the current directory for the program.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: dir
!*****
    integer :: i

    i = ChDir(dir)
    if (i == 0) then
      res = .true.
    else
      res = .false.
    end if

  end function ChangeDir

!******************************************************************************
!****f* PlatformsMod/CheckDir
!  NAME
!  CheckDir: This function is used to check if the specified directory exists.
!  DESCRIPTION
!  This function is used to check if the specified directory exists. This
!  function returns .true. if it does exist, .false. otherwise.
!  RETURNS
!  Logical: True or False returned to show if directory exists.
!  SYNOPSIS

  logical function CheckDir(dir) result(res)

!  ARGUMENTS
!  * dir: Directory to check if exists.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: dir
!*****

    !M.M. here: Not yet implemented for Lahey (not sure how).
    res = .false.

  end function CheckDir

!******************************************************************************
!****f* PlatformsMod/MakeDir
!  NAME
!  MakeDir: This function is used to create the specified directory.
!  DESCRIPTION
!  This function is used to create the specified directory. This function
!  returns .true. if it was successful, .false. otherwise.
!  RETURNS
!  Logical: True or False returned to show function success.
!  SYNOPSIS

  logical function MakeDir(dir) result(res)

!  ARGUMENTS
!  * dir: Directory to be created.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: dir
!*****

    !M.M. here: Not yet implemented for Lahey (not sure how).
    res = .false.

  end function MakeDir

!******************************************************************************
!****f* PlatformsMod/DelDir
!  NAME
!  DelDir: This function is used to delete the specified directory.
!  DESCRIPTION
!  This function is used to delete the specified directory, which must
!  be empty. This function returns .true. if it was successful, .false.
!  otherwise.
!  RETURNS
!  Logical: True or False returned to show function success.
!  SYNOPSIS

  logical function DelDir(dir) result(res)
!  ARGUMENTS
!  * dir: Directory to be deleted.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: dir
!*****

    !M.M. here: Not yet implemented for Lahey (not sure how).
    res = .false.

  end function DelDir

!******************************************************************************
!****f* PlatformsMod/DelFile
!  NAME
!  DelFile: This function is used to delete the specified file.
!  DESCRIPTION
!  This function is used to delete the specified file. This function
!  returns .true. if it was successful, .false. otherwise.
!  RETURNS
!  Logical: True or False returned to show function success.
!  SYNOPSIS

  logical function DelFile(file) result(res)

!  ARGUMENTS
!  * file: File to be deleted.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: file
!*****

    !M.M. here: Not yet implemented for Lahey (not sure how).
    res = .false.

  end function DelFile

!******************************************************************************
!****f* PlatformsMod/ChangeDrive
!  NAME
!  ChangeDrive: This function is used to make the specified drive the current
!  drive.
!  DESCRIPTION
!  This function is used to make the specified drive the current, default
!  drive for the program. This function returns .true. if it was
!  successful, .false. otherwise.
!  RETURNS
!  Logical: True or False returned to show function success.
!  SYNOPSIS

  logical function ChangeDrive(drive) result(res)

!  ARGUMENTS
!  * drive: Drive to make the current drive for the program.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: drive
!*****

    write(6, '()') "Error. Can't call ChangeDrive for the Lahey compiler."
    stop
    res = .false.

  end function ChangeDrive

!******************************************************************************
!****f* PlatformsMod/GetFullPath
!  NAME
!  GetFullPath: This function loads the full path of 'fileName' into
!  the 'path' variable.
!  DESCRIPTION
!  This function loads the full path of the file 'fileName' into the 'path'
!  variable. This function returns a non-zero value if it was successful,
!  and 0 if it fails.
!  RETURNS
!  Integer: Returns non-zero value if successful and zero if fails.
!  SYNOPSIS

  integer function GetFullPath(fileName, path) result(res)

!  ARGUMENTS
!  * fileName: File name.
!  * path: Full path of fileName.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: fileName
    character (len = *), intent(in) :: path
!*****

    ! Since Lahey doesn't have a command for getting the path, just set the
    ! path to the name of the file.
    path = fileName
    res = len_trim(path)

  end function GetFullPath

!******************************************************************************
!****f* PlatformsMod/GetLoginName
!  NAME
!  GetLoginName: This subroutine will fill 'loginName' with the user's login
!  name.
!  DESCRIPTION
!  This subroutine will fill 'loginName' with the user's login name. It will
!  fill it with all blanks if the subroutine fails.
!  SYNOPSIS

  subroutine GetLoginName(loginName)

!  ARGUMENTS
!  * loginName: User's login name.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(out) :: loginName
!*****

    call GetLog(loginName)

  end subroutine GetLoginName

!******************************************************************************
!****f* PlatformsMod/GetEnviron
!  NAME
!  GetEnviron: This function looks through all the environment variables and
!  finds 'varName'. It loads the environment variable into 'val'.
!  DESCRIPTION
!  This function looks through all the environment variables and finds the
!  one (if it exists) that is 'varName'. It loads the value of that
!  environment variable into 'val'. If 'varName' is not a defined
!  environment variable then the function returns 0, otherwise it returns
!  a positive number.
!  RETURNS
!  Integer: 0 is returned if 'varNam' is not defined, otherwise a positive
!  number is returned.
!  SYNOPSIS

  integer function GetEnviron(varName, val) result(res)

!  ARGUMENTS
!  * varName: Variable name.
!  * val: Environment variable.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: varName
    character (len = *), intent(out) :: val
!*****

    call GetEnv(varName, val)
    res = len_trim(val)

  end function GetEnviron

!******************************************************************************
!****f* PlatformsMod/SetEnviron
!  NAME
!  SetEnviron: This function sets an evironment variable.
!  DESCRIPTION
!  This function sets an evironment variable. The environment variable and
!  its value are both in the string 'varNameAndVal'. The variable name
!  is first, followed by an equal sign, followed by the value. The return
!  value is .true. if it was successful, .false. otherwise.
!  RETURNS
!  Logical: True or False returned to show function success.
!  SYNOPSIS

  logical function SetEnviron(varNameAndVal) result(res)

!  ARGUMENTS
!  * varNameAndVal: Environment variable and its value.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: varNameAndVal
!*****

    write(6, '()') "Error. Can't call SetEnviron for the Lahey compiler."
    stop
    res = .false.

  end function SetEnviron

!******************************************************************************
!****f* PlatformsMod/NumArguments
!  NAME
!  NumArguments: This function returns the number of arguments used to start
!  the program.
!  DESCRIPTION
!  This function returns the number of arguments used to start the program.
!  This count includes the program name itself.
!  RETURNS
!  Integer: Number of arguments.
!  SYNOPSIS

  integer function NumArguments() result(res)

!  ARGUMENTS
!  [None]
!*****

    ! Returns the number of arguments including the command itself.
    res = Nargs()

  end function NumArguments

!******************************************************************************
    ! Gets the argument where the command itself is argument zero.
!****f* PlatformsMod/GetArgument
!  NAME
!  GetArgument: This subroutine is used to retrieve the command-line arguments.
!  DESCRIPTION
!  This subroutine is used to retrieve the command-line arguments. The first
!  argument, retrieved when 'argNumber' is 0, is the command itself. The rest
!  of the arguments are retrieved using values for 'argNumber' of 1 or more.
!  If this subroutine fails, then stat (if passed in) will be given a value
!  of -1, otherwise it will be given any other value.
!  SYNOPSIS

  subroutine GetArgument(argNumber, argument, stat)

!  ARGUMENTS
!  * argNumber: Number of arguments.
!  * argument: Argument retrieved from the command line.
!  * stat: (optional) If fails the value is -1, otherwise given any other value.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: argNumber
    character (len = *), intent(out) :: argument
    integer, optional, intent(in) :: stat
!*****

    if (argNumber < 0 .or. argNumber > NumArguments()) then
      stat = -1
      argument = ""
    else
      stat = 1
      call GetArg(argNumber, argument)
    end if

  end subroutine GetArgument

!******************************************************************************
!****f* PlatformsMod/GetChar
!  NAME
!  GetChar: This function will immediately return the value of the first
!  key entered without hitting 'Enter'.
!  DESCRIPTION
!  This function will cause the program to wait for user input and will
!  immediately return the value of the first key the user entered, without
!  waiting for the user to hit 'Enter'.
!  RETURNS
!  Character string (of length 1): Value entered.
!  SYNOPSIS

  character (len = 1) function GetChar() result(res)

!  ARGUMENTS
!  [None]
!*****

    integer :: i

    i = Getc(res)

    if (i /= 0) then
      write(6, '()') "Error in GetChar() for the Lahey compiler."
      stop
    end if

  end function GetChar

!******************************************************************************
!****f* PlatformsMod/SlashString
!  NAME
!  SlashString: This function returns the type of slash used in paths.
!  DESCRIPTION
!  This function returns a single character which is a slash. It is the type
!  of slash used in paths depending on the operating system. For Windows,
!  it returns a back-slash, but for Linux it returns a forward-slash. This
!  function allows path names to be constructed without having to worry
!  about what type of slash the operating system is expecting.
!  RETURNS
!  Character string (of length 1): Type of slash used in paths.
!  SYNOPSIS

  character (len = 1) function SlashString() result(res)

!  ARGUMENTS
!  [None]
!*****

! Check if operarting system is \...
#if defined (_WIN32)
    res = "\"
! If not WIN32...
#else
! ...try Linux...
#if defined (LINUX)
      res = "/"
! ...otherwise, not supported.
#else
      write(6, '(A)') "Error. SlashString() unsupported!"
      stop
#endif
#endif

  end function SlashString

!******************************************************************************
!****f* PlatformsMod/SystemCall
!  NAME
!  SystemCall: This function makes a call to the operating system.
!  DESCRIPTION
!  This function makes a call to the operating system. It uses the command
!  in 'str'. It returns the value returned by the process called.
!  RETURNS
!  Integer: Value returned by the process called.
!  SYNOPSIS

  integer function SystemCall(str) result(res)

!  ARGUMENTS
!  * str: Operating system command.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: str
!*****

    res = System(str)

  end function SystemCall

!******************************************************************************
!****f* PlatformsMod/IsReadOnly
!  NAME
!  IsReadOnly: This function is used to check if a file is read-only.
!  DESCRIPTION
!  This function checks if the file specified in 'fileName' is read-only. It
!  returns .true. if it is, .false. otherwise. If the file doesn't exist, the
!  function returns .false.
!  RETURNS
!  Logical: True returned if file is read-only, false otherwise.
!  SYNOPSIS

  logical function IsReadOnly(fileName) result(res)

!  ARGUMENTS
!  * fileName: File name checked if read-only.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: fileName
!*****

    write(6, '()') "Error. Can't call IsReadOnly for the Lahey compiler."
    stop
    res = .false.

  end function IsReadOnly

!******************************************************************************
! NOTE: For now, the next tag is an "if" tag instead of just an "f". This is
! to let Robodoc know that it is an internal function, so that Robodoc won't
! generate documentation for it. As of now, we'll leave it undocumented
! since it's not working properly. Now, it just returns the value based on the
! compiler that was used to create the OFL, not based on the compiler that
! is coimpiling that program that is using the OFL.
!****if* PlatformsMod/GetCompName
!  NAME
!  GetCompName: This function returns the name of the compiler.
!  DESCRIPTION
!  This function returns a character string containing the name of the compiler.
!  RETURNS
!  Character string (of length 10000): Name of the compiler.
!  SYNOPSIS

  character (len = 10000) function GetCompName() result(rv)

!  ARGUMENTS
!  [None]
!*****

    rv = "Lahey/Gnu"

  end function GetCompName

!******************************************************************************
! NOTE: For now, the next tag is an "if" tag instead of just an "f". This is
! to let Robodoc know that it is an internal function, so that Robodoc won't
! generate documentation for it. As of now, we'll leave it undocumented
! since it's not working properly. Now, it just returns the value based on the
! compiler that was used to create the OFL, not based on the compiler that
! is coimpiling that program that is using the OFL.
!****if* PlatformsMod/GetCompVerMajor
!  NAME
!  GetCompVerMajor: This function returns the "major" version number of the
!  compiler.
!  DESCRIPTION
!  This function returns the "major" version number of the compiler as an
!  integer.
!  RETURNS
!  Integer: "Major" version number of the compiler.
!  SYNOPSIS

  integer function GetCompVerMajor() result(rv)

!  ARGUMENTS
!  [None]
!*****

    rv = __GNUC__

  end function GetCompVerMajor

!******************************************************************************
! NOTE: For now, the next tag is an "if" tag instead of just an "f". This is
! to let Robodoc know that it is an internal function, so that Robodoc won't
! generate documentation for it. As of now, we'll leave it undocumented
! since it's not working properly. Now, it just returns the value based on the
! compiler that was used to create the OFL, not based on the compiler that
! is coimpiling that program that is using the OFL.
!****if* PlatformsMod/GetCompVerMinor
!  NAME
!  GetCompVerMinor: This function returns the "minor" version number of the
!  compiler.
!  DESCRIPTION
!  This function returns the "minor" version number of the compiler as an
!  integer.
!  RETURNS
!  Integer: "Minor" version number of the compiler.
!  SYNOPSIS

  integer function GetCompVerMinor() result(rv)

!  ARGUMENTS
!  [None]
!*****

    rv = __GNUC_MINOR__
    ! There is also some called the patch level, defined with
    ! __GNUC_PATCHLEVEL__. Currently, we're not creating a function to
    ! retrieve it, as we don't have a corresponding function for the Intel
    ! compiler. Hopefully, no big deal.

  end function GetCompVerMinor

!******************************************************************************
! NOTE: For now, the next tag is an "if" tag instead of just an "f". This is
! to let Robodoc know that it is an internal function, so that Robodoc won't
! generate documentation for it. As of now, we'll leave it undocumented
! since it's not working properly. Now, it just returns the value based on the
! compiler that was used to create the OFL, not based on the compiler that
! is coimpiling that program that is using the OFL.
!****if* PlatformsMod/GetCompBuildDate
!  NAME
!  GetCompBuildDate: This function returns the build date of the compiler.
!  DESCRIPTION
!  This function returns the build date of the compiler as an integer.
!  RETURNS
!  Integer: Build date of the compiler.
!  SYNOPSIS

  integer function GetCompBuildDate() result(rv)

!  ARGUMENTS
!  [None]
!*****

    rv = 0

  end function GetCompBuildDate

!******************************************************************************
!****f* PlatformsMod/GetArchitectureType
!  NAME
!  GetArchitectureType: This function returns the architecture type used for
!  compilation.
!  DESCRIPTION
!  This function returns a character string containing the architecture type,
!  or platform, used for compilation (e.g., "32-bit", "64-bit").
!  RETURNS
!  Character string (of length 100): Architecture type used for compilation.
!  SYNOPSIS

  character (len = 100) function GetArchitectureType() result(rv)

    ! Intitialize it as "UNDEFINED" in case none of the macros lead to a
    ! resolution as to which architecture we're compiling with.
    rv = "UNDEFINED"

    ! Note: I don't think these will work for Lahey. But just use them here
    ! as a placeholder for now.
#if defined (_M_IX86)
    rv = "32-bit"
#endif    

#if defined (_M_X64)
    rv = "64-bit"
#endif

  end function GetArchitectureType

!******************************************************************************
  ! End of PLATFORM_LAHEY, now check for other platforms.
#elif defined (PLATFORM_DIGITAL_FORTRAN) || defined (PLATFORM_INTEL_FORTRAN)

!******************************************************************************

  logical function ChangeDir(dir) result(res)

    character (len = *), intent(in) :: dir

    res = ChangeDirQQ(dir)

  end function ChangeDir

!******************************************************************************

  logical function CheckDir(dir) result(res)

    character (len = *), intent(in) :: dir

#ifndef __INTEL_COMPILER
    ! Purposely cause a compiler error here.
    Error: Macro __INTEL_COMPILER should be defined in this scenario!
#elif (__INTEL_COMPILER >= 800 && __INTEL_COMPILER <= 810)
    inquire(file = dir // "\nul", exist = res)
#elif (__INTEL_COMPILER > 810)
    inquire(directory = trim(dir), exist = res)
#else
    ! Purposely cause a compiler error here.
    Error: Macro __INTEL_COMPILER should be at least 800!
#endif

  end function CheckDir

!******************************************************************************

  logical function MakeDir(dir) result(res)

    character (len = *), intent(in) :: dir

    res = MakeDirQQ(dir)

  end function MakeDir

!******************************************************************************

  logical function DelDir(dir) result(res)

    character (len = *), intent(in) :: dir

    res = DelDirQQ(dir)

  end function DelDir

!******************************************************************************

  logical function DelFile(file) result(res)

    character (len = *), intent(in) :: file

    ! Make sure to clear any read-only attribute that may be set, otherwise
    ! the file won't delete. The 'attrib' command needs to have quotes
    ! surrounding the file name, in case it contains spaces. (The "unlink"
    ! routine doesn't care about spaces in the file name.)
    res = system("attrib -R " // '"' // trim(file) // '"' // " > nul")
    res = (Unlink(file) == 0)

  end function DelFile

!******************************************************************************

  logical function ChangeDrive(drive) result(res)

    character (len = *), intent(in) :: drive

    res = ChangeDriveQQ(drive)

  end function ChangeDrive

!******************************************************************************
  integer function GetFullPath(fileName, path) result(res)

    character (len = *), intent(in) :: fileName, path

    res = FullPathQQ(fileName, path)

  end function GetFullPath

!******************************************************************************

  subroutine GetLoginName(loginName)

    character (len = *), intent(out) :: loginName

    call GetLog(loginName)

  end subroutine GetLoginName

!******************************************************************************

  integer function GetEnviron(varName, val) result(res)

    character (len = *), intent(in) :: varName
    character (len = *), intent(out) :: val

    res = GetEnvQQ(varName, val)

  end function GetEnviron

!******************************************************************************

  logical function SetEnviron(varNameAndVal) result(res)

    character (len = *), intent(in) :: varNameAndVal

    res = SetEnvQQ(varNameAndVal)

  end function SetEnviron

!******************************************************************************

  integer function NumArguments() result(res)

    ! Returns the number of arguments including the command itself.
    res = Nargs()

  end function NumArguments

!******************************************************************************

  subroutine GetArgument(argNumber, argument, stat)

    ! Gets the argument where the command itself is argument zero.
    integer, intent(in) :: argNumber
    character (len = *), intent(out) :: argument
    integer, optional, intent(in) :: stat
    integer(2) :: argNumber_2, stat_2

    argNumber_2 = argNumber
    if (present(stat)) then
      stat_2 = stat
      call GetArg(argNumber_2, argument, stat_2)
    else
      call GetArg(argNumber_2, argument)
    end if

  end subroutine GetArgument

!******************************************************************************

  character (len = 1) function GetChar() result(res)

    res = GetCharQQ()

  end function GetChar

!******************************************************************************

  character (len = 1) function SlashString() result(res)

! Check if operating system is WIN32...
#if defined (_WIN32)
    res = "\"
! If not WIN32, try Linux...
#elif defined (LINUX)
    res = "/"
! ...otherwise, not supported.
#else
    write(6, '(A)') "Error. SlashString() unsupported!"
    stop
#endif

  end function SlashString

!******************************************************************************

  integer function SystemCall(str) result(res)

    character (len = *), intent(in) :: str
    logical :: success

    ! Using SystemQQ had the problem that it only returns a logical (i.e,
    ! .true. or .false.), but not the actual return value from the proceess
    ! itself. Using system instead solves this because it actually returns
    ! the value returned by the process that was called.

    !success = SystemQQ(str)
    !if (success) then
    !  res = 0
    !else
    !  res = -1
    !end if
    res = system(str)

  end function SystemCall

!******************************************************************************

  logical function IsReadOnly(fileName) result(res)

    character (len = *), intent(in) :: fileName

    logical :: doesExist
    integer :: rv
    type(FILE$INFO) :: fileInfo
    integer(KIND=INT_PTR_KIND()) :: handle

    inquire(file = trim(fileName), exist = doesExist)
    if (doesExist) then
      handle = FILE$FIRST
      rv = GetFileInfoQQ(trim(fileName), fileInfo, handle)
      res = (fileInfo%permit .and. FILE$READONLY) /= 0
    else
      res = .false.
    end if

  end function IsReadOnly

!******************************************************************************

  character (len = 10000) function GetCompName() result(rv)

    rv = "Intel"

  end function GetCompName

!******************************************************************************

  integer function GetCompVerMajor() result(rv)

    rv = __INTEL_COMPILER

  end function GetCompVerMajor

!******************************************************************************

  integer function GetCompVerMinor() result(rv)

    rv = __INTEL_COMPILER_UPDATE

  end function GetCompVerMinor

!******************************************************************************

  integer function GetCompBuildDate() result(rv)

    rv = __INTEL_COMPILER_BUILD_DATE

  end function GetCompBuildDate

!******************************************************************************

  character (len = 100) function GetArchitectureType() result(rv)

    ! Intitialize it as "UNDEFINED" in case none of the macros lead to a
    ! resolution as to which architecture we're compiling with.
    rv = "UNDEFINED"

#if defined (_M_IX86)
    rv = "32-bit"
#endif    

#if defined (_M_X64)
    rv = "64-bit"
#endif

  end function GetArchitectureType

!******************************************************************************

#else
  ! Force a compilation error if no platform was defined.
  This line should give a compilation error.
  You got this error because one of the following symbols is not
  defined and must be:
    PLATFORM_LAHEY
    PLATFORM_INTEL_FORTRAN
    PLATFORM_DIGITAL_FORTRAN
#endif

!******************************************************************************

! This following routine(s) are not themselves compiler specific, but do make
! calls into the compiler-specific routines in this module.

!******************************************************************************
!****f* PlatformsMod/ValidateCreateSubDirs
!  NAME
!  ValidateCreateSubDirs: This subroutine is used to validate that the
!  subdirectories of a specified output directory exist. If they don't, then
!  it creates them.
!  DESCRIPTION
!  This subroutine is used to validate that the subdirectories of a specified
!  output directory exist. If they don't, then it creates them. It can validate
!  (and potentially create) up to 10 subdirectories at a time. If more than
!  that are needed, then multiple calls to this subroutine should be made.
!  If less than that are needed, then only pass in the relevant ones. All
!  arguments after the first 2 are optional. If the main output directory
!  itself does not exist, then it will print an error message and terminate
!  the program.
!  SYNOPSIS

  subroutine ValidateCreateSubDirs(outFolder, subDir1, subDir2, subDir3, &
    subDir4, subDir5, subDir6, subDir7, subDir8, subDir9, subDir10)

!  ARGUMENTS
!  * outFolder: Main out directory.
!  * subDir1: The 1st subdirectory to validate and/or create.
!  * subDir2: (optional) The 2nd subdirectory to validate and/or create.
!  * subDir3: (optional) The 3rd subdirectory to validate and/or create.
!  * subDir4: (optional) The 4th subdirectory to validate and/or create.
!  * subDir5: (optional) The 5th subdirectory to validate and/or create.
!  * subDir6: (optional) The 6th subdirectory to validate and/or create.
!  * subDir7: (optional) The 7th subdirectory to validate and/or create.
!  * subDir8: (optional) The 8th subdirectory to validate and/or create.
!  * subDir9: (optional) The 9th subdirectory to validate and/or create.
!  * subDir10: (optional) The 10th subdirectory to validate and/or create.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: outFolder
    character (len = *), intent(in) :: subDir1
    character (len = *), optional, intent(in) :: subDir2
    character (len = *), optional, intent(in) :: subDir3
    character (len = *), optional, intent(in) :: subDir4
    character (len = *), optional, intent(in) :: subDir5
    character (len = *), optional, intent(in) :: subDir6
    character (len = *), optional, intent(in) :: subDir7
    character (len = *), optional, intent(in) :: subDir8
    character (len = *), optional, intent(in) :: subDir9
    character (len = *), optional, intent(in) :: subDir10
!*****

    integer :: rv

    ! Check to see if the output directory exists.
    if (CheckDir(trim(outFolder)) == .false.) then
      write(6, *) "Error in ValidateCreateSubDirs.  " // &
        trim(outFolder) // " directory does not exist!"
      stop
    end if

    ! Validate/create the subdirectories no need to worry that the
    ! subdirectory may already exists. If it does, then MakeDir just
    ! returns .false., but we are ignoring the return value.

    ! There must be at least one subdirectory!
    rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir1))
    ! Now, we must check if the optional arguments are there. Create the
    ! subdirectory if there is a folder listed.
    if (present(subDir2)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir2))
    if (present(subDir3)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir3))
    if (present(subDir4)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir4))
    if (present(subDir5)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir5))
    if (present(subDir6)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir6))
    if (present(subDir7)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir7))
    if (present(subDir8)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir8))
    if (present(subDir9)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir9))
    if (present(subDir10)) &
      rv = MakeDir(trim(outFolder) // SlashString() // trim(subDir10))

  end subroutine ValidateCreateSubDirs

!******************************************************************************
! NOTE: For now, the next tag is an "if" tag instead of just an "f". This is
! to let Robodoc know that it is an internal function, so that Robodoc won't
! generate documentation for it. As of now, we'll leave it undocumented
! since it's not working properly. Now, it just returns the value based on the
! compiler that was used to create the OFL, not based on the compiler that
! is coimpiling that program that is using the OFL.
!****if* PlatformsMod/GetFullCompVer
!  NAME
!  GetFullCompVer: This function returns the full compiler version information.
!  DESCRIPTION
!  This function returns a character string (of length 100) containing the
!  full compiler version information.
!  RETURNS
!  Character string (of length 100): The full compiler version information.
!  SYNOPSIS

  character (len = 100) function GetFullCompVer() result(rv)

!  ARGUMENTS
!  [None]
!*****

    integer :: integ
    character (len = 100) :: asc

    integ = GetCompVerMajor()
    write(asc, '(I)') integ
    rv = trim(GetCompName()) // " " //adjustl(asc)

    integ = GetCompVerMinor()
    write(asc, '(I)') integ
    rv = trim(rv) // "." // adjustl(asc)

    integ = GetCompBuildDate()
    write(asc, '(I)') integ
    rv = trim(rv) // "." // adjustl(asc)
 
  end function GetFullCompVer

!******************************************************************************

end module PlatformsMod
