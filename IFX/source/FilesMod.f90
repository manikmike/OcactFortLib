! $Id: FilesMod.f90 1.51 2017/05/24 13:03:55EDT 622325 Development  $
module FilesMod

!****h* source/FilesMod
!  NAME
!  FilesMod: OcactFortLib FilesMod module.
!  REVISION
!  $Revision: 1.51 $
!  MODULE DESCRIPTION
!  The FilesMod module contains subroutines and functions used to open, close,
!  and read files. The module also contains subroutines used to set, reset, and
!  unset macros. The macros are used in "files lists", which are read by the
!  OpenFiles (and CloseFiles) subroutines, to give more control in determining
!  exactly which files should be opened (or closed). The GetLine function
!  is used to read an entire line from a file, and is often used in conjuntion
!  with the ParseLine function in the StringsMod.
!  MODULE ROUTINES
!  CheckFileData
!  CloseAllFiles
!  CloseFile
!  CloseFiles
!  FilesModBeginVerbose
!  FilesModEndVerbose
!  FilesModGetMacro
!  FilesModResetMacro
!  FilesModSetMacro
!  FilesModUnsetMacro
!  FlushAllFiles
!  GetFileNumFromName
!  GetLine
!  GetNumOpenFiles
!  LoadOpenFiles
!  OpenFile
!  OpenFiles
!  SkipLines
!*****

  use StringsMod
  use PlatformsMod ! Needed for GetFullPath

  implicit none
  save

  private
  public :: OpenFiles, OpenFile, CloseAllFiles, CloseFiles, CloseFile
  public :: GetLine, SkipLines, CheckFileData
  public :: FilesModBeginVerbose, FilesModEndVerbose
  public :: FilesModSetMacro, FilesModGetMacro
  public :: FilesModResetMacro, FilesModUnsetMacro
  public :: GetFileNumFromName
  public :: GetNumOpenFiles
  public :: FlushAllFiles
  public :: LoadOpenFiles

  ! The maximum number of files this module can handle.
  integer, parameter :: MAX_FILES_MANAGEABLE = 1000
  ! The maximum length for a character string in this module.
  integer, parameter :: MAX_CHAR_LENGTH = 10000
  ! The maximum number of macros that can be defined for a files list
  ! input file.
  integer, parameter :: MAX_MACROS = 50

  ! The number of files this module is currently handling.
  integer :: numFiles = 0
  ! The unit numbers of the files this module is currently handling.
  ! (This also initializes all the values in the array to 0.)
  integer, dimension(MAX_FILES_MANAGEABLE) :: unitNums = 0
  ! The files names based on how they were passed in by the user, either
  ! directly to OpenFile or indirectly through OpenFiles. These may be either
  ! relative paths or full paths, depending on how the user passed them in.
  character (len = MAX_CHAR_LENGTH), dimension(MAX_FILES_MANAGEABLE) :: &
    fileNames
  ! The full path names of the files, irrespective of whether the user
  ! passed them in as relative paths or full paths.
  character (len = MAX_CHAR_LENGTH), dimension(MAX_FILES_MANAGEABLE) :: &
    fullFileNames
  ! This is true if it should output a statement every time a file is opened
  ! or closed, otherwise it's false.
  logical :: verbose = .false.
  ! The file number to use for outputting verbose statements.
  integer :: verboseFileNum = 6
  ! The width of the field for formatted verbose output (0 if unformatted).
  integer :: verboseFieldWidth = 0
  ! Whether or not to output full path names even if only relative paths
  ! were specified when the file was opened.
  logical :: fullPathNames = .false.
  ! The file name to use for outputting verbose statements.
  character (len = MAX_CHAR_LENGTH) :: verboseFileName = ""
  ! The names of the macros that are defined for a files list input file.
  character (len = MAX_CHAR_LENGTH), dimension(MAX_MACROS) :: macroNames
    ! The first way (below) to initialize doesn't seem to work properly,
    ! but the next two do. I'm not sure why the first isn't working.
    ! For now, we won't use any of these, b/c we don't need to worry
    ! about the junk values since we'll always check numMacros before
    ! accessing the array.
    !macroNames = ""
    !macroNames = (/("", i=1,10)/)
    !macroNames = (/ "", "", "", "", "", "", "", "", "", "" /)
  ! The meanings (expansions) of the macros that are defined for a files list
  ! input file.
  character (len = MAX_CHAR_LENGTH), dimension(MAX_MACROS) :: macroExpansions
  ! The number of macros that have been defined my the user.
  integer :: numMacros = 0
  ! In SkipLines, internalCallToGetLine is true otherwise it is false.
  ! When SkipLines calls GetLine, if an error occurs in 'extraCheck', a
  ! descriptive error message will print out identifying if the error occurred
  ! in GetLine or SkipLines.
  logical :: internalCallToGetLine = .false.

  ! Allow LoadOpenFiles to be the generic name for these subroutines.
!****f* FilesMod/LoadOpenFiles
!  NAME
!  LoadOpenFiles: This subroutine is used to load the numbers or names of
!  files that are currently open.
!  DESCRIPTION
!  This subroutine is used to load the numbers or names of files that are
!  currently open. The first argument is either an array of integers or an
!  array of character strings. If integers, then file numbers are loaded.
!  If character strings, then file names are loaded. The second argument
!  returns the numbers of files that were loaded into the first argument's
!  array. This will typically be however many files are open. However, if the
!  first argument's array is not large enough, then a value of -1 is returned.
!  SYNOPSIS
!  subroutine LoadOpenFiles(fileNums, numFilesLoaded)
!  subroutine LoadOpenFiles(fileNames, numFilesLoaded)
!  ARGUMENTS
!  * fileNums: 1-dimensional array (of any size) of integers.
!  * fileNames: 1-dimensional array (of any size) of character strings.
!  * numFilesLoaded: The number of files (either numbers or names) that loaded
!    into the 'fileNums' or 'fileNames' array.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface LoadOpenFiles
    module procedure LoadOpenFilesNums, LoadOpenFilesNames
  end interface LoadOpenFiles

  contains

!******************************************************************************
!****f* FilesMod/OpenFiles
!  NAME
!  OpenFiles: This subroutine is used to open files.
!  DESCRIPTION
!  This subroutine is used to open files in a structured and convenient
!  manner. The list of files to be opened should be in a text file,
!  with the name of the text file given by 'mainFile'. The text file
!  can have any number of blank lines or comment lines throughout. Comments
!  can also be put at the end of a line. (Comments begin with an '!'.)
!  Each non-blank and non-comment line should have information about one
!  file to open.
!
!  For each file to be opened, there should either be 3 pieces of information
!  or 6. These pieces of information are used to open the file exactly
!  as described in the documentation for 'OpenFile', with one exception:
!  File number 99 should not be used for opening a file. However, using
!  'OpenFile' directly does not have this limitation.
!
!  If the optional argument, 'openFilesAction', is passed in, it should be set
!  to "read", "write", or "readwrite". This specifies the action (or mode) in
!  which the files are opened. If this argument is not passed in, it defaults
!  to "readwrite". However, if a file is read-only, then it will deafult to
!  "read".
!
!  See the documentation for 'FilesModSetMacro' for instructions on how to use
!  macros in the 'mainFile'.
!  SYNOPSIS

  subroutine OpenFiles(mainFile, openFilesAction)

!  ARGUMENTS
!  * mainFile: Text file containing the list of files to be opened.
!  * openFilesAction: (optional) Action in which the files are opened ('read',
!    'write', 'readwrite').
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: mainFile
    character(len = *), optional, intent(in)  :: openFilesAction
!*****
    integer :: fileNum, fileRecl, errCheck, lineNum, commentInd
    character (len = MAX_CHAR_LENGTH) :: fileStuff, fileName, &
      fileStatus, fileForm, fileAccess, fileAction


    ! Open mainFile which contains a list of all files to be used.
    open(99, file = mainFile, status = 'old', action = "read", &
      iostat = errCheck)
    if (errCheck /= 0) then
      write(6, '(2A, /, 2A)') "Error! OpenFiles() can not open the file ", &
        "containing the list of files to be opened.", "File name: ", mainFile
      stop
    end if

    lineNum = 0
    ! Endless loop to keep reading and opening files until end-of-file
    ! is reached.
    do
      fileStuff = GetLine(99, .false.)
      lineNum = lineNum + 1
      ! Check for end-of-file
      if(trim(fileStuff) == "") then
        close(99)
        exit
      end if

      ! First strip off any comments (from an exclamation point till the
      ! end of the line).
      commentInd = index(fileStuff, "!")
      if (commentInd > 0) then
        fileStuff = fileStuff(1:commentInd - 1)
      end if
      ! If the remainder of the line is all white space, then skip it.
      if (trim(fileStuff) == "") cycle

      ! Break 'fileStuff' into its components.
      fileNum = AscToInt(ParseLine(fileStuff))
      ! (Expand any macros in the file name.)
      fileName = trim(ExpandMacros(ParseLine(fileStuff)))
      fileStatus = ParseLine(fileStuff)
      fileForm = ParseLine(fileStuff)
      fileAccess = ParseLine(fileStuff)
      ! Before calling AscToInt, we must make sure that there is something
      ! in filestuff to read, otherwise it will give an error message.
      if (trim(fileStuff) == "") then
        fileRecl = 0
      else
        fileRecl = AscToInt(fileStuff)
      end if
      ! If an "openFilesAction" was passed in (either "read", "write", or
      ! "readwrite"), then use it, else default to "readwrite".  However,
      ! if a file is read-only, then deafult to "read".
      if (present(openFilesAction)) then
        fileAction = openFilesAction
      else if (IsReadOnly(trim(fileName))) then
        fileAction = "read"
      else
        fileAction = "readwrite"
      end if

      ! Since file number 99 is used by this subroutine, then it can not
      ! open a file with the number 99.
      if(fileNum == 99) then
        write(6, '(2A, A, I5, /, 2A)') "Error in syntax of file ", mainFile, &
             " in line number ", lineNum, "Files with number 99 ", &
             "can not be opened using OpenFiles()"
        stop
      end if

      ! Open file. (If the name begins with "NULL", then don't open it.)
      if (fileName(1:len("NULL")) == "NULL") cycle
      if (trim(fileForm) == "") then
        call OpenFile(fileNum, fileName, fileStatus, &
          openFileAction = fileAction)
      else
        call OpenFile(fileNum, fileName, fileStatus, fileForm, fileAccess, &
          fileRecl, openFileAction = fileAction)
      end if
    end do

  end subroutine OpenFiles
!******************************************************************************
!****f* FilesMod/OpenFile
!  NAME
!  OpenFile: This subroutine opens one file at a time.
!  DESCRIPTION
!  The first argument, 'fileNum', is the number which will be. If a
!  number is specified that was already opened by this module (either with
!  OpenFiles() or with OpenFile()), then the subroutine will print an error
!  message and terminate the program.
!
!  The second argument, 'fileName', should be the name of the file to be opened
!  (including the path, if needed).
!
!  The third argument, 'fileStatus', should be the status of the file to be
!  opened (i.e. 'old', 'new', 'replace', 'scratch', or 'unknown').
!
!  The fourth through sixth arguments are optional, but if one is present
!  then all must be present. The fourth argument, 'fileForm', is the form
!  that the file should be opened in (i.e. 'formatted' or 'unformatted').
!  The fifth argument, 'fileAccess' is the access (i.e. 'direct' or
!  'sequential'). The sixth argument is the record length. The seventh
!  argument specifies the action (or mode) to be used to open the file.
!  It should be "read", "write", or "readwrite". If not specifed, it
!  will default to "readwrite". However, if the file is read-only, then
!  it will deafult to "read".
!  SYNOPSIS

  subroutine OpenFile(fileNum, fileName, fileStatus, fileForm, fileAccess, &
    fileRecl, openFileAction)

!  ARGUMENTS
!  * fileNum: File number to be opened.
!  * fileName: Name of the file to be opened.
!  * fileStatus: Status of the file to be opened ('old', 'new', 'replace',
!    'scratch', 'unknown').
!  * fileForm: (optional) Form that the file should be opened in ('formatted'
!    or 'unformatted').
!  * fileAccess: (optional) File access ('direct' or 'sequential').
!  * fileRecl: (optional) Record length.
!  * openFileAction: (optional) Action to be used to open the file ('read',
!    'write', 'readwrite').
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: fileNum
    character (len = *), intent(in) :: fileName
    character (len = *), intent(in) :: fileStatus
    character (len = *), optional, intent(in) :: fileForm
    character (len = *), optional, intent(in) :: fileAccess
    integer, optional, intent(in) :: fileRecl
    character (len = *), optional, intent(in) :: openFileAction
!*****
    integer :: errCheck
    character (len = MAX_CHAR_LENGTH) :: fileAction, pos, fileStatusActual
    character (len = MAX_CHAR_LENGTH) :: field1, field2, field3
    logical :: doesExist

    ! First make sure that the fileNum is available.
    if ( any(unitNums(1:numFiles) == fileNum) ) then
      write(6, '(A, I, A)') "Error in OpenFile. Attempting to open fileNum ", &
        fileNum, ", but that fileNum is already being used for an open file."
      stop
    end if

    if (trim(fileStatus) == "append") then
      fileStatusActual = "unknown"
      pos = "append"
    else
      fileStatusActual = trim(fileStatus)
      pos = "asis"
    end if

    ! If an "openFileAction" was passed in (either "read", "write", or
    ! "readwrite"), then use it, else default to "readwrite".  However,
    ! if the file is read-only, then deafult to "read".
    if (present(openFileAction)) then
      fileAction = openFileAction
    else if (IsReadOnly(trim(fileName))) then
      fileAction = "read"
    else
      fileAction = "readwrite"
    end if

    if (present(fileForm)) then
      ! If 1 of the optional arguments are passed in, we require all 3 to
      ! be passed in.
      if (present(fileAccess) .and. present(fileRecl)) then
        ! The "position" specifier is only valid with files connected for
        ! sequential access.
        if (fileAccess == "sequential") then
          open(fileNum, file = fileName, status = fileStatusActual, &
               iostat = errCheck, form = fileForm, access = fileAccess, &
               recl = fileRecl, action = fileAction, position = pos)
        else
          open(fileNum, file = fileName, status = fileStatusActual, &
               iostat = errCheck, form = fileForm, access = fileAccess, &
               recl = fileRecl, action = fileAction)
        end if
      else
        write(6, '(3A, /, A)') "Error in OpenFile() opening file ", fileName, &
          ".", "If passing any of the 3 optional arguments, must pass all 3."
        stop
      end if
    else
      ! If no optional arguments are passed in, perform the simple open file.
      open(fileNum, file = fileName, status = fileStatusActual, &
        iostat = errCheck, action = fileAction, position = pos)
    end if

    ! Temporary check in place. See comments for the OpenFileTemp function.
    if (errCheck /= 0) then
      errCheck = OpenFileTemp(fileNum, fileName, fileStatusActual, pos, &
        fileForm, fileAccess, fileRecl)
    end if
    ! Check for errors.
    if (errCheck /= 0) then
      write(6, '(A)') "Error in OpenFile(). Could not open file with:"
      write(6, '(A)') "  fileNum: " // trim(IntToAsc(fileNum))
      write(6, '(A)') "  fileName: " // trim(fileName)
      write(6, '(A)') "  fileStatus: " // trim(fileStatus)
      if (trim(fileStatus) == "append") then
        write(6, '(A)') "  fileStatusActual: " // trim(fileStatusActual)
      end if
      ! Note that we've already checked above to make sure that if fileForm
      ! is present, then fileAccess and fileRecl are also present.
      if (present(fileForm)) then
        write(6, '(A)') "  fileForm: " // trim(fileForm)
        write(6, '(A)') "  fileAccess: " // trim(fileAccess)
        write(6, '(A)') "  fileRecl: " // trim(IntToAsc(fileRecl))
      end if
      if (present(openFileAction)) then
        write(6, '(A)') "  openFileAction: " // trim(openFileAction)
      end if
      inquire(file = trim(fileName), exist = doesExist)
      if (.not. doesExist) then
        call ValidatePath(fileName)
        ! If ValidatePath did not stop the program, then the path must exist
        ! and it's the file itself that does not exist.
        write(6, '(A)') "The file does not exist: " // trim(fileName)
        stop
      end if
      stop
    end if

    ! Update the module-level variables.
    numFiles = numFiles + 1
    unitNums(numFiles) = fileNum
    fileNames(numFiles) = trim(fileName)
    errCheck = GetFullPath(fileName, fullFileNames(numFiles))

    if (verbose) then
      call WriteVerboseMessage("Opened", fileNum)
    end if

  end subroutine OpenFile
!******************************************************************************
! This is a temporary function (hopefully). It is being added to take care of
! certain cases where the file is not being opened, but it really can be.
! Specififcally: If a fileAction was not passed in to OpenFile (or OpenFiles),
! then it defaults to try to use "readwrite".  If the file is read-only, then
! it changes to use "read". However, if the file itself is not read-only, but
! appears as read-only because the user doesn't have write permission to the
! file, then it still tries to use "readwrite", which then failes. This
! function is a temporary hack to try to alleviate that problem and allow
! the file to be opened in "read" mode.
  integer function OpenFileTemp(fileNum, fileName, fileStatus, pos, fileForm, &
    fileAccess, fileRecl) result(rv)

    integer, intent(in) :: fileNum
    character (len = *), intent(in) :: fileName
    character (len = *), intent(in) :: fileStatus
    character (len = *), intent(in) :: pos
    character (len = *), optional, intent(in) :: fileForm
    character (len = *), optional, intent(in) :: fileAccess
    integer, optional, intent(in) :: fileRecl

    integer :: errCheck

    if (present(fileForm)) then
      ! If 1 of the optional arguments are passed in, we require all 3 to
      ! be passed in.
      if (present(fileAccess) .and. present(fileRecl)) then
        ! The "position" specifier is only valid with files connected for
        ! sequential access.
        if (fileAccess == "sequential") then
          open(fileNum, file = fileName, status = fileStatus, &
               iostat = errCheck, form = fileForm, access = fileAccess, &
               recl = fileRecl, action = "read", position = pos)
        else
          open(fileNum, file = fileName, status = fileStatus, &
               iostat = errCheck, form = fileForm, access = fileAccess, &
               recl = fileRecl, action = "read")
        end if
      else
        write(6, '(3A, /, A)') "Error in OpenFile() opening file ", fileName, &
          ".", "If passing any of the 3 optional arguments, must pass all 3."
        stop
      end if
    else
      ! If no optional arguments are passed in, perform the simple open file.
      open(fileNum, file = fileName, status = fileStatus, iostat = errCheck, &
        action = "read", position = pos)
    end if

    rv = errCheck

  end function OpenFileTemp
!******************************************************************************
!****f* FilesMod/CloseAllFiles
!  NAME
!  CloseAllFiles: This subroutine closes all files that have been opened.
!  DESCRIPTION
!  This subroutine closes all files that have been opened with either
!  'OpenFiles' or 'OpenFile', and have not yet been closed (with
!  'CloseAllFiles', 'CloseFiles', or CloseFile').
!  SYNOPSIS

  subroutine CloseAllFiles()

!  ARGUMENTS
!  [None]
!*****

    integer :: i

    ! Close the files in reverse order because the CloseFile() subroutine
    ! adjusts the values of unitNums(i) for all i greater than the current i.
    do i = numFiles, 1, -1
      call CloseFile(unitNums(i))
    end do

  end subroutine CloseAllFiles
!******************************************************************************
!****f* FilesMod/CloseFiles
!  NAME
!  CloseFiles: This subroutine is used to close all file numbers listed in a
!  text file.
!  DESCRIPTION
!  This subroutine is used to close all file numbers listed in a text file, with
!  the name of the text file given by 'mainFile'. The text file can have any
!  number of blank lines or comment lines throughout. (Comment lines begin with
!  an '!'.) The file name given by 'mainFile' will usually be the same file name
!  used with a previous call to'OpenFiles'.
!  SYNOPSIS

  subroutine CloseFiles(mainFile)

!  ARGUMENTS
!  * mainFile: Name of the text file.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: mainFile
!*****
    character (len = MAX_CHAR_LENGTH) :: fileStuff, fileName
    integer :: fileNum, errCheck, lineNum, commentInd

    ! Open mainFile which contains a list of all files to be used.
    open(99, file = mainFile, status = 'old', action = "read", &
      iostat = errCheck)
    if (errCheck /= 0) then
      write(6, '(2A, /, 2A)') "Error! CloseFiles() can not open the file ", &
        "containing the list of files to be closed.", "File name: ", mainFile
      stop
    end if

    lineNum = 0
    ! Endless loop to keep reading and opening files until end-of-file
    ! is reached.
    do
      fileStuff = GetLine(99, .false.)
      lineNum = lineNum + 1
      ! Check for end-of-file
      if (trim(fileStuff) == "") then
        close(99)
        exit
      end if

      ! First strip off any comments (from an exclamation point till the
      ! end of the line).
      commentInd = index(fileStuff, "!")
      if (commentInd > 0) then
        fileStuff = fileStuff(1:commentInd - 1)
      end if
      ! If the remainder of the line is all white space, then skip it.
      if (trim(fileStuff) == "") cycle

      ! Read the fileNum, which is the first thing on each line.
      fileNum = AscToInt(ParseLine(fileStuff))
      ! Also, we need the name, to know not to close the file if it begins
      ! with NULL.
      fileName = trim(ExpandMacros(ParseLine(fileStuff)))

      ! Since file number 99 is used by this subroutine, then there can not
      ! be an open a file with the number 99.
      if(fileNum == 99) then
        write(6, '(2A, A, I5, /, 2A)') "Error in syntax of file ", mainFile, &
             " in line number ", lineNum, "Files with number 99 ", &
             "can not be closed using CloseFiles()"
        stop
      end if

      ! Close file. (If the name begins with "NULL", then don't close it.)
      if (fileName(1:len("NULL")) == "NULL") cycle
      call CloseFile(fileNum)
    end do
    

  end subroutine CloseFiles
!******************************************************************************
!****f* FilesMod/CloseFile
!  NAME
!  CloseFile: This subroutine closes the file associated with the number 
!  'unitNum'.
!  DESCRIPTION
!  This subroutine closes the file associated with the number 'unitNum'.
!  SYNOPSIS

  subroutine CloseFile(unitNum)

!  ARGUMENTS
!  * unitNum: Unit number of the file.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: unitNum
!*****
    integer :: errCheck, i
    logical :: dropOneSlot
    character (len = MAX_CHAR_LENGTH) :: field1, field2, field3

    ! First make sure that the fileNum has been opened (with this module).
    if ( all(unitNums(1:numFiles) /= unitNum) ) then
      write(6, '(2A, I, 2A)') "Error in CloseFile. Attempting to close ", &
        "unitNum ", unitNum, ", but that unitNum has not been opened with ", &
        "the FilesMod."
      stop
    end if

    if (verbose) then
      call WriteVerboseMessage("Closed", unitNum)
    end if

    close(unit = unitNum, iostat = errCheck)
    if (errCheck /= 0) then
      write(6, '(A, I6)') "Error in CloseFile() while closing unit:", &
        unitNum
      stop
    end if

    ! Update the module-level variables.
    dropOneSlot = .false.
    do i = 1, numFiles
      if (unitNums(i) == unitNum) then
        dropOneSlot = .true.
      end if
      if (dropOneSlot) then
        unitNums(i) = unitNums(i + 1)
        fileNames(i) = fileNames(i + 1)
        fullFileNames(i) = fullFileNames(i + 1)
      end if
    end do
    ! dropOneSlot will be set to true if the unit number was found, otherwise
    ! it should be set to false.
    if (dropOneSlot) numFiles = numFiles - 1

  end subroutine CloseFile
!******************************************************************************
!****f* FilesMod/FlushAllFiles
!  NAME
!  FlushAllFiles: This subroutine calls the Fortran 'Flush' command for
!  all files that have been opened.
!  DESCRIPTION
!  This subroutine calls the Fortran 'Flush' command for all files that have
!  been opened with either 'OpenFiles' or 'OpenFile'.
!  SYNOPSIS

  subroutine FlushAllFiles()

!  ARGUMENTS
!  [None]
!*****

    integer :: i

    ! Calls the 'Flush' command for the files opened using 'OpenFiles'
    ! or 'OpenFile'.
    do i = 1, numFiles
      call Flush(unitNums(i))
    end do

  end subroutine FlushAllFiles
!******************************************************************************
!****f* FilesMod/GetLine
!  NAME
!  GetLine: This function reads in and returns the next line of a file.
!  DESCRIPTION
!  This function reads in the next line of a file and returns it as a
!  a character string (of length 10000). The file it reads is the one
!  associated with 'unitNum'. If 'extraCheck' is true or omitted then
!  the file needs to have been opened using either 'OpenFiles' or
!  'OpenFile'. If 'extraCheck' is false, then it does not check to make
!  sure that the 'fileNum' is associated with a file that was opened
!  using either 'OpenFiles' or 'OpenFile'. If 'readAllLines' is false or
!  omitted then the function skips any blank lines or comment lines
!  (beginning with an '!') and only reads lines with actual data. If
!  'readAllLines' is true, the function reads all lines including blank
!  and comment lines. The character string that is returned
!  can be parsed using the 'ParseLine' function in the StringsMod.
!  RETURNS
!  Character string (of length 10000): Next line of file.
!  SYNOPSIS
!  MAX_CHAR_LENGTH is set equal to 10,000.
  character (len = MAX_CHAR_LENGTH) &
      function GetLine(unitNum, extraCheck, readAllLines) result(line)

!  ARGUMENTS
!  * unitNum: Unit number of the file.
!  * extraCheck: (optional) If true or omitted, the file associated with unitNum
!    needs to be opened using either 'OpenFiles' or 'OpenFile'. If false, it does
!    not check to make sure the file was opened using either 'OpenFiles' or
!    'OpenFile'.
!  * readAllLines: (optional) If false or omitted, the function skips blank or
!    comment lines. If true, the function reads blank and comment lines.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: unitNum
    logical, optional, intent(in) :: extraCheck
    logical, optional, intent(in) :: readAllLines
!*****
    integer :: ios
    logical :: checkUnitNum
    logical :: allLines

    if (present(extraCheck)) then
      checkUnitNum = extraCheck
    else
      checkUnitNum = .true.
    end if

    if (present(readAllLines)) then
      allLines = readAllLines
    else
      allLines = .false.
    end if

    if (checkUnitNum) then
      ! Check if all the values in the unitNums array are not equal to unitNum
      ! (i.e. unitNum does not equal any value in unitNums), then the file
      ! has not been opened (at least not by this module).
      if ( all(unitNums /= unitNum) ) then
        if (internalCallToGetLine) then
          write(6, '(A, I6)') "Error in SkipLines(). Invalid unitNum: ", unitNum
          stop
        else
          write(6, '(A, I6)') "Error in GetLine(). Invalid unitNum: ", unitNum
          stop
        end if
      end if
    end if

    do
      read (unitNum, '(A)', iostat = ios) line
      ! Check for end-of-file
      if(ios /= 0) then
        line = " "
        exit
      end if
      ! Ignore blank lines and comment lines (beginning with '!')
      if((.not. allLines) .and. &
          (trim(line) .eq. "" .or. line(1:1) .eq. "!")) cycle
      ! Exit this do-loop because a valid line has been read in.
      exit
    end do

  end function GetLine
!******************************************************************************
!****f* FilesMod/SkipLines
!  NAME
!  SkipLines: This subroutine skips the specified number of lines while
!  reading a file.
!  DESCRIPTION
!  The number of lines skipped while reading a file is specified in
!  'numLinesToSkip'. The file it reads is the one associated with 'unitNum'.
!  If 'extraCheck' is true or omitted then the file needs to have been opened
!  using either 'OpenFiles' or 'OpenFile'. If 'extraCheck' is false, then it
!  does not check to make sure that the 'fileNum' is associated with a file that
!  was opened using either 'OpenFiles' or 'OpenFile'.
!  SYNOPSIS

  subroutine SkipLines(unitNum, numLinesToSkip, extraCheck)

!  ARGUMENTS
!  * unitNum: Unit number of the file.
!  * numLinesToSkip: Number of lines to skip while reading a file.
!  * extraCheck: (optional) If true or omitted, the file associated with unitNum
!    needs to be opened using either 'OpenFiles' or 'OpenFile'. If false, it does
!    not check to make sure the file was opened using either 'OpenFiles' or
!    'OpenFile'.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: unitNum
    integer, intent(in) :: numLinesToSkip
    logical, optional, intent(in) :: extraCheck
!*****
    integer :: i
    character (len = MAX_CHAR_LENGTH) :: line

    internalCallToGetLine = .true.
    do i = 1, numLinesToSkip
      line = GetLine(unitNum, extraCheck, .true.)
    end do
    internalCallToGetLine = .false.

  end subroutine SkipLines
!******************************************************************************
!****f* FilesMod/CheckFileData
!  NAME
!  CheckFileData: This subroutine is used to check that read in data matches the
!  expected data.
!  DESCRIPTION
!  This subroutine is used to check that read in data matches the expected
!  data. For example, if your program is reading in data, one year at a
!  time, then you may be in a do-loop with an index named 'year'.
!  Assuming the input file has a year indicator for each year of data,
!  then it is wise to check that the read-in year matches the 'year'
!  variable that is your do-loop index.
!
!  Say the read-in year is stored in a variable named 'dummyYear', and that the
!  open file being read is associated with unit number 10.  Then you can call
!  CheckFileData(year, dummyYear, 10). If 'year' and 'dummyYear' do not match,
!  then an error message is printed out which tells you the name of the file
!  where the error occurred, and the program is terminated. If the years
!  do match, then the program just proceeds.
!
!  If 'extraCheck' is true or omitted then the file needs to have been opened
!  using either 'OpenFiles' or 'OpenFile'. If 'extraCheck' is false, then it
!  does not check to make sure that the 'fileNum' is associated with a file that
!  was opened using either 'OpenFiles' or 'OpenFile'.
!  SYNOPSIS

  subroutine CheckFileData(expectedData, readData, unitNum, extraCheck)

!  ARGUMENTS
!  * expectedData: Expected data, for example the do-loop variable.
!  * readData: Read in data.
!  * unitNum: Unit number of the file.
!  * extraCheck: (optional) If true or omitted, the file associated with unitNum
!    needs to be open. If false, it does not check to make sure the
!    file was opened.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: expectedData
    integer, intent(in) :: readData
    integer, intent(in) :: unitNum
    logical, optional, intent(in) :: extraCheck
!****
    logical :: checkUnitNum
    character (len = MAX_CHAR_LENGTH) :: fileName
    integer :: ind

    if (present(extraCheck)) then
      checkUnitNum = extraCheck
    else
      checkUnitNum = .true.
    end if

    ind = IndexFromUnitNum(unitNum)

    if (checkUnitNum) then
      ! Check if all the values in the unitNums array are not equal to unitNum
      ! (i.e. unitNum does not equal any value in unitNums), then the file
      ! has not been opened (at least not by this module).
      if (ind == -1) then
        write(6, '(A, I6)') "Error in CheckFileData(). Invalid unitNum: ", &
          unitNum
        stop
      end if
      fileName = trim(fileNames(ind))
    else
      inquire(unitNum, name = fileName)
    end if

    if (expectedData /= readData) then
      write(6, '(A)') "Error in CheckFileData: unexpected data in " // &
        trim(fileName) // "."
      write(6, '(A, I)') "  Expected: ", expectedData
      write(6, '(A, I)') "  Read: ", readData
      stop
    end if

  end subroutine CheckFileData
!******************************************************************************
!****f* FilesMod/FilesModBeginVerbose
!  NAME
!  FilesModBeginVerbose:This subroutine is helpful for keeping track of which
!  files are open and which file numbers are in use.
!  DESCRIPTION
!  This subroutine tells the FilesMod module to enter "verbose" mode. This will
!  cause the module to print out an output message every time a file is opened
!  or closed using the routines in this module. Files that were opened or closed
!  before this subroutine was called will not be logged.
!
!  If this subroutine is called with 'fileNum' and 'fileName' both omitted, then
!  the  message is printed to unit 6, standard output (usually the screen,
!  unless redirected). If the argument 'fileName' is passed in but 'fileNum' is
!  omitted, then 'fileName' has no effect. If only the argument 'fileNum' is
!  passed in but 'fileName' is omitted, then that is the unit number where the
!  output messages will go. If this unit number is not associated with a file,
!  it will create a fort.* file for the output. If both 'fileNum' and 'fileName'
!  are passed in, then the output file will be associated with the 'fileNum'
!  number, and will have the name 'fileName' (instead of being a fort.* file).
!
!  If the argument 'fieldWidth' is omitted (or passed in with a value of 0)
!  then the output will be unformatted. If it is passed in, then the
!  output will be formatted such that it is in tabular format.  This
!  is used to line up all of the file names in a nice column with a field
!  width of 'fieldWidth'.
!
!  The 'fullPath' argument is used to determine if the full path for the
!  file is written out or not. If it's .true. then the full path names
!  of the files will be output each time. If it's .false. or omitted
!  then the file name will be the same as how the it was passed in
!  (via 'OpenFile' or 'OpenFiles') -- either a relative path or a full path.
!  SYNOPSIS

  subroutine FilesModBeginVerbose(fileNum, fileName, fieldWidth, fullPath)

!  ARGUMENTS
!  * fileNum: (optional) File number associated with the output file.
!  * fileName: (optional) File name associated with the output file.
!  * fieldWidth: (optional) Output format.
!  * fullPath: (optional) If true, the full path names for the files will be in
!    the output. If false or omitted, the file name will be the same as
!    how it was passed in.
!  ARGUMENT DECLARATIONS
    integer, optional, intent(in) :: fileNum
    character (len = *), optional, intent(in) :: fileName
    integer, optional, intent(in) :: fieldWidth
    logical, optional, intent(in) :: fullPath
!*****

    verbose = .true.
    if (present(fileNum)) verboseFileNum = fileNum
    if (present(fieldWidth)) then
      verboseFieldWidth = fieldWidth
    else
      verboseFieldWidth = 0
    end if
    if (present(fileName)) then
      verboseFileName = fileName
      call OpenFile(verboseFileNum, verboseFileName, "replace")
    end if
    if (present(fullPath)) then
      fullPathNames = fullPath
    end if

  end subroutine FilesModBeginVerbose
!******************************************************************************
!****f* FilesMod/FilesModEndVerbose
!  NAME
!  FilesModEndVerbose: This subroutine ends the verbose output.
!  DESCRIPTION
!  This subroutine ends the verbose output that was begun with a call
!  to the subroutine FilesModBeginVerbose(). If 'FilesModBeginVerbose'
!  was not yet called, then this subroutine does nothing. If
!  'FilesModBeginVerbose' was called with both arguments passed in
!  (so that a specific named-file was opened for the output), then
!  this subroutine will close that file (after writing a message that
!  it was closed). Files that are opened or closed after this
!  subroutine was called will not be logged.
!  SYNOPSIS

  subroutine FilesModEndVerbose()

!  ARGUMENTS
!  [None]
!*****

    integer :: i

    do i = 1, numFiles
      if (unitNums(i) /= verboseFileNum) then
        write(verboseFileNum, '(A)') "FilesMod: " // &
          "WARNING: File not closed: unit number " // &
          trim(IntToAsc(unitNums(i)))
      end if
    end do

    if (verboseFileName /= "") call CloseFile(verboseFileNum)
    verbose = .false.

  end subroutine FilesModEndVerbose
!******************************************************************************
!****f* FilesMod/FilesModSetMacro
!  NAME
!  FilesModSetMacro: This subroutine is used to set a macro for use in the
!  'mainFile' associated with the 'OpenFiles' and 'CloseFiles' subroutines.
!  DESCRIPTION
!  The 'macroName' is the name of the macro and 'macroExpansion' is the
!  string that the macro will be expanded to when read by 'OpenFiles'
!  or 'CloseFiles'. To use the macro in the 'mainFile' (i.e., the file
!  that is passed as an argument to 'OpenFiles' or 'CloseFiles') just
!  insert the name of the macro surrounded by parenthesis, and with a
!  leading dollar sign ($). For example, a call like this:
!    call FilesModSetMacro("TR_YEAR", "2006")
!  will set the macro "TR_YEAR" (without the quotes) to be interpreted
!  as the string "2006" (without the quotes). So, in the list of files
!  to be opened or closed ('mainFile'), any place that "$(TR_YEAR)"
!  (without the quotes) is encountered, it will be replaced by "2006"
!  (without the quotes). If 'macroName' is already in use (having been
!  set already), then this subroutine will give an error message and stop
!  the program. To re-use a macro (i.e., to set it to something after it
!  has already been set once in the program), see the documentation to
!  'FilesModResetMacro'. Also, see the documentation to
!  'FilesModUnsetMacro' to un-set a macro.
!  SYNOPSIS

  subroutine FilesModSetMacro(macroName, macroExpansion)

!  ARGUMENTS
!  * macroName: Name of the macro.
!  * macroExpansion: String that the macro will be expanded to in 'OpenFiles'
!    or 'CloseFiles'.
!  ARGUMENT DECLARATIONS
    character(len = *), intent(in) :: macroName
    character(len = *), intent(in) :: macroExpansion
!*****
    integer :: i

    if (numMacros == MAX_MACROS) then
      write(6, '(A)') "ERROR in FilesModSetMacro."
      write(6, '(A)') "  Trying to set more macros than available."
      write(6, '(A)') "  MAX_MACROS = " // trim(IntToAsc(MAX_MACROS))
      write(6, '(A)') "  Could not set macro: " // trim(macroName)
      stop
    end if

    do i = 1, numMacros
      if (trim(macroNames(i)) == trim(macroName)) then
        write(6, '(A)') "ERROR in FilesModSetMacro."
        write(6, '(A)') "  The macro '" // trim(macroName) // &
          "' is already defined."
        stop
      end if
    end do

    numMacros = numMacros + 1
    macroNames(numMacros) = trim(macroName)
    macroExpansions(numMacros) = trim(macroExpansion)

  end subroutine FilesModSetMacro
!******************************************************************************
!****f* FilesMod/FilesModGetMacro
!  NAME
!  FilesModGetMacro: This function takes a macro name and returns the string
!  associated with that macro.
!  DESCRIPTION
!  This function is used to return the expanded macro string that has already
!  been set by using 'FilesModSetMacro'. If the specified macro has not already
!  been set, then this subroutine gives an error message and stops the
!  program. For more information on setting and using macros in this
!  module see the documentation for 'FilesModSetMacro'.
!  RETURNS
!  Character: String that the macro will be expanded to in 'OpenFiles'
!  or 'CloseFiles'.
!  SYNOPSIS
  character(len = MAX_CHAR_LENGTH) function FilesModGetMacro(macroName) &
    result(returnMacroExpansion)

  !  ARGUMENTS
!  * macroName: Name of the macro surrounded by quotes (e.g.
!    FilesModGetMacro("TRYR")).
!  ARGUMENT DECLARATIONS
  character (len = *), intent(in) :: macroName
!*****
  integer :: i
  logical :: foundMacro

  foundMacro = .false.
  do i = 1, numMacros
    if (trim(macroNames(i)) == trim(macroName)) then
      returnMacroExpansion = trim(macroExpansions(i))
      foundMacro = .true.
      exit
    end if
  end do

  if (.not. foundMacro) then
    write(6, '(A)') "ERROR in FilesModGetMacro."
    write(6, '(A)') "  Trying to get the macro '" // trim(macroName) // &
        "', but it is not currently defined."
    stop
  end if

  end function FilesModGetMacro
!******************************************************************************
!****f* FilesMod/FilesModResetMacro
!  NAME
!  FilesModResetMacro: This subroutine is used to re-set a macro that has
!  already been set.
!  DESCRIPTION
!  This subroutine is used to re-set a macro that has already been set
!  by using 'FilesModSetMacro'. If the specified macro has not already
!  been set, then this subroutine gives an error message and stops the
!  program. For more information on setting and using macros in this
!  module see the documentation for 'FilesModSetMacro'.
!  SYNOPSIS

  subroutine FilesModResetMacro(macroName, macroExpansion)

!  ARGUMENTS
!  * macroName: Name of the macro.
!  * macroExpansion: String that the macro will be expanded to in 'OpenFiles'
!    or 'CloseFiles'.
!  ARGUMENT DECLARATIONS
    character(len = *), intent(in) :: macroName
    character(len = *), intent(in) :: macroExpansion
!*****
    integer :: i
    logical :: foundMacro

    foundMacro = .false.
    do i = 1, numMacros
      if (trim(macroNames(i)) == trim(macroName)) then
        macroExpansions(i:numMacros) = trim(macroExpansion)
        foundMacro = .true.
        exit
      end if
    end do

    if (.not. foundMacro) then
      write(6, '(A)') "ERROR in FilesModResetMacro."
      write(6, '(A)') "  Trying to reset the macro '" // trim(macroName) // &
        "', but it is not currently defined."
      stop
    end if

  end subroutine FilesModResetMacro
!******************************************************************************
!****f* FilesMod/FilesModUnsetMacro
!  NAME
!  FilesModUnsetMacro: This subroutine is used to un-set a macro that has
!  already been set.
!  DESCRIPTION
!  This subroutine is used to un-set a macro that has already been set
!  by using 'FilesModSetMacro'. If the specified macro has not already
!  been set, then this subroutine gives an error message and stops the
!  program. For more information on setting and using macros in this
!  module see the documentation for 'FilesModSetMacro'. After the macro
!  is un-set then any occurrences of that string in the list of files
!  ('mainFile') will not be expanded; but rather interpreted literally.
!  There is no need to call this subroutine for "clean-up" purposes.
!  Macros can be left set until the end of the program.
!  SYNOPSIS

  subroutine FilesModUnsetMacro(macroName)

!  ARGUMENTS
!  * macroName: Name of the macro.
!  ARGUMENT DECLARATIONS
    character(len = *), intent(in) :: macroName
!*****
    integer :: i
    logical :: foundMacro

    foundMacro = .false.
    do i = 1, numMacros
      if (trim(macroNames(i)) == trim(macroName)) then
        macroNames(i:numMacros) = eoshift(macroNames(i:numMacros), 1)
        macroExpansions(i:numMacros) = eoshift(macroExpansions(i:numMacros), 1)
        numMacros = numMacros - 1
        foundMacro = .true.
        exit
      end if
    end do

    if (.not. foundMacro) then
      write(6, '(A)') "ERROR in FilesModUnsetMacro."
      write(6, '(A)') "  Trying to unset the macro '" // trim(macroName) // &
        "', but it is not currently defined."
      stop
    end if

  end subroutine FilesModUnsetMacro
!******************************************************************************
  character(len = MAX_CHAR_LENGTH) function ExpandMacros(fileName) &
    result(expandedFileName)

    character (len = *), intent(in) :: fileName
    integer :: startMacroInd, endMacroInd, i
    character (len = MAX_CHAR_LENGTH) :: macroName, macroExpansion
    logical :: foundMacro

    expandedFileName = fileName

    ! Set the position of the starting macro indicator, i.e. the string "$(".
    startMacroInd = index(expandedFileName, "$(")
    do
      ! If no macro opening is found, then exit this loop.
      if (startMacroInd == 0) exit
      ! Find the position of the ending macro indicator, i.e. the string ")".
      endMacroInd = startMacroInd - 1 + &
        index(expandedFileName(startMacroInd:), ")")
      ! If there is no ending indicator, then it isn't a macro, so just exit.
      if (startMacroInd == 0) exit
      ! Find the macro name (excluding the macro indicators).
      macroName = expandedFileName(startMacroInd + 2 : endMacroInd - 1)
      ! Now find the expansion for that macro name.
      foundMacro = .false.
      do i = 1, numMacros
        if (trim(macroNames(i)) == trim(macroName)) then
          macroExpansion = trim(macroExpansions(i))
          foundMacro = .true.
          exit
        end if
      end do
      ! Check to make sure the macro was interpreted correctly.
      if (.not. foundMacro) then
        write(6, '(A)') "Error trying to expand macro in files list."
        write(6, '(A)') "  Undefined macro: " // trim(macroName)
        stop
      end if
      ! Re-construct the new expandedFileName, with the expanded macro.
      expandedFileName = &
        expandedFileName(1:startMacroInd - 1) // &
        trim(macroExpansion) // &
        trim(expandedFileName(endMacroInd + 1:))
      ! Get the starting position of the next macro and start the loop again.
      startMacroInd = index(expandedFileName, "$(")
    end do

  end function ExpandMacros
!******************************************************************************
  subroutine WriteVerboseMessage(str, fileNum)

    ! This should either be "Opened" or "Closed".
    character (len = *), intent(in) :: str
    integer, intent(in) :: fileNum
    integer :: numFilesOpen
    character (len = MAX_CHAR_LENGTH) :: fileName 
    character (len = MAX_CHAR_LENGTH) :: field1, field2, field3

    ! Get the file name.
    if (fullPathNames) then
      fileName = trim(fullFileNames(IndexFromUnitNum(fileNum)))
    else
      fileName = trim(fileNames(IndexFromUnitNum(fileNum)))
    end if

    select case (str)
    case ("Opened")
      numFilesOpen = numFiles
    case ("Closed")
      ! Ideally, this function would be called once the file has been closed
      ! (and then numFiles would contain the correct value). But, when
      ! closing the file verboseFileNum, we need to make sure to write this
      ! line before we actually close the file, or else it would write the
      ! line to a fort.* file. So, since numFiles is not updated, yet, we
      ! need to use numFiles - 1.
      numFilesOpen = numFiles - 1
    case default
      write(6, '(A)') "Internal error in WriteVerboseMessage()."
      stop
    end select

    if(verboseFieldWidth == 0) then
      ! Unformatted output.
      write(verboseFileNum, '(A)') &
        str // " file " // trim(IntToAsc(fileNum)) // ", " // &
        trim(fileName) // ", files currently open: " // &
        trim(IntToAsc(numFilesOpen))
    else
      ! Formatted output.
      field1 = str // " file " // trim(IntToAsc(fileNum)) // ","
      if (len_trim(fileName) > verboseFieldWidth) then
        ! Subtracting 3 characters off the end, but adding 4 back. This is b/c
        ! we'll use verboseFeildWidth + 1 to accomodate the comma at the end.
        field2 = fileName(1:verboseFieldWidth - 3) // "...,"
      else
        field2 = trim(fileName) // ","
      end if
      field3 = " files currently open: " // trim(IntToAsc(numFilesOpen))
      write(verboseFileNum, '(3A)') &
        field1(1:24), field2(1:verboseFieldWidth + 1), trim(field3)
    end if

  end subroutine WriteVerboseMessage
!******************************************************************************
! Returns the index of a file based on its unit number. The index can be used
! to get the file name from the fileNames variable or the fullFileNames
! variable. If the unit number is not found, then -1 is returns. This is not
! a public function for this module; it is for internal use.
  integer function IndexFromUnitNum(unitNum) result(rv)

    integer, intent(in) :: unitNum
    logical :: found

    found = .false.
    do rv = 1, numFiles
      if (unitNum == unitNums(rv)) then
        found = .true.
        exit
      end if
    end do

    if (.not. found) then
      rv = -1
    end if

  end function IndexFromUnitNum
!******************************************************************************
! This function takes a file name and returns the unit number associated
! with that file. The name can be just the file name itself or it can be a
! portion of (or the entire) directory structure leading up to the file name.
! For example, if the file's full name is D:\dirA\dirB\myFile.txt,
! then the following would be the complete set a ways to reference that file:
!   myFile.txt
!   dirB\myFile.txt
!   dirA\dirB\myFile.txt
!   D:\dirA\dirB\myFile.txt

!****f* FilesMod/GetFileNumFromName
!  NAME
!  GetFileNumFromName: This function takes a file name and returns the unit
!  number associated with that file.
!  DESCRIPTION
!  This function takes a file name, in 'fileName' and returns the unit
!  number associated with that file. The name can be just the file
!  name itself, or it can be all of or a portion of the full path
!  name of the file. 
!
!  For example, if the file's full name is
!  D:\usr\dirA\dirB\myFile.txt, then any of the following ways can
!  be used to reference that file: myFile.txt, dirB\myFile.txt,
!  dirA\dirB\myFile.txt, or D:\dirA\dirB\myFile.txt. Note that the
!  file must have been opened using a subroutine from this module
!  (i.e. 'OpenFile' or 'OpenFiles'). If no files are found that
!  match 'fileName' then the program prints an error message and
!  stops.
!
!  However if 'abortOnNoFile' is passed in and is .false.,
!  then the function will just return -1 to indicate that no
!  match was found. If more than 1 match is found (for example
!  if 2 files with the same name but residing in different
!  directories have been opened, and the user called this function
!  with just the simple file name), then an error message will
!  be printed and the program will stop.
!
!  Note that the 'fileName'
!  variable is not case-sensitive (so the user does not need to
!  worry about matching the case of the file name), and that it
!  is also not slash-sensitive (so the user can use backslashes
!  or forward-slashes in 'fileName').
!  RETURNS
!  Integer: Unit number associated with the 'fileName' file.
!  SYNOPSIS

  integer function GetFileNumFromName(fileName, abortOnNoFile) result(rv)

!  ARGUMENTS
!  * fileName: Can be just the file name, or all or part of the full path name
!    of the file.
!  * abortOnNoFile: (optional) If .false., a -1 will be returned to indicate
!    that no match was found. If more then one match is found, the program will
!    stop.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: fileName
    logical, optional, intent(in) :: abortOnNoFile
!*****
    integer :: i
    integer :: numFound
    character (len = MAX_CHAR_LENGTH) :: fileNameUpper
    character (len = MAX_CHAR_LENGTH) :: storedFileNameUpper
    logical :: shouldAbortOnNoFile

    if (present(abortOnNoFile)) then
      shouldAbortOnNoFile = abortOnNoFile
    else
      shouldAbortOnNoFile = .true.
    end if

    ! First convert the passed in file name to upper-case for easy comparison.
    fileNameUpper = trim(ToUpper(trim(fileName)))
    ! Also, convert any backslashes to forward slashes for easy comparison.
    fileNameUpper = trim(ConvertToBackslashes(trim(fileNameUpper)))

    rv = -1
    numFound = 0
    do i = 1, numFiles
      storedFileNameUpper = trim(ToUpper(trim(fullFileNames(i))))
      storedFileNameUpper = &
        trim(ConvertToBackslashes(trim(storedFileNameUpper)))
      if (FileNamesMatch(trim(storedFileNameUpper), trim(fileNameUpper))) then
        numFound  = numFound + 1
        rv = unitNums(i)
      end if
    end do

    if (numFound == 0) then
      if (shouldAbortOnNoFile) then
        write(6, '(A)') "Error in GetFileNumFromName: " // &
          "Can not get file number from file with name '" // &
          trim(fileName) // "' because no file matched that name!"
        stop
      end if
    else if (numFound > 1) then
      write(6, '(A)') "Error in GetFileNumFromName: " // &
        "Can not get file number from file with name '" // trim(fileName) // &
        "' because more than one file matched with that name!"
      stop
    end if

  end function GetFileNumFromName
!******************************************************************************
!****f* FilesMod/GetNumOpenFiles
!  NAME
!  GetNumOpenFiles: This function returns a count of all files that have been
!  opened.
!  DESCRIPTION
!  This function returns a count of all files that have been opened with either
!  'OpenFiles' or 'OpenFile', and have not yet been closed (with
!  'CloseAllFiles', 'CloseFiles', or CloseFile').
!  SYNOPSIS

  integer function GetNumOpenFiles()

!  ARGUMENTS
!  [None]
!*****

    GetNumOpenFiles = numFiles

  end function GetNumOpenFiles
!******************************************************************************
! Compares two file names to see if they are "equal". The comparison is
! case-sensitive, so if a case-insensistive comparison is desired, make sure
! to first convert the file names to all lower-case or all upper-case. Also,
! remember to make sure that both file names use the same types of slashes
! or the comparison will fail. Both should use forward slashes or both should
! use backslashes. For the purpose of this comparison, "equal" means that
! they have the same actual file name plus the same path leading up to the
! file name. For example, any of the following two file names would be
! considered a match: "D:\dirA\MyFile.txt", "dirA\MyFile.txt", "MyFile.txt".
  logical function FileNamesMatch(file1, file2) result(rv)

    character(len = *), intent(in) :: file1
    character(len = *), intent(in) :: file2
    integer :: len1
    integer :: len2
    integer :: diffLen

    rv = .false.

    len1 = len_trim(file1)
    len2 = len_trim(file2)

    diffLen = len1 - len2

    if (diffLen > 0) then
      if (trim(file2) == trim(file1(diffLen + 1:))) then
        rv = .true.
      end if
    else
      if (trim(file1) == trim(file2(diffLen + 1:))) then
        rv = .true.
      end if
    endif

  end function FileNamesMatch
!******************************************************************************
! This function takes a string and returns the same string, except that any
! forward slashes are replaced with backslashes.
  character (len = MAX_CHAR_LENGTH) function ConvertToBackslashes(str) &
    result(rv)

    character (len = *), intent(in) :: str
    integer :: i

    rv = ""
    do i = 1, len_trim(str)
      if (str(i:i) == "/") then
        rv(i:i) = "\"
      else
        rv(i:i) = str(i:i)
      end if
    end do

  end function ConvertToBackslashes
!******************************************************************************
  subroutine ValidatePath(fileName)

    ! The full path (including the name of the file).
    character (len = *), intent(in) :: fileName
    ! The fileName with any forward slashes replaced with backslashes.
    character (len = MAX_CHAR_LENGTH) :: newFileName
    ! The most recent "piece" of the path (delimitted by backslashes).
    character (len = MAX_CHAR_LENGTH) :: piece
    ! Points to the "current" position -- the most recently used rightmost
    ! backslash
    integer :: currPos
    ! Points to the "next" position -- the first backslash to the left of the
    ! backslash at currPos
    integer :: nextPos
    !
    integer :: len

    newFileName = trim(ConvertToBackslashes(trim(fileName)))

    ! If the newFileName ends with a trailing backslash, then that will be
    ! the starting currPos. Otherwise, the starting currPos will be the
    ! length of newFileName plus 1.
    len = len_trim(newFileName)
    if (newFileName(len:len) == "\") then
      currPos = len
    else
      currPos = len + 1
    end if
    nextPos = index(newFileName(1:currPos - 1), "\", .true.)
    piece = trim(newFileName(nextPos + 1:currPos - 1))

    do while (nextPos < currPos)
      ! To check the full path name to see if it exists, keep chopping off
      ! a "piece" and check the remaining path. If the remaining portion
      ! exists, then the first one that doesn't exist is the portion of
      ! the path that remains, plus the most recent "piece".

      if (nextPos == 0) then
        write(6, '(A)') "Error validating path existence!"
        write(6, '(A)') "  Validating:     " // trim(fileName)
        write(6, '(A)') "  Does not exist: " // trim(piece)
        stop
      else if (CheckDir(newFileName(1:nextPos))) then
        write(6, '(A)') "Error validating path existence!"
        write(6, '(A)') "  Validating:     " // trim(fileName)
        write(6, '(A)') "  Does not exist: " // newFileName(1:nextPos) // &
          trim(piece)
        stop
      end if
      currPos = nextPos
      nextPos = index(newFileName(1:currPos - 1), "\", .true.)
      piece = trim(newFileName(nextPos + 1:currPos - 1))
    end do

  end subroutine ValidatePath
!******************************************************************************
!This subroutine returns a list of the files that are loaded along  with the 
!total number of files loaded

  subroutine LoadOpenFilesNums(unitNumsArr, numFilesLoaded)

    integer, dimension(:), intent(out) :: unitNumsArr
    integer, intent(out) :: numFilesLoaded
    integer :: i

    numFilesLoaded = numFiles

    if (size(unitNumsArr) < numFiles) then
      numFilesLoaded = -1
    end if 

    do i = 1, numFilesLoaded
      unitNumsArr(i) = unitNums(i)
    end do

  end subroutine LoadOpenFilesNums
!******************************************************************************
  subroutine LoadOpenFilesNames(fileNamesArr, numFilesLoaded)

    character (len = *), dimension(:), intent(out) :: fileNamesArr
    integer, intent(out) :: numFilesLoaded
    integer :: i

    numFilesLoaded = numFiles

    if (size(fileNamesArr) < numFiles) then
      numFilesLoaded = -1
    end if 

    do i = 1, numFilesLoaded
      fileNamesArr(i) = fileNames(i)
    end do

  end subroutine LoadOpenFilesNames
!******************************************************************************
  
end module FilesMod