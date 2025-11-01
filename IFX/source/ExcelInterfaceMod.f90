! $Id: ExcelInterfaceMod.f90 1.40 2018/07/26 16:57:54EDT 622325 Development  $
module ExcelInterfaceMod

!****h* source/ExcelInterfaceMod
!  NAME
!  ExcelInterfaceMod: OcactFortLib Excel interface module.
!  REVISION
!  $Revision: 1.40 $
!  MODULE DESCRIPTION
!  ExcelInterfaceMod is an interface module between Fortran and Excel.
!  The subroutine ExcelInitObjects must be called before using most of the
!  routines in this module (with the exception of routines that don't use an
!  Excel workbook directly, such as ExcelGetColNumFromCell and
!  ExcelGetNextCellDown). Make sure to call ExcelUnInitObjects after all the
!  Excel work is finished.
!
!  This module contains subroutines and functions that are called
!  to perform Excel tasks, such as:
!  * Copy, paste, clear, and delete data in Excel files
!  * Move around within Excel workbooks and worksheets
!  * Read data from and write data to Excel files
!  * Open, make changes, and save Excel files
!  MODULE ROUTINES
!  ExcelAddSheet
!  ExcelBegin
!  ExcelBeginNew
!  ExcelClear
!  ExcelClearContents
!  ExcelClearContentsRegion
!  ExcelCopy
!  ExcelCopyRegion
!  ExcelDeleteSheet
!  ExcelEnd
!  ExcelFixNumberStoredAsText
!  ExcelGetCalculationMode
!  ExcelGetCellFromRange
!  ExcelGetColNumFromCell
!  ExcelGetColStrFromCell
!  ExcelGetDoublePrecFromCell
!  ExcelGetDoublePrecsFromCells
!  ExcelGetFormulaFromCell
!  ExcelGetFormulasFromCells
!  ExcelGetIntFromCell
!  ExcelGetIntsFromCells
!  ExcelGetLastCell
!  ExcelGetNextCellDown
!  ExcelGetNextCellLeft
!  ExcelGetNextCellRight
!  ExcelGetNextCellUp
!  ExcelGetNumSheets
!  ExcelGetRealFromCell
!  ExcelGetRealsFromCells
!  ExcelGetRowNumFromCell
!  ExcelGetSheetName
!  ExcelGetStringFromCell
!  ExcelGetStringsFromCells
!  ExcelGetWorkbook
!  ExcelInitObjects
!  ExcelIsVisible
!  ExcelPaste
!  ExcelPasteSpecial
!  ExcelRunMacro
!  ExcelSave
!  ExcelSetCalculationMode
!  ExcelSetNumberFormat
!  ExcelSetSheetName
!  ExcelSwitchToSheet
!  ExcelTextToColumns
!  ExcelUnInitObjects
!  ExcelWriteToCells
!*****

  use Excel2016
  use ifcom  ! Needed for ConvertStringToBSTR()
  use StringsMod
  use PlatformsMod ! Needed for GetFullPath

  implicit none

  private
  !public :: ExcelTestFunction
  !public :: ExcelTestSumFunction
  !public :: ExcelTestVLookupFunction_DP
  public :: ExcelInitObjects
  public :: ExcelBegin
  public :: ExcelBeginNew
  public :: ExcelEnd
  public :: ExcelUnInitObjects
  public :: ExcelRunMacro
  public :: ExcelGetIntFromCell
  public :: ExcelGetIntsFromCells
  public :: ExcelGetRealFromCell
  public :: ExcelGetRealsFromCells
  public :: ExcelGetDoublePrecFromCell
  public :: ExcelGetDoublePrecsFromCells
  public :: ExcelGetStringFromCell
  public :: ExcelGetStringsFromCells
  public :: ExcelGetFormulaFromCell
  public :: ExcelGetFormulasFromCells
  public :: ExcelWriteToCells
  public :: ExcelSetNumberFormat
  public :: ExcelClear
  public :: ExcelClearContents
  public :: ExcelClearContentsRegion
  public :: ExcelCopy
  public :: ExcelCopyRegion
  public :: ExcelPaste
  public :: ExcelPasteSpecial
  public :: ExcelFixNumberStoredAsText
  public :: ExcelSetCalculationMode
  public :: ExcelGetCalculationMode
  public :: ExcelTextToColumns
  public :: ExcelGetLastCell
  public :: ExcelSetSheetName
  public :: ExcelGetSheetName
  public :: ExcelSwitchToSheet
  public :: ExcelGetNumSheets
  public :: ExcelAddSheet
  public :: ExcelDeleteSheet
  public :: ExcelSave
  public :: ExcelGetWorkbook
  public :: ExcelIsVisible
  public :: ExcelGetNextCellRight
  public :: ExcelGetNextCellLeft
  public :: ExcelGetNextCellDown
  public :: ExcelGetNextCellUp
  public :: ExcelGetCellFromRange
  public :: ExcelGetRowNumFromCell
  public :: ExcelGetColNumFromCell
  public :: ExcelGetColStrFromCell

  ! These are parameters that originate from Excel2016Mod.f90.
  ! XlPasteSpecialOperation
	public ::	xlPasteSpecialOperationAdd
	public ::	xlPasteSpecialOperationDivide
	public ::	xlPasteSpecialOperationMultiply
	public ::	xlPasteSpecialOperationNone
	public ::	xlPasteSpecialOperationSubtract
  ! XlPasteType
	public ::	xlPasteAll
	public ::	xlPasteAllExceptBorders
	public ::	xlPasteFormats
	public ::	xlPasteFormulas
	public ::	xlPasteComments
	public ::	xlPasteValues
	public ::	xlPasteColumnWidths
	public ::	xlPasteValidation
	public ::	xlPasteFormulasAndNumberFormats
	public ::	xlPasteValuesAndNumberFormats
  ! XlTextParsingType 
  public ::	xlDelimited
  public ::	xlFixedWidth
  ! XlCalculation 
  public ::	xlCalculationAutomatic
  public ::	xlCalculationManual
  public ::	xlCalculationSemiautomatic

  ! Allow ExcelWriteToCells to be the generic name for the specific subroutines.
!****f* ExcelInterfaceMod/ExcelWriteToCells
!  NAME
!  ExcelWriteToCells: Writes data to Excel cells.
!  DESCRIPTION
!  This subroutine is used to write data to Excel cells. The cells to be
!  written to are represented as a string in 'cellsAsString'. For example,
!  'cellsAsString' can be "A15", "A15:D15", etc.... If it represents a
!  single cell (as in "A15") then 'toWrite' should be a single value
!  (character string, integer, real, or double precision). If it
!  represents an array of cells (as in "A15:D15"), then 'toWrite' can
!  be a 1-dimensionsal array or a single value (of type character string,
!  integer, real, or double precision), and the size of the array
!  (if an array) should match the size of the array represented by
!  'cellsAsString'. If the array sizes do not match, an error message
!  will be given.
!
!  Examples:
!  * ExcelWriteToCells("A1", 5) will write the number 5 to cell A1.
!  * ExcelWriteToCells("A1", "5") will write the string "5" to cell A1,
!    which Excel will probably convert to the number 5.
!  * ExcelWriteToCells("A1:D1", arr) will write the number 1 to cell A1,
!    2 to cell B1, 3 to cell C1, and 4 to cell D1, if arr was declared to be a
!    real array of size 4 containing the values 1 through 4, respectively.
!  * ExcelWriteToCells("A1:D1", 12.34) will write the (real) number
!    12.34 to cells A1, B1, C1, and D1.
!  SYNOPSIS
!  subroutine ExcelWriteToCells(cellsAsString, toWrite)
!  ARGUMENTS
!  * cellsAsString: The cells to be written to are represented as a
!    character string (of variable length).
!  * toWrite: The value to be written to cellsAsString. Can be a character 
!    string (of variable length), an integer, a real, a double precision,
!    or a 1-dimensional array of any of those.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface ExcelWriteToCells
    module procedure ExcelWriteStringToCell, ExcelWriteStringToCells, &
                     ExcelWriteIntToCell, ExcelWriteIntToCells, &
                     ExcelWriteRealToCell, ExcelWriteRealToCells, &
                     ExcelWriteDoublePrecToCell, ExcelWriteDoublePrecToCells
  end interface ExcelWriteToCells

  integer, parameter :: MAX_CHAR = 10000
  character (len = 10), parameter :: ALL_DIGITS = "1234567890"
  integer, parameter :: FIRST_COL = 1
  integer, parameter :: LAST_COL = 16384
  integer, parameter :: FIRST_ROW = 1
  integer, parameter :: LAST_ROW = 1048576
  character (len = 30), parameter :: BOTTOM_RIGHT_CELL = "XFD1048576"

  integer :: status

  integer(INT_PTR_KIND()) :: excelapp
  integer(INT_PTR_KIND()) :: workbooks
  integer(INT_PTR_KIND()) :: workbook
  integer(INT_PTR_KIND()) :: worksheets
  integer(INT_PTR_KIND()) :: worksheet
  integer(INT_PTR_KIND()) :: range

  type (VARIANT) :: vBSTR1
  type (VARIANT) :: vBSTR2
  type (VARIANT) :: vBSTR3
  type (VARIANT) :: vBSTR4
  type (VARIANT) :: vBSTR5
  type (VARIANT) :: vBSTR6
  type (VARIANT) :: vBSTR7
  type (VARIANT) :: vBSTR8
  type (VARIANT) :: vBSTR9
  type (VARIANT) :: vBSTR10
  type (VARIANT) :: vBSTR11
  type (VARIANT) :: vInt1
  type (VARIANT) :: vInt2
  type (VARIANT) :: vDouble1
  type (VARIANT) :: vDouble2
  type (VARIANT) :: vBool1
  type (VARIANT) :: vBool2
  type (VARIANT) :: vBool3
  type (VARIANT) :: vDisp1
  type (VARIANT) :: vDisp2

  type, private :: CellRangePartsT
    integer :: row1
    integer :: row2
    integer :: col1
    integer :: col2
  end type CellRangePartsT

contains
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelInitObjects
!  NAME
!  ExcelInitObjects: Initializes the ExcelInterfaceMod.
!  DESCRIPTION
!  This subroutine is used to initialize the ExcelInterfaceMod and should
!  be called before any other subroutines or functions in this module are
!  called. Make sure to call 'ExcelUnInitObjects' after all the Excel work
!  is finished.
!  SYNOPSIS

  subroutine ExcelInitObjects()

!  ARGUMENTS
!  [None]
!*****

    excelapp = 0
    workbooks = 0
    workbook = 0
    worksheets = 0
    worksheet = 0
    range = 0

    CALL VariantInit(vBSTR1)
    vBSTR1%VT = VT_BSTR
    CALL VariantInit(vBSTR2)
    vBSTR2%VT = VT_BSTR
    CALL VariantInit(vBSTR3)
    vBSTR3%VT = VT_BSTR
    CALL VariantInit(vBSTR4)
    vBSTR4%VT = VT_BSTR
    CALL VariantInit(vBSTR5)
    vBSTR5%VT = VT_BSTR
    CALL VariantInit(vBSTR6)
    vBSTR6%VT = VT_BSTR
    CALL VariantInit(vBSTR7)
    vBSTR7%VT = VT_BSTR
    CALL VariantInit(vBSTR8)
    vBSTR8%VT = VT_BSTR
    CALL VariantInit(vBSTR9)
    vBSTR9%VT = VT_BSTR
    CALL VariantInit(vBSTR10)
    vBSTR10%VT = VT_BSTR
    CALL VariantInit(vBSTR11)
    vBSTR11%VT = VT_BSTR
    CALL VariantInit(vInt1)
    vInt1%VT = VT_I4
    CALL VariantInit(vInt2)
    vInt2%VT = VT_I4
    CALL VariantInit(vDouble1)
    vDouble1%VT = VT_R8
    CALL VariantInit(vDouble2)
    vDouble2%VT = VT_R8
    CALL VariantInit(vBool1)
    vBool1%VT = VT_BOOL
    CALL VariantInit(vBool2)
    vBool2%VT = VT_BOOL
    CALL VariantInit(vBool3)
    vBool3%VT = VT_BOOL
    CALL VariantInit(vDisp1)
    vDisp1%VT = VT_DISPATCH
    CALL VariantInit(vDisp2)
    vDisp2%VT = VT_DISPATCH

    ! Create an Excel object.
    call ComInitialize(status)
    call ComCreateObject("Excel.Application", excelapp, status)
    !call ComCreateObject("Excel.Application.11", excelapp, status)
    if (excelapp == 0) then
      write (*, '(" Unable to create Excel object; Aborting")')
      call exit()
    end if
    !call $Application_SetVisible(excelapp, .true.)

    workbooks = $Application_GetWorkbooks(excelapp, $STATUS = status)
    call CheckStatus(status, " Unable to get WORKBOOKS object")

  end subroutine ExcelInitObjects
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelBegin
!  NAME
!  ExcelBegin: This subroutine is used to open an existing Excel workbook.
!  DESCRIPTION
!  The name of the workbook should be specified in 'excelFileName', and can
!  either be a relative path name or a full path name. If the workbook
!  being opened contains links to other workbooks, then the
!  'doUpdateLinks' variable can be used to specify whether those links
!  should be updated or not upon opening. If it is not passed in (and
!  the workbook does contain links), then Excel will give a pop-up
!  dialog box prompting the user if the links should be updated or not.
!  If 'isReadOnly' is passed in and it is true, then the file will be
!  opened in read-only mode. Otherwise it will be opened in the regular
!  read-write mode. If the workbook being opened is password-protected,
!  then the password can be passed in through 'wrPassword'. Use the
!  subroutine 'ExcelEnd' to close the workbook. To create a new workbook,
!  use the 'ExcelBeginNew' subroutine.
!  SYNOPSIS

  subroutine ExcelBegin(excelFileName, doUpdateLinks, isReadOnly, wrPassword)

!  ARGUMENTS
!  * excelFileName: Name of the existing Excel workbook to be opened.
!  * doUpdateLinks: (optional) Specifies whether the Excel workbook links should
!    be updated when the file is opened.
!  * isReadOnly: (optional) Opens the file in read-only or read-write mode.
!  * wrPassword: (optional) Password for the password-protected Excel file.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: excelFileName
    logical, optional, intent(in) :: doUpdateLinks
    logical, optional, intent(in) :: isReadOnly
    character (len = *), optional, intent(in) :: wrPassword
!****
    character (len = MAX_CHAR) :: fullExcelFileName
    integer :: retVal
    logical :: doesExist

    ! This subroutine opens an existing Excel file. So first make sure the
    ! file really does exist.
    inquire(file = trim(excelFileName), exist = doesExist)
    if (.not. doesExist) then
      write(6, '(A)') "Error in ExcelBegin. The specified file, '" // &
        trim(excelFileName) // "', must already exist."
      stop
    end if

    ! The Workbooks_Open function expects the full path name for the
    ! Excel file being opened.
    retVal = GetFullPath(excelFileName, fullExcelFileName)
    if (retVal == 0) then
      write(6, '(A)') "Error in ExcelBegin opening file: " // &
        trim(excelFileName) // ". Could not find full path name."
      stop
    end if

    vBool1%VU%BOOL_VAL = .false.
    if (present(isReadOnly)) vBool1%VU%BOOL_VAL = isReadOnly
    ! Open the specified spreadsheet file (note: specify the full file path).
    if (present(wrPassword)) then
      vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(wrPassword))
      if (present(doUpdateLinks)) then
        vBool1%VU%BOOL_VAL = doUpdateLinks
        workbook = Workbooks_Open(workbooks, fullExcelFileName, &
            UpdateLinks = vBool2, ReadOnly = vBool1, &
            WriteResPassword = vBSTR1, $STATUS = status)
      else
        workbook = Workbooks_Open(workbooks, fullExcelFileName, &
            ReadOnly = vBool1, WriteResPassword = vBSTR1, $STATUS = status)
      end if
    else
      if (present(doUpdateLinks)) then
        vBool1%VU%BOOL_VAL = doUpdateLinks
        workbook = Workbooks_Open(workbooks, fullExcelFileName, &
            UpdateLinks = vBool2, ReadOnly = vBool1, $STATUS = status)
      else
        workbook = Workbooks_Open(workbooks, fullExcelFileName, &
            ReadOnly = vBool1, $STATUS = status)
      end if
    end if
    call CheckStatus(status, &
      " Unable to get WORKBOOK object; ensure that the file path is correct")

    worksheets = $Workbook_GetWorksheets(workbook, status)
    call CheckStatus(status, " Unable to get worksheets object")

    worksheet = $Workbook_GetActiveSheet(workbook, status)
    call CheckStatus(status, " Unable to get WORKSHEET object")

  end subroutine ExcelBegin
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelBeginNew
!  NAME
!  ExcelBeginNew: This subroutine is used to open a new Excel workbook with the
!  ability to specify the number of sheets.
!  DESCRIPTION
!  This subroutine is used to open a new Excel workbook. If 'numSheets'
!  is passed in, then that specifies how many sheets will be in this
!  newly created workbook. If it is omitted, then the workbook will
!  contain 1 sheet. Use the subroutine 'ExcelEnd' to close the workbook.
!  To open an existing workbook, use the 'ExcelBegin' subroutine.
!  SYNOPSIS

  subroutine ExcelBeginNew(numSheets)

!  ARGUMENTS
!  * numSheets: (optional) Specifies the number of sheets in the new Excel
!    workbook.
!  ARGUMENT DECLARATIONS
    integer, optional, intent(in) :: numSheets
!*****
    integer :: origNumSheetsForNewWorkbook
    integer :: newNumSheetsForNewWorkbook
    integer :: workbookNum
    integer :: rv

    ! Store the original setting so we can restore it createing the new
    ! workbook.
    origNumSheetsForNewWorkbook = &
      $Application_GetSheetsInNewWorkbook(excelapp, $STATUS = status)
    call CheckStatus(status, " Unable to get number of sheets in new workbook")

    ! Figure out which new value to use.
    if (present(numSheets)) then
      newNumSheetsForNewWorkbook = numSheets
    else
      newNumSheetsForNewWorkbook = 1
    end if

    ! Set the new value
    call $Application_SetSheetsInNewWorkbook(excelapp, &
      newNumSheetsForNewWorkbook, $STATUS = status)
    call CheckStatus(status, " Unable to set number of sheets in new workbook")

    ! Create the new workbook.
    workbook = Workbooks_Add(workbooks, $STATUS = status)
    call CheckStatus(status, " Unable to begin new workbook")

    ! Switch to the new workbook.
    ! Since this workbook was just added, it's workbookNum will be the
    ! highest, i.e., the total number of workbooks currently open.
    workbookNum = Workbooks_GetCount(workbooks, status)
    call CheckStatus(status, " Unable to get number of workbooks")
    rv = ExcelGetWorkbook(workbookNum)
    if (rv /= 0) then
      write(6, '(A)') "Error in ExcelBeginNew. Could not open new workbook."
      stop
    end if

    ! Restore the old value.
    call $Application_SetSheetsInNewWorkbook(excelapp, &
      origNumSheetsForNewWorkbook, $STATUS = status)
    call CheckStatus(status, " Unable to set number of sheets in new workbook")

  end subroutine ExcelBeginNew
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelUnInitObjects
!  NAME
!  ExcelUnInitObjects: This subroutine is used to do any cleanup for
!  the ExcelInterfaceMod.
!  DESCRIPTION
!  There should be no more calls to the functions or subroutines in this
!  module after this subroutine has been called (except, perhaps, a new
!  call to 'ExcelInitObjects').
!  SYNOPSIS

  subroutine ExcelUnInitObjects()

!  ARGUMENTS
!  [None]
!*****

    call Workbooks_Close(workbooks, status)
    call ReleaseObjects()
    call ComUninitialize()

  end subroutine ExcelUnInitObjects
!******************************************************************************
  subroutine ReleaseObjects()
  
    status = VariantClear(vBSTR1)
    status = VariantClear(vBSTR2)
    status = VariantClear(vBSTR3)
    status = VariantClear(vBSTR4)
    status = VariantClear(vBSTR5)
    status = VariantClear(vBSTR6)
    status = VariantClear(vBSTR7)
    status = VariantClear(vBSTR8)
    status = VariantClear(vBSTR9)
    status = VariantClear(vBSTR10)
    status = VariantClear(vBSTR11)
    status = VariantClear(vInt1)
    status = VariantClear(vInt2)
    status = VariantClear(vDouble1)
    status = VariantClear(vDouble2)
    status = VariantClear(vBool1)
    status = VariantClear(vBool2)
    status = VariantClear(vBool3)
    status = VariantClear(vDisp1)
    status = VariantClear(vDisp2)

    ! These lines seem to be causing some unexplained crashes when trying to
    ! re-open excel after it has been closed once already.
    if (range /= 0) status = ComReleaseObject(range)
    if (worksheet /= 0) status = ComReleaseObject(worksheet)
    if (worksheets /= 0) status = ComReleaseObject(worksheets)
    if (workbook /= 0) status = ComReleaseObject(workbook)
    if (workbooks /= 0) status = ComReleaseObject(workbooks)
    if (excelapp /= 0) status = ComReleaseObject(excelapp)

  end subroutine ReleaseObjects
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelEnd
!  NAME
!  ExcelEnd: This subroutine is used to close an Excel workbook.
!  DESCRIPTION
!  This subroutine is used to close an Excel workbook, after it is finished
!  being used. If 'savedChanges' is passed in and it is true, then the
!  workbook will be saved before being closed. If 'savedChanges' is
!  passed in and it is false, then the workbook will be closed without
!  being saved. (To save the workbook under a different name, use the
!  'ExcelSave' subroutine.) If 'savedChanges' is not passed in, then an
!  Excel pop-up dialog box will prompt the use whether or not to save the
!  changes before closing. Note that if no changes have been made to the
!  workbook, then the dialog box will not come up.
!  SYNOPSIS

  subroutine ExcelEnd(saveChanges)

!  ARGUMENTS
!  * saveChanges: (optional) Option to save changes before closing an Excel
!    workbook.
!  ARGUMENT DECLARATIONS
    logical, optional, intent(in) :: saveChanges
!*****
    integer :: rv

    ! If told whether or not to save Excel before closing, then do so.
    ! If not told, then an Excel dialog box will pop up to prompt the user
    ! (unless no changes have been made to the file).
    if (present(saveChanges)) then
      vBool1%VU%BOOL_VAL = saveChanges
      call $Workbook_Close(workbook, vBool1, $STATUS = status)
    else
      call $Workbook_Close(workbook, $STATUS = status)
    end if
    call CheckStatus(status, " Unable to close workbook")
    ! If there was more than 1 workbook open, then we need to reset the current
    ! workbook to be one of the still-open workbooks.
    if (Workbooks_GetCount(workbooks, status) > 0) then
      rv = ExcelGetWorkbook(1)
    end if

  end subroutine ExcelEnd
!******************************************************************************
  subroutine CheckStatus(olestatus, errorMsg)

    integer, intent(in) :: olestatus
    character (len = *), intent(in) :: errorMsg

    if (olestatus >= 0) then
      return
    end if

    ! Error handling code
    call ReleaseObjects()
    write (*, '(A, "; OLE error status = 0x", Z8.8, "; Aborting")') &
      trim(errorMsg), olestatus
    call exit(-1)

  end subroutine CheckStatus
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelRunMacro
!  NAME
!  ExcelRunMacro: This subroutine is used to call an Excel macro.
!  DESCRIPTION
!  The name of the macro is passed in through 'macroName'. Currently, this
!  subroutine supports passing up to 10 arguments to the macro, and this can
!  be modified if the need arises.  If 1 argument is to be passed to the
!  macro, it is passed in through 'arg1'. Subseguent arguments are passed
!  to the macro by passing corresponding arguments to this subroutine.
!  Excel macros can be very powerful and flexible. Since having Fortran
!  manipulate Excel can be very cumbersome, it is recommended that, as
!  much as possible, put the heavy duty work into macros. Then, use this
!  subroutine to call the macros.
!  SYNOPSIS

  subroutine ExcelRunMacro(macroName, arg1, arg2, arg3, arg4, arg5, arg6, &
    arg7, arg8, arg9, arg10)

!  ARGUMENTS
!  * macroName: The Excel macro to be called.
!  * arg1: (optional) The first argument passed to the Excel macro.
!  * arg2: (optional) The second argument passed to the Excel macro.
!  * arg3: (optional) The third argument passed to the Excel macro.
!  * arg4: (optional) The fourth argument passed to the Excel macro.
!  * arg5: (optional) The fifth argument passed to the Excel macro.
!  * arg6: (optional) The sixth argument passed to the Excel macro.
!  * arg7: (optional) The seventh argument passed to the Excel macro.
!  * arg8: (optional) The eigth argument passed to the Excel macro.
!  * arg9: (optional) The ninth argument passed to the Excel macro.
!  * arg10: (optional) The tenth argument passed to the Excel macro.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: macroName
    character (len = *), optional, intent(in) :: arg1
    character (len = *), optional, intent(in) :: arg2
    character (len = *), optional, intent(in) :: arg3
    character (len = *), optional, intent(in) :: arg4
    character (len = *), optional, intent(in) :: arg5
    character (len = *), optional, intent(in) :: arg6
    character (len = *), optional, intent(in) :: arg7
    character (len = *), optional, intent(in) :: arg8
    character (len = *), optional, intent(in) :: arg9
    character (len = *), optional, intent(in) :: arg10
!*****
    type (VARIANT) :: vTemp

    ! Running an Excel macro.
    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(macroName)
    if (present(arg10)) then
      ! Call Excel macro passing it 10 arguments.
      vBSTR11%VU%PTR_VAL = ConvertStringToBSTR(arg10)
      vBSTR10%VU%PTR_VAL = ConvertStringToBSTR(arg9)
      vBSTR9%VU%PTR_VAL = ConvertStringToBSTR(arg8)
      vBSTR8%VU%PTR_VAL = ConvertStringToBSTR(arg7)
      vBSTR7%VU%PTR_VAL = ConvertStringToBSTR(arg6)
      vBSTR6%VU%PTR_VAL = ConvertStringToBSTR(arg5)
      vBSTR5%VU%PTR_VAL = ConvertStringToBSTR(arg4)
      vBSTR4%VU%PTR_VAL = ConvertStringToBSTR(arg3)
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, vBSTR4, &
        vBSTR5, vBSTR6, vBSTR7, vBSTR8, vBSTR9, vBSTR10, vBSTR11, &
        $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg9)) then
      ! Call Excel macro passing it 9 arguments.
      vBSTR10%VU%PTR_VAL = ConvertStringToBSTR(arg9)
      vBSTR9%VU%PTR_VAL = ConvertStringToBSTR(arg8)
      vBSTR8%VU%PTR_VAL = ConvertStringToBSTR(arg7)
      vBSTR7%VU%PTR_VAL = ConvertStringToBSTR(arg6)
      vBSTR6%VU%PTR_VAL = ConvertStringToBSTR(arg5)
      vBSTR5%VU%PTR_VAL = ConvertStringToBSTR(arg4)
      vBSTR4%VU%PTR_VAL = ConvertStringToBSTR(arg3)
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, vBSTR4, &
        vBSTR5, vBSTR6, vBSTR7, vBSTR8, vBSTR9, vBSTR10, $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg8)) then
      ! Call Excel macro passing it 8 arguments.
      vBSTR9%VU%PTR_VAL = ConvertStringToBSTR(arg8)
      vBSTR8%VU%PTR_VAL = ConvertStringToBSTR(arg7)
      vBSTR7%VU%PTR_VAL = ConvertStringToBSTR(arg6)
      vBSTR6%VU%PTR_VAL = ConvertStringToBSTR(arg5)
      vBSTR5%VU%PTR_VAL = ConvertStringToBSTR(arg4)
      vBSTR4%VU%PTR_VAL = ConvertStringToBSTR(arg3)
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, vBSTR4, &
        vBSTR5, vBSTR6, vBSTR7, vBSTR8, vBSTR9, $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg7)) then
      ! Call Excel macro passing it 7 arguments.
      vBSTR8%VU%PTR_VAL = ConvertStringToBSTR(arg7)
      vBSTR7%VU%PTR_VAL = ConvertStringToBSTR(arg6)
      vBSTR6%VU%PTR_VAL = ConvertStringToBSTR(arg5)
      vBSTR5%VU%PTR_VAL = ConvertStringToBSTR(arg4)
      vBSTR4%VU%PTR_VAL = ConvertStringToBSTR(arg3)
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, vBSTR4, &
        vBSTR5, vBSTR6, vBSTR7, vBSTR8, $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg6)) then
      ! Call Excel macro passing it 6 arguments.
      vBSTR7%VU%PTR_VAL = ConvertStringToBSTR(arg6)
      vBSTR6%VU%PTR_VAL = ConvertStringToBSTR(arg5)
      vBSTR5%VU%PTR_VAL = ConvertStringToBSTR(arg4)
      vBSTR4%VU%PTR_VAL = ConvertStringToBSTR(arg3)
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, vBSTR4, &
        vBSTR5, vBSTR6, vBSTR7, $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg5)) then
      ! Call Excel macro passing it 5 arguments.
      vBSTR6%VU%PTR_VAL = ConvertStringToBSTR(arg5)
      vBSTR5%VU%PTR_VAL = ConvertStringToBSTR(arg4)
      vBSTR4%VU%PTR_VAL = ConvertStringToBSTR(arg3)
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, vBSTR4, &
        vBSTR5, vBSTR6, $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg4)) then
      ! Call Excel macro passing it 4 arguments.
      vBSTR5%VU%PTR_VAL = ConvertStringToBSTR(arg4)
      vBSTR4%VU%PTR_VAL = ConvertStringToBSTR(arg3)
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, vBSTR4, &
        vBSTR5, $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg3)) then
      ! Call Excel macro passing it 3 arguments.
      vBSTR4%VU%PTR_VAL = ConvertStringToBSTR(arg3)
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, vBSTR4, &
        $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg2)) then
      ! Call Excel macro passing it 2 arguments.
      vBSTR3%VU%PTR_VAL = ConvertStringToBSTR(arg2)
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, vBSTR3, &
        $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else if (present(arg1)) then
      ! Call Excel macro passing it 1 argument.
      vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(arg1)
      vTemp = $Application_Run(excelapp, vBSTR1, vBSTR2, $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    else
      ! Call Excel macro passing it 0 arguments.
      vTemp = $Application_Run(excelapp, vBSTR1, $STATUS = status)
      call CheckStatus(status, " Unable to run macro: " // macroName)
    end if

  end subroutine ExcelRunMacro
!******************************************************************************
  subroutine ExcelWriteIntToCell(cellsAsString, toWrite)

    character (len = *), intent(in) :: cellsAsString
    integer, intent(in) :: toWrite

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoSetProperty(range, "VALUE", toWrite)
    call CheckStatus(status, " Unable to AutoSetProperty")

  end subroutine ExcelWriteIntToCell
!******************************************************************************
  subroutine ExcelWriteIntToCells(cellsAsString, toWrite)

    character (len = *), intent(in) :: cellsAsString
    integer, dimension(:), intent(in) :: toWrite
    type (CellRangePartsT) :: rangeParts
    integer :: rowNum

    rangeParts = CellRangeToParts(trim(cellsAsString))
    call CheckRange1D(rangeParts, size(toWrite))

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoSetProperty(range, "VALUE", toWrite)
    call CheckStatus(status, " Unable to AutoSetProperty")

    ! The above works for single cells and for horizontal ranges only.
    ! Vertical ranges need to written one cell at a time.
    do rowNum = rangeParts.row1, rangeParts.row2
      call ExcelWriteToCells(ColNumRowNumToCell(rangeParts.col1, rowNum), &
        toWrite(rowNum - rangeParts.row1 + 1))
    end do

  end subroutine ExcelWriteIntToCells
!******************************************************************************
  subroutine ExcelWriteRealToCell(cellsAsString, toWrite)

    character (len = *), intent(in) :: cellsAsString
    real, intent(in) :: toWrite

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoSetProperty(range, "VALUE", toWrite)
    call CheckStatus(status, " Unable to AutoSetProperty")

  end subroutine ExcelWriteRealToCell
!******************************************************************************
  subroutine ExcelWriteRealToCells(cellsAsString, toWrite)

    character (len = *), intent(in) :: cellsAsString
    real, dimension(:), intent(in) :: toWrite
    type (CellRangePartsT) :: rangeParts
    integer :: rowNum

    rangeParts = CellRangeToParts(trim(cellsAsString))
    call CheckRange1D(rangeParts, size(toWrite))

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoSetProperty(range, "VALUE", toWrite)
    call CheckStatus(status, " Unable to AutoSetProperty")

    ! The above works for single cells and for horizontal ranges only.
    ! Vertical ranges need to written one cell at a time.
    do rowNum = rangeParts.row1, rangeParts.row2
      call ExcelWriteToCells(ColNumRowNumToCell(rangeParts.col1, rowNum), &
        toWrite(rowNum - rangeParts.row1 + 1))
    end do

  end subroutine ExcelWriteRealToCells
!******************************************************************************
  subroutine ExcelWriteDoublePrecToCell(cellsAsString, toWrite)

    character (len = *), intent(in) :: cellsAsString
    double precision, intent(in) :: toWrite

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoSetProperty(range, "VALUE", toWrite)
    call CheckStatus(status, " Unable to AutoSetProperty")

  end subroutine ExcelWriteDoublePrecToCell
!******************************************************************************
  subroutine ExcelWriteDoublePrecToCells(cellsAsString, toWrite)

    character (len = *), intent(in) :: cellsAsString
    double precision, dimension(:), intent(in) :: toWrite
    type (CellRangePartsT) :: rangeParts
    integer :: rowNum

    rangeParts = CellRangeToParts(trim(cellsAsString))
    call CheckRange1D(rangeParts, size(toWrite))

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoSetProperty(range, "VALUE", toWrite)
    call CheckStatus(status, " Unable to AutoSetProperty")

    ! The above works for single cells and for horizontal ranges only.
    ! Vertical ranges need to written one cell at a time.
    do rowNum = rangeParts.row1, rangeParts.row2
      call ExcelWriteToCells(ColNumRowNumToCell(rangeParts.col1, rowNum), &
        toWrite(rowNum - rangeParts.row1 + 1))
    end do

  end subroutine ExcelWriteDoublePrecToCells
!******************************************************************************
  subroutine ExcelWriteStringToCell(cellsAsString, toWrite)

    ! Note: If the stringToWrite is really a number (that's been converted
    ! to a string to pass to this subroutine), that's OK, because when
    ! it's written to the excel sheet, excel will convert it back to a number.
    character (len = *), intent(in) :: cellsAsString
    character (len = *), intent(in) :: toWrite

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoSetProperty(range, "VALUE", toWrite)
    call CheckStatus(status, " Unable to AutoSetProperty")

  end subroutine ExcelWriteStringToCell
!******************************************************************************
  subroutine ExcelWriteStringToCells(cellsAsString, toWrite)

    ! Note: If the string toWrite is really a number (that's been converted
    ! to a string to pass to this subroutine), that's OK, because when
    ! it's written to the excel sheet, excel will convert it back to a number.
    character (len = *), intent(in) :: cellsAsString
    character (len = *), dimension(:), intent(in) :: toWrite
    type (CellRangePartsT) :: rangeParts
    integer :: rowNum

    rangeParts = CellRangeToParts(trim(cellsAsString))
    call CheckRange1D(rangeParts, size(toWrite))

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoSetProperty(range, "VALUE", toWrite)
    call CheckStatus(status, " Unable to AutoSetProperty")

    ! The above works for single cells and for horizontal ranges only.
    ! Vertical ranges need to written one cell at a time.
    do rowNum = rangeParts.row1, rangeParts.row2
      call ExcelWriteToCells(ColNumRowNumToCell(rangeParts.col1, rowNum), &
        toWrite(rowNum - rangeParts.row1 + 1))
    end do

  end subroutine ExcelWriteStringToCells
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetIntFromCell
!  NAME
!  ExcelGetIntFromCell: This function is used to read data from Excel.
!  DESCRIPTION
!  The name of the cell being read is represented as a string in 'cellAsString'.
!  For example, 'cellAsString' can be "A15", "AM123", etc....  The contents
!  of that cell are returned as an integer.  If the cell is empty, the integer
!  0 is returned.
!  RETURNS
!  Integer: Contents of the cell.
!  SYNOPSIS

  integer function ExcelGetIntFromCell(cellAsString) result(int)

!  ARGUMENTS
!  * cellAsString: Name of cell being read.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellAsString
!*****

    int = AscToInt(trim(ExcelGetStringFromCell(cellAsString)), .false.)

  end function ExcelGetIntFromCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetRealFromCell
!  NAME
!  ExcelGetRealFromCell: This function is used to read data from Excel.
!  DESCRIPTION
!  The name of the cell being read is represented as a string in 'cellAsString'.
!  For example, 'cellAsString' can be "A15", "AM123", etc....  The contents 
!  of that cell are returned as a real. If the cell is empty, the real 0.0
!  is returned.
!  RETURNS
!  Real: Selected cell.
!  SYNOPSIS

  real function ExcelGetRealFromCell(cellAsString) result(float)

!  ARGUMENTS
!  * cellAsString: Name of cell being read.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellAsString
!*****

    float = AscToReal(trim(ExcelGetStringFromCell(cellAsString)), .false.)

  end function ExcelGetRealFromCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetDoublePrecFromCell
!  NAME
!  ExcelGetDoublePrecFromCell: This function is used to read data from Excel.
!  DESCRIPTION
!  The name of the cell being read is represented as a string in 'cellAsString'.
!  For example, 'cellAsString' can be "A15", "AM123", etc....  The contents 
!  of that cell are returned as a double precision. If the cell is empty, the
!  double precision 0.0d0 is returned.
!  RETURNS
!  Double Precision: Contents of the cell.
!  SYNOPSIS

  double precision function ExcelGetDoublePrecFromCell(cellAsString) &
    result(float)

!  ARGUMENTS
!  * cellAsString: Name of cell being read.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellAsString
!*****

    float = &
      AscToDoublePrec(trim(ExcelGetStringFromCell(cellAsString)), .false.)

  end function ExcelGetDoublePrecFromCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetStringFromCell
!  NAME
!  ExcelGetStringFromCell: This function is used to read data from Excel.
!  DESCRIPTION
!  The name of the cell being read is represented as a string in 'cellAsString'.
!  For example, 'cellAsString' can be "A15", "AM123", etc....  The contents 
!  of that cell are returned as a string. See the documentation of
!  'ExcelGetIntFromCell', 'ExcelGetRealFromCell', and
!  'ExcelGetDoublePrecFromCell' for details on returning the contents
!  of the cell as an integer, real, and double precision, respectively.
!  RETURNS
!  Character String (of lenth 10000): Contents of the cell.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetStringFromCell(cellAsString) &
    result(str)

!  ARGUMENTS
!  * cellAsString: Selected cell.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellAsString
!*****

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoGetProperty(range, "VALUE", str)
    call CheckStatus(status, " Unable to AutoGetProperty")

  end function ExcelGetStringFromCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetFormulaFromCell
!  NAME
!  ExcelGetFormulaFromCell: This function is used to read a formula from an 
!  Excel cell.
!  DESCRIPTION
!  The name of the cell being read is represented as a string in 'cellAsString'.
!  For example 'cellAsString' can be "A15", "AM123", etc.... The formula
!  in that cell is returned as a string. This differs from
!  'ExcelGetStringFromCell' (see the documentation to that function) in
!  that it returns the formula of the cell instead of the result of
!  that formula. For example, say the cell "A1" contains "= 1 + 2".
!  Then ExcelGetFormulaFromCell("A1") will return the string "=1 + 2"
!  (padded with spaces on the right). And ExcelGetStringFromCell("A1")
!  will return the string "3" (padded with spaces on the right).
!  RETURNS
!  Character String (of lenth 10000): Returns the formula of the cell.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetFormulaFromCell(cellAsString) &
    result(str)

!  ARGUMENTS
!  * cellAsString: Name of cell being read.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellAsString
!*****

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoGetProperty(range, "FORMULA", str)
    call CheckStatus(status, " Unable to AutoGetProperty")

  end function ExcelGetFormulaFromCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetIntsFromCells
!  NAME
!  ExcelGetIntsFromCells: This function is used to read data from a range of 
!  cells in Excel.
!  DESCRIPTION
!  The name of the cell range being read is represented as a string in
!  'cellsAsString'. For example, 'cellsAsString' can be "A15:A20",
!  "A15:E15", etc....  The contents of the cells in that range are
!  returned as an array of integers. If a cell is empty, the integer 0 is
!  returned in the corresponding array slot.
!  RETURNS
!  1-dimensional array of integers (with the size specified by 'expectedSize'):
!  Contents in the cells.
!  SYNOPSIS
  integer function ExcelGetIntsFromCells(cellsAsString, expectedSize) &
    result(integ)

!  ARGUMENTS
!  * cellAsString: Name of cells being read.
!  * expectedSize: The size of the array of integers being returned.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
    integer, intent(in) :: expectedSize
!*****
    dimension :: integ(expectedSize)
    character (len = MAX_CHAR), dimension(expectedSize) :: str
    integer :: i

    str = ExcelGetStringsFromCells(cellsAsString, expectedSize)
    do i = 1, expectedSize
      integ(i) = AscToInt(trim(str(i)), .false.)
    end do

  end function ExcelGetIntsFromCells
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetRealsFromCells
!  NAME
!  ExcelGetRealsFromCells: This function is used to read data from a range of
!  cells in Excel.
!  DESCRIPTION
!  The name of the cell range being read is represented as a string in
!  'cellsAsString'. For example, 'cellsAsString' can be "A15:A20",
!  "A15:E15", etc....  The contents of the cells in that range are
!  returned as an array of reals. If a cell is empty, the real 0.0 is
!  returned in the corresponding array slot.
!  RETURNS
!  1-dimensional array of reals (with the size specified by 'expectedSize'):
!  Contents in the cell range.
!  SYNOPSIS
  real function ExcelGetRealsFromCells(cellsAsString, expectedSize) &
    result(flt)

!  ARGUMENTS
!  * cellAsString: Name of cells being read.
!  * expectedSize: The size of the array of integers being returned.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
    integer, intent(in) :: expectedSize
!*****
    dimension :: flt(expectedSize)
    character (len = MAX_CHAR), dimension(expectedSize) :: str
    integer :: i

    str = ExcelGetStringsFromCells(cellsAsString, expectedSize)
    do i = 1, expectedSize
      flt(i) = AscToReal(trim(str(i)), .false.)
    end do

  end function ExcelGetRealsFromCells
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetDoublePrecsFromCells
!  NAME
!  ExcelGetDoublePrecsFromCells: This function is used to read data from a
!  range of cells in Excel.
!  DESCRIPTION
!  The name of the cell range being read is represented as a string in
!  'cellsAsString'. For example, 'cellsAsString' can be "A15:A20",
!  "A15:E15", etc....  The contents of the cells in that range are
!  returned as an array of double precisions. If a cell is empty, the
!  double precision 0.0d0 is returned in the corresponding array slot.
!  RETURNS
!  1-dimensional array of double precisions (with the size specified by 
!  'expectedSize'): Contents in the cell range.
!  SYNOPSIS

  double precision function ExcelGetDoublePrecsFromCells(cellsAsString, &
    expectedSize) result(flt)

!  ARGUMENTS
!  * cellAsString: Name of cells being read.
!  * expectedSize: The size of the array of integers being returned.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
    integer, intent(in) :: expectedSize
!*****
    dimension :: flt(expectedSize)
    character (len = MAX_CHAR), dimension(expectedSize) :: str
    integer :: i

    str = ExcelGetStringsFromCells(cellsAsString, expectedSize)
    do i = 1, expectedSize
      flt(i) = AscToDoublePrec(trim(str(i)), .false.)
    end do

  end function ExcelGetDoublePrecsFromCells
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetStringsFromCells
!  NAME
!  ExcelGetStringsFromCells: This function is used to read data from a
!  range of cells in Excel.
!  DESCRIPTION
!  The name of the cell range being read is represented as a string in
!  'cellsAsString'. For example, 'cellsAsString' can be "A15:A20",
!  "A15:E15", etc....  The contents of the cells in that range are
!  returned as an array of character strings. If a cell is empty, the
!  empty string "" is returned in the corresponding array slot.
!  RETURNS
!  1-dimensional array of strings, each of length 10000: Contents of the
!  cell range.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetStringsFromCells(cellsAsString, &
    expectedSize) result(str)

!  ARGUMENTS
!  * cellAsString: Name of cells being read.
!  * expectedSize: The size of the array of integers being returned.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
    integer, intent(in) :: expectedSize
!*****
    dimension :: str(expectedSize)
    type(CellRangePartsT) :: rangeParts

    rangeParts = CellRangeToParts(trim(cellsAsString))
    call CheckRange1D(rangeParts, expectedSize)

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoGetProperty(range, "VALUE", str)
    call CheckStatus(status, " Unable to AutoGetProperty")

  end function ExcelGetStringsFromCells
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetFormulasFromCells
!  NAME
!  ExcelGetFormulasFromCells: This function is used to read formulas from a
!  range of cells in Excel.
!  DESCRIPTION
!  The name of the cell range being read is represented as a string in
!  'cellsAsString'. For example, 'cellsAsString' can be "A15:A20",
!  "A15:E15", etc....  The formula in each cell is returned as a string.
!  If a cell is empty, the empty string "" is returned in the
!  corresponding array slot. See the documentation of
!  'ExcelGetFormulaFromCell' for more information about returning
!  formulas instead of values.
!  RETURNS
!  1-dimensional array of strings, each of length 10000: Formulas of the cells.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetFormulasFromCells( &
    cellsAsString, expectedSize) result(str)

!  ARGUMENTS
!  * cellAsString: Name of cells being read.
!  * expectedSize: The size of the array of integers being returned.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
    integer, intent(in) :: expectedSize
!*****
    dimension :: str(expectedSize)
    type(CellRangePartsT) :: rangeParts

    rangeParts = CellRangeToParts(trim(cellsAsString))
    call CheckRange1D(rangeParts, expectedSize)

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    status = AutoGetProperty(range, "FORMULA", str)
    call CheckStatus(status, " Unable to AutoGetProperty")

  end function ExcelGetFormulasFromCells
!******************************************************************************
  ! The passed in numberFormat should be set to a valid format the
  ! way Excel expects formats. For example, "General" is for default
  ! formatting, "@" is for text formatting, "#,##0.00" for displaying
  ! numbers two decimal places with the thousands separator, etc....

!****f* ExcelInterfaceMod/ExcelSetNumberFormat
!  NAME
!  ExcelSetNumberFormat: This subroutine is used to change the number format 
!  of a cell or range of cells.
!  DESCRIPTION
!  The variable 'cellsAsString' can be a single cell or a range of cells 
!  (1-dimensional or 2-dimensional). The 'numberFormat' is the new format that
!  will be applied to the cell(s) specified in 'cellsAsString'. The format can
!  be of any form that Excel recognizes as a valid number format. For example,
!  "General" is for default formatting, "@" is for text formatting, "#,##0.00"
!  for displaying numbers two decimal places with the thousands separator,
!  etc....
!  SYNOPSIS

  subroutine ExcelSetNumberFormat(cellsAsString, numberFormat)

!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  * numberFormat: New format that will be applied to the selected cells.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
    character (len = *), intent(in) :: numberFormat
!*****

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(trim(numberFormat))
    call CellFormat_SetNumberFormat(range, vBSTR2, status)
    call CheckStatus(status, " Unable to SetNumberFormat")

  end subroutine ExcelSetNumberFormat
!******************************************************************************
  ! This subroutine clears the formulas and formatting for the range. To
  ! clear the formulas only, but leave the formatting intact, use
  ! ExcelClearContents.

!****f* ExcelInterfaceMod/ExcelClear
!  NAME
!  ExcelClear: This subroutine is used to clear a cell or range of cells of its
!  formulas and formatting.
!  DESCRIPTION
!  The variable 'cellsAsString' can be a single cell or a range of cells
!  (1-dimensional or 2-dimensional). To clear only the formulas, but keep the
!  formatting, see the 'ExcelClearContents' subroutine.
!  SYNOPSIS

  subroutine ExcelClear(cellsAsString)

!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
!*****

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    vBSTR2 = Range_Clear(range, status)
    call CheckStatus(status, " Unable to Clear RANGE")

  end subroutine ExcelClear
!******************************************************************************
  ! This subroutine clears the formulas for the range, but leaves the
  ! formatting intact. To clear the formulas and formatting use ExcelClear.

!****f* ExcelInterfaceMod/ExcelClearContents
!  NAME
!  ExcelClearContents: This subroutine is used to clear a cell or range of cells
!  of its formulas, but not of its formatting.
!  DESCRIPTION
!  The variable 'cellsAsString' can be a single cell or a range of cells
!  (1-dimensional or 2-dimensional). To clear the formulas and the formatting,
!  see the 'ExcelClear' subroutine.
!  SYNOPSIS

  subroutine ExcelClearContents(cellsAsString)

!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
!*****

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    vBSTR2 = Range_ClearContents(range, status)
    call CheckStatus(status, " Unable to ClearContents RANGE")

  end subroutine ExcelClearContents
!******************************************************************************
  ! This subroutine takes the passed in cell and gets the entire region as
  ! the range. (This is equivalent to clicking on that cell, and then doing a
  ! ctrl+a.) It clears the contents of the cells in that range.

!****f* ExcelInterfaceMod/ExcelClearContentsRegion
!  NAME
!  ExcelClearContentsRegion: This subroutine is used to clear a region of cells
!  of their contents (i.e., just of their formulas, but not of their
!  formatting).
!  DESCRIPTION
!  The variable 'cellsAsString' can be a single cell or a range of cells
!  (1-dimensional or 2-dimensional). The cell region contains the cells that 
!  are adjacent to the specified cell(s), without a blank row or column 
!  interupting. (The region can be determined by selecting the cells in 
!  'cellsAsString' and then typing ctrl-a.)
!  SYNOPSIS

  subroutine ExcelClearContentsRegion(cellsAsString)

!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
!*****

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    range = Range_GetCurrentRegion(range, status)
    call CheckStatus(status, " Unable to GetCurrentRegion RANGE")

    vBSTR2 = Range_ClearContents(range, status)
    call CheckStatus(status, " Unable to ClearContents RANGE")

! This is not needed here, but just save it for future reference.
! It shows how to convert the 'range' variable into a normal
! character string that can be used to display the range.
!    vBool1%VU%BOOL_VAL = .false.
!    vBool2%VU%BOOL_VAL = .false.
!    write(6, '(A)') trim(Range_GetAddress(range, vBool1, vBool2,
!      $STATUS = status))
!    call CheckStatus(status, " Unable to Range_GetAddress")

  end subroutine ExcelClearContentsRegion
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelCopy
!  NAME
!  ExcelCopy: This subroutine is used to copy a cell or range of cells.
!  DESCRIPTION
!  This subroutine is used to copy to the clipboard a cell or range of cells.
!  The variable 'cellsAsString' can be a single cell or a range of cells
!  (1-dimensional or 2-dimensional).
!  SYNOPSIS

  subroutine ExcelCopy(cellsAsString)
  
!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
!*****

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    vBSTR2 = Range_Copy(range, $STATUS = status)
    call CheckStatus(status, " Unable to CopyRange")

  end subroutine ExcelCopy
!******************************************************************************
  ! This subroutine takes the passed in cell and gets the entire region as
  ! the range. (This is equivalent to clicking on that cell, and then doing a
  ! ctrl+a.) It copies contents of the cells in that range to the clipboard.

!****f* ExcelInterfaceMod/ExcelCopyRegion
!  NAME
!  ExcelCopyRegion: This subroutine is used to copy a region of cells.
!  DESCRIPTION
!  This subroutine is used to copy a region of cells to the clipboard. The
!  variable 'cellsAsString' can be a single cell or a range of cells
!  (1-dimensional or 2-dimensional). The cell region contains the cells
!  that are adjacent to the specified cell(s), without a blank row
!  or column interupting. (The region can be determined by selecting
!  the cells in 'cellsAsString' and then typing ctrl-a.)
!  SYNOPSIS

  subroutine ExcelCopyRegion(cellsAsString)

!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
!****
    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    range = Range_GetCurrentRegion(range, status)
    call CheckStatus(status, " Unable to GetCurrentRegion RANGE")

    vBSTR2 = Range_Copy(range, $STATUS = status)
    call CheckStatus(status, " Unable to CopyRange")

  end subroutine ExcelCopyRegion
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelPaste
!  NAME
!  ExcelPaste: This subroutine is used to paste to cell(s).
!  DESCRIPTION
!  This subroutine is used to paste from the clipboard into a cell or range
!  of cells. The variable 'cellsAsString' can be a single cell or a range
!  of cells (1-dimensional or 2-dimensional).
!  SYNOPSIS

  subroutine ExcelPaste(cellsAsString)

!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
!*****

    call ExcelPasteSpecial(trim(cellsAsString), xlPasteAll, &
      xlPasteSpecialOperationNone)

  end subroutine ExcelPaste
!******************************************************************************
  ! For "pasteType" and operationType, see the "XlPasteType" and
  ! "XlPasteSpecialOperation" declarations near the top (declaring them
  ! as public).

!****f* ExcelInterfaceMod/ExcelPasteSpecial
!  NAME
!  ExcelPasteSpecial: This subroutine is used to paste from the clipboard into 
!  a cell or range of cells, using the Excel "Paste Special" features.
!  DESCRIPTION
!  The variable 'cellsAsString' can be a single cell or a range of cells
!  (1-dimensional or 2-dimensional). The optional variables 'pasteType' and 
!  'operationType' are declared as integers, but the user should pass in one of
!  the predefined values for each of them. The default value for 'pasteType' 
!  (used if this argument is omitted) is xlPasteAll. The default value for 
!  'operationType' (used if this argument is omitted) is
!  xlPasteSpecialOperationNone.
!
!  For example:
!     call ExcelPasteSpecial("A1", xlPasteAll, xlPasteSpecialOperationNone)
!  is equivalent to:
!     call ExcelPasteSpecial("A1")
!  which is also equivalent to:
!     call ExcelPaste("A1")
!  SYNOPSIS

  subroutine ExcelPasteSpecial(cellsAsString, pasteType, operationType)

!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  * pasteType: (optional) Paste Special type. The predefined values are:
!    xlPasteAll, xlPasteAllExceptBorders, xlPasteFormats, xlPasteFormulas,
!    xlPasteComments, xlPasteValues, xlPasteColumnWidths, xlPasteValidation,
!    xlPasteFormulasAndNumberFormats, or xlPasteValuesAndNumberFormats.
!  * operationType: (optional) Operation type. The predefined values are:
!    xlPasteSpecialOperationAdd, xlPasteSpecialOperationDivide,
!    xlPasteSpecialOperationMultiply, xlPasteSpecialOperationNone,
!    or xlPasteSpecialOperationSubtract.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
    integer, optional, intent(in) :: pasteType
    integer, optional, intent(in) :: operationType
!*****

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    vBSTR2 = Range_PasteSpecial(range, pasteType, operationType, &
      $STATUS = status)
    call CheckStatus(status, " Unable to PasteSpecial")

  end subroutine ExcelPasteSpecial
!******************************************************************************
  ! This will work for cells that contain digits or other regular text (i.e.
  ! it will not "harm" regular text cells). It will not work for cells that
  ! contain formulas (i.e. it will cause a #VALUE error).
  ! This subroutine needs to be able to grap a blank cell to copy, so it
  ! will try for the bottom right (BOTTOM_RIGHT_CELL). If this is not blank
  ! this subroutine will print an error message and stop.

!****f* ExcelInterfaceMod/ExcelFixNumberStoredAsText
!  NAME
!  ExcelFixNumberStoredAsText: This subroutine can be used to "fix" a cell, or
!  range of cells, that Excel has imported as text but should really be stored
!  as numbers.
!  DESCRIPTION
!  The cell, or range of cells, to be fixed is stored in 'cellsAsString'.
!  In order for this subroutine to work, cell
!  IV65536 must be empty. If it's not, the subroutine will print
!  an error message and cause the program to terminate. If there are
!  cells in the range specified by 'cellsAsString' that are regular
!  text, this subroutine will not affect them. However, if the cells
!  contain formulas, then this subroutine may corrupt them.
!  SYNOPSIS

  subroutine ExcelFixNumberStoredAsText(cellsAsString)

!  ARGUMENTS
!  * cellAsString: The cell(s) to be fixed.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
!*****

    if (trim(ExcelGetStringFromCell(trim(BOTTOM_RIGHT_CELL))) /= "") then
      write(6, '(A)') "Error in ExcelFixNumberStoredAsText: " // &
        "Cell " // trim(BOTTOM_RIGHT_CELL) // " must be empty."
      stop
    end if

    call ExcelCopy(trim(BOTTOM_RIGHT_CELL))
    call ExcelPasteSpecial(trim(cellsAsString), xlPasteValues, &
      xlPasteSpecialOperationAdd)

  end subroutine ExcelFixNumberStoredAsText
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelTextToColumns
!  NAME
!  ExcelTextToColumns: This subroutine is used to convert a vertical array range
!  of cells, that are stored as text, into multiple columns.
!  DESCRIPTION
!  It uses Excel's "Convert Text to Columns" wizard. The vertical range is
!  passed in as the variable 'cellsAsString'. The variable 'parseType' can
!  either be the integer 'xlDelimited' or 'xlFixedWidth', without the single
!  quotes. (These integers are already defined.) When 'xlDelimited' is used,
!  the wizard will use the tab character and the space character as the
!  delimiters. When 'xlFixedWidth' is used, the wizard will use the default
!  "fixed-width" columns separations. If 'parseType' is omitted, then it is left
!  up to Excel to figure out which one to use. If Excel detects that there is
!  something already in the destination cells, it will usually prompt the user
!  to see if these contents should be overwritten. If 'forceOverwite' is passed
!  in and is true, then the destination contents will be overwritten without
!  prompting.
!  SYNOPSIS

  subroutine ExcelTextToColumns(cellsAsString, parseType, forceOverwrite)

!  ARGUMENTS
!  * cellAsString: Selected cell(s).
!  * parseType: (optional) Can either be xlDelimited or xlFixedWidth.
!  * forceOverwrite: (optional) If omitted or false, Excel may popup with an
!    "Overwrite contents of destination cells?" type of question.
!    If true, cells will be overwritten, without prompting.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cellsAsString
    integer, optional, intent(in) :: parseType
    logical, optional, intent(in) :: forceOverwrite
    !*****
    ! Saved value of Excel's displayAlerts setting.
    logical(2) :: savedDisplayAlerts

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    ! Save current setting for displayAlerts
    savedDisplayAlerts = $Application_GetDisplayAlerts(excelapp, status)
    call CheckStatus(status, " Unable to get displayAlerts setting")

    ! This is set to true. Used to signify that Excel should treat
    ! consecutive delimiters as 1.
    vBool1%VU%BOOL_VAL = .true.
    ! These are both set to true. Used for signifying that a tab and space,
    ! respectively, are delimitters (when parseType = xlDelimited).
    vBool2%VU%BOOL_VAL = .true.
    vBool3%VU%BOOL_VAL = .true.

    ! Temporarily turn off the displayAlerts, if user opts for this.
    if (present(forceOverwrite)) then
      if (forceOverwrite) then
        call $Application_SetDisplayAlerts(excelapp, .false._2, status)
        call CheckStatus(status, " Unable to set displayAlerts setting")
      end if
    end if

    ! Do the actual "Text to Columns".
    vBSTR2 = Range_TextToColumns(range, DataType = parseType, &
      ConsecutiveDelimiter = vBool1, Tab = vBool2, Space = vBool3, &
      $STATUS = status)
    call CheckStatus(status, " Unable to TextToColumns")

    ! Reset the displayAlerts to whatever they were set to before.
    call $Application_SetDisplayAlerts(excelapp, savedDisplayAlerts, status)
    call CheckStatus(status, " Unable to set displayAlerts setting")

  end subroutine ExcelTextToColumns
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelSetCalculationMode
!  NAME
!  ExcelSetCalculationMode: This subroutine is used to set the calculation
!  mode of Excel.
!  DESCRIPTION
!  The calculation mode of Excel is a predefined integer type. There are three
!  valid calculation modes:
!  * xlCalculationAutomatic: Automatic - Excel automatically recalculates when
!    the workbook is opened, and at each and every change.
!  * xlCalculationManual: Manual - Excel only recalculates when the user
!    requests it.
!  * xlCalculationSemiautomatic: Automatic except for data tables - Same as
!    automatic, except for data tables, which are manual.
!  SYNOPSIS

  subroutine ExcelSetCalculationMode(calcMode)

!  ARGUMENTS
!  * calcMode: The calculation mode.
!  ARGUMENT DECLARATIONS
    integer :: calcMode
    !****

    ! Set the calculation mode.
    call $Application_SetCalculation(excelapp, calcMode, status)
    call CheckStatus(status, " Unable to set calculation mode")

  end subroutine ExcelSetCalculationMode
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetCalculationMode
!  NAME
!  ExcelGetCalculationMode: This function is used to retrieve the current
!  calculation mode of Excel.
!  DESCRIPTION
!  The calculation mode of Excel is a predefined integer type. There are three
!  valid calculation modes:
!  * xlCalculationAutomatic: Automatic - Excel automatically recalculates when
!    the workbook is opened, and at each and every change.
!  * xlCalculationManual: Manual - Excel only recalculates when the user
!    requests it.
!  * xlCalculationSemiautomatic: Automatic except for data tables - Same as
!    automatic, except for data tables, which are manual.
!  RETURNS
!  integer: The calculation mode of Excel, as one of the predefined integer
!  types (xlCalculationAutomatic, xlCalculationManual,
!  or xlCalculationSemiautomatic).
!  SYNOPSIS

  integer function ExcelGetCalculationMode() result(int)

!  ARGUMENTS
!  [None]
!****

    ! Retrieve the calculation mode.
    int = $Application_GetCalculation(excelapp, status)
    call CheckStatus(status, " Unable to get calculation mode")

  end function ExcelGetCalculationMode
!******************************************************************************
  ! This function returns the "last cell" of the worksheet. This is the same
  ! cell that Excel takes you to when you press ctrl-end.

!****f* ExcelInterfaceMod/ExcelGetLastCell
!  NAME
!  ExcelGetLastCell: This function is used to retrieve the "last cell" of the
!  currently active worksheet.
!  DESCRIPTION
!  The "last cell" means, as defined by Excel, the bottom-most and right-most
!  cell used. This is equivalent to pressing ctrl-end while in Excel. The string
!  representing the last cell is returned, but should usually be "trimmed" to
!  remove the trailing whitespace. For example, this is how this function will
!  often be called: trim(ExcelGetLastCell()).
!  RETURNS
!  Character string (of length 10000): Last cell of the worksheet.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetLastCell() result(lastCellStr)

!  ARGUMENTS
!  [None]
!*****
    integer(INT_PTR_KIND()) :: lastCell

    ! First, set the range to be somewhere (anywhere) on the worksheet.
    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim("A1"))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    ! Then, get the last cell of the worksheet.
    lastCell = Range_SpecialCells(range, xlCellTypeLastCell, $STATUS = status)
    call CheckStatus(status, " Unable to Range_SpecialCells")

    ! Finally, convert the cell to it's address (a character string).
    vBool1%VU%BOOL_VAL = .false.
    vBool2%VU%BOOL_VAL = .false.
    lastCellStr = Range_GetAddress(lastCell, vBool1, vBool2, $STATUS = status)
    call CheckStatus(status, " Unable to Range_GetAddress")

  end function ExcelGetLastCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelSetSheetName
!  NAME
!  ExcelSetSheetName: This subroutine is used to rename the active worksheet.
!  DESCRIPTION
!  This subroutine is used to rename the active worksheet. The argument
!  'newSheetName' is the new name to use for the sheet.
!  SYNOPSIS

  subroutine ExcelSetSheetName(newSheetName)

!  ARGUMENTS
!  * newSheetName: New name of the active worksheet.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: newSheetName
!*****

    call $Worksheet_SetName(worksheet, trim(newSheetName), status)
    call CheckStatus(status, " Unable to set worksheet name " // &
      trim(newSheetName))

  end subroutine ExcelSetSheetName
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetSheetName
!  NAME
!  ExcelGetSheetName: This function retrieves the name of the active worksheet.
!  DESCRIPTION
!  This function is used to retrieve the name of the active worksheet. The 
!  return value is the name of the sheet, but should usually be "trimmed" to
!  remove trailing whitespace. For example, this is how this function will often
!  be called: trim(ExcelGetSheetName()).
!  RETURNS
!  Character string (of length 10000): Sheet Name of active worksheet.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetSheetName() result(rv)

!  ARGUMENTS
!  [None]
!****

    rv = $Worksheet_GetName(worksheet, status)
    call CheckStatus(status, " Unable to get worksheet name")

  end function ExcelGetSheetName
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelSwitchToSheet
!  NAME
!  ExcelSwitchToSheet: This function is used to switch between worksheets of a
!  workbook.
!  DESCRIPTION
!  The argument 'sheetName' represents the name of the sheet that should be
!  switched to. It is case sensitive. This function returns 0 if it successfully
!  switched to the specified sheet. Otherwise it returns a non-zero value.
!
!  As an example, assume 'retVal', 'num1', and 'num2' are declared to be
!  integers, and that the workbook has 2 sheets in it, "Sheet1" and "Sheet2":
!     retVal = ExcelSwitchToSheet("Sheet1")
!     num1 = trim(AscToInt(ExcelGetStringFromCell("A1")))
!     retVal = ExcelSwitchToSheet("Sheet2")
!     num2 = trim(AscToInt(ExcelGetStringFromCell("A1")))
!  Then 'num1' would hold the integer from cell A1 of "Sheet1", and 'num2' would
!  hold the integer from cell A1 of "Sheet2". Ideally, it would be wise to check
!  'retVal' after each call to make sure it is set to 0.
!  RETURNS
!  Integer: Returns 0 if switch was successful and a non-zero value if an error
!  occurred.
!  SYNOPSIS

  integer function ExcelSwitchToSheet(sheetName) result(rv)

!  ARGUMENTS
!  * sheetName: Name of the sheet that should be switched to.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: sheetName
!****
    character (len = MAX_CHAR) :: thisSheetName
    integer :: numSheets, sheetNum

    rv = 1
    numSheets = ExcelGetNumSheets()
    do sheetNum = 1, numSheets
      vInt1%VU%LONG_VAL = sheetNum
      thisSheetName = &
        trim($Worksheet_GetName(Worksheets_GetItem(worksheets, vInt1), status))
      call CheckStatus(status, " Unable to get worksheet name")
      if (trim(thisSheetName) == trim(sheetName)) then
        worksheet = Worksheets_GetItem(worksheets, vInt1, status)
        call CheckStatus(status, " Unable to get worksheet " // trim(sheetName))
        call $Worksheet_Activate(worksheet, status)
        call CheckStatus(status, " Unable to activate worksheet " // trim(sheetName))
        rv = 0
        exit
      end if
    end do

    ! NOTE: This works very nicely except for one detail: If the sheetName
    ! does not match an actual sheet name, then the program crashes nastily
    ! with no way to control it. Even though there is a status variable
    ! passed in, the Worksheets_GetItem() crashes before returning.
    !vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(sheetName))
    !worksheet = Worksheets_GetItem(worksheets, vBSTR1, status)
    !call CheckStatus(status, " Unable to get worksheet " // trim(sheetName))

  end function ExcelSwitchToSheet
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetNumSheets
!  NAME
!  ExcelGetNumSheets: This function retrieves the number of worksheets in the
!  workbook.
!  DESCRIPTION
!  This function is used to retrieve the number of worksheets in the workbook.
!  RETURNS
!  Integer: Number of worksheets.
!  SYNOPSIS

  integer function ExcelGetNumSheets() result(numSheets)

!  ARGUMENTS
!  [None]
!*****

    numSheets = Worksheets_GetCount(worksheets, status)
    call CheckStatus(status, " Unable to get number of worksheet")

  end function ExcelGetNumSheets
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelAddSheet
!  NAME
!  ExcelAddSheet: This subroutine will insert a worksheet in the workbook.
!  DESCRIPTION
!  The worksheet will be inserted immediately before the current active
!  worksheet, and will be made into the new active worksheet. If 'newSheetName'
!  is passed in, the new sheet will be given the specified name, otherwise
!  Excel will use its default naming convention for new sheets (e.g., 'Sheet1',
!  'Sheet2', etc...).
!  SYNOPSIS

  subroutine ExcelAddSheet(newSheetName)

!  ARGUMENTS
!  * newSheetName: (optional) Name of the new worksheet.
!  ARGUMENT DECLARATIONS
    character (len = *), optional, intent(in) :: newSheetName
!*****

    worksheet = Worksheets_Add(worksheets, $STATUS = status)
    call CheckStatus(status, " Unable to add a worksheet")

    if (present(newSheetName)) then
      call ExcelSetSheetName(newSheetName)
    end if

  end subroutine ExcelAddSheet
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelDeleteSheet
!  NAME
!  ExcelDeleteSheet: This subroutine deletes the current active worksheet from
!  the workbook.
!  DESCRIPTION
!  The worksheet that immediately follows the deleted worksheet is the new
!  active worksheet. If there is no "immediately following" worksheet (because
!  the deleted worksheet was the last one in the workbook, then the one that
!  immediately precedes the deleted worksheet will become the new active
!  worksheet. If there are no worksheets left in the workbook, then an error
!  message will be displayed and the program will stop.
!  SYNOPSIS

  subroutine ExcelDeleteSheet()

!  ARGUMENTS
!  [None]
!****

    logical(2) :: origDisplayAlerts

    ! Check to make sure there is at least one other worksheet in this
    ! workbook. Excel gets upset if a workbook has no remaining sheets!
    if (ExcelGetNumSheets() == 1) then
      write(6, '(A)') "Error in ExcelDeleteSheet. Can not delete last " // &
        "sheet of the workbook."
      stop
    end if

    ! Save original settings for DisplayAlerts, so that it can be restored
    ! at the end of this subroutine.
    origDisplayAlerts = $Application_GetDisplayAlerts(excelapp, status)
    call CheckStatus(status, " Unable to get DisplayAlerts")

    ! Turn off DisplayAlerts, otherwise the deletion will silently fail.
    call $Application_SetDisplayAlerts(excelapp, .false._2, status)
    call CheckStatus(status, " Unable to set DisplayAlerts")

    ! Delete the current worksheet.
    call $Worksheet_Delete(worksheet, status)
    call CheckStatus(status, " Unable to delete a worksheet")

    ! Set the new current worksheet to be the new active sheet.
    worksheet = $Workbook_GetActiveSheet(workbook, status)
    call CheckStatus(status, " Unable to get worksheet object")

    ! Restore DisplayAlerts back to its original settings.
    call $Application_SetDisplayAlerts(excelapp, origDisplayAlerts, status)
    call CheckStatus(status, " Unable to set DisplayAlerts")

  end subroutine ExcelDeleteSheet
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelSave
!  NAME
!  ExcelSave: This subroutine is used to save the excel workbook.
!  DESCRIPTION
!  If the 'saveAsFileName' argument is not passed in, then it saves the workbook
!  by its default name (the one by which it was opened). If it is passed in,
!  then that is the name it uses to save the workbook. The file name can be
!  either a relative path name or an absolute path name. If 'saveAsFileName' is
!  not passed in, then 'warnBeforeOverwrite' has no effect. If 'saveAsFileName'
!  is passed in, then 'warnBeforeOverwrite' determines whether or not Excel
!  should warn the user (via a pop-up dialog box) that a file already exists
!  with the same name as the argument 'saveAsFileName'. (Of course, if a file
!  with the same name does not already exist, then no pop-up dialog box will
!  appear.) If 'warnBeforeOverwrite' is omitted, then the previous settings in
!  Excel will determine whether or not to a warning is given. Usually the
!  settings are set to give the warning.
!  SYNOPSIS
  subroutine ExcelSave(saveAsFileName, warnBeforeOverwrite)

!  ARGUMENTS
!  * saveAsFileName: (optional) Name used to save the workbook.
!  * warnBeforeOverwrite: (optional) Warns before overwriting an existing file.
!  ARGUMENT DECLARATIONS
    character (len = *), optional, intent(in) :: saveAsFileName
    logical, optional, intent(in) :: warnBeforeOverwrite
!*****
    logical(2) :: origDisplayAlerts, newDisplayAlerts
    character (len = MAX_CHAR) :: fullSaveAsFileName
    integer :: retVal

    if (present(saveAsFileName)) then
      retVal = GetFullPath(saveAsFileName, fullSaveAsFileName)
      if (retVal == 0) then
        write(6, '(A)') "Error in ExcelSave for file: " // &
          trim(saveAsFileName) // ". Could not find full path name."
        stop
      end if
      vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(fullSaveAsFileName)
      if (present(warnBeforeOverwrite)) then
        origDisplayAlerts = $Application_GetDisplayAlerts(excelapp, status)
        call CheckStatus(status, " Unable to get displayAlerts")
        newDisplayAlerts = warnBeforeOverwrite
        call $Application_SetDisplayAlerts(excelapp, newDisplayAlerts, status)
        call CheckStatus(status, " Unable to set new displayAlerts")
      end if
      call $Workbook_SaveAs(workbook, vBSTR1, $STATUS = status)
      call CheckStatus(status, " Unable to save workbook " // &
        trim(saveAsFileName))
      if (present(warnBeforeOverwrite)) then
        call $Application_SetDisplayAlerts(excelapp, origDisplayAlerts, status)
        call CheckStatus(status, " Unable to set original displayAlerts")
      end if
    else
      call $Workbook_Save(workbook, $STATUS = status)
      call CheckStatus(status, " Unable to save workbook")
    end if

  end subroutine ExcelSave
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetWorkbook
!  NAME
!  ExcelGetWorkbook: This function switches workbooks if more than one workbook
!  is opened.
!  DESCRIPTION
!  This function is used when keeping more than 1 workbook open at a time.
!  The workbooks should be opened with calls to 'ExcelBegin' or 'ExcelBeginNew'.
!  The order in which the workbooks are opened represents the workbook number
!  of the workbook. So if 2 workbooks are opened, the first one that was
!  opened would be 1, and the second one would be 2. Then, to switch between the
!  workbooks, use this function. The argument 'workbookNum' represents the
!  workbook that you want to access. The returned value is 0 if it successfully
!  accessed the workbook, otherwise non-zero.
!
!  For example, assume 'retVal', 'num1', and 'num2' are declared to be integers,
!  and that 2 workbooks have been opened:
!     retVal = ExcelGetWorkbook(1)
!     num1 = trim(AscToInt(ExcelGetStringFromCell("A1")))
!     retVal = ExcelGetWorkbook(2)
!     num2 = trim(AscToInt(ExcelGetStringFromCell("A1")))
!  Then 'num1' would hold the integer from cell A1 of the first workbook,
!  and 'num2' would hold the integer from cell A1 of the second workbook.
!  Ideally, it would be wise to check 'retVal' after each call to make
!  sure it is set to 0.
!  RETURNS
!  Integer: The returned value is 0 if it successfully accessed the workbook,
!           otherwise non-zero.
!  SYNOPSIS

  integer function ExcelGetWorkbook(workbookNum) result(rv)

!  ARGUMENTS
!  * workbookNum: The order in which the workbooks were opened.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: workbookNum
!*****
    integer :: numWorkbooks
    rv = 0

    ! Safety check.
    numWorkbooks = Workbooks_GetCount(workbooks, status)
    call CheckStatus(status, " Unable to get number of workbooks")
    if (workbookNum < 1 .or. workbookNum > numWorkbooks) then
      rv = 1
      return
    end if

    vInt1%VU%LONG_VAL = workbookNum
    workbook = Workbooks_GetItem(workbooks, vInt1, status)
    call CheckStatus(status, " Unable to get workbook")

    worksheets = $Workbook_GetWorksheets(workbook, status)
    call CheckStatus(status, " Unable to get worksheets object")

    worksheet = $Workbook_GetActiveSheet(workbook, status)
    call CheckStatus(status, " Unable to get worksheet object")

  end function ExcelGetWorkbook
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelIsVisible
!  NAME
!  ExcelIsVisible: This subroutine is used to turn on or off the "visibility" 
!  of Excel.
!  DESCRIPTION
!  By defualt, when 'BeginExcel' is used to open an Excel workbook, the
!  visibility is off. This means that Excel is running as a background process,
!  with no window for the user to see. If 'isVisible' is true, then this makes
!  Excel visible. This can be very useful for debugging purposes. If 'isVisible'
!  is false then it makes Excel not visible.
!  SYNOPSIS

  subroutine ExcelIsVisible(isVisible)

!  ARGUMENTS
!  * isVisible: Turns Excel "visibility" on.
!  ARGUMENT DECLARATIONS
    logical, intent(in) :: isVisible
!*****

    ! Can't use isVisible in the call to $Application_SetVisible, because
    ! that subroutine expects a logical(2) instead of a default logical.
    if (isVisible) then
      call $Application_SetVisible(excelapp, .true._2, status)
      call CheckStatus(status, " Unable to set visible to true")
    else
      call $Application_SetVisible(excelapp, .false._2, status)
      call CheckStatus(status, " Unable to set visible to false")
    end if

  end subroutine ExcelIsVisible
!******************************************************************************
  integer function ColStrToNum(colStr) result(num)

    character (len = *), intent(in) :: colStr

    character (len = len(colStr)) :: str
    integer :: placeValue
    integer :: pos

    ! Start with the right-most "digit" (i.e., letter, which represents a
    ! digit) as the ones place. As we move leftward, multiply the place value
    ! by 26 for each additional digit.
    str = adjustl(colStr)
    placeValue = 1
    num = 0
    do pos = len_trim(str), 1, -1
      num = num + placeValue * (ichar(str(pos:pos)) - ichar("A") + 1)
      placeValue = placeValue * 26
    end do

  end function ColStrToNum
!******************************************************************************
  character (len = MAX_CHAR) function NumToColStr(num) result(colStr)

    integer, intent(in) :: num

    integer :: currNum

    currNum = num
    colStr = ""
    do while (currNum > 0)
      ! Pre-pend the new "digit" (i.e., letter) to colStr.
      colStr = char(mod(currNum - 1, 26) + ichar("A")) // trim(colStr)
      ! Integer division on purpose.
      ! Remove a factor of 26, which represents the digit that was just
      ! pre-pended to colStr.
      currNum = (currNum - 1) / 26
    end do

  end function NumToColStr
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetNextCellRight
!  NAME
!  ExcelGetNextCellRight: This function is used to move to the "next" cell to
!  the right.
!  DESCRIPTION
!  If 'arrayRange' is omitted, then the cell to the right of 'cell' is returned.
!  If 'cell' was already at the right-most column, then a blank string is
!  returned. If 'arrayRange' is present, then the cell to the right of 'cell'
!  is returned, unless it is outside of the 'arrayRange'. If so, then the
!  left-most cell of the next row within that range is returned. If 'cell'
!  is the bottom-right cell in the range, then a blank cell is returned.
!
!  Examples:
!  * ExcelGetNextCellRight("B3") will return the string "C3" (padded on the
!    right with blanks).
!  * ExcelGetNextCellRight("XFD3") will return an empty string.
!  * ExcelGetNextCellRight("B3", "A1:E5") would return "C3" (padded on the right
!    with blanks).
!  * ExcelGetNextCellRight("E3", "A1:E5") will return "A4" (padded with blanks
!    on the right).
!  RETURNS
!  Character string of length 10000: "Next" cell to the right.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetNextCellRight(cell, arrayRange) &
    result (nextCell)

!  ARGUMENTS
!  * cell: This function moves one cell to the right of 'cell'.
!  * arrayRange: (optional) Range of cells to stay in while moving to the right.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cell
    character (len = *), optional, intent(in) :: arrayRange
!*****
    integer :: colNum, rowNum, maxColNum

    if (.not. CheckCell(cell, arrayRange)) then
      call AbortGetNextCell("Right", cell, arrayRange)
    end if

    colNum = ExcelGetColNumFromCell(cell)
    rowNum = ExcelGetRowNumFromCell(cell)
    if (present(arrayRange)) then
      maxColNum = Col2FromRange(arrayRange)
      if (colNum == maxColNum) then
        rowNum = rowNum + 1
        colNum = Col1FromRange(arrayRange) - 1
      end if
    end if
    colNum = colNum + 1
    nextCell = trim(ColNumRowNumToCell(colNum, rowNum))

    if (.not. CheckCell(nextCell, arrayRange)) then
      nextCell = ""
    end if

  end function ExcelGetNextCellRight
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetNextCellLeft
!  NAME
!  ExcelGetNextCellLeft: This function is used to move to the "next" cell to
!  the left.
!  DESCRIPTION
!  This function is the same as 'ExcelGetNextCellRight' (see its documentation)
!  except that it moves to the left (and then up) instead of to the right
!  (and then down).
!  RETURNS
!  Character string of length 10000: "Next" cell to the left.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetNextCellLeft(cell, arrayRange) &
    result (nextCell)

!  ARGUMENTS
!  * cell: This function moves one cell to the left of 'cell'.
!  * arrayRange: (optional) Range of cells to stay in while moving to the left.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cell
    character (len = *), optional, intent(in) :: arrayRange
!*****
    integer :: colNum, rowNum, minColNum

    if (.not. CheckCell(cell, arrayRange)) then
      call AbortGetNextCell("Left", cell, arrayRange)
    end if

    colNum = ExcelGetColNumFromCell(cell)
    rowNum = ExcelGetRowNumFromCell(cell)
    if (present(arrayRange)) then
      minColNum = Col1FromRange(arrayRange)
      if (colNum == minColNum) then
        rowNum = rowNum - 1
        colNum = Col2FromRange(arrayRange) + 1
      end if
    end if
    colNum = colNum - 1
    nextCell = trim(ColNumRowNumToCell(colNum, rowNum))

    if (.not. CheckCell(nextCell, arrayRange)) then
      nextCell = ""
    end if

  end function ExcelGetNextCellLeft
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetNextCellDown
!  NAME
!  ExcelGetNextCellDown: This function is used to move to the "next" cell down.
!  DESCRIPTION
!  This function is the same as 'ExcelGetNextCellRight' (see its documentation)
!  except that it moves down (and then to the right) instead of to the right
!  (and then down).
!  RETURNS
!  Character string of length 10000: "Next" cell down.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetNextCellDown(cell, arrayRange) &
    result (nextCell)

!  ARGUMENTS
!  * cell: This function moves one cell down of 'cell'.
!  * arrayRange: (optional) Range of cells to stay in while moving down.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cell
    character (len = *), optional, intent(in) :: arrayRange
!*****
    integer :: colNum, rowNum, maxRowNum

    if (.not. CheckCell(cell, arrayRange)) then
      call AbortGetNextCell("Down", cell, arrayRange)
    end if

    colNum = ExcelGetColNumFromCell(cell)
    rowNum = ExcelGetRowNumFromCell(cell)
    if (present(arrayRange)) then
      maxRowNum = Row2FromRange(arrayRange)
      if (rowNum == maxRowNum) then
        colNum = colNum + 1
        rowNum = Row1FromRange(arrayRange) - 1
      end if
    end if
    rowNum = rowNum + 1
    nextCell = trim(ColNumRowNumToCell(colNum, rowNum))

    if (.not. CheckCell(nextCell, arrayRange)) then
      nextCell = ""
    end if

  end function ExcelGetNextCellDown
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetNextCellUp
!  NAME
!  ExcelGetNextCellUp: This function is used to move to the "next" cell up.
!  DESCRIPTION
!  This function is the same as 'ExcelGetNextCellRight' (see its documentation)
!  except that it moves up (and then to the left) instead of to the right
!  (and then down).
!  RETURNS
!  Character string of length 10000: "Next" cell up.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetNextCellUp(cell, arrayRange) &
    result (nextCell)

!  ARGUMENTS
!  * cell: This function moves one cell up of 'cell'.
!  * arrayRange: (optional) Range of cells to stay in while moving up.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cell
    character (len = *), optional, intent(in) :: arrayRange
!*****
    integer :: colNum, rowNum, minRowNum

    if (.not. CheckCell(cell, arrayRange)) then
      call AbortGetNextCell("Up", cell, arrayRange)
    end if

    colNum = ExcelGetColNumFromCell(cell)
    rowNum = ExcelGetRowNumFromCell(cell)
    if (present(arrayRange)) then
      minRowNum = Row1FromRange(arrayRange)
      if (rowNum == minRowNum) then
        colNum = colNum - 1
        rowNum = Row2FromRange(arrayRange) + 1
      end if
    end if
    rowNum = rowNum - 1
    nextCell = trim(ColNumRowNumToCell(colNum, rowNum))

    if (.not. CheckCell(nextCell, arrayRange)) then
      nextCell = ""
    end if

  end function ExcelGetNextCellUp
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetRowNumFromCell
!  NAME
!  ExcelGetRowNumFromCell: This function returns the row number of 'cell'.
!  DESCRIPTION
!  This function returns the row number of the cell 'cell', where the cell is
!  passed in as a character string. For example, ExcelGetRowNumFromCell("D25")
!  would return 25.
!  RETURNS
!  Integer: Row number of 'cell'.
!  SYNOPSIS

  integer function ExcelGetRowNumFromCell(cell) result(rowNum)

!  ARGUMENTS
!  * cell: This function returns the row number of 'cell'.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cell
!*****
    integer  :: ind

    ind = scan(cell, ALL_DIGITS)
    rowNum = AscToInt(cell(ind:))

  end function ExcelGetRowNumFromCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetColNumFromCell
!  NAME
!  ExcelGetColNumFromCell: This function returns the column number of 'cell'.
!  DESCRIPTION
!  This function returns the column number of the cell 'cell', where the cell
!  is passed in as a character string. For example,
!  ExcelGetColNumFromCell("D25") would return 4, since the column is "D", and
!  that is the 4th column.
!  RETURNS
!  Integer: Column number of 'cell'.
!  SYNOPSIS

  integer function ExcelGetColNumFromCell(cell) result(colNum)

!  ARGUMENTS
!  * cell: This function returns the column number of 'cell'.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cell
!*****
    character (len = MAX_CHAR) :: colStr
    integer  :: ind

    ind = scan(cell, ALL_DIGITS)
    colStr = cell(1:ind - 1)
    colNum = ColStrToNum(trim(colStr))

  end function ExcelGetColNumFromCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetColStrFromCell
!  NAME
!  ExcelGetColStrFromCell: This function returns the column, as a string,
!  of 'cell'.
!  DESCRIPTION
!  This function returns the column, as a string, of the cell 'cell', where the
!  cell is passed in as a character string. For example,
!  'ExcelGetColStrFromCell("D25") would return "D".
!  RETURNS
!  Character string of length 10000: Column value of 'cell'.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetColStrFromCell(cell) &
    result(colStr)

!  ARGUMENTS
!  * cell: This function returns the column of 'cell'.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: cell
!*****
    integer  :: ind

    ind = scan(cell, ALL_DIGITS)
    colStr = cell(1:ind - 1)

  end function ExcelGetColStrFromCell
!******************************************************************************
!****f* ExcelInterfaceMod/ExcelGetCellFromRange
!  NAME
!  ExcelGetCellFromRange: This function returns either the first or the second
!  cell in an Excel range.
!  DESCRIPTION
!  This function returns either the first or the second cell in an Excel range
!  'arrRange', where the range is passed in as a character string. The 'cellNum'
!  variable must either be a 1 or a 2, which tells the function which cell to
!  return. For example, ExcelGetCellFromRange("B5:D25", 1) would return "B5",
!  and ExcelGetCellFromRange("B5:D25", 2) would return "D25". Passing in
!  a 'cellNum' other than 1 or 2 will terminate the program after printing an
!  error message.
!  RETURNS
!  Character string of length 10000: First or second cell of 'arrRange'.
!  SYNOPSIS
!  MAX_CHAR is set equal to 10,000.
  character (len = MAX_CHAR) function ExcelGetCellFromRange(arrRange, &
    cellNum) result(cell)

!  ARGUMENTS
!  * arrRange: Range of cells in Excel.
!  * cellNum: Cell to return, either 1 or 2.
!  ARGUMENT DECLARATIONS
    character (len = *), intent(in) :: arrRange
    integer, intent(in) :: cellNum
!*****
    integer :: ind

    select case(cellNum)
      case (1)
        ind = index(arrRange, ":")
        cell = arrRange(1:ind - 1)
      case (2)
        ind = index(arrRange, ":")
        cell = arrRange(ind + 1:)
      case default
        write(6, '(A)') "Error in ExcelGetCellFromRange. The 'cellNum' " // &
          "must be a 1 or 2, but was: " // trim(IntToAsc(cellNum))
        stop
    end select

  end function ExcelGetCellFromRange
!******************************************************************************
  integer function Col1FromRange(arrRange) result(col)

    character (len = *), intent(in) :: arrRange
    character (len = MAX_CHAR) :: colStr

    colStr = arrRange(1:scan(arrRange, ALL_DIGITS) - 1)
    col = ColStrToNum(trim(colStr))

  end function Col1FromRange
!******************************************************************************
  integer function Row1FromRange(arrRange) result(row)

    character (len = *), intent(in) :: arrRange

    row = &
      AscToInt(arrRange(scan(arrRange, ALL_DIGITS):index(arrRange, ":") - 1))

  end function Row1FromRange
!******************************************************************************
  integer function Col2FromRange(arrRange) result(col)

    character (len = *), intent(in) :: arrRange
    integer :: cell2Ind
    character (len = MAX_CHAR) :: colStr

    cell2Ind = index(arrRange, ":") + 1
    colStr = arrRange(cell2ind:verify(trim(arrRange), ALL_DIGITS, .true.))
    col = ColStrToNum(trim(colStr))

  end function Col2FromRange
!******************************************************************************
  integer function Row2FromRange(arrRange) result(row)

    character (len = *), intent(in) :: arrRange

    row = AscToInt(arrRange(verify(trim(arrRange), ALL_DIGITS, .true.) + 1:))

  end function Row2FromRange
!******************************************************************************
  character (len = MAX_CHAR) function ColNumRowNumToCell(colNum, rowNum) &
    result(cell)

    integer, intent(in) :: colNum, rowNum

    cell = trim(NumToColStr(colNum)) // trim(IntToAsc(rowNum))

  end function ColNumRowNumToCell
!******************************************************************************
  type(CellRangePartsT) function CellRangeToParts(cellsAsString) result(parts)

    character (len = *), intent(in) :: cellsAsString
    character (len = MAX_CHAR) :: fullCell

    ! To make things simpler, as far as getting row1, row2, etc..., we'll
    ! make sure we have a range, by converting even a single cell into
    ! a range. For example, "A1" becomes "A1:A1".
    if (index(cellsAsString, ":") == 0) then
      fullCell = trim(cellsAsString) // ":" // trim(cellsAsString)
    else
      fullCell = cellsAsString
    end if

    parts.row1 = Row1FromRange(fullCell)
    parts.row2 = Row2FromRange(fullCell)
    parts.col1 = Col1FromRange(fullCell)
    parts.col2 = Col2FromRange(fullCell)

  end function CellRangeToParts
!******************************************************************************
  integer function GetRangeSize1D(rangeParts) result(rangeSize)

    type(CellRangePartsT), intent(in) :: rangeParts

    rangeSize = max(rangeParts.row2 - rangeParts.row1 + 1, &
                    rangeParts.col2 - rangeParts.col1 + 1)

  end function GetRangeSize1D
!******************************************************************************
  subroutine CheckRange1D(rangeParts, arrSize)

    type(CellRangePartsT), intent(in) :: rangeParts
    integer, optional, intent(in) :: arrSize
    integer :: rangeSize

    if (rangeParts.row1 /= rangeParts.row2 .and. &
        rangeParts.col1 /= rangeParts.col2) then
      write(6, '(A)') "Error in cell range: " // &
        "Illegal cell range -- must be 1-dimensional or single cell."
      stop
    end if

    if (present(arrSize)) then
      rangeSize = GetRangeSize1D(rangeParts)
      if (rangeSize /= arrSize) then
        write(6, '(A)') "Error in cell range: " // &
          "Expected (based on cell range) size of array to be " // &
          trim(IntToAsc(rangeSize)) // ", but found it to be " // &
          trim(IntToAsc(arrSize))
        stop
      end if
    end if

  end subroutine CheckRange1D
!******************************************************************************
  logical function CheckCell(cell, arrRange) result(check)

    character (len = *), intent(in) :: cell
    character (len = *), optional, intent(in) :: arrRange
    integer :: excelCol1, excelCol2, excelRow1, excelRow2
    integer :: cellCol, cellRow

    if (trim(cell) == "") then
      check = .false.
      return
    end if

    if (present(arrRange)) then
      excelCol1 = Col1FromRange(arrRange)
      excelCol2 = Col2FromRange(arrRange)
      excelRow1 = Row1FromRange(arrRange)
      excelRow2 = Row2FromRange(arrRange)
    else
      excelCol1 = FIRST_COL
      excelCol2 = LAST_COL
      excelRow1 = FIRST_ROW
      excelRow2 = LAST_ROW
    end if

    cellCol = ExcelGetColNumFromCell(cell)
    cellRow = ExcelGetRowNumFromCell(cell)

    if (cellCol < excelCol1 .or. cellCol > excelCol2 .or. &
        cellRow < excelRow1 .or. cellRow > excelRow2) then
      check = .false.
    else
      check = .true.
    end if

  end function CheckCell
!******************************************************************************
  subroutine AbortGetNextCell(direction, cell, arrayRange)

    character (len = *), intent(in) :: direction, cell
    character (len = *), optional, intent(in) :: arrayRange

    write(6, '(A)') "Error in GetNextCell" // trim(direction) // &
      ", illegal cell: " // trim(cell)
    if (present(arrayRange)) then
      write(6, '(A)') "  which is not in the range: " // trim(arrayRange)
    end if
    stop

  end subroutine
!******************************************************************************
  integer function ExcelTestFunction() result(rv)

    type (VARIANT) :: vInt_MM
    type (VARIANT) :: vReal_MM
    type (VARIANT) :: vDouble_MM
    type (VARIANT) :: vPtr_MM
	  TYPE (VARIANT) :: vRange
    character (len = 10000) :: str
    double precision :: dp_rv
    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR("VLookup")
    vBSTR2%VU%PTR_VAL = ConvertStringToBSTR("Day 1")
    !vBSTR2%VU%PTR_VAL = ConvertStringToBSTR("Day 3")
    vBSTR3%VU%PTR_VAL = ConvertStringToBSTR("B4")
    vBSTR4%VU%PTR_VAL = ConvertStringToBSTR("1")
    vInt2%VU%LONG_VAL = 1
    vBool1%VU%BOOL_VAL = .false.

    range = $Worksheet_GetRange(worksheet, vBSTR3, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")

    !rv = ConvertBSTRtoString(vBSTR1%VU%PTR_VAL, str)
    !write(6, '(A)') "Inside1: " // trim(str)
    !vInt1 = $Application__WSFunction(excelapp, vBSTR1, vBSTR2, vBSTR3, vInt2, $STATUS = status)
    !rv = vInt1%VU%LONG_VAL

    !vBSTR10 = WorksheetFunction_Vlookup(worksheet, vBSTR2, vBSTR3, vInt2, vBool1, $STATUS = status)
    !rv = ConvertBSTRtoString(vBSTR10%VU%PTR_VAL, str)
    !write(6, '(A)') "Inside1: " // trim(str)
    !write(6, '(A, A)') "Inside2: ", vBSTR10%VU%PTR_VAL
    !rv = ConvertBSTRtoString(vBSTR10%VU%PTR_VAL, str)
    !write(6, '(A)') "Inside: " // trim(str)
    !rv = vInt1%VU%LONG_VAL
    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim("B6"))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    !vInt2%VU%LONG_VAL = range
    !dp_rv = WorksheetFunction_Sum(worksheet, vBSTR1, $STATUS = status)

    call VariantInit(vInt_MM)
    vInt_MM%VT = VT_I4
    vInt_MM%VU%LONG_VAL = 111
    dp_rv = WorksheetFunction_Sum(excelapp, vInt_MM, $STATUS = status)
    call CheckStatus(status, " Unable to SUM")
    status = VariantClear(vInt_MM)
    write(6, '(F20.6)') dp_rv

    call VariantInit(vInt_MM)
    vInt_MM%VT = VT_I4
    vInt_MM%VU%LONG_VAL = 112
    dp_rv = WorksheetFunction_Sum(excelapp, vInt_MM, $STATUS = status)
    call CheckStatus(status, " Unable to SUM")
    status = VariantClear(vInt_MM)
    write(6, '(F20.6)') dp_rv

    call VariantInit(vReal_MM)
    vReal_MM%VT = VT_R4
    vReal_MM%VU%FLOAT_VAL = 113.0
    dp_rv = WorksheetFunction_Sum(excelapp, vReal_MM, $STATUS = status)
    call CheckStatus(status, " Unable to SUM")
    status = VariantClear(vReal_MM)
    write(6, '(F20.6)') dp_rv

    call VariantInit(vDouble_MM)
    vDouble_MM%VT = VT_R8
    vDouble_MM%VU%DOUBLE_VAL = 114.0d0
    dp_rv = WorksheetFunction_Sum(excelapp, vDouble_MM, $STATUS = status)
    call CheckStatus(status, " Unable to SUM")
    status = VariantClear(vDouble_MM)
    write(6, '(F20.6)') dp_rv

    call VariantInit(vPtr_MM)
    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR("B6")
    range = $Application_GetRange(excelapp, vBSTR1, $STATUS = status)
    !range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    call CheckStatus(status, " Unable to get RANGE object")
    !rv = put_variant_array(range, vPtr_MM)
    vPtr_MM%VT = VT_INT_PTR
    vPtr_MM%VU%LONG_VAL = range
    !vPtr_MM%VU%PTR_VAL = range
    !dp_rv = WorksheetFunction_Sum(worksheet, vPtr_MM, $STATUS = status)
    dp_rv = WorksheetFunction_Sum(excelapp, vPtr_MM, $STATUS = status)
    call CheckStatus(status, " Unable to SUM")
    status = VariantClear(vPtr_MM)
    write(6, '(F20.6)') dp_rv

   ! call VariantInit(vRange)
   ! vBSTR1%VU%PTR_VAL = ConvertStringToBSTR("B6")
   ! range = $Worksheet_GetRange(worksheet, vBSTR1, vBSTR1, $STATUS = status)
   ! call CheckStatus(status, " Unable to get RANGE object")
	  !vRange = Range_Activate(range, status)
	  !!vRange = Range_Select(range, status)
   ! call CheckStatus(status, " Unable to select RANGE")
   ! !call VariantInit(vPtr_MM)
   ! !vPtr_MM%VT = VT_UNKNOWN
   ! !vPtr_MM%VU%PTR_VAL = ConvertStringToBSTR("B6")
   ! !vPtr_MM%VU%PTR_VAL = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
   ! !dp_rv = WorksheetFunction_Sum(excelapp, vBSTR1, $STATUS = status)
   ! dp_rv = WorksheetFunction_Sum(excelapp, vRange, $STATUS = status)
   ! !dp_rv = WorksheetFunction_Sum(excelapp, vPtr_MM, $STATUS = status)
   ! call CheckStatus(status, " Unable to SUM")
   ! status = VariantClear(vPtr_MM)
   ! status = VariantClear(vRange)
   ! write(6, '(F20.6)') dp_rv
    
    rv = 0

  end function ExcelTestFunction
!******************************************************************************
  double precision function ExcelTestSumFunction(cellsAsString) result(rv)

    character (len = *), intent(in) :: cellsAsString

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(cellsAsString))
    range = $Worksheet_GetRange(worksheet, vBSTR1, $STATUS = status)
    vDisp1%VU%PTR_VAL = range
    rv = WorksheetFunction_Sum(excelapp, vDisp1, $STATUS = status)

  end function ExcelTestSumFunction
!******************************************************************************
  double precision function ExcelTestVLookupFunction_DP(lookupValue, &
    tableArray, colIndNum) result(rv)

    character (len = *), intent(in) :: lookupValue
    character (len = *), intent(in) :: tableArray
    integer, intent(in) :: colIndNum

    vBSTR1%VU%PTR_VAL = ConvertStringToBSTR(trim(lookupValue))
    vBSTR2%VU%PTR_VAL = ConvertStringToBSTR(trim(tableArray))
    range = $Worksheet_GetRange(worksheet, vBSTR2, $STATUS = status)
    vDisp1%VU%PTR_VAL = range
    vInt1%VU%LONG_VAL = colIndNum
    vDouble1 = WorksheetFunction_VLookup(excelapp, vBSTR1, vDisp1, vInt1, &
      $STATUS = status)
    rv = vDouble1%VU%DOUBLE_VAL

  end function ExcelTestVLookupFunction_DP
!******************************************************************************

end module ExcelInterfaceMod
