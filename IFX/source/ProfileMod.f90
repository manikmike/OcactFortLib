! $Id: ProfileMod.f90 1.11 2012/06/22 10:45:43EDT 622325 Development  $
module ProfileMod

!****h* source/ProfileMod
!  NAME
!  ProfileMod: OcactFortLib ProfileMod module.
!  REVISION
!  $Revision: 1.11 $
!  MODULE DESCRIPTION
!  The ProfileMod contains subroutines and functions used to profile a program.
!  Checkpoints are set in the program to count how many times the program
!  hits each checkpoint and times how long it takes to get from one checkpoint
!  to the next. Before using this module, call the ProfileInit subroutine to
!  initialize the ProfileMod. Near the end of the program, call one (or more)
!  of the ProfileWrite... subroutines to output the results of the profiling.
!  MODULE ROUTINES
!  ProfileInit
!  ProfileNameCheckpointTransition
!  ProfileCheckpoint
!  ProfileGetTotalTime
!  ProfileWriteCheckpointTransitionReport
!  ProfileWriteCheckpointCountReport
!  ProfileWriteFullReport
!*****

  use FilesMod

  implicit none
  save

  private
  public :: ProfileInit, ProfileNameCheckpointTransition, ProfileCheckpoint
  public :: ProfileGetTotalTime
  public :: ProfileWriteCheckpointTransitionReport
  public :: ProfileWriteCheckpointCountReport, ProfileWriteFullReport

  integer, parameter :: MAX_CHAR = 1000
  integer, parameter :: MAX_NUM_CHECKPOINTS = 50
  integer, parameter :: MAX_NAME_LEN = 29

  ! The first time ProfileCheckpoint(n) is called (for any n, not each n),
  ! there is really no transitions. So we call this a transition from 0 to n.
  ! Thus the first dimension of this array starts at 0, but the second one
  ! doesn't. When we print out the times, we only loop from 1 on, so we
  ! don't need to worry about the junk value in this first transition from
  ! 0 to n.
  double precision, dimension(0:MAX_NUM_CHECKPOINTS, MAX_NUM_CHECKPOINTS) :: &
    checkpointTimes
  ! The name of each checkpoint (actually, of each checkpoint transition).
  character (len = MAX_NAME_LEN), &
    dimension(MAX_NUM_CHECKPOINTS, MAX_NUM_CHECKPOINTS) :: checkpointNames
  ! The number of clock counts per second.
  double precision :: countRate
  ! The maximum number of clock counts until the system clock resets.
  integer :: countMax
  ! The number of times each checkpoint was hit.
  integer, dimension(MAX_NUM_CHECKPOINTS) :: numCheckpointHits
  ! The number of times each checkpoint transition was hit.
  integer, dimension(0:MAX_NUM_CHECKPOINTS, MAX_NUM_CHECKPOINTS) :: &
     numCheckpointTransitionHits

contains

!******************************************************************************
!****f* ProfileMod/ProfileInit
!  NAME
!  ProfileInit: Initializes ProfileMod.
!  DESCRIPTION
!  This subroutine is used to initialize the ProfileMod and should be
!  called before any other subroutines or function in this module are
!  called.
!  SYNOPSIS

  subroutine ProfileInit()

!  ARGUMENTS
!  [None]
!*****

    integer :: cr

    checkpointNames = ""
    checkpointTimes = 0.0d0
    numCheckpointHits = 0
    numCheckpointTransitionHits = 0
    call system_clock(count_rate = cr, count_max = countMax)
    ! Convert the integer cr into a double precision.
    countRate = cr

  end subroutine ProfileInit

!******************************************************************************
!****f* ProfileMod/ProfileNameCheckpointTransition
!  NAME
!  ProfileNameCheckpointTransition: This subroutine is used to set the name of
!  transitions between checkpoints.
!  DESCRIPTION
!  This subroutine is used to set the name (or a description) of
!  transitions between checkpoints. The variable 'point1' is the
!  first checkpoint in the transition, and 'point2' is the second.
!  The variable 'name' should be some type of short description of
!  what happens in the code between 'point1' and 'point2'. It's
!  maximum length is 29. If it is longer than 29, a warning message
!  will be printed and it will be truncated to the first 29
!  characters. Every transitition does not need to be named. If it
!  is left un-named, it will be given an empty string as it's name.
!  SYNOPSIS

  subroutine ProfileNameCheckpointTransition(point1, point2, name)

!  ARGUMENTS
!  * point1: First checkpoint in the transition.
!  * point2: Second checkpoint in the transition.
!  * name: Short description of the transition.
!   ARGUMENT DECLARATIONS
    integer, intent(in) :: point1
    integer, intent(in) :: point2
    character (len = *), intent(in) :: name
!*****

    checkpointNames(point1, point2) = trim(name)
    if (len_trim(name) > MAX_NAME_LEN) then
      write(6, '(A)') "Warning from ProfileNameCheckpointTransition():"
      write(6, '(A, I6)') "  Naming the transition from checkpoint: ", point1
      write(6, '(A, I6)') "  to checkpoint: ", point2
      write(6, '(A)') "  with the name: " // trim(name)
      write(6, '(A)') "  is being truncated to: " // &
        trim(checkpointNames(point1, point2))
    end if

  end subroutine ProfileNameCheckpointTransition

!******************************************************************************
!****f* ProfileMod/ProfileCheckpoint
!  NAME
!  ProfileCheckpoint: This subroutine is used to set the checkpoints in the
!  code.
!  DESCRIPTION
!  Each checkpoint should be a distinct integer (i.e. no two calls to this
!  subroutine should have the same argument 'checkpointNum' passed
!  in). By putting these checkpoints throughout the code, this module
!  will time how long the code between the checkpoints is taking.
!  These times are written out through a call to the subroutine
!  'ProfileWriteCheckpointTransitionReport' or 'ProfileWriteFullReport'.
!  See the documentation of those subroutines for more details. The
!  maximum number of checkpoints that can be put in is 50, so the
!  possible values for 'checkpointNum' are 1 through 50, inclusive.
!  SYNOPSIS

  subroutine ProfileCheckpoint(checkpointNum)

!  ARGUMENTS
!  * checkpointNum: Checkpoint number.
!  ARGUMENT DECLARATIONS
    integer, intent(in):: checkpointNum
!*****
    integer, save :: lastCheckpointNum = 0
    integer, save :: lastTime
    integer :: timeNow
    double precision :: incTime

    numCheckpointHits(checkpointNum) = numCheckpointHits(checkpointNum) + 1
    numCheckpointTransitionHits(lastCheckpointNum, checkpointNum) = &
      numCheckpointTransitionHits(lastCheckpointNum, checkpointNum) + 1

    call system_clock(timeNow)
    if (timeNow < lastTime) then
      ! timeNow should only be less than lastTime if there was an overflow.
      incTime = countMax - lastTime + timeNow
    else
      incTime = timeNow - lastTime
    end if
    checkpointTimes(lastCheckpointNum, checkpointNum) = &
      checkpointTimes(lastCheckpointNum, checkpointNum) + incTime
    lastCheckpointNum = checkpointNum
    lastTime = timeNow

  end subroutine ProfileCheckpoint

!******************************************************************************
!****f* ProfileMod/ProfileGetTotalTime
!  NAME
!  ProfileGetTotalTime: This function returns the current total amount of time
!  for all the checkpoint transitions.
!  DESCRIPTION
!  This function returns the current total amount of time for all the
!  checkpoint transitions.
!  RETURNS
!  Double Precision: Total amount of time.
!  SYNOPSIS

  double precision function ProfileGetTotalTime() result(rv)

!  ARGUMENTS
!  [None]
!*****

    ! Make sure to specify that we're starting the first diemnsion from
    ! index 1, b/c otherwise we'll end up summing the entire array
    ! (which is declared to start from 0), and end up including the junk
    ! values that are stored in that first part of the array.
    rv = sum(checkpointTimes(1:,:)) / countRate

  end function ProfileGetTotalTime

!******************************************************************************
!****f* ProfileMod/ProfileWriteCheckpointTransitionReport
!  NAME
!  ProfileWriteCheckpointTransitionReport: This subroutine writes out the 
!  profiling information having to do with the checkpoint transitions.
!  DESCRIPTION
!  This subroutine writes out the profiling information having to do
!  with the checkpoint transitions. Mainly, it writes out the amount
!  of time spent for each transition and a count of the number of
!  times each transition occured. The output is in chart format
!  with 4 columns:
!  * The first column is the checkpoint transition. For example, "3==> 4" means
!    that the code went from checkpoint 3 to checkpoint 4. (Note that these
!    checkpoints are set with calls to the subroutine 'ProfileCheckpoint'. See
!    the documentation of that subroutine for more details.) 
!  * The second column is the name of that transition. The names can be set
!    using calls to the subroutine 'ProfileNameCheckpointTransition'. If the
!    transition was not named then this column will be empty (i.e. its name is
!    the empty string).
!  * The third column is the amount of time (in seconds, to 3 decimal places)
!    that this portion of the code took.
!  * The final (fourth) column is the number of times that this transition
!    occured.
!  Only transitions for which there is at least 1 occurance will be printed in
!  this output chart.
!
!  If the argument 'fileName' is passed in, then this subroutine will open the
!  file (using the name specified in 'fileName'), write the output, and close
!  the file when it is done. If 'fileName' is not passed in, then it is
!  expected that the user will be in charge of opening (before this
!  subroutine is called) and closing (after this subroutine is called) the file.
!  SYNOPSIS

  subroutine ProfileWriteCheckpointTransitionReport(unit, fileName)

!  ARGUMENTS
!  * unit: Unit Number.
!  * fileName: (optional) Opens files, writes output, and closes the file.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: unit
    character (len = *), optional, intent(in) :: fileName
!*****
    integer :: i, j
    integer, dimension(8) :: dateTime
    double precision :: totalTime, timeInSeconds

    ! Open file and write header.
    if (present(fileName)) call OpenFile(unit, trim(fileName), "replace")
    write(unit, '(A10, A30, A15, A15)') "Checkpoint", "Name of", &
      "Time (in", "Number of"
    write(unit, '(A10, A30, A15, A15)') "transition", "transition", &
      "seconds)", "transitions"
    write(unit, '(A)') repeat("-", 70)

    totalTime = 0.0d0
    do i = 1, MAX_NUM_CHECKPOINTS
      do j = 1, MAX_NUM_CHECKPOINTS
        if (numCheckpointTransitionHits(i, j) > 0) then
          timeInSeconds = checkpointTimes(i, j) / countRate
          totalTime = totalTime + timeInSeconds
          write(unit, '(I5, A3, I2, A30, F15.3, I15)') i, "==>", j, &
            trim(checkpointNames(i, j)), timeInSeconds, &
            numCheckpointTransitionHits(i, j)
        end if
      end do
    end do
    write(unit, '(/, A40, F15.3)') "Total time:", totalTime
    if (present(fileName)) call CloseFile(unit)

  end subroutine ProfileWriteCheckpointTransitionReport

!******************************************************************************
!****f* ProfileMod/ProfileWriteCheckpointCountReport
!  NAME
!  ProfileWriteCheckpointCountReport: This subroutine writes out the profiling
!  information having to do with the number of times each checkpoint was hit.
!  DESCRIPTION
!  This subroutine writes out the profiling information having to do
!  with the number of times each checkpoint was hit. The output is
!  in chart format with 2 columns:
!  * The first column is the checkpoint number (Note that these checkpoints
!    are set with calls to the subroutine 'ProfileCheckpoint'. See the
!    documentation of that subroutine for more details.)
!  * The second column is the number of times that that checkpoint was hit.
!  Only transitions for which there is at least 1 occurance will be printed in
!  this output chart.
!
!  If the argument 'fileName' is passed in, then this
!  subroutine will open the file (using the name specified in
!  'fileName'), write the output, and close the file when it is done.
!  If 'fileName' is not passed in, then it is expected that the user
!  will be in charge of opening (before this subroutine is called)
!  and closing (after this subroutine is called) the file.
!  SYNOPSIS

  subroutine ProfileWriteCheckpointCountReport(unit, fileName)

!  ARGUMENTS
!  * unit: Unit Number.
!  * fileName: (optional) Opens files, writes output, and closes the file.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: unit
    character (len = *), optional, intent(in) :: fileName
!*****
    integer :: i

    write(unit, '(A10, A15)') "Checkpoint", " Number"
    write(unit, '(A10, A15)') "    number", "of hits"
    write(unit, '(A)') repeat("-", 25)
    do i = 1, MAX_NUM_CHECKPOINTS
      if (numCheckpointHits(i) > 0) then
        write(unit, '(I10, I15)') i, numCheckpointHits(i)
      end if
    end do

  end subroutine ProfileWriteCheckpointCountReport

!******************************************************************************
!****f* ProfileMod/ProfileWriteFullReport
!  NAME
!  ProfileWriteFullReport: This subroutine writes out all the profiling
!  information available.
!  DESCRIPTION
!  This subroutine writes out all the profiling information available.
!  More specifically, it calls the subroutine
!  'ProfileWriteCheckpointTransitionReport' and
!  'ProfileWriteCheckpointCountReport' and writes the output to a
!  single file (associated with the unit number specified by 'unit').
!  A few separating lines are printed between the 2 sets of output.
!
!  If the argument 'fileName' is passed in, then this subroutine will
!  open the file (using the name specified in 'fileName'), write both
!  sets of output to that file, and close the file when it is done.
!  If 'fileName' is not passed in, then it is expected that the user
!  will be in charge of opening (before this subroutine is called)
!  and closing (after this subroutine is called) the file.
!  SYNOPSIS

  subroutine ProfileWriteFullReport(unit, fileName)

!  ARGUMENTS
!  * unit: Unit Number.
!  * fileName: (optional) Opens File Name, writes output, and closes the file.
!  ARGUMENT DECLARATIONS
    integer, intent(in) :: unit
    character (len = *), optional, intent(in) :: fileName
!*****

    if (present(fileName)) call OpenFile(unit, trim(fileName), "replace")

    call ProfileWriteCheckpointTransitionReport(unit)

    ! Write out a seperation between the portions of the report.
    write(unit, '(/, 3(A, /))') &
      repeat("\", 75), repeat("/", 75), repeat("\", 75)

    call ProfileWriteCheckpointCountReport(unit)

    if (present(fileName)) call CloseFile(unit)

  end subroutine ProfileWriteFullReport

!******************************************************************************

end module ProfileMod
