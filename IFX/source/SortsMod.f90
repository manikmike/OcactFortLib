! $Id: SortsMod.f90 1.8 2012/06/22 10:45:41EDT 622325 Development  $
module SortsMod

!****h* source/SortsMod
!  NAME
!  SortsMod: OcactFortLib SortsMod module.
!  REVISION
!  $Revision: 1.8 $
!  MODULE DESCRIPTION
!  The SortsMod contains subroutines used to sort an array, either with a full
!  sort (HeapSort) or with a partition/partial sort (Nth_element).
!  MODULE ROUTINES
!  HeapSort
!  Nth_element
!*****

  implicit none
  save

  private
  public :: Nth_element, HeapSort

  ! Allow Nth_element to be the generic name for the specific subroutines.
!****f* SortsMod/Nth_element
!  NAME
!  Nth_element: This subroutine "partitions" an array.
!  DESCRIPTION
!  This subroutine does a "partitioning" of the array 'arr'. The array is
!  partitioned such that all elements from 1 to 'nthElem'-1 will be less
!  than the 'nthElem' (in an arbitrary order) and all the elements from
!  'nthElem'+1 to size(arr) will be greater than nthElem (also in an
!  arbitrary order). The new (partitioned) array is stored in 'arr',
!  thus overwriting the original array.
!
!  For example, say that 'arr' is
!  a 1-dimensional array of reals and has a size of five with the
!  following elements (in order): 2.0, 10.0, 4.0, 8.0, 6.0. Then calling
!  the subroutine Nth_element(2, arr) may return the following array:
!  2.0, 4.0, 10.0, 8.0, 6.0; or it may return: 2.0, 4.0, 8.0, 6.0, 10.0.
!  It may return other variations of these also. The only guarantee is
!  that the second element (since 2 was passed in as the first argument)
!  will be in its proper sorted order, i.e. a 4.0, and that all elements
!  to the left will be less than (or equal) to 4.0, and all elements to
!  the right of it will be greater than (or equal) to 4.0.
!  SYNOPSIS
!  subroutine Nth_element(nthElem, arr)
!  ARGUMENTS
!  * nthElem: The nth element of an array as an integer.
!  * arr: 1-dimensional array (of any size) of integers, reals, or double
!    precisions. New (partitioned) array is stored in 'arr', overwriting the
!    original array.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface Nth_element
    module procedure Nth_elementReal, Nth_elementInt, Nth_elementDoublePrec
  end interface Nth_element

  ! Allow HeapSort to be the generic name for the specific subroutines.
!****f* SortsMod/HeapSort
!  NAME
!  HeapSort: This subroutine sorts an array using the "heap sort" method.
!  DESCRIPTION
!  This subroutine sorts the array 'arr' using the "heap sort" method.
!  The resulting array is sorted in ascending order and is returned in
!  'arr', thus overwriting the original array.
!  SYNOPSIS
!  subroutine HeapSort(arr)
!  ARGUMENTS
!  * arr: 1-dimensional array (of any size) of integers, reals, or double
!    precisions. New sorted array is returned in 'arr', overwriting the
!    original array.
!  ARGUMENT DECLARATIONS
!  See ARGUMENTS section.
!*****
  interface HeapSort
    module procedure HeapSortReal, HeapSortInt, HeapSortDoublePrec
  end interface HeapSort

contains

!******************************************************************************
!From NUMERICAL RECIPES IN FORTRAN 77
  subroutine Nth_elementReal(nthElem, arr)
    integer, intent(in) :: nthElem
    real, intent(inout) :: arr(:)
    integer n
    ! Returns the kth smallest value in the array arr(1:n). The input array
    ! will be rearranged to have this value in location arr(k), with all
    ! smaller elements moved to arr(1:k-1) (in arbitrary order) and all larger
    ! elements in arr[k+1..n] (also in arbitrary order).
    integer i,ir,j,l,mid
    real a,temp

    n = size(arr)
    l=1
    ir=n
  1 if(ir-l.le.1)then     !Active partition contains 1 or 2 elements.
      if(ir-l.eq.1)then   !Active partition contains 2 elements.
        if(arr(ir).lt.arr(l))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
      endif
      return
    else
      !Choose median of left, center, and right elements as partitioning
      mid=(l+ir)/2
      !element a. Also rearrange so that arr(l) . arr(l+1), arr(ir) . arr(l+1).
      temp=arr(mid)
      arr(mid)=arr(l+1)
      arr(l+1)=temp
      if(arr(l).gt.arr(ir))then
        temp=arr(l)
        arr(l)=arr(ir)
        arr(ir)=temp
      endif
      if(arr(l+1).gt.arr(ir))then
        temp=arr(l+1)
        arr(l+1)=arr(ir)
        arr(ir)=temp
      endif
      if(arr(l).gt.arr(l+1))then
        temp=arr(l)
        arr(l)=arr(l+1)
        arr(l+1)=temp
      endif
      i=l+1               !Initialize pointers for partitioning.
      j=ir
      a=arr(l+1)          !Partitioning element.
  3   continue            !Beginning of innermost loop.
      i=i+1               !Scan up to find element > a.
      if(arr(i).lt.a)goto 3
  4   continue
      j=j-1               !Scan down to find element < a.
      if(arr(j).gt.a)goto 4
      if(j.lt.i)goto 5    !Pointers crossed. Exit with partitioning complete.
      temp=arr(i)         !Exchange elements.
      arr(i)=arr(j)
      arr(j)=temp
      goto 3              !End of innermost loop.
  5   arr(l+1)=arr(j)     !Insert partitioning element.
      arr(j)=a
      !Keep active the partition that contains the kth element.
      if(j .ge. nthElem)ir=j-1
      if(j .le. nthElem)l=i
    endif
    goto 1
  end subroutine Nth_elementReal
!******************************************************************************
  subroutine Nth_elementDoublePrec(nthElem, arr)
    integer, intent(in) :: nthElem
    double precision, intent(inout) :: arr(:)
    integer n
    ! Returns the kth smallest value in the array arr(1:n). The input array
    ! will be rearranged to have this value in location arr(k), with all
    ! smaller elements moved to arr(1:k-1) (in arbitrary order) and all larger
    ! elements in arr[k+1..n] (also in arbitrary order).
    integer i,ir,j,l,mid
    double precision :: a,temp

    n = size(arr)
    l=1
    ir=n
  1 if(ir-l.le.1)then     !Active partition contains 1 or 2 elements.
      if(ir-l.eq.1)then   !Active partition contains 2 elements.
        if(arr(ir).lt.arr(l))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
      endif
      return
    else
      ! Choose median of left, center, and right elements as partitioning
      mid=(l+ir)/2
      ! element a. Also rearrange so that arr(l) . arr(l+1), arr(ir) . arr(l+1).
      temp=arr(mid)
      arr(mid)=arr(l+1)
      arr(l+1)=temp
      if(arr(l).gt.arr(ir))then
        temp=arr(l)
        arr(l)=arr(ir)
        arr(ir)=temp
      endif
      if(arr(l+1).gt.arr(ir))then
        temp=arr(l+1)
        arr(l+1)=arr(ir)
        arr(ir)=temp
      endif
      if(arr(l).gt.arr(l+1))then
        temp=arr(l)
        arr(l)=arr(l+1)
        arr(l+1)=temp
      endif
      i=l+1               !Initialize pointers for partitioning.
      j=ir
      a=arr(l+1)          !Partitioning element.
  3   continue            !Beginning of innermost loop.
      i=i+1               !Scan up to find element > a.
      if(arr(i).lt.a)goto 3
  4   continue
      j=j-1               !Scan down to find element < a.
      if(arr(j).gt.a)goto 4
      if(j.lt.i)goto 5    !Pointers crossed. Exit with partitioning complete.
      temp=arr(i)         !Exchange elements.
      arr(i)=arr(j)
      arr(j)=temp
      goto 3              !End of innermost loop.
  5   arr(l+1)=arr(j)     !Insert partitioning element.
      arr(j)=a
      !Keep active the partition that contains the kth element.
      if(j .ge. nthElem)ir=j-1
      if(j .le. nthElem)l=i
    endif
    goto 1
  end subroutine Nth_elementDoublePrec
!******************************************************************************
  subroutine Nth_elementInt(nthElem, arr)
    integer, intent(in) :: nthElem
    integer, intent(inout) :: arr(:)
    integer n
    ! Returns the kth smallest value in the array arr(1:n). The input array
    ! will be rearranged to have this value in location arr(k), with all
    ! smaller elements moved to arr(1:k-1) (in arbitrary order) and all larger
    ! elements in arr[k+1..n] (also in arbitrary order).
    integer i,ir,j,l,mid
    integer a,temp

    n = size(arr)
    l=1
    ir=n
  1 if(ir-l.le.1)then     !Active partition contains 1 or 2 elements.
      if(ir-l.eq.1)then   !Active partition contains 2 elements.
        if(arr(ir).lt.arr(l))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
      endif
      return
    else
      !Choose median of left, center, and right elements as partitioning
      mid=(l+ir)/2
      !element a. Also rearrange so that arr(l) . arr(l+1), arr(ir) . arr(l+1).
      temp=arr(mid)
      arr(mid)=arr(l+1)
      arr(l+1)=temp
      if(arr(l).gt.arr(ir))then
        temp=arr(l)
        arr(l)=arr(ir)
        arr(ir)=temp
      endif
      if(arr(l+1).gt.arr(ir))then
        temp=arr(l+1)
        arr(l+1)=arr(ir)
        arr(ir)=temp
      endif
      if(arr(l).gt.arr(l+1))then
        temp=arr(l)
        arr(l)=arr(l+1)
        arr(l+1)=temp
      endif
      i=l+1               !Initialize pointers for partitioning.
      j=ir
      a=arr(l+1)          !Partitioning element.
  3   continue            !Beginning of innermost loop.
      i=i+1               !Scan up to find element > a.
      if(arr(i).lt.a)goto 3
  4   continue
      j=j-1               !Scan down to find element < a.
      if(arr(j).gt.a)goto 4
      if(j.lt.i)goto 5    !Pointers crossed. Exit with partitioning complete.
      temp=arr(i)         !Exchange elements.
      arr(i)=arr(j)
      arr(j)=temp
      goto 3              !End of innermost loop.
  5   arr(l+1)=arr(j)     !Insert partitioning element.
      arr(j)=a
      !Keep active the partition that contains the kth element.
      if(j .ge. nthElem)ir=j-1
      if(j .le. nthElem)l=i
    endif
    goto 1
  end subroutine Nth_elementInt
!******************************************************************************
!Sample page from NUMERICAL RECIPES IN FORTRAN 77
  subroutine HeapSortReal(arr)

    real, intent(inout) :: arr(:)
    ! Sorts an array arr(1:n) into ascending numerical order using the
    ! Heapsort algorithm. n is input; arr is replaced on output by its
    ! sorted rearrangement.
    integer :: i,ir,j,l,n
    real :: rra

    n = size(arr)
    if (n.lt.2) return
    ! The index l will be decremented from its initial value down to 1 during
    ! the “hiring” (heap creation) phase. Once it reaches 1, the index ir will
    ! be decremented from its initial value down to 1 during the
    ! "retirement-and-promotion" (heap selection) phase.
    l=n/2+1
    ir=n
10  continue
    if(l.gt.1)then         ! Still in hiring phase.
      l=l-1
      rra=arr(l)
    else                   ! In retirement-and-promotion phase.
      rra=arr(ir)           ! Clear a space at end of array.
      arr(ir)=arr(1)         ! Retire the top of the heap into it.
      ir=ir-1              ! Decrease the size of the corporation.
      if(ir.eq.1)then      ! Done with the last promotion.
        arr(1)=rra          ! The least competent worker of all!
        return
      endif
    endif
    ! Whether in the hiring phase or promotion phase, we here
    !   set up to sift down element rra to its proper level.
    i=l
    j=l+l
20  if(j.le.ir)then        ! "Do while j.le.ir:"
      if(j.lt.ir)then
        if(arr(j).lt.arr(j+1))j=j+1 ! Compare to the better underling.
      endif
      if(rra.lt.arr(j))then ! Demote rra.
        arr(i)=arr(j)
        i=j
        j=j+j
      else
        ! This is rra’s level. Set j to terminate the sift-down.
        j=ir+1
      endif
      goto 20
    endif
    arr(i)=rra              ! Put rra into its slot.
    goto 10

  end subroutine HeapSortReal
!******************************************************************************
  subroutine HeapSortDoublePrec(arr)

    double precision, intent(inout) :: arr(:)
    ! Sorts an array arr(1:n) into ascending numerical order using the
    ! Heapsort algorithm. n is input; arr is replaced on output by its
    ! sorted rearrangement.
    integer :: i,ir,j,l,n
    double precision :: rra

    n = size(arr)
    if (n.lt.2) return
    ! The index l will be decremented from its initial value down to 1 during
    ! the “hiring” (heap creation) phase. Once it reaches 1, the index ir will
    ! be decremented from its initial value down to 1 during the
    ! "retirement-and-promotion" (heap selection) phase.
    l=n/2+1
    ir=n
10  continue
    if(l.gt.1)then         ! Still in hiring phase.
      l=l-1
      rra=arr(l)
    else                   ! In retirement-and-promotion phase.
      rra=arr(ir)           ! Clear a space at end of array.
      arr(ir)=arr(1)         ! Retire the top of the heap into it.
      ir=ir-1              ! Decrease the size of the corporation.
      if(ir.eq.1)then      ! Done with the last promotion.
        arr(1)=rra          ! The least competent worker of all!
        return
      endif
    endif
    ! Whether in the hiring phase or promotion phase, we here
    !   set up to sift down element rra to its proper level.
    i=l
    j=l+l
20  if(j.le.ir)then        ! "Do while j.le.ir:"
      if(j.lt.ir)then
        if(arr(j).lt.arr(j+1))j=j+1 ! Compare to the better underling.
      endif
      if(rra.lt.arr(j))then ! Demote rra.
        arr(i)=arr(j)
        i=j
        j=j+j
      else
        ! This is rra’s level. Set j to terminate the sift-down.
        j=ir+1
      endif
      goto 20
    endif
    arr(i)=rra              ! Put rra into its slot.
    goto 10

  end subroutine HeapSortDoublePrec
!******************************************************************************
  subroutine HeapSortInt(arr)

    integer, intent(inout) :: arr(:)
    ! Sorts an array arr(1:n) into ascending numerical order using the
    ! Heapsort algorithm. n is input; arr is replaced on output by its
    ! sorted rearrangement.
    integer :: i,ir,j,l,n
    integer :: rra

    n = size(arr)
    if (n.lt.2) return
    ! The index l will be decremented from its initial value down to 1 during
    ! the “hiring” (heap creation) phase. Once it reaches 1, the index ir will
    ! be decremented from its initial value down to 1 during the
    ! "retirement-and-promotion" (heap selection) phase.
    l=n/2+1
    ir=n
10  continue
    if(l.gt.1)then         ! Still in hiring phase.
      l=l-1
      rra=arr(l)
    else                   ! In retirement-and-promotion phase.
      rra=arr(ir)           ! Clear a space at end of array.
      arr(ir)=arr(1)         ! Retire the top of the heap into it.
      ir=ir-1              ! Decrease the size of the corporation.
      if(ir.eq.1)then      ! Done with the last promotion.
        arr(1)=rra          ! The least competent worker of all!
        return
      endif
    endif
    ! Whether in the hiring phase or promotion phase, we here
    !   set up to sift down element rra to its proper level.
    i=l
    j=l+l
20  if(j.le.ir)then        ! "Do while j.le.ir:"
      if(j.lt.ir)then
        if(arr(j).lt.arr(j+1))j=j+1 ! Compare to the better underling.
      endif
      if(rra.lt.arr(j))then ! Demote rra.
        arr(i)=arr(j)
        i=j
        j=j+j
      else
        ! This is rra’s level. Set j to terminate the sift-down.
        j=ir+1
      endif
      goto 20
    endif
    arr(i)=rra              ! Put rra into its slot.
    goto 10

  end subroutine HeapSortInt
!******************************************************************************

end module SortsMod
