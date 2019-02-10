!
!  Copyright 2019 SALMON developers
!
!  Licensed under the Apache License, Version 2.0 (the "License");
!  you may not use this file except in compliance with the License.
!  You may obtain a copy of the License at
!
!      http://www.apache.org/licenses/LICENSE-2.0
!
!  Unless required by applicable law or agreed to in writing, software
!  distributed under the License is distributed on an "AS IS" BASIS,
!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!  See the License for the specific language governing permissions and
!  limitations under the License.
!


module sendrecv_grid
  use pack_unpack, only: array_shape

  implicit none

  public :: update_overlap
  public :: s_sendrecv_grid4d

  integer, parameter :: iside_up   = 1
  integer, parameter :: iside_down = 2
  integer, parameter :: itype_send = 1
  integer, parameter :: itype_recv = 2

  ! TODO: Move type defination to "common/structures.f90"
  type s_pcomm_cache4d
    real(8), allocatable :: dbuf(:, :, :, :)
    real(8), allocatable :: zbuf(:, :, :, :)
  end type s_pcomm_cache4d

  ! TODO: Move type defination to "common/structures.f90"
  type s_sendrecv_grid4d
    ! Lower(is) and upper(ie) bound of local grid (1:x,2:y,3:z)
    integer :: is(1:3), ie(1:3)
    ! Number of orbitals (4-th dimension of grid)
    integer :: ns
    ! Width of overlap region (4 is prefered)
    integer :: nd
    ! Communicator
    integer :: icomm, myrank
    ! Neightboring MPI id (1:x,2:y,3:z, 1:upside,2:downside):
    integer :: neig(1:3, 1:2) 
    ! Communication requests (1:x,2:y,3:z, 1:upside,2:downside, 1:send,2:recv):
    integer :: ireq(1:3, 1:2, 1:2)
    ! PComm cache (1:x,2:y,3:z, 1:upside,2:downside, 1:src/2:dst)
    type(s_pcomm_cache4d) :: cache(1:3, 1:2, 1:2)
    ! Range (dim=1:x,2:y,3:z, dir=1:upside,2:downside, 1:src/2:dst, axis=1...3)
    integer :: is_block(1:3, 1:2, 1:2, 1:3)
    integer :: ie_block(1:3, 1:2, 1:2, 1:3)
    logical :: pcomm_initialized
  end type s_sendrecv_grid4d

  interface update_overlap
  module procedure update_overlap_array4d_real8
  !module procedure update_overlap_array4d_complex8
  end interface


  contains

  integer function flip(i)
    implicit none
    integer, intent(in) :: i
    flip = 3 - i
  end function flip

  integer function tag(idir, iside)
    implicit none
    integer, intent(in) :: idir, iside
    tag = 2 * idir + iside
  end function tag

  subroutine init_sendrecv_grid4d(srg, icomm, is, ie, ns, nd, neig)
    implicit none
    type(s_sendrecv_grid4d), intent(inout) :: srg
    integer, intent(in) :: icomm
    integer, intent(in) :: is(4), ie(4), nd
    integer, intent(in) :: neig(1:3, 1:2)
    integer :: is_block(1:3, 1:2, 1:2, 1:3)
    integer :: ie_block(1:3, 1:2, 1:2, 1:3)
    
    integer :: idummy

    ! Calculate shape (upper and lower bounds) of overlapped region:
    ! NOTE:
    !
    !  nd nd         nd nd    U/D: up/down side of `idir`-th direction
    ! +==+--+-------+--+==+   S/R: send/recv region, representing the
    ! |DR|DS|       |US|UR|        inner and outer side of local grid.
    ! +==+--+-------+--+==+   
    !    is            ie     
    !    <------------->      
    !    Local grid size
    !
    ! is_block|ie(idir, iside, iblock, iaxis): 
    !   lower/upper bounds of each region
    !   * idir: direction (1:x, 2:y, 3:z)
    !   * iside: kind of region (1:upside, 2:downside)
    !   * iblock: kind of region (1:send, 2:recv)
    !   * iaxis: axis (1:x, 2:y, 3:z)
    do idir = 1, 3 ! 1:x,2:y,3:z
      do iaxis = 1, 3 ! 1:x,2:y,3:z
        if (idir == iaxis) then
          ! upside-send (US) block:
          is_block(idir, iside_up, itype_send, idir) = ie(idir) + 1
          ie_block(idir, iside_up, itype_send, idir) = ie(idir) + nd
          ! upside-recv (UR) block:
          is_block(idir, iside_up, itype_recv, idir) = ie(idir) - nd
          ie_block(idir, iside_up, itype_recv, idir) = ie(idir)
          ! downside-send (DS) block:
          is_block(idir, iside_down, itype_send, idir) = is(idir)
          ie_block(idir, iside_down, itype_send, idir) = is(idir) + nd
          ! downside-recv (DR) block:
          is_block(idir, iside_down, itype_recv, idir) = is(idir) - nd
          ie_block(idir, iside_down, itype_recv, idir) = is(idir) - 1
        else
          is_block(idir, :, :, iaxis) = is(iaxis)
          ie_block(idir, :, :, iaxis) = ie(iaxis)
        end if
      end do
    end do

    ! Assign to s_sendrecv_grid4d structure:
    srg%is(1:3) = is(1:3)
    srg%ie(1:3) = ie(1:3)
    srg%ns = ns
    srg%nd = nd
    srg%neig = neig
    srg%is_block(:, :, :, :) = is_block
    srg%ie_block(:, :, :, :) = ie_block
    srg%icomm = icomm
    call comm_get_groupinfo(icomm, srg%myrank, idummy)
  end subroutine init_sendrecv_grid4d


  subroutine alloc_cache_real8(srg)
    implicit none
    type(s_sendrecv_grid4d), intent(inout) :: srg
    integer :: is_b(3), ie_b(3)
    ! Allocate cache region for persistent communication:
    do idir = 1, 3 ! 1:x,2:y,3:z
      do iside = 1, 2 ! 1:up,2:down
        do iblock = 1, 2 ! 1:send, 2:recv
          do iaxis = 1, 3 ! 1:x,2:y,3:z
            is_b(1:3) = is_block(idir, iside, iblock, iaxis, 1:3)
            ie_b(1:3) = ie_block(idir, iside, iblock, iaxis, 1:3)
            allocate(srg%cache%dbuf(idir, iside, iblock, iaxis)( &
              is_b(1):is_e(1), is_b(1):is_e(1), is_b(1):is_e(1), 1:nbk))
          end do
        end do
      end do
    end do
    ! Set pcomm_initialization flag
    srg%pcomm_initialized = .false.
  end subroutine


  subroutine update_overlap_array4d_real8(srg, data)
    use salmon_communication, only: comm_start_all, comm_wait_all, comm_proc_null
    implicit none
    type(s_sendrecv_grid4d), intent(inout) :: srg
    real(8), intent(inout) :: data(:, :, :, :)
    integer :: idir, iside

    do idir = 1, 3 ! 1:x,2:y,3:z
      do iside = 1, 2 ! 1:up,2:down
        if (srg%neig(idir, iside) /= comm_proc_null) then
          if (srg%neig(idir, iside) /= srg%myrank) then
            call pack_cache(idir, iside) ! Store the overlap reigion into cache 
            if (.not. srg%pcomm_initialized) call init_pcomm(idir, iside)
            call comm_start_all(srg%ireq(idir, iside, :))
          end if
        end if
      end do
    end do

    do idir = 1, 3 ! 1:x,2:y,3:z
      do iside = 1, 2 ! 1:upside,2:downside
        if (srg%neig(idir, iside) /= comm_proc_null) then
          if (srg%neig(idir, iside) /= srg%myrank) then
            call comm_wait_all(srg%ireq(idir, iside, :))
            call unpack_cache(idir, iside) ! Write back the recieved cache
          else
            ! NOTE: If neightboring nodes are itself (periodic sys with single proc),
            !       a simple side-to-side copy is used instead of the MPI.
            call copy_self(idir, iside)
          end if
        end if
      end do
    end do
    srg%pcomm_initialized = .true.

  contains

    subroutine init_pcomm(jdir, jside)
      implicit none
      integer, intent(in) :: jdir, jside
      ! Send (and initialize persistent communication)
      srg%ireq(jdir, jside, itype_send) = comm_send_init( &
        srg%cache(jdir, jsidr, itype_send)%dbuf, &
        srg%neig(jdir, jside), &
        tag(jdir, jside), &
        srg%icomm)
      ! Recv (and initialize persistent communication)
      srg%ireq(jdir, jside, itype_recv) = comm_recv_init( &
        srg%cache(jdir, jsidr, itype_recv)%dbuf, &
        srg&neig(jdir, jside), &
        tag(jdir, flip(jside)), & ! flip(jside) is jside in sender
        srg%icomm)
    end subroutine init_pcomm

    subroutine pack_cache(jdir, jside)
      use pack_unpack, only: copy_data
      integer, intent(in) :: jdir, jside
      integer :: is_s(1:3), ie_s(1:3) ! src region
      is_s(1:3) = srg%is_block(jdir, jside, itype_send, 1:3)
      ie_s(1:3) = srg%ie_block(jdir, jside, itype_send, 1:3)
      call copy_data( &
        data(is_s(1):ie_s(1), is_s(2):ie_s(2), is_s(3):ie_s(3), 1:srg%ns), &
        srg%cache(jdir, jside, itype_send)%dbuf)
    end subroutine pack_cache

    subroutine unpack_cache(jdir, jside)
      use pack_unpack, only: copy_data
      integer, intent(in) :: jdir, jside
      integer :: is_d(1:3), ie_d(1:3) ! dst region
      is_d(1:3) = srg%is_block(jdir, jside, itype_recv, 1:3)
      ie_d(1:3) = srg%ie_block(jdir, jside, itype_recv, 1:3)
      call copy_data( &
        srg%cache(jdir, jside, itype_recv)%dbuf, &
        data(is_d(1):ie_d(1), is_d(2):ie_d(2), is_d(3):ie_d(3), 1:srg%ns))
    end subroutine unpack_cache

    subroutine copy_self(jdir, jside)
      use pack_unpack, only: copy_data
      integer, intent(in) :: jdir, jside
      type(array_shape) :: rsrc(1:4), rdst(1:4)
      integer :: is_s(1:3), ie_s(1:3) ! src region
      integer :: is_d(1:3), ie_d(1:3) ! dst region
      is_s(1:3) = srg%is_block(jdir, jside, itype_send, 1:3)
      ie_s(1:3) = srg%ie_block(jdir, jside, itype_send, 1:3)
      is_d(1:3) = srg%is_block(jdir, jside, itype_recv, 1:3)
      ie_d(1:3) = srg%ie_block(jdir, jside, itype_recv, 1:3)
      call copy_data( &
        data(is_s(1):ie_s(1), is_s(2):ie_s(2), is_s(3):ie_s(3), 1:srg%ns), &
        data(is_d(1):ie_d(1), is_d(2):ie_d(2), is_d(3):ie_d(3), 1:srg%ns))
    end subroutine copy_self

  end subroutine update_overlap_array4d_real8





end module sendrecv_grid


