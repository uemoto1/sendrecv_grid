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

#define FLIP12(i) 3-i

module sendrecv_grid
  use pack_unpack, only: array_shape

  implicit none

  public :: update_overlap
  public :: s_sendrecv_grid

  integer, parameter :: iside_up   = 1
  integer, parameter :: iside_down = 2
  integer, parameter :: imode_send = 1
  integer, parameter :: imode_recv = 2

  ! TODO: Move type defination to "common/structures.f90"
  type s_pcomm_cache4d
    real(8), allocatable :: d_buf(:, :, :, :)
    real(8), allocatable :: z_buf(:, :, :, :)
  end type s_pcomm_cache4d

  ! TODO: Move type defination to "common/structures.f90"
  type s_sendrecv_grid
    ! Communicator
    integer :: icomm, myrank
    ! Neightboring MPI id (1:x,2:y,3:z, 1:upside,2:downside):
    integer :: neig(1:3, 1:2) 
    ! Communication requests (1:x,2:y,3:z, 1:upside,2:downside, 1:send,2:recv):
    integer :: ireq(1:3, 1:2, 1:2)
    ! PComm cache (1:x,2:y,3:z, 1:upside,2:downside, 1:src/2:dst)
    type(s_pcomm_cache4d) :: cache(1:3, 1:2, 1:2)
    ! Range (dim=1:x,2:y,3:z, dir=1:upside,2:downside, 1:src/2:dst, axis=1...4)
    type(array_shape) :: nshape(1:3, 1:2, 1:2, 1:4)
  end type s_sendrecv_grid



  interface update_overlap
  module procedure update_overlap_array4d_double
  !module procedure update_overlap_array4d_dcomplex

  end interface


  contains


  subroutine update_overlap_array4d_double(srg, data)
    use salmon_communication, only: comm_start_all, comm_wait_all, comm_proc_null
    implicit none
    type(s_sendrecv_grid), intent(inout) :: srg
    real(8), intent(inout) :: data(:, :, :, :)

    integer :: idir, iside

    ! SEND overlap region:
    do idir = 1, 3 ! 1:x,2:y,3:z
      do iside = 1, 2 ! 1:up,2:down
        if (srg%neig(idir, iside) /= comm_proc_null) then
          if (srg%neig(idir, iside) /= srg%myrank) then
            call pack_cache(idir, iside) ! Store the overlap reigion into cache 
            call comm_start_all(srg%ireq(idir, iside, :)) ! Start to SEND
          end if
        end if
      end do
    end do

    ! RECV overlap region:
    do idir = 1, 3 ! 1:x,2:y,3:z
      do iside = 1, 2 ! 1:upside,2:downside
        if (srg%neig(idir, iside) /= comm_proc_null) then
          if (srg%neig(idir, iside) /= srg%myrank) then
            call comm_wait_all(srg%ireq(idir, iside, :)) ! Wait for RECV
            call unpack_cache(idir, iside) ! Write back the recieved cache
          else
            ! NOTE: If a neightboring node is itself (periodic system in no-mpi),
            !       a simple side-to-side copy is used instead of the MPI.
            call copy_self(idir, iside)
          end if
        end if
      end do
    end do
    return

  contains

    subroutine pack_cache(jdir, jside)
      use pack_unpack, only: copy_data
      integer, intent(in) :: jdir, jside
      type(array_shape) :: rsrc(1:4)
      rsrc(1:4) = srg%nshape(jdir, jside, imode_send, 1:4)
      call copy_data( &
        data(rsrc(1)%nbeg:rsrc(1)%nend, rsrc(2)%nbeg:rsrc(2)%nend, &
             rsrc(3)%nbeg:rsrc(3)%nend, rsrc(4)%nbeg:rsrc(4)%nend), &
        srg%cache(jdir, jside, imode_send)%d_buf &
      )
    end subroutine pack_cache

    subroutine unpack_cache(jdir, jside)
      use pack_unpack, only: copy_data
      integer, intent(in) :: jdir, jside
      type(array_shape) :: rdst(1:4)
      rdst(1:4) = srg%nshape(jdir, jside, imode_recv, 1:4)
      call copy_data( &
        srg%cache(jdir, jside, imode_recv)%d_buf, &
        data(rdst(1)%nbeg:rdst(1)%nend, rdst(2)%nbeg:rdst(2)%nend, &
             rdst(3)%nbeg:rdst(3)%nend, rdst(4)%nbeg:rdst(4)%nend) &
      )
    end subroutine unpack_cache

    subroutine copy_self(jdir, jside)
      use pack_unpack, only: copy_data
      integer, intent(in) :: jdir, jside
      type(array_shape) :: rsrc(1:4), rdst(1:4)
      rsrc(1:4) = srg%nshape(jdir, FLIP12(jside), imode_send, 1:4)
      rdst(1:4) = srg%nshape(jdir, jside, imode_recv, 1:4)
      call copy_data( &
        data(rsrc(1)%nbeg:rsrc(1)%nend, rsrc(2)%nbeg:rsrc(2)%nend, &
             rsrc(3)%nbeg:rsrc(3)%nend, rsrc(4)%nbeg:rsrc(4)%nend), &
        data(rdst(1)%nbeg:rdst(1)%nend, rdst(2)%nbeg:rdst(2)%nend, &
             rdst(3)%nbeg:rdst(3)%nend, rdst(4)%nbeg:rdst(4)%nend) &
      )
    end subroutine copy_self

  end subroutine update_overlap_array4d_double



end module sendrecv_grid

#undef FLIP12

