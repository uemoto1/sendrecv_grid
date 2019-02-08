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

  public :: s_sendrecv_grid
  public :: sendrecv7d


  integer, parameter :: iside_src = 1
  integer, parameter :: iside_dst = 2


  !! TODO: Move type defination to "common/structures.f90"
  type s_sendrecv_grid
    !! Communicator
    integer :: icomm
    !! Neightboring MPI id ( 1:x,2:y,3:z, 1:upward,2:downward):
    integer :: neig(1:3, 1:2) 
    !! Communication requests (1:x,2:y,3:z, 1:upward,2:downward, 1:send,2:recv):
    integer :: ireq(1:3, 1:2, 1:2)
    !! PComm cache (1:x,2:y,3:z, 1:upside,2:downside, 1:src/2:dst)
    type(s_pcomm_cache4d) :: cache(1:3, 1:2, 1:2)
    !! Range (dim=1:x,2:y,3:z, dir=1:upside,2:downside, axis=1-7)
    type(array_shape) :: nshape(1:3, 1:2, 1:4)
  end type s_sendrecv_grid


  type s_pcomm_cache4d
    real(8), allocatable :: d(:, :, :, :)
    real(8), allocatable :: z(:, :, :, :)
  end type s_pcomm_cache4d


  interface update_overlap
  module procedure update_overlap_array4d_double
  module procedure update_overlap_array4d_dcomplex
  end interface


  contains



  subroutine update_overlap_array4d_double(srg, data)
    use salmon_communication, only: comm_start_all, comm_wait_all, comm_proc_null
    implicit none
    type(s_sendrecv_grid), intent(inout) :: srg
    real(8), intent(inout) :: data

    integer :: idim, idir

    !! SEND overlapped region:
    do idim = 1, 3 !! 1:x,2:y,3:z
      do idir = 1, 2 !! 1:up,2:down
        if (srg%neig(idim, idir) /= comm_proc_null) then
          call pack_cache(idim, idir)
        end if
        call comm_start_all(srg%ireq(idim, idir, :)) !! SEND
      end do
    end do

    !! RECV overlapped region:
    do idim = 1, 3 !! 1:x,2:y,3:z
      do idir = 1, 2 !! 1:up,2:down
        call comm_wait_all(srg%ireq(idim, idir, :)) !! RECV
        if (srg%neig(idim, idir) /= comm_proc_null) then
          call unpack_cache(srg, wf, idim, idir)
        end if
      end do
    end do
    return
  contains

    subroutine pack_cache()
      use pack_unpack, only: copy_data
      type(array_shape) :: r(1:4)
      r(1:4) = srg%nshape(idim, idir, 1:4)
      call copy_data( &
        srg%cache(idim, idir, iside_src)%d, &
        data(r(1)%nbeg:r(1)%nend, r(2)%nbeg:r(2)%nend, &
             r(3)%nbeg:r(3)%nend, r(4)%nbeg:r(4)%nend) &
      )
    end subroutine pack_cache

    subroutine unpack_cache()
      use pack_unpack, only: copy_data
      type(array_shape) :: r(1:4)
      r(1:4) = srg%nshape(idim, idir, 1:4)
      call copy_data( &
        data(r(1)%nbeg:r(1)%nend, r(2)%nbeg:r(2)%nend, &
             r(3)%nbeg:r(3)%nend, r(4)%nbeg:r(4)%nend), &
        srg%cache(idim, idir, iside_dst)%d &
      )
    end subroutine unpack_cache

  end subroutine update_overlap_array4d_double



  subroutine update_overlap_array4d_dcomplex(srg, data)
    use salmon_communication, only: comm_start_all, comm_wait_all, comm_proc_null
    implicit none
    type(s_sendrecv_grid), intent(inout) :: srg
    complex(8), intent(inout) :: data

    integer :: idim, idir

    !! SEND overlapped region:
    do idim = 1, 3 !! 1:x,2:y,3:z
      do idir = 1, 2 !! 1:up,2:down
        if (srg%neig(idim, idir) /= comm_proc_null) then
          call pack_cache(idim, idir)
        end if
        call comm_start_all(srg%ireq(idim, idir, :)) !! SEND
      end do
    end do

    !! RECV overlapped region:
    do idim = 1, 3 !! 1:x,2:y,3:z
      do idir = 1, 2 !! 1:up,2:down
        call comm_wait_all(srg%ireq(idim, idir, :)) !! RECV
        if (srg%neig(idim, idir) /= comm_proc_null) then
          call unpack_cache(srg, wf, idim, idir)
        end if
      end do
    end do
    return
  contains

    subroutine pack_cache()
      use pack_unpack, only: copy_data
      type(array_shape) :: r(1:4)
      r(1:4) = srg%nshape(idim, idir, 1:4)
      call copy_data( &
        srg%cache(idim, idir, iside_src)%z, &
        data(r(1)%nbeg:r(1)%nend, r(2)%nbeg:r(2)%nend, &
             r(3)%nbeg:r(3)%nend, r(4)%nbeg:r(4)%nend) &
      )
    end subroutine pack_cache

    subroutine unpack_cache()
      use pack_unpack, only: copy_data
      type(array_shape) :: r(1:4)
      r(1:4) = srg%nshape(idim, idir, 1:4)
      call copy_data( &
        data(r(1)%nbeg:r(1)%nend, r(2)%nbeg:r(2)%nend, &
             r(3)%nbeg:r(3)%nend, r(4)%nbeg:r(4)%nend), &
        srg%cache(idim, idir, iside_dst)%z &
      )
    end subroutine unpack_cache
    
  end subroutine update_overlap_array4d_dcomplex

  

end module sendrecv_grid
