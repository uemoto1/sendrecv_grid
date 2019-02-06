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
  use structures, only: s_wavefunction
  use pack_unpack, only: array_shape

  implicit none

  public :: s_sendrecv_grid
  public :: sendrecv7d


  integer, parameter :: iside_src = 1
  integer, parameter :: iside_dst = 2


  !! TODO: Move type defination to "common/structures.f90"
  type s_sendrecv_grid
    !! Type:
    logical :: use_complex !! .true.:complex(8), .false.:real(8)
    logical :: use_corner !! .false.:orthogonal, .true.:non-orthogonal

    !! Neightboring MPI proc id:
    integer :: neig(1:3, 1:2) !! 1:x,2:y,3:z, 1:upward,2:downward
    ! integer :: iup_array(1)
    ! integer :: idw_array(1)
    ! integer :: jup_array(1)
    ! integer :: jdw_array(1)
    ! integer :: kup_array(1)
    ! integer :: kdw_array(1)

    !! Communication requests:
    integer :: ireq(1:3, 1:2, 1:2) !! 1:x,2:y,3:z, 1:upward,2:downward, 1:send,2:recv
    !integer :: ireq(1:12)
    
    !! pcomm cache (1:x,2:y,3:z, 1:up,2:down, 1=src/2=dst)
    type(s_wavefunction) :: srmatbox7d(1:3, 1:2, 1:2)
    ! real(8), allocatable :: srmatbox1_x_3d(:,:,:)
    ! real(8), allocatable :: srmatbox2_x_3d(:,:,:)
    ! real(8), allocatable :: srmatbox3_x_3d(:,:,:)
    ! real(8), allocatable :: srmatbox4_x_3d(:,:,:)
    ! real(8), allocatable :: srmatbox1_y_3d(:,:,:)
    ! real(8), allocatable :: srmatbox2_y_3d(:,:,:)
    ! real(8), allocatable :: srmatbox3_y_3d(:,:,:)
    ! real(8), allocatable :: srmatbox4_y_3d(:,:,:)
    ! real(8), allocatable :: srmatbox1_z_3d(:,:,:)
    ! real(8), allocatable :: srmatbox2_z_3d(:,:,:)
    ! real(8), allocatable :: srmatbox3_z_3d(:,:,:)
    ! real(8), allocatable :: srmatbox4_z_3d(:,:,:)
    ! real(8), allocatable :: srmatbox1_x_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox2_x_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox3_x_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox4_x_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox1_y_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox2_y_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox3_y_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox4_y_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox1_z_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox2_z_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox3_z_5d(:,:,:,:,:)
    ! real(8), allocatable :: srmatbox4_z_5d(:,:,:,:,:)
  
    !! Range (dim=1:x,2:y,3:z, dir=1:up,2:down, axis=1-7)
    type(array_shape) :: nshape(1:3, 1:2, 1:7)
    !type(array_shape) :: nrange(1:3,1:3)

  end type s_sendrecv_grid

  contains



  subroutine sendrecv7d(srg, wf)
    use structures, only: s_rgrid, s_wavefunction
    use salmon_communication, only: comm_start_all, comm_wait_all, comm_proc_null
    implicit none
    type(s_sendrecv_grid), intent(inout) :: srg
    type(s_wavefunction),  intent(inout) :: wf

    integer :: idim, idir

    !! SEND overlapped region:
    do idim = 1, 3 !! 1:x,2:y,3:z
      do idir = 1, 2 !! 1:up,2:down
        if (srg%neig(idim, idir) /= comm_proc_null) then
          call pack_smatbox7d(srg, wf, idim, idir)
        end if
        call comm_start_all(srg%ireq(idim, idir, :))
      end do
    end do

    !! RECV overlapped region:
    do idim = 1, 3 !! 1:x,2:y,3:z
      do idir = 1, 2 !! 1:up,2:down
        call comm_wait_all(srg%ireq(idim, idir, :))
        if (srg%neig(idim, idir) /= comm_proc_null) then
          call unpack_smatbox7d(srg, wf, idim, idir)
        end if
      end do
    end do
    return
  end subroutine sendrecv7d

  subroutine pack_smatbox7d(srg, wf, jdim, jdir)
    use pack_unpack, only: copy_data
    type(s_sendrecv_grid), intent(inout) :: srg
    type(s_wavefunction),  intent(in)    :: wf
    integer, intent(in) :: jdim, jdir
    type(array_shape) :: r(1:7)

    r(1:7) = srg%nshape(jdim, jdir, 1:7)
    
    if (srg%use_complex) then
      call copy_data( &
        wf%zwf( &
          r(1)%nbeg:r(1)%nend, &
          r(2)%nbeg:r(2)%nend, &
          r(3)%nbeg:r(3)%nend, &
          r(4)%nbeg:r(4)%nend, &
          r(5)%nbeg:r(5)%nend, &
          r(6)%nbeg:r(6)%nend, &
          r(7)%nbeg:r(7)%nend &
        ), &
        srg%srmatbox7d(jdim, jdir, iside_src)%zwf &
      )
    else
      call copy_data( &
        wf%rwf( &
          r(1)%nbeg:r(1)%nend, &
          r(2)%nbeg:r(2)%nend, &
          r(3)%nbeg:r(3)%nend, &
          r(4)%nbeg:r(4)%nend, &
          r(5)%nbeg:r(5)%nend, &
          r(6)%nbeg:r(6)%nend, &
          r(7)%nbeg:r(7)%nend &
        ), &
        srg%srmatbox7d(jdim, jdir, iside_src)%rwf &
      )
    end if
    return
  end subroutine pack_smatbox7d


  subroutine unpack_smatbox7d(srg, wf, jdim, jdir)
    use pack_unpack, only: copy_data
    type(s_sendrecv_grid), intent(in)    :: srg
    type(s_wavefunction),  intent(inout) :: wf
    integer, intent(in) :: jdim, jdir
    type(array_shape) :: r(1:7)

    r(1:7) = srg%nshape(jdim, jdir, 1:7)
    
    if (srg%use_complex) then
      call copy_data( &
        srg%srmatbox7d(jdim, jdir, iside_dst)%zwf, &
        wf%zwf( &
          r(1)%nbeg:r(1)%nend, &
          r(2)%nbeg:r(2)%nend, &
          r(3)%nbeg:r(3)%nend, &
          r(4)%nbeg:r(4)%nend, &
          r(5)%nbeg:r(5)%nend, &
          r(6)%nbeg:r(6)%nend, &
          r(7)%nbeg:r(7)%nend &
        ) &
      )
    else
      call copy_data( &
        srg%srmatbox7d(jdim, jdir, iside_dst)%rwf, &
        wf%rwf( &
          r(1)%nbeg:r(1)%nend, &
          r(2)%nbeg:r(2)%nend, &
          r(3)%nbeg:r(3)%nend, &
          r(4)%nbeg:r(4)%nend, &
          r(5)%nbeg:r(5)%nend, &
          r(6)%nbeg:r(6)%nend, &
          r(7)%nbeg:r(7)%nend &
        ) &
      )
    end if
    return
  end subroutine unpack_smatbox7d
  

end module sendrecv_grid
