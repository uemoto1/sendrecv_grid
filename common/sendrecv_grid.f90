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
#define FLIP2(i) 3-i

module sendrecv_grid
  implicit none

  !! TODO: Move type defination to "common/structures.f90"
  type s_srmatbox7d
    real(8), allocatable :: rbuf(:, :, :, :, :, :, :)
    complex(8), allocatable :: cbuf(:, :, :, :, :, :, :)
    type(array_range) :: nshape(7)
  end type s_srmatbox7d

  !! TODO: Move type defination to "common/structures.f90"
  type s_sendrecv_grid
    !! Type information
    logical :: is_complex 

    !! Communicator:
    integer :: icomm

    !! Neightboring MPI proc id:
    integer :: neig(1:3, 1:2) !! 1=x/2=y/3=z, 1=down/2=up
    ! integer :: iup_array(1)
    ! integer :: idw_array(1)
    ! integer :: jup_array(1)
    ! integer :: jdw_array(1)
    ! integer :: kup_array(1)
    ! integer :: kdw_array(1)

    !! Array shape
    type(array_range) :: nshape(7)
    !type(array_shape) :: nrange(1:3,1:3)

    !! Communication requests:
    integer :: ireq(1:3, 1:2, 1:2) !! 1=x/2=y/3=z, 1=downward/2=upward, 1=send/2=recv
    !integer :: ireq(1:12)
    
    !! Neightboring mpi nodes (1=x/2=y/3=z, 1=down/2=up, 1=src/2=dst)
    type(s_srmatbox7d) :: srmatbox7d(1:3, 1:2, 1:2)
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

  end type s_sendrecv_grid

  contains

  subroutine pack_srmatbox7d(src, dst)
    use pack_unpack
    implicit none

    return
  end subroutine

  subroutine unpack_srmatbox7d(src, dst)
    use pack_unpack
    implicit none
    return
  end subroutine


  subroutine sendrecv7d(srg, rg, wf)
    use structures, only: s_rgrid, s_wavefunction
    use salmon_communication, only: comm_start_all, comm_proc_null
    implicit none
    type(s_sendrecv_grid), intent(inout) :: srg
    type(s_rgrid),         intent(in)    :: rg
    type(s_wavefunction),  intent(inout) :: wf

    integer :: idim, idir

    !! SEND overlapped region:
    do idim = 1, 3 !! 1=x, 2=y, 3=z
      do idir = 1, 2 !! 1=down, 2=up
        if (srg%neig(idim, idir) /= comm_proc_null) then
          call pack_srmatbox(srg%nrange(idim, idir), wf, srg%srmatbox(idim, idir))
        end if
        call comm_start_all(srg%ireq(idim, idir, 1:2))
      end do
    end do

    !! RECV overlapped region:
    do idim = 1, 3 !! 1=x, 2=y, 3=z
      do idir = 1, 2 !! 1=down, 2=up
        call comm_wait_all(srg%ireq(idim, idir, 1:2))
        if (srg%neig(idim, idir) /= comm_proc_null) then
          call unpack_srmatbox(srg%nrange(idim, idir), srg%srmatbox(idim, idir), wf)
        end if
      end do
    end do

    return
  end subroutine sendrecv7d


  subroutine sendrecv7d(srg, rg)


  end subroutine
  
end module sendrecv_grid
