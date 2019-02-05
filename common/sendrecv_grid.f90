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
  implicit none


  type s_sendrecv_grid
    integer :: nshape(1:3)
    integer :: nrange(1:3,1:1)
    integer :: ireq(1:12)
    integer :: iup_array(1:4)
    integer :: idw_array(1:4)
    integer :: jup_array(1:4)
    integer :: jdw_array(1:4)
    integer :: kup_array(1:4)
    integer :: kdw_array(1:4)
    ! Temporaly buffer for persistent communication:
    real(8), allocatable :: r_srmatbox1_x_3d(:,:,:)
    real(8), allocatable :: r_srmatbox3_x_3d(:,:,:)
    real(8), allocatable :: r_srmatbox1_y_3d(:,:,:)
    real(8), allocatable :: r_srmatbox3_y_3d(:,:,:)
    real(8), allocatable :: r_srmatbox1_z_3d(:,:,:)
    real(8), allocatable :: r_srmatbox3_z_3d(:,:,:)
    real(8), allocatable :: r_srmatbox2_x_3d(:,:,:)
    real(8), allocatable :: r_srmatbox4_x_3d(:,:,:)
    real(8), allocatable :: r_srmatbox2_y_3d(:,:,:)
    real(8), allocatable :: r_srmatbox4_y_3d(:,:,:)
    real(8), allocatable :: r_srmatbox2_z_3d(:,:,:)
    real(8), allocatable :: r_srmatbox4_z_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox1_x_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox3_x_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox1_y_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox3_y_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox1_z_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox3_z_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox2_x_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox4_x_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox2_y_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox4_y_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox2_z_3d(:,:,:)
    complex(16), allocatable :: c_srmatbox4_z_3d(:,:,:)
  end type s_sendrecv_grid

  contains
  subroutine sendrecv(srg, rg, wf)
    use structures, only: s_rgrid, s_wavefunction
    ! use salmon_parallel, only 
    use salmon_communication, only: comm_start_all, comm_proc_null
    use pack_unpack
    implicit none
    type(s_sendrecv_grid), intent(inout) :: srg
    type(s_rgrid),         intent(in)    :: rg
    type(s_wavefunction),  intent(inout) :: wf

    integer :: iup,idw,jup,jdw,kup,kdw
  


    iup = srg%iup_array(1)
    idw = srg%idw_array(1)
    jup = srg%jup_array(1)
    jdw = srg%jdw_array(1)
    kup = srg%kup_array(1)
    kdw = srg%kdw_array(1)

    srg%iup_array(1:3) = 0
    
    !send from idw to iup
    if(iup/=comm_proc_null)then
      call pack_data(srg%nshape(1:3), srg%nrange, wf%rwf(:,:,:,1,1,1,1), srg%r_srmatbox1_x_3d)
    end if
    call comm_start_all(srg%ireq(1:2))

    !send from idw to iup
    if(iup/=comm_proc_null)then
      call pack_data(srg%nshape, srg%nrange, wf%rwf(:,:,:,1,1,1,1), srg%r_srmatbox1_x_3d)
    end if
    call comm_start_all(srg%ireq(1:2))
  
    !send from iup to idw
    if(idw/=comm_proc_null)then
      call pack_data(srg%nshape, srg%nrange, wf%rwf(:,:,:,1,1,1,1), srg%r_srmatbox3_x_3d)
    end if
    call comm_start_all(srg%ireq(3:4))
  
    !send from jdw to jup
    if(jup/=comm_proc_null)then
      call pack_data(srg%nshape, srg%nrange, wf%rwf(:,:,:,1,1,1,1), srg%r_srmatbox1_y_3d)
    end if
    call comm_start_all(srg%ireq(5:6))
  
    !send from jup to jdw
    if(jdw/=comm_proc_null)then
      call pack_data(srg%nshape, srg%nrange, wf%rwf(:,:,:,1,1,1,1), srg%r_srmatbox3_y_3d)
    end if
    call comm_start_all(srg%ireq(7:8))
  
    !send from kdw to kup
    if(kup/=comm_proc_null)then
      call pack_data(srg%nshape, srg%nrange, wf%rwf(:,:,:,1,1,1,1), srg%r_srmatbox1_z_3d)
    end if
    call comm_start_all(srg%ireq(9:10))
  
    !send from kup to kdw
    if(kdw/=comm_proc_null)then
      call pack_data(srg%nshape, srg%nrange, wf%rwf(:,:,:,1,1,1,1), srg%r_srmatbox3_z_3d)
    end if
    call comm_start_all(srg%ireq(11:12))
  
    call comm_wait_all(srg%ireq(1:2))
    if(idw/=comm_proc_null)then
      call unpack_data(srg%nshape, srg%nrange, srg%r_srmatbox2_x_3d, wf%rwf(:,:,:,1,1,1,1))
    end if
  
    call comm_wait_all(srg%ireq(3:4))
    if(iup/=comm_proc_null)then
      call unpack_data(srg%nshape, srg%nrange, srg%r_srmatbox4_x_3d, wf%rwf(:,:,:,1,1,1,1))
    end if
  
    call comm_wait_all(srg%ireq(5:6))
    if(jdw/=comm_proc_null)then
      call unpack_data(srg%nshape, srg%nrange, srg%r_srmatbox2_y_3d, wf%rwf(:,:,:,1,1,1,1))
    end if
  
    call comm_wait_all(srg%ireq(7:8))
    if(jup/=comm_proc_null)then
      call unpack_data(srg%nshape, srg%nrange, srg%r_srmatbox4_y_3d, wf%rwf(:,:,:,1,1,1,1))
    end if
  
    call comm_wait_all(srg%ireq(9:10))
    if(kdw/=comm_proc_null)then
      call unpack_data(srg%nshape, srg%nrange, srg%r_srmatbox2_z_3d, wf%rwf(:,:,:,1,1,1,1))
    end if
  
    call comm_wait_all(srg%ireq(11:12))
    if(kup/=comm_proc_null)then
      call unpack_data(srg%nshape, srg%nrange, srg%r_srmatbox4_z_3d, wf%rwf(:,:,:,1,1,1,1))
    end if

    return
  
  end subroutine sendrecv
  
end module sendrecv_grid
