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
    integer :: iup_array(1:4)
    integer :: idw_array(1:4)
    integer :: jup_array(1:4)
    integer :: jdw_array(1:4)
    integer :: kup_array(1:4)
    integer :: kdw_array(1:4)
    real(8), allocatable :: r_srmatbox1_x_3d(:,:,:)
    real(8), allocatable :: r_srmatbox3_x_3d(:,:,:)
    real(8), allocatable :: r_srmatbox1_y_3d(:,:,:)
    real(8), allocatable :: r_srmatbox3_y_3d(:,:,:)
    real(8), allocatable :: r_srmatbox1_z_3d(:,:,:)
    real(8), allocatable :: r_srmatbox3_z_3d(:,:,:)
    real(8), allocatable :: c_srmatbox1_x_3d(:,:,:)
    real(8), allocatable :: c_srmatbox3_x_3d(:,:,:)
    real(8), allocatable :: c_srmatbox1_y_3d(:,:,:)
    real(8), allocatable :: c_srmatbox3_y_3d(:,:,:)
    real(8), allocatable :: c_srmatbox1_z_3d(:,:,:)
    real(8), allocatable :: c_srmatbox3_z_3d(:,:,:)
  end type s_sendrecv_grid

  subroutine sendrecv_grid(srg, rg, wf)
    implicit none
    s_sendrecv_grid, intent(in)    :: srg
    s_rgrid,         intent(in)    :: rg
    s_wavefunction,  intent(inout) :: wf

    integer :: iup,idw,jup,jdw,kup,kdw
  
    iup = srg % iup_array(1)
    idw = srg % idw_array(1)
    jup = srg % jup_array(1)
    jdw = srg % jdw_array(1)
    kup = srg % kup_array(1)
    kdw = srg % kdw_array(1)
    
    !send from idw to iup
    if(iup/=comm_proc_null)then
      call pack_data(nshape(1:3), nrange(1:3,1), tpsi, srmatbox1_x_3d)
    end if
    call comm_start_all(ireq(1:2))

    !send from idw to iup
    if(iup/=comm_proc_null)then
      call pack_data(nshape(1:3), nrange(1:3,1), tpsi, srmatbox1_x_3d)
    end if
    call comm_start_all(ireq(1:2))
  
    !send from iup to idw
    if(idw/=comm_proc_null)then
      call pack_data(nshape(1:3), nrange(1:3,2), tpsi, srmatbox3_x_3d)
    end if
    call comm_start_all(ireq(3:4))
  
    !send from jdw to jup
    if(jup/=comm_proc_null)then
      call pack_data(nshape(1:3), nrange(1:3,3), tpsi, srmatbox1_y_3d)
    end if
    call comm_start_all(ireq(5:6))
  
    !send from jup to jdw
    if(jdw/=comm_proc_null)then
      call pack_data(nshape(1:3), nrange(1:3,4), tpsi, srmatbox3_y_3d)
    end if
    call comm_start_all(ireq(7:8))
  
    !send from kdw to kup
    if(kup/=comm_proc_null)then
      call pack_data(nshape(1:3), nrange(1:3,5), tpsi, srmatbox1_z_3d)
    end if
    call comm_start_all(ireq(9:10))
  
    !send from kup to kdw
    if(kdw/=comm_proc_null)then
      call pack_data(nshape(1:3), nrange(1:3,6), tpsi, srmatbox3_z_3d)
    end if
    call comm_start_all(ireq(11:12))
  
    call comm_wait_all(ireq(1:2))
    if(idw/=comm_proc_null)then
      call unpack_data(nshape(1:3), nrange(1:3,7), srmatbox2_x_3d, tpsi)
    end if
  
    call comm_wait_all(ireq(3:4))
    if(iup/=comm_proc_null)then
      call unpack_data(nshape(1:3), nrange(1:3,8), srmatbox4_x_3d, tpsi)
    end if
  
    call comm_wait_all(ireq(5:6))
    if(jdw/=comm_proc_null)then
      call unpack_data(nshape(1:3), nrange(1:3,9), srmatbox2_y_3d, tpsi)
    end if
  
    call comm_wait_all(ireq(7:8))
    if(jup/=comm_proc_null)then
      call unpack_data(nshape(1:3), nrange(1:3,10), srmatbox4_y_3d, tpsi)
    end if
  
    call comm_wait_all(ireq(9:10))
    if(kdw/=comm_proc_null)then
      call unpack_data(nshape(1:3), nrange(1:3,11), srmatbox2_z_3d, tpsi)
    end if
  
    call comm_wait_all(ireq(11:12))
    if(kup/=comm_proc_null)then
      call unpack_data(nshape(1:3), nrange(1:3,12), srmatbox4_z_3d, tpsi)
    end if
  
  end subroutine sendrecv_grid
  
end module sendrecv_grid
