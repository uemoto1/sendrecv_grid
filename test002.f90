! 単一ノード内の自己通信の実験

subroutine test001()
    use sendrecv_grid
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: nx = 10
    integer, parameter :: ny = 10
    integer, parameter :: nz = 10
    integer, parameter :: nb = 3
    integer, parameter :: nd = 4
    integer :: ix, iy, iz

    type(s_sendrecv_grid4d) :: srg
    integer :: is(1:3)
    integer :: ie(1:3)
    integer :: neig(1:3, 1:2)

    integer :: idir, iside, itype
    real(8) :: temp(1-nd:nx+nd,1-nd:ny+nd,1-nd:nz+nd, 1:nb)

    neig(1:3, 1:2) = nproc_id_global
    is(1:3) = (/ 1, 1, 1/)
    ie(1:3) = (/ nx, ny, nz/)

    call init_sendrecv_grid4d(srg, nproc_group_global, nproc_id_global, is, ie, nb, nd, neig)
    call alloc_cache_real8(srg)

end subroutine test002


