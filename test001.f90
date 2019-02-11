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

    ! Check allocated region
    do idir = 1, 3
        do iside = 1, 2
            do itype = 1, 2
                write(777, '(a)') "################"
                write(777, '("#cache-index:",3(i5,1x))') idir, iside, itype
                write(777, '("#cache-size:",4(i5,1x))') shape(srg%cache(idir, iside, itype)%dbuf)
                write(777, '("#cache-range:",4(i5,":",i5,1x))') &
                lbound(srg%cache(idir, iside, itype)%dbuf, 1), &
                ubound(srg%cache(idir, iside, itype)%dbuf, 1), &
                lbound(srg%cache(idir, iside, itype)%dbuf, 2), &
                ubound(srg%cache(idir, iside, itype)%dbuf, 2), &
                lbound(srg%cache(idir, iside, itype)%dbuf, 3), &
                ubound(srg%cache(idir, iside, itype)%dbuf, 3), &
                lbound(srg%cache(idir, iside, itype)%dbuf, 4), &
                ubound(srg%cache(idir, iside, itype)%dbuf, 4)
            end do
        end do
    end do
    flush(777)

    temp = 0d0

    do ix = 1, nx
        do iy = 1, ny
            do iz = 1, nz
                temp(ix, iy, iz, :) = 1d0 * ix + 2d0 * iy + 3d0 * iz
            end do
        end do
    end do

    do ix = 1-nd, nx+nd
        do iy = 1-nd, ny+nd
            do iz = 1-nd, nz+nd
                write(777, '(3(i5,1x),f7.3,1x,a)') ix, iy, iz, temp(ix, iy, iz,1), "# before"
            end do
        end do
    end do

    call update_overlap(srg, temp)

    do ix = 1-nd, nx+nd
        do iy = 1-nd, ny+nd
            do iz = 1-nd, nz+nd
                write(777, '(3(i5,1x),f7.3,1x,a)') ix, iy, iz, temp(ix, iy, iz,1), "# after"
            end do
        end do
    end do

end subroutine test001


