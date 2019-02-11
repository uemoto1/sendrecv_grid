! 単一ノード内の自己通信の実験

subroutine test001()
    use sendrecv_grid
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: nd = 4
    integer, parameter :: nb = 10
    integer, parameter :: nx = 12
    integer, parameter :: ny = 14
    integer, parameter :: nz = 16
    integer :: ix, iy, iz, ib

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

    ! Check initialization
    write(777, '("#is:", 3(i5,1x))') srg%is
    write(777, '("#ie:", 3(i5,1x))') srg%ie
    write(777, '("#is_overlap:", 3(i5,1x))') srg%is_overlap
    write(777, '("#ie_overlap:", 3(i5,1x))') srg%ie_overlap
    write(777, '("#nd:", i5)') srg%nd
    write(777, '("#nb:", i5)') srg%nb
    write(777, '("#icomm:", i5)') srg%icomm
    write(777, '("#myrank:", i5)') srg%myrank
    do idir = 1, 3
        do iside = 1, 2
            write(777, '("#neig:", 2(i5, 1x), "=", i5)') idir, iside, neig(idir, iside)
        end do
    end do
    do idir = 1, 3
        do iside = 1, 2
            do itype = 1, 2
                write(777, '("#is_block:", 3(i5,1x), "=", 3(i5,1x))') idir, iside, itype, srg%is_block(idir, iside, itype, 1:3)
                write(777, '("#ie_block:", 3(i5,1x), "=", 3(i5,1x))') idir, iside, itype, srg%ie_block(idir, iside, itype, 1:3)
            end do
        end do
    end do


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
                do ib = 1, nb
                    write(777, '(4(i5,1x),f7.3,1x,a)') ix, iy, iz, ib, temp(ix, iy, iz,ib), "#before"
                end do
            end do
        end do
    end do

    call update_overlap(srg, temp)

    do ix = 1-nd, nx+nd
        do iy = 1-nd, ny+nd
            do iz = 1-nd, nz+nd
                do ib = 1, nb
                    write(777, '(4(i5,1x),f7.3,1x,a)') ix, iy, iz, ib, temp(ix, iy, iz,ib), "#after"
                end do
            end do
        end do
    end do

end subroutine test001


