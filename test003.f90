! 隣接ノードからのデータ転送実験

subroutine test003(mx, my, mz)
    use sendrecv_grid
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, intent(in) :: mx, my, mz

    integer, parameter :: nd = 4
    integer, parameter :: nb = 2
    integer, parameter :: nx = 12
    integer, parameter :: ny = 14
    integer, parameter :: nz = 16
    integer :: myrank
    integer :: ix, iy, iz, ib
    integer :: itmp, itmp3d(1:3)


    integer :: map_id(0:mx+1,0:my+1,0:mz+1)
    integer :: id_map(0:mx*my*mz-1, 1:3)

    type(s_sendrecv_grid4d) :: srg
    integer :: is(1:3)
    integer :: ie(1:3)
    integer :: neig(1:3, 1:2)

    integer :: idir, iside, itype
    real(8), allocatable :: temp(:, :, :, :)

    myrank = nproc_id_global
    map_id = comm_proc_null

    itmp = 0
    do ix = 1, mx
        do iy = 1, my
            do iz = 1, mz
                map_id(ix, iy, iz) = itmp
                id_map(itmp, 1:3) = (/ix, iy, iz/)
                itmp = itmp + 1
            end do
        end do
    end do
    ! periodic for z-axis
    map_id(:, :, 0) = map_id:, :, mz)
    map_id(:, :, mz+1) = map_id(:, :, 1)

    do idir = 1, 3
        do iside = 1, 2
            itmp3d(1:3) = id_map(myrank, 1:3)
            if (iside == 1) then
                itmp3d(idir) = itmp3d(idir) + 1
            else
                itmp3d(idir) = itmp3d(idir) - 1
            end if
            neig(idir, iside) = map_id(itmp3d(1), itmp3d(2), itmp3d(3))
        end do
    end do

    is(1:3) = (/nx, ny, nz/) * (id_map(myrank, 1:3) - 1) + 1
    ie(1:3) = (/nx, ny, nz/) * id_map(myrank, 1:3)

    allocate(temp(is(1)-nd:ie(1)+nd,is(2)-nd:ie(2)+nd,is(3)-nd:ie(3)+nd,1:nb))

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

    do ix = is(1), ie(1)
        do iy = is(2), ie(2)
            do iz = is(3), ie(3)
                temp(ix, iy, iz, :) = 1d0 * ix + 2d0 * iy + 3d0 * iz
            end do
        end do
    end do

    call update_overlap(srg, temp)

    do idir = 1, 3
        do iside = 1, 2
            do itype = 1, 2
                write(777, '("#ireq:", 3(i5,1x), "=", i5)') idir, iside, itype, srg%ireq(idir, iside, itype)
            end do
        end do
    end do

    call update_overlap(srg, temp)
    call update_overlap(srg, temp)
    call update_overlap(srg, temp)
    call update_overlap(srg, temp)


    do ix = is(1)-nd, ie(1)+nd
        do iy = is(2)-nd, ie(2)+nd
            do iz = is(3)-nd, ie(3)+nd
                do ib = 1, nb
                    write(777, '(4(i5,1x),f7.3,1x,a)') ix, iy, iz, ib, temp(ix, iy, iz,ib), "#after"
                end do
            end do
        end do
    end do

    write(777, "(a)") "#SAFE POINT"
    flush(777)
    return

end subroutine test003


