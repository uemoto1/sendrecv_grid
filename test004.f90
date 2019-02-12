! 通信時間計測: 孤立境界条件

subroutine test004()
    use sendrecv_grid
    use salmon_parallel
    use salmon_communication
    use omp_lib
    implicit none
    integer :: mx, my, mz, nx, ny, nz, nb

    integer, parameter :: nd = 4, nt = 10000
    
    integer :: myrank
    integer :: ix, iy, iz, ib, it
    integer :: itmp, itmp3d(1:3)

    integer, allocatable :: map_id(:, :, :)
    integer, allocatable :: id_map(:, :)

    type(s_sendrecv_grid4d) :: srg
    integer :: is(1:3)
    integer :: ie(1:3)
    integer :: neig(1:3, 1:2)

    integer :: idir, iside, itype
    real(8), allocatable :: temp(:, :, :, :)

    real(8) :: t1, t2

    if (comm_is_root(nproc_id_global)) then 
        read(*, *) mx, my, mz, nx, ny, nz, nb
    end if
    call comm_bcast(mx ,nproc_group_global)
    call comm_bcast(my ,nproc_group_global)
    call comm_bcast(mz ,nproc_group_global)
    call comm_bcast(nx ,nproc_group_global)
    call comm_bcast(ny ,nproc_group_global)
    call comm_bcast(nz ,nproc_group_global)
    call comm_bcast(nb ,nproc_group_global)
    write(777, '("#args:", 7(i5,1x))')  mx, my, mz, nx, ny, nz, nb

    if (nproc_size_global /= mx * my * mz) then
        stop "Number of Procs Mismatch!"
    endif

    allocate(map_id(0:mx+1,0:my+1,0:mz+1))
    allocate(id_map(0:mx*my*mz-1, 1:3))
    



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

    t1 = omp_get_wtime()
    do it = 1, nt
    call update_overlap(srg, temp)
    end do
    t2 = omp_get_wtime()

    write(777, '("cputime:", es24.15e4)') t2 - t1

    return

end subroutine test004


