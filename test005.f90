! 通信時間計測: 孤立境界条件

subroutine test005()
    use structures
    use sendrecv_grid
    use salmon_parallel
    use salmon_communication
    use omp_lib
    implicit none
    integer :: mx, my, mz, nx, ny, nz, nb

    integer, parameter :: nd = 4, nt = 1000
    
    integer :: icomm, myrank
    integer :: ix, iy, iz, ib, it
    integer :: itmp, itmp3d(1:3)

    integer, allocatable :: map_id(:, :, :)
    integer, allocatable :: id_map(:, :)

    type(s_sendrecv_grid4d) :: srg
    type(s_rgrid) :: rg
    
    integer :: neig(1:3, 1:2)

    integer :: idir, iside, itype
    complex(8), allocatable :: data4d(:, :, :, :)

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

    ! ノード番号確認
    icomm = nproc_group_global
    myrank = nproc_id_global

    ! テスト用ノード配置マップを作成
    allocate(map_id(0:mx+1,0:my+1,0:mz+1))
    allocate(id_map(0:mx*my*mz-1, 1:3))
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

    ! テスト用実空間グリッド情報
    rg%nd = nd
    rg%is(1:3) = (/nx, ny, nz/) * id_map(myrank, :) + 1
    rg%ie(1:3) = (/nx, ny, nz/) * (id_map(myrank, :) + 1)
    rg%is_overlap = rg%is - nd
    rg%ie_overlap = rg%ie + nd
    rg%is_array = rg%is_overlap
    rg%ie_array = rg%ie_overlap

    allocate(data4d( &
        rg%is_array(1):rg%ie_array(1), &
        rg%is_array(2):rg%ie_array(2), &
        rg%is_array(3):rg%ie_array(3), &
        1:nb))

    ! 初期化
    call init_sendrecv_grid4d(srg, rg, nb, icomm, myrank, neig)
    
    ! 初期化状態のチェック
    write(777, '("#is:", 3(i5,1x))') srg%rg%is
    write(777, '("#ie:", 3(i5,1x))') srg%rg%ie
    write(777, '("#is_overlap:", 3(i5,1x))') srg%rg%is_overlap
    write(777, '("#ie_overlap:", 3(i5,1x))') srg%rg%ie_overlap
    write(777, '("#is_array:", 3(i5,1x))') srg%rg%is_array
    write(777, '("#ie_array:", 3(i5,1x))') srg%rg%ie_array
    write(777, '("#nd:", i5)') srg%rg%nd
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

    call alloc_cache_complex8(srg)

    ! 領域確保のチェック
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


    data4d = 0d0

    do ix = rg%is(1), rg%ie(1)
        do iy = rg%is(2), rg%ie(2)
            do iz = rg%is(3), rg%ie(3)
                data4d(ix, iy, iz, :) = dcmplx(2,2) + exp(dcmplx(0, 1d-1 * ix + 2d-2 * iy + 3d-3 * iz))
            end do
        end do
    end do

    write(777, '("#start communication:", i5)') nt

    t1 = omp_get_wtime()
    do it = 1, nt
    call update_overlap(srg, data4d)
    end do
    t2 = omp_get_wtime()

    write(777, '("#cputime:", es24.15e4)') t2 - t1

    ! 通信結果の出力
    do ix = rg%is_overlap(1), rg%ie_overlap(1)
        do iy = rg%is_overlap(2), rg%ie_overlap(2)
            do iz = rg%is_overlap(3), rg%ie_overlap(3)
                do ib = 1, nb
                    write(777, '(4(i5,1x),2(f7.3,1x),a)') &
                    ix, iy, iz, ib, real(data4d(ix, iy, iz,ib)), &
                    aimag(data4d(ix, iy, iz,ib)), "#Re,Im"
                end do
            end do
        end do
    end do

    return

end subroutine test005


