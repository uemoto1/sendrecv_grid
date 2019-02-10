subroutine test001()
    use sendrecv_grid
    use salmon_parallel
    use salmon_communication
    implicit none

    type(s_sendrecv_grid4d) :: srg
    integer :: is(1:3)
    integer :: ie(1:3)
    integer :: neig(1:3, 1:2)

    integer :: idir, iside, itype

    neig(1:3, 1:2) = nproc_id_global
    is(1:3) = (/ 1, 1, 1/)
    ie(1:3) = (/ 16, 16, 16/)

    call init_sendrecv_grid4d(srg, nproc_group_global, nproc_id_global, is, ie, 1, 4, neig)
    call alloc_cache_real8(srg)

    do idir = 1, 3
        do iside = 1, 2
            do itype = 1, 2
                 write(777, '("#cache:",7(i5))') idir, iside, itype, shape(srg%cache(idir, iside, itype)%dbuf)
            end do
        end do
    end do
end subroutine test001


