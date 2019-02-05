! 全体通信のテスト
subroutine test001()
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: N = 16, M = 4
    integer :: i
    real(8) :: buff(N), tmp(N)

    buff(1:N) = 0d0
    if (comm_is_root(nproc_id_global)) then
        do i = 1, N
            buff(i) = i
        end do
    end if
    call comm_sync_all(nproc_group_global)

    write(777, *) "test001: initial state"
    write(777,'(99(X,F5.2))') buff(1:N)

    write(777, *) "test001: comm_bcast"
    call comm_bcast(buff, nproc_group_global)
    write(777,'(99(X,F5.2))') buff(1:N)

    write(777, *) "test001: comm_summation (allreduce)"
    call comm_summation(buff, tmp, N, nproc_group_global)
    write(777,'(99(X,F5.2))') tmp(1:N)    
end subroutine test001


subroutine dummy_data3d(n, buff, iter)
    implicit none
    integer, intent(in) :: n, iter
    real(8), intent(out) :: buff(n, n, n)

    integer :: i, j, k
    
    do i = 1, n
        do j = 1, n
            do k = 1, n
                buff(k, j, i) = iter * 1d0 + i * 1d-1 + j * 1d-2 + k * 1d-3
            end do
        end do
    end do

    return
end subroutine dummy_data3d



! 一対一同期通信のテスト
subroutine test002()
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: N = 3
    real(8) :: buff(N, N, N, 1, 1)

    if (nproc_size_global < 2) then
        write(777,*) "test002: number of proc must be larger than 2. skip."
        return
    end if

    buff(:, :, :, :, :) = 0d0
    if (nproc_id_global == 0) then
        call dummy_data3d(N, buff(:, :, :, 1, 1), 0)
    end if
    write(777,*) "test002: before send/recv"
    write(777,'(99(x, f7.3))') buff

    if (nproc_id_global == 0) then
        write(777,*) "test002: comm_send"
        call comm_send(buff, 1, 0, nproc_group_global)
    elseif (nproc_id_global == 1) then
        write(777,*) "test002: comm_recv"
        call comm_recv(buff, 0, 0, nproc_group_global)
    endif
    write(777,*) "test002: after send/recv"
    write(777,'(99(x, f7.3))') buff

end subroutine test002

! 非同期通信のテスト
subroutine test003()
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: N = 3
    real(8) :: buff(N, N, N)

    integer, parameter :: nreq = 1
    integer :: ireq(nreq)

    if (nproc_size_global < 2) then
        write(777,*) "test003: number of proc must be larger than 2. skip."
        return
    end if

    buff(:, :, :) = 0d0
    if (nproc_id_global == 0) then
        call dummy_data3d(N, buff(:, :, :), 0)
    end if
    write(777,*) "test003: before send/recv"
    write(777,'(99(x, f7.3))') buff

    if (nproc_id_global == 0) then
        write(777,*) "test003: comm_isend"
        ireq(1) = comm_isend(buff, 1, 0, nproc_group_global)
    elseif (nproc_id_global == 1) then
        write(777,*) "test003: comm_irecv"
        ireq(1) = comm_irecv(buff, 0, 0, nproc_group_global)
    else
        write(777,*) "test003: nothing"
        return
    endif
    write(777,*) "test003: after isend/irecv"
    write(777,'(99(x, f7.3))') buff
    call comm_wait_all(ireq(1:nreq))
    write(777,*) "test003: after comm_wait_all"
    write(777,'(99(x, f7.3))') buff
    return
end subroutine test003


! 永続通信のテスト
subroutine test004()
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: N = 3
    integer :: iter
    real(8) :: buff(N, N, N)

    integer, parameter :: nreq = 1
    integer :: ireq(nreq)

    if (nproc_size_global < 2) then
        write(777,*) "test004: number of proc must be larger than 2. skip."
        return
    end if

    if (2 <= nproc_id_global) then
        write(777,*) "test004: do nothing"
        return
    end if

    buff = 0d0
    if (nproc_id_global == 0) then
        call dummy_data3d(N, buff(:, :, :), 0)
    end if
    write(777,*) "test004: before send_init/recv_init"
    write(777,'(99(x, f7.3))') buff

    if (nproc_id_global == 0) then
        write(777,*) "test004: comm_send_init"
        ireq(1) = comm_send_init(buff, 1, 0, nproc_group_global)
    elseif (nproc_id_global == 1) then
        write(777,*) "test004: comm_recv_init"
        ireq(1) = comm_recv_init(buff, 0, 0, nproc_group_global)
    end if
    
    write(777,*) "test004: after comm_send_init/comm_recv_init"
    write(777,'(99(x, f7.3))') buff

    write(777,*) "test004: comm_start_all"
    call comm_start_all(ireq(1:nreq))

    write(777,*) "test004: after comm_start_all"
    write(777,'(99(x, f7.3))') buff

    write(777,*) "test004: comm_wait_all"
    call comm_wait_all(ireq(1:nreq))

    write(777,*) "test004: after comm_wait_all"
    write(777,'(99(x, f7.3))') buff

    do iter = 1, 9
        write(777, '("iter=", i6)') iter

        buff = 0d0
        if (nproc_id_global == 0) then
            call dummy_data3d(N, buff(:, :, :), iter)
        end if

        write(777,*) "test004: before comm_start_all"
        write(777,'(99(x, f7.3))') buff
    
        write(777,*) "test004: comm_start_all"
        call comm_start_all(ireq(1:nreq))
    
        write(777,*) "test004: after comm_start_all"
        write(777,'(99(x, f7.3))') buff
    
        write(777,*) "test004: comm_wait_all"
        call comm_wait_all(ireq(1:nreq))
    
        write(777,*) "test004: after comm_wait_all"
        write(777,'(99(x, f7.3))') buff
    end do

    return
end subroutine test004