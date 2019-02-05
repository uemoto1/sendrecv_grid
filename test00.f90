


subroutine test002()
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: N = 3
    integer :: i, j, k
    real(8) :: buff(N, N, N, 1, 1)

    buff(:, :, :, :, :) = 0d0
    if (nproc_id_global == 0) then
        do i = 1, N
            do j = 1, N
                do k = 1, N
                    buff(i, j, k, 1, 1) = 1d0 * i + 1d1 * j + 1d2 * k
                end do
            end do
        end do
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


program main
    use salmon_parallel
    implicit none

    character(64) :: logfile

    call setup_parallel()
    write(logfile, '("log", i6.6, ".txt")') nproc_id_global
    open(777, file=logfile)

    !call test001()
    call test002()

    close(777)
    call end_parallel()

    stop
end program main

