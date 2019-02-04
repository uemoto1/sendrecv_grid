


subroutine test002()
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: N = 16
    integer :: i, buff(N)

    if (comm_is_root(nproc_id_global)) then
        do i = 1, N
            buff(i) = i
        end do
        write(777, *) "test001: initial state"
        write(777,'(99(X,I5))') buff(1:N)
    end if

    call comm_bcast(buff, nproc_group_global)
    write(777, *) "test001: bcasted"
    write(777,'(99(X,I5))') buff(1:N)
end subroutine test002



subroutine test001()
    use salmon_parallel
    use salmon_communication
    implicit none
    integer, parameter :: N = 16
    integer :: i, buff(N)

    if (comm_is_root(nproc_id_global)) then
        do i = 1, N
            buff(i) = i
        end do
        write(777, *) "test001: initial state"
        write(777,'(99(X,I5))') buff(1:N)
    end if

    call comm_bcast(buff, nproc_group_global)
    write(777, *) "test001: bcasted"
    write(777,'(99(X,I5))') buff(1:N)
end subroutine test001


program main
    use salmon_parallel
    implicit none

    character(64) :: logfile

    call setup_parallel()
    write(logfile, '("log", i6.6, ".txt")') nproc_id_global
    open(777, file=logfile)

    call test001()

    close(777)
    call end_parallel()

    stop
end program main

