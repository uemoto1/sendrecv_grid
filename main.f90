program main
    use salmon_parallel
    implicit none
    character(64) :: logfile

    call setup_parallel()

    write(logfile, '("log", i6.6, ".txt")') nproc_id_global
    write(*, '(i6, x, i6, x, a)') nproc_size_global, nproc_id_global, logfile
    open(777, file=logfile)

    ! call test001()
    ! call test002(nproc_size_global, 1, 1)
    if (nproc_size_global == 8) then
        call test003(2, 2, 2)
    end if
    ! call test004()

    close(777)
    call end_parallel()

    stop
end program main

