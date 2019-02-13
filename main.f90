program main
    use salmon_parallel
    implicit none
    character(64) :: logfile

    call setup_parallel()

    write(logfile, '("log", i6.6, ".txt")') nproc_id_global
    write(*, '(i6, x, i6, x, a)') nproc_size_global, nproc_id_global, logfile
    open(777, file=logfile)

    ! call test001()
    ! call test002(4, 1, 1)
    ! call test003(2, 2, 4)
    ! call test004()
    call test005()

    close(777)
    call end_parallel()

    stop
end program main

