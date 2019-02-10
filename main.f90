program main
    use salmon_parallel
    implicit none
    character(64) :: logfile

    call setup_parallel()

    write(logfile, '("log", i6.6, ".txt")') nproc_id_global
    write(*, '(i6, x, i6, x, a)') nproc_size_global, nproc_id_global, logfile
    open(777, file=logfile)



    ! call test001()
    ! call test002()
    ! call test003()
    ! call test004()


    close(777)
    call end_parallel()

    stop
end program main

