


subroutine test001()
    implicit none
    write(*,*) "test001"
end subroutine test001


program main
    implicit none

    call test001()
    stop
end program main

