program main
    implicit none
    call fun_1()
contains
    subroutine fun_0()
    end subroutine fun_0

    subroutine fun_1()
        call fun_0()
        write (*, "(A)", advance="no") "C"
    end subroutine fun_1
end program main
