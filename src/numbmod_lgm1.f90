subroutine numbmod(n)
    !================================================
    ! This subroutine sets the number
    ! of control variables
    !================================================
    implicit none

    integer n

    ! Here you determine whether you differentiate with respect to 
    ! n = 5 ! diff0, diff2, diff4, alw and hocn ("Present-day (PD) case no. 1")
    ! n = 6 ! diff0, diff2, diff4, alw, blw and hocn ("Present-day (PD) case no. 2")
    n = 1 ! dqco2x2 ("LGM case no. 1")
    ! n = 4 ! diff0, diff2, diff4, dqco2x2 ("LGM case no. 2")

end subroutine
