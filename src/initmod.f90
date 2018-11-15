subroutine initmod(n, x)
    ! INITMOD Set first guess of the parameter vector.
    ! INITMOD is called before the optimization. It must set a first guess of 
    ! the parameter vector. It may also contain the initialization of the
    ! model.
    !
    ! Author:           Andre Paul
    ! Written:          12 April 2010
    ! Last updated:     29 April 2010
    !
    ! Input arguments:
    !     n = number of independent variables
    !
    ! Output arguments:
    !     x = initial values of independent variables
    use OptimizationParameters
    use Ebm1DParameters
    implicit none

    ! Dummy arguments
    integer :: n
    real, dimension(1:n) :: x
    
    ! Local variables
    integer :: i

    ! Set fixed model parameters and arrays
    call initializeFixed()

    ! Read input data
    call readInitialConditions()
    call readReferenceData()
    call readObservations()

    ! Set scaling factors
    if (n == 5) then
        ! "Present-day (PD) case no. 1"
        cond(1) = ep_diff0
        cond(2) = ep_diff2
        cond(3) = ep_diff4
        cond(4) = ep_alw
        cond(5) = ep_hocn
    else if (n == 6) then
        ! "Present-day (PD) case no. 2"
        cond(1) = ep_diff0
        cond(2) = ep_diff2
        cond(3) = ep_diff4
        cond(4) = ep_alw
        cond(5) = ep_blw  
        cond(6) = ep_hocn 
    else if (n == 1) then
        ! "LGM case no. 1"
        cond(1) = ep_dqco2x2
    else if (n == 4) then
        ! "LGM case no. 2"
        cond(1) = ep_diff0
        cond(2) = ep_diff2
        cond(3) = ep_diff4
        cond(4) = ep_dqco2x2  
    end if
    ! do i=1,n
    !     cond(i) = 1.0
    ! end do
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a)') ">>> Scaling factors:"
    do i=1,n
        write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, i2, a, f10.3, a)') "cond(", i,") = ", cond(i)
    end do

    ! Set first guess of (potential) control variables and do scaling/
    ! conditioning using scaling factors "cond(i)".
    if (n == 5) then
        ! "Present-day (PD) case no. 1"
        x(1) = ep_diff0/cond(1)
        x(2) = ep_diff2/cond(2)
        x(3) = ep_diff4/cond(3)
        x(4) = ep_alw/cond(4)   
        x(5) = ep_hocn/cond(5)  
    else if (n == 6) then
        ! "Present-day (PD) case no. 2"
        x(1) = ep_diff0/cond(1)
        x(2) = ep_diff2/cond(2)
        x(3) = ep_diff4/cond(3)
        x(4) = ep_alw/cond(4)   
        x(5) = ep_blw/cond(5)   
        x(6) = ep_hocn/cond(6)  
    else if (n == 1) then
        ! "LGM case no. 1"
        x(1) = ep_dqco2x2/cond(1)  
    else if (n == 4) then
        ! "LGM case no. 2"
        x(1) = ep_diff0/cond(1)
        x(2) = ep_diff2/cond(2)
        x(3) = ep_diff4/cond(3)
        x(4) = ep_dqco2x2/cond(4)  
    end if

end subroutine initmod
