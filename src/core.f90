subroutine core()
    use Sizes
    use RunParameters
    use Ebm1DGrids
    use Ebm1DVariables
    use Ebm1DParameters
    use Cost1DVariables
    implicit none

    ! Local variables
    integer :: j         ! latitudinal index
    real    :: dayOfYear ! relative day number referenced to the beginning
                         ! of the current year (real)
    real, external :: orbTime2Longitude ! external function to compute
                                        ! Earth's true longitude

! TAMC directive to initialize a tape for the trajectory
! Note: In Fortran 90 free-format style, TAMC directives apparently begin with
! '!adj' rather than 'cadj'.
!adj init tape1 = COMMON, 2000000

    ! Initialize model time
    rp_myTime    = rp_startTime ! model time/s, how to move to readRunParameters?
    ev_deltaTBeg = 0.5          ! assumes that integration starts at 1 January, 0:00
    dayOfYear    = ev_deltaTBeg + 0.5*rp_deltaT/86400.0
    ev_deltaTEnd = ev_deltaTBeg +     rp_deltaT/86400.0

    ! Time loop
    do rp_myIter=1,rp_nTimeSteps

!adj store ev_tsfc = tape1, key = rp_myIter
!adj store rp_myTime = tape1, key = rp_myIter

!adj store dayOfYear = tape1, key = rp_myIter
!adj store ev_deltaTBeg = tape1, key = rp_myIter
!adj store ev_deltaTEnd = tape1, key = rp_myIter
    
        ! Record simulated time
        rp_myTime = rp_myTime + rp_deltaT

        ! Compute relative day number referenced to the beginning
        ! of the current year          
        dayOfYear = dayOfYear + rp_deltaT/86400.0
        if (dayOfYear > 365.5) dayOfYear = dayOfYear - 365.0

        ! Repeat for beginning and end of time step
        ev_deltaTBeg = ev_deltaTBeg + rp_deltaT/86400.0
        if (ev_deltaTBeg > 365.5) ev_deltaTBeg = ev_deltaTBeg - 365.0
        ev_deltaTEnd = ev_deltaTEnd + rp_deltaT/86400.0
        if (ev_deltaTEnd > 365.5) ev_deltaTEnd = ev_deltaTEnd - 365.0
        
        ! Compute true longitude of the Earth with respect to winter solstice
        ! ep_lambdap = orbTime2Longitude(ep_eccen, ep_perihp, dayOfYear)
        ! ep_lambda  = ep_lambdap - 90.0
        ! AP 2011-01-04: renamed to ev_lambdap and ev_lambda
        ev_lambdap = orbTime2Longitude(ep_eccen, ep_perihp, dayOfYear)
        ev_lambda  = ev_lambdap - 90.0

        ! >>> Temporary fix: As long as "orbLongitude2Time" does not work properly, <<<
        ! >>> the beginnning and ending days for taking long-term (seasonal) means  <<<
        ! >>> are hard-coded in "model".                                            <<<
        
        ! Compute daily insolation
        do j=2,jmt-1
            ! call orbDaily(ep_scon0, ep_eccen, ep_obliq, ep_perihp, &
            !               ep_lambdap, eg_yt(j), ev_solin(j), ev_cosz(j))
            ! AP 2011-01-04: ep_lambdap renamed to ev_lambdap
            call orbDaily(ep_scon0, ep_eccen, ep_obliq, ep_perihp, &
                          ev_lambdap, eg_yt(j), ev_solin(j), ev_cosz(j))
        end do
        
        ! Call the atmosphere model once for each time step
        call ebm1DIntegrate()

        ! Accumulate long-term means for computing cost function
        call cost1DAccumulateMeans()
             
    end do ! of time loop

end subroutine core
