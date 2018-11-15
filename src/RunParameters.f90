module RunParameters
    use Constants
    implicit none

    ! Time stepping parammeters
    integer :: rp_nTimeSteps, & ! maximum number of time steps
               rp_myIter        ! current model iteration count
    real    :: rp_startTime, &  ! integration start time/s
               rp_endTime,   &  ! integration ending time/s
               rp_deltaT,    &  ! time step/s
               rp_myTime        ! current model time/s

    ! Input file names
    character(len=c_MAX_LEN_FILENAME) :: &
        rp_initialConditionsFileName,    & ! file name for inital conditions
        rp_restartDataFileName,          & ! file name for final or "pickup" values
        rp_referenceDataFileName,        & ! file name for reference data
        rp_observationsFileName            ! file name for observations
   
end module RunParameters
