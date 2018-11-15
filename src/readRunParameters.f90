subroutine readRunParameters
    use Constants
    use RunParameters
    implicit none
    ! Read run parameters from file "RunParameters.nml"
      
    ! Local variables
    integer :: errIO ! IO error flag
    real :: startTime, &  ! time to start/a
            endTime,   &  ! time to stop/a
            deltaT        ! time step/s
    character(len=c_MAX_LEN_FILENAME) ::  parameterFileName,         &
                                          initialConditionsFileName, &
                                          restartDataFileName,       &
                                          referenceDataFileName,     &
                                          observationsFileName
      
    ! Namelist variables
    namelist /run_parameters/ startTime, endTime, deltaT,           &
                                         initialConditionsFileName, &
                                         restartDataFileName,       &
                                         referenceDataFileName,     &
                                         observationsFileName

    ! Format statements
    9000 format (1X,10A)
   
    ! Open parameter file
    parameterFileName = "RunParameters.nml"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000)      & 
        "Run parameters are read from namelist file '", &
         trim( parameterFileName),"':"
    open(unit=c_MODEL_DATA_UNIT, file= parameterFileName, status="old", iostat=errIO)
    read (unit=c_MODEL_DATA_UNIT, nml=run_parameters, iostat=errIO)
    close(c_MODEL_DATA_UNIT)

    ! Report contents of model parameter file
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "startTime = ", startTime, " a"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "endTime   = ", endTime  , " a"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "deltaT    = ", deltaT   , " s"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, a, a)') "initialConditionsFileName = '", &
        trim(initialConditionsFileName), "'"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a ,a, a)') "restartDataFileName       = '", &
        trim(restartDataFileName), "'"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, a, a)') "referenceDataFileName     = '", &
        trim(referenceDataFileName), "'"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, a ,a)') "observationsFileName      = '", &
        trim(observationsFileName), "'"
   
    ! Convert units
    rp_startTime = startTime*c_DAYS_PER_YEAR*c_SECONDS_PER_DAY
    rp_endTime   = endTime*c_DAYS_PER_YEAR*c_SECONDS_PER_DAY
 
    ! Set run parameters
    rp_deltaT     = deltaT                                   ! time step/s
    rp_nTimeSteps = (rp_endTime - rp_startTime)/rp_deltaT    ! maximum number of time steps
    ! rp_myTime     = rp_startTime                             ! model time/s, see core
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f20.1, a)') "startTime  = ", rp_startTime, " s"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f20.1, a)') "endTime    = ", rp_endTime  , " s"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, I20)')      "nTimeSteps = ", rp_nTimeSteps
    
    rp_initialConditionsFileName = initialConditionsFileName ! file name for inital conditions
    rp_restartDataFileName       = restartDataFileName       ! file name for final or "pickup" values
    rp_referenceDataFileName     = referenceDataFileName     ! file name for reference data
    rp_observationsFileName      = observationsFileName      ! file name for observations

end subroutine readRunParameters
