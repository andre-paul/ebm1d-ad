subroutine ebm1DWriteParameters()
    use Constants
    use Ebm1DParameters
    implicit none
    ! Write Ebm1D parameters to file "Ebm1DParametersOut.nml"
      
    ! Local variables
    logical :: useIceAlbedoFeedback
    integer :: errIO ! IO error flag
    real    :: rhowat,  & ! density of pure water at 0 degC/(kg m^-3)
               cp0,     & ! specific heat of liquid water at 0 degC/(J kg^-1 K^-1)
               scon0,   & ! present-day solar constant/(W m^(-2)) (Hartmann 1994)
               pyear,   & ! paleo-year for computing orbital parameters (0 = 1950 AD)
               hocn,    & ! ocean mixed-layer depth/m
               hcrat,   & ! heat capacity ratio, ocean to land
               tcrit,   & ! temperature at which the surface becomes ice covered/degC
               alw,     & ! OLW, constant term/(W m^(-2))
               blw,     & ! OLW, constant factor/(W m^(-2) K^(-1))
               dqco2x2, & ! 2xCO2 radiative forcing/(W m^(-2))
               co2ref,  & ! reference atmospheric CO2 concentration/ppmv
               co2ccn,  & ! actual atmospheric CO2 concentration/ppmv   
               apln0,   & ! background albedo coeffcients       
               apln1,   &
               apln2,   &
               aice,    & ! albedo of snow and ice
               diff0,   & ! diffusion coefficent, constant factor/(m^2 s^(-1)
               diff2,   &
               diff4
    character(len=c_MAX_LEN_FILENAME) :: parameterFileName, gridFileName, &
                                         fracLandFileName,  fracCloudFileName      
      
    ! Namelist variables
    namelist /ebm1d_parameters/ useIceAlbedoFeedback,    &
                                rhowat,  cp0,    scon0,  &   
                                pyear,   hocn,   hcrat,  &
                                tcrit,   alw,    blw,    &
                                dqco2x2, co2ref, co2ccn, &
                                apln0,   apln1,  apln2,  &
                                aice,                    &
                                diff0,   diff2,  diff4,  &
                                gridFileName,            &
                                fracLandFileName,        &
                                fracCloudFileName

    ! Format statements
    9000 format (1x, 10a)

    ! Copy to module variables
    useIceAlbedoFeedback = ep_useIceAlbedoFeedback
    rhowat  = ep_rhowat
    cp0     = ep_cp0
    scon0   = ep_scon0
    pyear   = ep_pyear
    hocn    = ep_hocn
    hcrat   = ep_hcrat
    tcrit   = ep_tcrit
    alw     = ep_alw
    blw     = ep_blw
    dqco2x2 = ep_dqco2x2
    co2ref  = ep_co2ref
    co2ccn  = ep_co2ccn
    apln0   = ep_apln0
    apln1   = ep_apln1
    apln2   = ep_apln2
    aice    = ep_aice
    diff0   = ep_diff0
    diff2   = ep_diff2
    diff4   = ep_diff4
    gridFileName      = ep_gridFileName
    fracLandFileName  = ep_fracLandFileName
    fracCloudFileName = ep_fracCloudFileName

    ! Open parameter file for writing
    parameterFileName = "Ebm1DParametersOut.nml"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000)  & 
        "Ebm1D parameters are written to namelist file '", &
         TRIM(parameterFileName),"'."
    open(unit=c_MODEL_DATA_UNIT, file=parameterFileName, status="replace", &
        iostat=errIO)
    write (unit=c_MODEL_DATA_UNIT, nml=ebm1d_parameters, iostat=errIO)
    close(c_MODEL_DATA_UNIT)

end subroutine ebm1DWriteParameters
