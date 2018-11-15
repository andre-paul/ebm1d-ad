subroutine ebm1DReadParameters()
    use Constants
    use Ebm1DParameters
    implicit none
    ! Read Ebm1D parameters from file "Ebm1DParameters.nml"
      
    ! Local variables
    logical :: useIceAlbedoFeedback
    integer :: errIO ! IO error flag
    real    :: rhowat,    & ! density of pure water at 0 degC/(kg m^-3)
               cp0,       & ! specific heat of liquid water at 0 degC/(J kg^-1 K^-1)
               scon0,     & ! present-day solar constant/(W m^(-2)) (Hartmann 1994)
               pyear,     & ! paleo-year for computing orbital parameters (0 = 1950 AD)
               hocn,      & ! ocean mixed-layer depth/m
               hcrat,     & ! heat capacity ratio, ocean to land
               tcrit,     & ! temperature at which the surface becomes ice covered/degC
               alw,       & ! OLW, constant term/(W m^(-2))
               blw,       & ! OLW, constant factor/(W m^(-2) K^(-1))
               dqco2x2,   & ! 2xCO2 radiative forcing/(W m^(-2))
               co2ref,    & ! reference atmospheric CO2 concentration/ppmv
               co2ccn,    & ! actual atmospheric CO2 concentration/ppmv   
               apln0,     & ! background albedo coeffcients       
               apln1,     &
               apln2,     &
               aice,      & ! albedo of snow and ice
               diff0,     & ! diffusion coefficent, constant factor/(m^2 s^(-1)
               diff2,     &
               diff4
    real    :: hocnerr,    & ! ocean mixed-layer depth error/m
               alwerr,     & ! OLW error, constant term/(W m^(-2))
               blwerr,     & ! OLW error, constant factor/(W m^(-2))
               dqco2x2err, & ! 2xCO2 radiative forcing error/(W m^(-2))
               diff0err,   & ! diffusion coefficent error,
                             ! constant factor/(m^2 s^(-1))
               diff2err,   &
               diff4err
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
    namelist /ebm1d_parameters/ hocnerr,  alwerr,        &
                                blwerr,   dqco2x2err,    &
                                diff0err, diff2err,      &
                                diff4err
    
    ! Format statements
    9000 format (1x, 10a)

    ! Open parameter file
    parameterFileName = "Ebm1DParametersIn.nml"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000)  & 
        "Ebm1D parameters are read from namelist file '", &
         TRIM(parameterFileName),"':"
    open(unit=c_MODEL_DATA_UNIT, file=parameterFileName, status="old", &
        iostat=errIO)
    read (unit=c_MODEL_DATA_UNIT, nml=ebm1d_parameters, iostat=errIO)
    close(c_MODEL_DATA_UNIT)

    ! Report contents of model parameter file
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=*)                   "useIceAlbedoFeedback = ", useIceAlbedoFeedback
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, a)')        "gridFileName         = ", trim(gridFileName)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "hocn    = ", hocn
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "tcrit   = ", tcrit
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "apln0   = ", apln0
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "apln1   = ", apln1
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "apln2   = ", apln2
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.2, a)') "aice    = ", aice
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "alw     = ", alw
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.2, a)') "blw     = ", blw
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "diff0   = ", diff0
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.2, a)') "diff2   = ", diff2
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.2, a)') "diff4   = ", diff4
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "dqco2x2 = ", dqco2x2
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "co2ref  = ", co2ref
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "co2ccn  = ", co2ccn
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "hocnerr    = ", hocnerr
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "alwerr     = ", alwerr
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "blwerr     = ", blwerr
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "dqco2x2err = ", dqco2x2err
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.1, a)') "diff0err   = ", diff0err
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.2, a)') "diff2err   = ", diff2err
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.2, a)') "diff4err   = ", diff4err
    
    ! Copy to module variables
    ep_rhowat  = rhowat
    ep_cp0     = cp0
    ep_scon0   = scon0
    ep_pyear   = pyear
    ep_hocn    = hocn
    ep_hcrat   = hcrat
    ep_tcrit   = tcrit
    ep_alw     = alw
    ep_blw     = blw
    ep_dqco2x2 = dqco2x2
    ep_co2ref  = co2ref
    ep_co2ccn  = co2ccn
    ep_apln0   = apln0
    ep_apln1   = apln1
    ep_apln2   = apln2
    ep_aice    = aice
    ep_diff0   = diff0
    ep_diff2   = diff2
    ep_diff4   = diff4
    ep_hocnerr  = hocnerr
    ep_alwerr     = alwerr
    ep_blwerr     = blwerr
    ep_dqco2x2err = dqco2x2err
    ep_diff0err   = diff0err
    ep_diff2err   = diff2err
    ep_diff4err   = diff4err
    ep_hocnbak    = ep_hocn
    ep_alwbak     = ep_alw
    ep_blwbak     = ep_blw
    ep_dqco2x2bak = dqco2x2
    ep_diff0bak   = ep_diff0
    ep_diff2bak   = ep_diff2
    ep_diff4bak   = ep_diff4
    ep_useIceAlbedoFeedback = useIceAlbedoFeedback
    ep_gridFileName         = gridFileName
    ep_fracLandFileName     = fracLandFileName
    ep_fracCloudFileName    = fracCloudFileName

end subroutine ebm1DReadParameters
