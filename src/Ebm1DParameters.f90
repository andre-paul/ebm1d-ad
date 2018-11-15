module Ebm1DParameters
    use Sizes
    use Constants
    implicit none

    ! Ebm1D-specific parameters

    ! Physical constants
    REAL :: ep_rhowat, & ! density of pure water at 0 degC/(kg m^-3)
            ep_cp0,    & ! specific heat of liquid water at 0 degC/(J kg^-1 K^-1)
            ep_scon0     ! present-day solar constant/(W m^(-2)) (Hartmann 1994)

    ! Orbital parameters
    REAL :: ep_pyear, &   ! paleo-year for computing orbital parameters (0 = 1950 AD)
            ep_eccen, &   ! numerical eccentricity of Earth's orbit
            ep_perih, &   ! longitude of the perihelion with respect to the 
                          ! moving vernal equinox/deg
            ep_perihp, &  ! longitude of the perihelion with respect to the 
                          ! moving winter solstice/deg
            ep_obliq, &   ! obliquity of Earth's axis of rotation/deg
            ep_clipr      ! climatic precession/precessional parameter

    ! AP 2011-01-04: moved to Ebm1DVariables.f90
    ! REAL :: ep_lambda,      & ! true longitude with respect to the 
    !                           ! moving vernal equinox/deg
    !         ep_lambdap        ! true longitude with respect to the 
    !                           ! moving winter solstice/deg
     
    ! Model parameters
    REAL :: ep_hocn,   & ! ocean mixed-layer depth/m
            ep_hcrat,  & ! heat capacity ratio, ocean to land
            ep_tcrit     ! temperature at which the surface becomes ice covered/degC

    ! Linearized longwave radiation/(W m^(-2)):
    REAL :: ep_alw,     & ! constant term/(W m^(-2))
            ep_blw,     & ! constant factor/(W m^(-2) K^(-1))
            ep_dqco2x2, & ! 2xCO2 radiative forcing/(W m^(-2))
            ep_co2ref,  & ! reference atmospheric CO2 concentration/ppmv
            ep_co2ccn     ! actual atmospheric CO2 concentration/ppmv
               
    ! (Co-) Albedo coefficients
    REAL :: ep_apln0, &        
            ep_apln1, &        
            ep_apln2, &
            ep_aice   

    ! Diffusion coefficients
    REAL :: ep_diff0, &  ! constant factor/(m^2 s^(-1)
            ep_diff2, &
            ep_diff4

    ! Parameter background values
    REAL :: ep_hocnbak,    & ! ocean mixed-layer depth/m
            ep_alwbak,     & ! constant term/(W m^(-2))
            ep_blwbak,     & ! constant factor/(W m^(-2))
            ep_dqco2x2bak, & ! 2xCO2 radiative forcing error/(W m^(-2))
            ep_diff0bak,   & ! diffusion coefficients,
                             ! constant factor/(m^2 s^(-1)
            ep_diff2bak,   &
            ep_diff4bak
    
    ! Parameter errors
    REAL :: ep_hocnerr,    & ! ocean mixed-layer depth/m
            ep_alwerr,     & ! constant term/(W m^(-2))
            ep_blwerr,     & ! constant factor/(W m^(-2))
            ep_dqco2x2err, & ! 2xCO2 radiative forcing error/(W m^(-2))
            ep_diff0err,   & ! diffusion coefficients,
                             ! constant factor/(m^2 s^(-1)
            ep_diff2err,   &
            ep_diff4err  

    ! Fixed fields
    ! real, allocatable, dimension(:) :: ep_solin, & ! insolation/(W m^-2))
    ! real, dimension(1:jmt) :: ep_solin, & ! insolation/(W m^-2))
    !                           ep_apln,  & ! planetary albedo
    !                           ep_diff_cnt ! diffusion coefficient/(m^2 s^-1)  
    real, dimension(1:jmt) :: ep_fland,  & ! land fraction
                              ep_focean  ! ocean fraction
    !                         ep_ceff      ! effective heat capacity of the atmosphere-ocean 
    !                                      ! system/(J m^-2 K^-1)

    ! Internal flags
    logical :: ep_useIceAlbedoFeedback ! true if ice-albedo feedback is to be used

    ! Input file names
    character(len=c_MAX_LEN_FILENAME) :: &
        ep_gridFileName,     & ! file name for Ebm1D grid
        ep_fracLandFileName, & ! file name for Ebm1D land fraction
        ep_fracCloudFileName   ! file name for Ebm1D ocean fraction

end module Ebm1DParameters
