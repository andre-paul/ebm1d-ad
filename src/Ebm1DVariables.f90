module Ebm1DVariables
    use Sizes
    implicit none

    ! EBM1D-specific variables and fields

    integer :: ev_jice_s, & ! latitudinal index of ice boundary (southern hemisphere)
               ev_jice_n    ! latitudinal index of ice boundary (northern hemisphere)
    real    :: ev_ceff,   & ! effective heat capacity of the atmosphere-ocean 
                            ! system/(J m^-2 K^-1)
               ev_yice_s, & ! latitude of ice boundary (southern hemisphere)
               ev_yice_n, & ! latitude of ice boundary (northern hemisphere)
               ! AP 2011-01-04: moved here from Ebm1DParameters.f90
               ev_lambda,      & ! true longitude with respect to the 
                                 ! moving vernal equinox/deg
               ev_lambdap,     & ! true longitude with respect to the 
                                 ! moving winter solstice/deg
               ev_deltaTBeg,   & ! day of year at beginning of time step
               ev_deltaTEnd      ! day of year at end       of time step
             
    ! real, allocatable, dimension(:) :: ev_tsfc, &      ! surface temperature/degC
    real, dimension(1:jmt) :: ev_tsfc, &      ! surface temperature/degC
                              ev_netswpln,  & ! net shortwave radiation 
                                              ! absorbed by 
                                              ! planet/(W m^(-2))
                              ev_netlwpln,  & ! net longwave radiation 
                                              ! emitted by planet/
                                              ! (W m^(-2))
                              ev_heddy,     & ! meridional heat transport by
                                              ! transient eddies/W
                              ev_div_heddy    ! divergence of eddy 
                                              ! meridonal heat transport/
                                              ! (W m^(-2))

    ! real, allocatable, dimension(:) :: ep_solin, & ! insolation/(W m^-2))
    real, dimension(1:jmt) :: ev_solin, & ! insolation/(W m^-2))
                              ev_cosz,  & ! daily cosine of solar zenith angle, 
                                          ! averaged with respect to insolation
                              ev_apln_icefree, & ! ice-free planetary albedo
                              ev_apln,  & ! planetary albedo
                              ev_diff_cnt ! diffusion coefficient/(m^2 s^-1)  

end module Ebm1DVariables
