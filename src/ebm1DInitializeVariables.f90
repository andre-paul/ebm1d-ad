subroutine ebm1DInitializeVariables()
    ! Set initial conditions for dynamics variables and time-dependent 
    ! arrays
    use Sizes
    use Ebm1DParameters
    use Ebm1DGrids
    use Ebm1DVariables
    use InitialConditions
    implicit none
    
    ! allocate (ev_tsfc(1:jmt),      &
    !           ev_netswpln(1:jmt),  &
    !           ev_netlwpln(1:jmt),  & 
    !           ev_heddy(1:jmt),     &
    !           ev_div_heddy(1:jmt))

    ! Set initial conditions
    ! ev_tsfc      = 0.0   ! AP 2011-01-04: for debugging only
    ev_tsfc      = ic_tsfc ! starting value of dependent variable 
                           ! (surface temperature/degC)
    ev_netswpln  = 0.0     ! net shortwave radiation absorbed 
                           ! by planet/(W m^(-2))
    ev_netlwpln  = 0.0     ! net longwave radiation emitted
                           ! by planet/(W m^(-2))
    ev_heddy     = 0.0     ! meridional heat transport by transient 
                           ! eddies/W
    ev_div_heddy = 0.0     ! divergence of eddy meridonal heat 
                           ! transport/(W m^(-2))
    
    ! Set further Ebm1D-specific fields (not yet time-dependent)
    ev_solin     = 0.0
    ev_apln      = 0.0
    ev_diff_cnt  = 0.0
    
    ! Set derived model parameters
    ! ev_ceff   = ep_cp0*ep_rhowat*ep_hocn ! effective heat capacity of
    !                                      ! the atmosphere-ocean system

end subroutine ebm1DInitializeVariables
