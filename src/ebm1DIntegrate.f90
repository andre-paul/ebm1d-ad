subroutine ebm1DIntegrate()
    use Constants
    use RunParameters
    use Ebm1DGrids
    use Ebm1DParameters
    use Ebm1DVariables
    implicit none

    ! Local variables
    integer :: j
    real    :: grad_tsfc, &
               dareat,    &
               dqco2        ! CO2 radiative forcing
    real    :: pl1, pl2     ! first and second Legendre polynomial
    ! real, allocatable, dimension(:) :: source, yp
    real, dimension(1:jmt) :: source, yp

    !----------------------------------------------------------------------
    ! Set fields other than prognostic variables
    !----------------------------------------------------------------------

    do j=2,jmt-2
        ! Compute meridional temperature gradient
        ! (temperature difference / distance)
        grad_tsfc   = (ev_tsfc(j+1) - ev_tsfc(j))/eg_dyu(j)

        ! Compute meridional heat transport by transient eddies
        ! (cross-sectional area * heat capacity * diffusivity * 
        !  temperature gradient)
        ! ev_diff_cnt(j) = ep_diff0*(1.0 + ep_diff2*eg_sint(j)**2 + ep_diff4*eg_sint(j)**4)
        ! AP 2011-01-04: replaced eg_sint(j) by eg_sinu(j)
        ev_diff_cnt(j) = ep_diff0*(1.0 + ep_diff2*eg_sinu(j)**2 + ep_diff4*eg_sinu(j)**4) ! Hyde et al. (1990)
        ! ev_diff_cnt(j) = (ep_diff2 + ep_diff4*eg_csu(j))*ep_diff0 ! Stocker (2008)
        ev_heddy(j) = eg_dxu*eg_csu(j)*ev_ceff*(-ev_diff_cnt(j)*grad_tsfc)
    end do

    do j=2,jmt-1
        ! Compute divergence of meridional heat transport
        ! after Sellers (1969) and North (1975)
        dareat          = eg_dxt*eg_cst(j)*eg_dyt(j)
        ev_div_heddy(j) = (ev_heddy(j) - ev_heddy(j-1))/dareat
    end do

    do j=2,jmt-1
        ! Calculate Legendre polynomials
        pl1 = eg_sint(j)                    ! l=1
        pl2 = 0.5*(3.0*eg_sint(j)**2 - 1.0) ! l=2
        ! Calculate ice-free planetary albedo
        ev_apln_icefree(j) = 1.0 - (ep_apln0 + ep_apln1*pl1 + ep_apln2*pl2) ! Graves et al. (1993), Hartmann (1994)
    end do
 
    ! Calculate planetary albedo
    if (ep_useIceAlbedoFeedback) then
        call ebm1DCalculateIceAlbedo(eg_dyt, eg_dyu, eg_yt, eg_yu, ep_tcrit, &
                                     ev_apln_icefree, ep_aice, ev_tsfc,      &
                                     ev_apln, ev_jice_s, ev_jice_n,          &
                                     ev_yice_s, ev_yice_n)
    else
        ev_apln(2:jmt-1) = ev_apln_icefree(2:jmt-1)
    end if
   
    do j=2,jmt-1
        ! Compute shortwave radiation budget
        ! ev_solin(j)    = ep_scon0*0.25*(0.5294 + 0.706*eg_cst(j)**2)
        ! ev_apln(j)     = ep_apln0 + ep_apln2*eg_cst(j)
        ev_netswpln(j) = ev_solin(j)*(1.0 - ev_apln(j))

        ! Compute longwave radiation budget
        dqco2  = -ep_dqco2x2*LOG(ep_co2ccn/ep_co2ref)/LOG(2.0)
        ev_netlwpln(j) = ep_alw + ep_blw*ev_tsfc(j) + dqco2
            
        ! Set source term for energy balance equation
        source(j)   = ev_netswpln(j) - ev_netlwpln(j)
    end do

    !----------------------------------------------------------------------
    ! Compute rate of change ("slope") of atmospheric temperature   
    !----------------------------------------------------------------------
    
    do j=2,jmt-1
        yp(j) = (source(j) - ev_div_heddy(j))/ev_ceff
    end do

    !----------------------------------------------------------------------
    ! Step atmospheric temperature forward in time (by "Euler forward"
    ! or "forward-in-time" method)
    !----------------------------------------------------------------------
    
    do j=2,jmt-1
        ev_tsfc(j) = ev_tsfc(j) + rp_deltaT*yp(j)
    end do

end subroutine ebm1DIntegrate
