subroutine model(n, x, fc)
    ! MODEL Calculate cost function.
    ! MODEL is called by the optimization procedure. It has to calculate the 
    ! cost function FC depending on the control vector X(N).
    use OptimizationParameters
    use Sizes
    use Ebm1DGrids
    use Ebm1DParameters
    use Ebm1DVariables
    use Cost1DParameters
    use Cost1DVariables
    use ReferenceData
    use Observations
    implicit none

    ! Dummy arguments
    integer :: n
    real    :: fc
    real, dimension(1:n) :: x(n)

    ! Local variables
    integer :: i, j, l
    real    :: weight, value, sumOfWeights, weightedSum
    real    :: fc_reg, fc_sse
    logical :: useSeasonalMeansOnly, useLongTermMean

    ! Copy (potential) control variables and undo scaling/conditioning
    ! using scaling factors "cond(i)".
    if (n == 5) then
        ! "Present-day (PD) case no. 1"
        ep_diff0   = x(1)*cond(1)
        ep_diff2   = x(2)*cond(2)
        ep_diff4   = x(3)*cond(3)
        ep_alw     = x(4)*cond(4)   
        ep_hocn    = x(5)*cond(5)  
    else if (n == 6) then
        ! "Present-day (PD) case no. 2"
        ep_diff0   = x(1)*cond(1)
        ep_diff2   = x(2)*cond(2)
        ep_diff4   = x(3)*cond(3)
        ep_alw     = x(4)*cond(4)   
        ep_blw     = x(5)*cond(5)   
        ep_hocn    = x(6)*cond(6)  
    else if (n == 1) then
        ! "LGM case no. 1"
        ep_dqco2x2 = x(1)*cond(1)  
    else if (n == 4) then
        ! "LGM case no. 2"
        ep_diff0   = x(1)*cond(1)
        ep_diff2   = x(2)*cond(2)
        ep_diff4   = x(3)*cond(3)
        ep_dqco2x2 = x(4)*cond(4)  
    end if
    print '(1x, a)', ">>> Control variables:"
    do i=1,n
        print '(1x, a, i2, a, i2, a, e14.8)', " i = ", i, ", x(", i, ") = ", x(i)
    end do
    print *," ep_useIceAlbedoFeedback = ", ep_useIceAlbedoFeedback

    ! Set initial conditions for dynamics variables and time dependent arrays
    ev_ceff   = ep_cp0*ep_rhowat*ep_hocn ! effective heat capacity of
                                         ! the atmosphere-ocean system
    call initializeVariables()

    ! Initialize cost function - move to different place (initmod)?
    fc = 0.0
 
    ! >>> Temporary fix: As long as "orbLongitude2Time" does not work properly, <<<
    ! >>> the beginnning and ending days for taking long-term (seasonal) means  <<<
    ! >>> are hard-coded in "model".                                            <<<

   if (n == 5) then
        ! Present-day (1950 AD) orbit
        cv_seasonBeg(cp_iFeb) =  31.50
        cv_seasonEnd(cp_iFeb) =  59.50
        cv_seasonBeg(cp_iAug) = 212.50
        cv_seasonEnd(cp_iAug) = 243.50
    else
        ! Last Glacial Maximum (LGM, 21,000 ka BP) orbit
        cv_seasonBeg(cp_iFeb) =  31.91
        cv_seasonEnd(cp_iFeb) =  59.68
        cv_seasonBeg(cp_iAug) = 212.27
        cv_seasonEnd(cp_iAug) = 243.52
    end if

    ! Run model
    print *,'**************************************'
    print *,'***     RUNNING EBM1D               **'
    print *,'**************************************'
    call core()

    ! Compute long-term means
    do l=1,cp_nLongTermPeriods
        cv_longTermMeans(l, cp_iyice_s) = cv_longTermMeans(l, cp_iyice_s)/cv_longTermTimes(l)
        cv_longTermMeans(l, cp_iyice_n) = cv_longTermMeans(l, cp_iyice_n)/cv_longTermTimes(l)
        do j=2,jmt-1
            cv_longTermMeansTGrid(j, l, cp_isolin) = cv_longTermMeansTGrid(j, l, cp_isolin)/cv_longTermTimes(l)
            cv_longTermMeansTGrid(j, l, cp_iapln ) = cv_longTermMeansTGrid(j, l, cp_iapln )/cv_longTermTimes(l)
            cv_longTermMeansTGrid(j, l, cp_itsfc ) = cv_longTermMeansTGrid(j, l, cp_itsfc )/cv_longTermTimes(l)
        end do
        do j=2,jmt-2
            cv_longTermMeansUGrid(j, l, cp_iheddy) = cv_longTermMeansUGrid(j, l, cp_iheddy)/cv_longTermTimes(l)
        end do
    end do

    ! Do output at end of run
    call ebm1DOutput()

    ! Compute contribution to cost function by regularization
    if (n == 5) then
        ! "Present-day (PD) case no. 1"
        fc_reg = 0.5*((ep_hocn  - ep_hocnbak )**2/ep_hocnerr**2  + &
                      (ep_alw   - ep_alwbak  )**2/ep_alwerr**2   + &
                      (ep_diff0 - ep_diff0bak)**2/ep_diff0err**2 + &
                      (ep_diff2 - ep_diff2bak)**2/ep_diff2err**2 + &
                      (ep_diff4 - ep_diff4bak)**2/ep_diff4err**2)
    else if (n == 6) then
        ! "Present-day (PD) case no. 2"
        fc_reg = 0.5*((ep_hocn  - ep_hocnbak )**2/ep_hocnerr**2  + &
                      (ep_alw   - ep_alwbak  )**2/ep_alwerr**2   + &
                      (ep_blw   - ep_blwbak  )**2/ep_blwerr**2   + &
                      (ep_diff0 - ep_diff0bak)**2/ep_diff0err**2 + &
                      (ep_diff2 - ep_diff2bak)**2/ep_diff2err**2 + &
                      (ep_diff4 - ep_diff4bak)**2/ep_diff4err**2)
    else if (n == 1) then
        ! "LGM case no. 1"
        fc_reg = 0.5*((ep_dqco2x2 - ep_dqco2x2bak)**2/ep_dqco2x2err**2)
    else if (n == 4) then
        ! "LGM case no. 2"
        fc_reg = 0.5*((ep_dqco2x2 - ep_dqco2x2bak)**2/ep_dqco2x2err**2 + &
                      (ep_diff0   - ep_diff0bak  )**2/ep_diff0err**2   + &
                      (ep_diff2   - ep_diff2bak  )**2/ep_diff2err**2   + &
                      (ep_diff4   - ep_diff4bak)**2/ep_diff4err**2)
    end if
            
    ! Compute contribution to cost function by weighted sum of squared errors of surface temperature
    useLongTermMean      = .true.
    useSeasonalMeansOnly = .true.
    sumOfWeights = 0.0
    weightedSum  = 0.0
    print *,"useLongTermMean      = ", useLongTermMean
    print *,"useSeasonalMeansOnly = ", useSeasonalMeansOnly
    print *,"cv_longTermTimes(",cp_iFeb,") = ",cv_longTermTimes(cp_iFeb)
    print *,"cv_longTermTimes(",cp_iAug,") = ",cv_longTermTimes(cp_iAug)
    print *,"cv_longTermTimes(",cp_iAnn,") = ",cv_longTermTimes(cp_iAnn)
    do j=2,jmt-1
        if (useSeasonalMeansOnly) then
            ! Northern Hemisphere winter (February) mean
            if (ob_tsfc(j, cp_iFeb) == ob_tsfc(j, cp_iFeb) .AND. ob_tsfc(j, cp_iFeb) /= -99.99) then ! check for "NaN"
                weight = eg_cst(j)
                if (useLongTermMean) then
                    value  = ((cv_longTermMeansTGrid(j, cp_iFeb, cp_itsfc) - rf_tsfc(j, cp_iFeb)) - ob_tsfc(j, cp_iFeb))**2
                else
                    value  = ((ev_tsfc(j) - rf_tsfc(j, cp_iFeb)) - ob_tsfc(j, cp_iFeb))**2
                end if
                sumOfWeights = sumOfWeights + weight
                weightedSum  = weightedSum  + weight*value
            end if
            ! Northern Hemisphere summer (August) mean
            if (ob_tsfc(j, cp_iAug) == ob_tsfc(j, cp_iAug) .AND. ob_tsfc(j, cp_iAug) /= -99.99) then ! check for "NaN"
                weight = eg_cst(j)
                if (useLongTermMean) then
                    value  = ((cv_longTermMeansTGrid(j, cp_iAug, cp_itsfc) - rf_tsfc(j, cp_iAug)) - ob_tsfc(j, cp_iAug))**2
                else
                    value  = ((ev_tsfc(j) - rf_tsfc(j, cp_iAug)) - ob_tsfc(j, cp_iAug))**2
                end if
                sumOfWeights = sumOfWeights + weight
                weightedSum  = weightedSum  + weight*value
            end if
        else
            ! Annual mean
            if (ob_tsfc(j, cp_iAnn) == ob_tsfc(j, cp_iAnn) .AND. ob_tsfc(j, cp_iAnn) /= -99.99) then ! check for "NaN"
                weight = eg_cst(j)
                if (useLongTermMean) then
                    value  = ((cv_longTermMeansTGrid(j, cp_iAnn, cp_itsfc) - rf_tsfc(j, cp_iAnn)) - ob_tsfc(j, cp_iAnn))**2
                else
                    value  = ((ev_tsfc(j) - rf_tsfc(j, cp_iAnn)) - ob_tsfc(j, cp_iAnn))**2
                end if
                sumOfWeights = sumOfWeights + weight
                weightedSum  = weightedSum  + weight*value
            end if
        end if
    end do
    fc_sse = 0.5*weightedSum/sumOfWeights
    fc     = fc_reg + fc_sse
    
    ! Write Ebm1D cost function terms to file "cost_function_terms.dat"
    Call ebm1DWriteCosts(n, fc_reg, fc_sse, fc)

end subroutine model
