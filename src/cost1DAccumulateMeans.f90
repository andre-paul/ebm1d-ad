subroutine cost1DAccumulateMeans
    use Constants
    use RunParameters
    use Ebm1DParameters
    use Ebm1DVariables
    use Cost1DParameters
    use Cost1DVariables
    implicit none

    ! Accumulate long-term means (AP 2011-01-04: corrected for fractional time steps)

    ! Local variables
    integer :: itp, &  ! time period index
               j       ! latitudinal index
    real :: deltaTFrac ! fraction of time step/d

    if (rp_myTime > (rp_endTime - cp_lastInterval)) then
        ! Annual mean (does this computation need to be made more precise?)
        cv_longTermTimes(cp_iAnn) = cv_longTermTimes(cp_iAnn) + rp_deltaT
        cv_longTermMeans(cp_iAnn, cp_iyice_s) = cv_longTermMeans(cp_iAnn, cp_iyice_s) + ev_yice_s*rp_deltaT
        cv_longTermMeans(cp_iAnn, cp_iyice_n) = cv_longTermMeans(cp_iAnn, cp_iyice_n) + ev_yice_n*rp_deltaT
        do j=2,jmt-1
            cv_longTermMeansTGrid(j, cp_iAnn, cp_isolin) = cv_longTermMeansTGrid(j, cp_iAnn, cp_isolin) + ev_solin(j)*rp_deltaT
            cv_longTermMeansTGrid(j, cp_iAnn, cp_iapln ) = cv_longTermMeansTGrid(j, cp_iAnn, cp_iapln ) + ev_apln(j) *rp_deltaT
            cv_longTermMeansTGrid(j, cp_iAnn, cp_itsfc ) = cv_longTermMeansTGrid(j, cp_iAnn, cp_itsfc ) + ev_tsfc(j) *rp_deltaT
        end do
        do j=2,jmt-2
            cv_longTermMeansUGrid(j, cp_iAnn, cp_iheddy) = cv_longTermMeansUGrid(j, cp_iAnn, cp_iheddy) + ev_heddy(j)*rp_deltaT
        end do
        if ((cv_seasonBeg(cp_iFeb)) < ev_deltaTEnd .AND. (ev_deltaTBeg <  cv_seasonEnd(cp_iFeb))) then
            ! Northern Hemisphere winter (February) mean
            if (ev_deltaTBeg <  cv_seasonBeg(cp_iFeb)) then
                deltaTFrac = ev_deltaTEnd - cv_seasonBeg(cp_iFeb)
            else if (cv_seasonEnd(cp_iFeb) < ev_deltaTEnd) then
                deltaTFrac =  cv_seasonEnd(cp_iFeb) - ev_deltaTBeg
            else
                deltaTFrac = ev_deltaTEnd - ev_deltaTBeg
            end if
            ! PRINT *,"NH winter, deltaTFrac = ",deltaTFrac
            cv_longTermTimes(cp_iFeb) = cv_longTermTimes(cp_iFeb) + deltaTFrac
            cv_longTermMeans(cp_iFeb, cp_iyice_s) = cv_longTermMeans(cp_iFeb, cp_iyice_s) + ev_yice_s*deltaTFrac
            cv_longTermMeans(cp_iFeb, cp_iyice_n) = cv_longTermMeans(cp_iFeb, cp_iyice_n) + ev_yice_n*deltaTFrac
            do j=2,jmt-1
                cv_longTermMeansTGrid(j, cp_iFeb, cp_isolin) = cv_longTermMeansTGrid(j, cp_iFeb, cp_isolin) + ev_solin(j)*deltaTFrac
                cv_longTermMeansTGrid(j, cp_iFeb, cp_iapln ) = cv_longTermMeansTGrid(j, cp_iFeb, cp_iapln ) + ev_apln(j) *deltaTFrac
                cv_longTermMeansTGrid(j, cp_iFeb, cp_itsfc ) = cv_longTermMeansTGrid(j, cp_iFeb, cp_itsfc ) + ev_tsfc(j) *deltaTFrac
            end do
            do j=2,jmt-2
                cv_longTermMeansUGrid(j, cp_iFeb, cp_iheddy) = cv_longTermMeansUGrid(j, cp_iFeb, cp_iheddy) + ev_heddy(j)*deltaTFrac
            end do
        end if
        if ((cv_seasonBeg(cp_iAug)) < ev_deltaTEnd .AND. (ev_deltaTBeg <  cv_seasonEnd(cp_iAug))) then
            ! Northern Hemisphere summer (August) mean
            if (ev_deltaTBeg <  cv_seasonBeg(cp_iAug)) then
                deltaTFrac = ev_deltaTEnd - cv_seasonBeg(cp_iAug)
            else if (cv_seasonEnd(cp_iAug) < ev_deltaTEnd) then
                deltaTFrac =  cv_seasonEnd(cp_iAug) - ev_deltaTBeg
            else
                deltaTFrac = ev_deltaTEnd - ev_deltaTBeg
            end if
            ! PRINT *,"NH summer, deltaTFrac = ",deltaTFrac
            cv_longTermTimes(cp_iAug) = cv_longTermTimes(cp_iAug) + deltaTFrac
            cv_longTermMeans(cp_iAug, cp_iyice_s) = cv_longTermMeans(cp_iAug, cp_iyice_s) + ev_yice_s*deltaTFrac
            cv_longTermMeans(cp_iAug, cp_iyice_n) = cv_longTermMeans(cp_iAug, cp_iyice_n) + ev_yice_n*deltaTFrac
            do j=2,jmt-1
                cv_longTermMeansTGrid(j, cp_iAug, cp_isolin) = cv_longTermMeansTGrid(j, cp_iAug, cp_isolin) + ev_solin(j)*deltaTFrac
                cv_longTermMeansTGrid(j, cp_iAug, cp_iapln ) = cv_longTermMeansTGrid(j, cp_iAug, cp_iapln ) + ev_apln(j) *deltaTFrac
                cv_longTermMeansTGrid(j, cp_iAug, cp_itsfc ) = cv_longTermMeansTGrid(j, cp_iAug, cp_itsfc ) + ev_tsfc(j) *deltaTFrac
            end do
            do j=2,jmt-2
                cv_longTermMeansUGrid(j, cp_iAug, cp_iheddy) = cv_longTermMeansUGrid(j, cp_iAug, cp_iheddy) + ev_heddy(j)*deltaTFrac
            end do
        end if
    end if
      
end subroutine cost1DAccumulateMeans

