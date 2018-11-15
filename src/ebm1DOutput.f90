subroutine ebm1DOutput()
    use Sizes
    use Constants
    use RunParameters
    use Ebm1DParameters
    use Ebm1DGrids
    use Ebm1DVariables
    use Cost1DParameters
    use Cost1DVariables
    implicit none

    ! Local variables
    integer :: errIO, & ! IO error flag
               j        ! loop variable
    real    :: globalAverage
    character(LEN=c_MAX_LEN_FILENAME) :: outputFileName

    ! Format statements
    9000 format (1x, 10a)
    9100 format (1x, f12.4, 20e20.6)
    9150 format (1x, f12.4, 3f13.1)
    9200 format                                                                            &
        ("% Filename: restart.dat"/,                                                       &
         "% No. of header lines:  5"/,                                                     &
         "% No. of columns:       2"/,                                                     &
         "% Column  1: yt         = latitude/degN"/,                                       &
         "% Column  2: tsfc       = surface temperature/degC")
    9300 format                                                                            &
        ("% Filename: energy_transport_1d.dat"/,                                           &
         "% No. of header lines:  9"/,                                                     &
         "% No. of columns:       6"/,                                                     &
         "% Column  1: yu         = latitude/degN"/,                                       &
         "% Column  2: diff_cnt   = diffusivity/(m s^-2)"/,                                &
         "% Column  3: heddy      = instantaneous surface meridional energy transport/W"/, &
         "% Column  4: heddy_feb  = NH winter (February) meridional energy transport/W"/,  &
         "% Column  5: heddy_aug  = NH summer (August) meridional energy transport/W"/,    &
         "% Column  6: heddy_ann  = annual-mean meridional energy transport/W")
    9400 format                                                                            &
        ("% Filename: energy_balance_1d.dat"/,                                             &
         "% No. of header lines: 19"/,                                                     &
         "% No. of columns:      16"/,                                                     &
         "% Column  1: yt         = latitude/degN"/,                                       &
         "% Column  2: solin      = instantaneous solar insolation/(W m^-2)"/,             &
         "% Column  3: apln       = instantaneous planetary albedo"/,                      &
         "% Column  4: netswpln   = instantaneous net shortwave radiation absorbed by planet/(W m^-2)"/, &
         "% Column  5: netlwpln   = instantaneous net longwave radiation emitted by planet/(W m-2)"/,    &
         "% Column  6: div_heddy  = instantaneous divergence of meridional energy transport/(W m-2)"/,   &
         "% Column  7: tsfc       = instantaneous surface temperature/degC"/,              &
         "% Column  8: solin_feb  = NH winter (February) solar insolation/(W m^-2)"/,      &
         "% Column  9: solin_aug  = NH summer (August) solar insolation/(W m^-2)"/,        &
         "% Column 10: solin_ann  = annual-mean solar insolation/(W m^-2)"/,               &
         "% Column 11: apln_feb   = NH winter (February) planetary albedo"/,               &
         "% Column 12: apln_aug   = NH summer (August) planetary albedo"/,                 &
         "% Column 13: apln_ann   = annual-mean planetary albedo"/,                        &
         "% Column 14: tsfc_feb   = NH winter (February) surface temperature/degC"/,       &
         "% Column 15: tsfc_aug   = NH summer (August) surface temperature/degC"/,         &
         "% Column 16: tsfc_ann   = annual-mean surface temperature/degC")
    9500 format                                                                            &
        ("% Filename: reference.dat"/,                                                     &
         "% No. of header lines:  8"/,                                                     &
         "% No. of columns:       4"/,                                                     &
         "% Column  1: yt       = latitude/degN"/,                                         &
         "% Column  2: tsfc_feb = NH winter (February) surface temperature/degC"/,         &
         "% Column  3: tsfc_aug = NH summer (August) surface temperature/degC"/,           &
         "% Column  4: tsfc_ann = annual-mean surface temperature/degC")

    ! Open restart (pickup) file
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        'Restart data is written to file "', trim(rp_restartDataFileName),'".'
    open(unit=c_MODEL_DATA_UNIT, file=rp_restartDataFileName, status="REPLACE", &
        iostat=errIO)
    ! Write header
    write (unit=c_MODEL_DATA_UNIT, fmt=9200)
    ! Write restart data
    do j=2,jmt-1
        write (unit=c_MODEL_DATA_UNIT, fmt=9100)  & 
            eg_yt(j), ev_tsfc(j)
    end do
    close(c_MODEL_DATA_UNIT)

    ! Open output file no. 1 (for data on U grid) for writing
    outputFileName = "energy_transport_1d.dat"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        'Output on U grid is written to file "', trim(outputFileName),'".'
    open(unit=c_MODEL_DATA_UNIT, file=outputFileName, status="REPLACE", &
        iostat=errIO)
    ! Write header
    write (unit=c_MODEL_DATA_UNIT, fmt=9300)
    ! Write to output file no. 1 (data on U grid)
    do j=1,jmt-1
        write (unit=c_MODEL_DATA_UNIT, fmt=9100)          & 
            eg_yu(j), ev_diff_cnt(j), ev_heddy(j),        &
            cv_longTermMeansUGrid(j, cp_iFeb, cp_iheddy), &
            cv_longTermMeansUGrid(j, cp_iAug, cp_iheddy), &
            cv_longTermMeansUGrid(j, cp_iAnn, cp_iheddy)
    end do
    close(c_MODEL_DATA_UNIT)

    ! Open output file no. 2 (for data on T grid) for writing
    outputFileName = "energy_balance_1d.dat"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        'Output on T grid is written to file "', trim(outputFileName),'".'
    open(unit=c_MODEL_DATA_UNIT, file=outputFileName, status="REPLACE", &
        iostat=errIO)
    ! Write header
    write (unit=c_MODEL_DATA_UNIT, fmt=9400)
    ! Write to output file no. 2 (data on T grid)
    do j=2,jmt-1
        write (unit=c_MODEL_DATA_UNIT, fmt=9100)          &
            eg_yt(j), ev_solin(j), ev_apln(j),            &
            ev_netswpln(j), ev_netlwpln(j),               &
            ev_div_heddy(j), ev_tsfc(j),                  &
            cv_longTermMeansTGrid(j, cp_iFeb, cp_isolin), &
            cv_longTermMeansTGrid(j, cp_iAug, cp_isolin), &
            cv_longTermMeansTGrid(j, cp_iAnn, cp_isolin), &
            cv_longTermMeansTGrid(j, cp_iFeb, cp_iapln ), &
            cv_longTermMeansTGrid(j, cp_iAug, cp_iapln ), &
            cv_longTermMeansTGrid(j, cp_iAnn, cp_iapln ), &
            cv_longTermMeansTGrid(j, cp_iFeb, cp_itsfc ), &
            cv_longTermMeansTGrid(j, cp_iAug, cp_itsfc ), &
            cv_longTermMeansTGrid(j, cp_iAnn, cp_itsfc )
    end do
    close(c_MODEL_DATA_UNIT)

    ! Open output file no. 3 (reference data on T grid) for writing
    outputFileName = "reference.dat"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        'Output on T grid is written to file "', trim(outputFileName),'".'
    open(unit=c_MODEL_DATA_UNIT, file=outputFileName, status="REPLACE", &
        iostat=errIO)
    ! Write header
    write (unit=c_MODEL_DATA_UNIT, fmt=9500)
    ! Write to output file no. 3 (data on T grid)
    do j=2,jmt-1
        write (unit=c_MODEL_DATA_UNIT, fmt=9150)         & 
            eg_yt(j),                                    &
            cv_longTermMeansTGrid(j, cp_iFeb, cp_itsfc), &
            cv_longTermMeansTGrid(j, cp_iAug, cp_itsfc), &
            cv_longTermMeansTGrid(j, cp_iAnn, cp_itsfc)
    end do
    close(c_MODEL_DATA_UNIT)    

    ! Write model parameters
    call ebm1DWriteParameters()

    ! Write output to screen
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=*)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a)') &
        ' *** Some long-term mean values ***' 
    call calculateGlobalAverage(eg_cst, cv_longTermMeansTGrid(1:jmt, cp_iAnn, cp_iapln), globalAverage)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, f10.2)') &
        ' >> Global mean planetary albedo (annual)      : ', globalAverage
    call calculateGlobalAverage(eg_cst, cv_longTermMeansTGrid(1:jmt, cp_iAnn, cp_itsfc), globalAverage)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, f10.2)') &
        ' >> Global mean surface temperature (annual)   : ', globalAverage
    call calculateGlobalAverage(eg_cst, cv_longTermMeansTGrid(1:jmt, cp_iFeb, cp_itsfc), globalAverage)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, f10.2)') &
        ' >> Global mean surface temperature (February) : ', globalAverage
    call calculateGlobalAverage(eg_cst, cv_longTermMeansTGrid(1:jmt, cp_iAug, cp_itsfc), globalAverage)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, f10.2)') &
        ' >> Global mean surface temperature (August)   : ', globalAverage
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, f10.2)') &
        ' >> Annual-mean ice boundary in the southern hemisphere: lat = ', cv_longTermMeans(cp_iAnn, cp_iyice_s)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, f10.2)') &
        ' >> Annual-mean ice boundary in the northern hemisphere: lat = ', cv_longTermMeans(cp_iAnn, cp_iyice_n)

    ! Write output to screen
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=*)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a)') &
        ' *** Some instantaneous values ***' 
    call calculateGlobalAverage(eg_cst, ev_apln, globalAverage)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, f10.2)') &
        ' >> Global mean planetary albedo is    : ', globalAverage
    call calculateGlobalAverage(eg_cst, ev_tsfc, globalAverage)
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, f10.2)') &
        ' >> Global mean surface temperature is : ', globalAverage
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, i3, a, f10.2)') &
        ' >> Ice boundary in the southern hemisphere: j = ', ev_jice_s, ", lat = ",ev_yice_s
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(a, i3, a, f10.2)') &
        ' >> Ice boundary in the northern hemisphere: j = ', ev_jice_n, ", lat = ",ev_yice_n

end subroutine ebm1DOutput
