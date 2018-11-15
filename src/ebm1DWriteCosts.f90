subroutine ebm1DWriteCosts(n, fc_reg, fc_sse, fc)
    use Constants
    use Ebm1DParameters
    use Ebm1DGrids
    use Cost1DVariables
    implicit none
    !-----------------------------------------------------------------
    ! This subroutine writes the Ebm1D cost function terms to the file
    ! "cost_function_terms.dat".
    !
    ! Input arguments:
    !    fc     = cost function value
    !
    ! Output arguments:
    !    none
    !-----------------------------------------------------------------

    ! Dummy arguments
    INTEGER :: n
    REAL, INTENT(IN) :: fc_reg, fc_sse, fc

    ! Local variables
    integer :: errIO, & ! IO error flag
               j        ! loop variable
    character(LEN=c_MAX_LEN_FILENAME) :: outputFileName
    
    ! Format statements
    9000 format (1x, 10a)
    9100 format (1x, f12.4, 20e20.6)
    9200 format                                                                            &
        ("% Filename: cost_function_terms.dat"/,                                           &
         "% No. of header lines: 11"/,                                                     &
         "% No. of columns:       3"/,                                                     &
         "%"/,                                                                             &
         "% Values of control variables and cost function:")
    9300 format                                                                            &
        ("%"/,                                                                             &
         "% Column  1: yt       = latitude/degN"/,                                         &
         "% Column  2: tsfc_feb = NH winter (February) surface temperature/degC"/,         &
         "% Column  3: tsfc_aug = NH summer (August) surface temperature/degC")
    
    ! Open output file (control variables and cost function terms) for writing
    outputFileName = "cost_function_terms.dat"
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        'Output (control variables and cost function terms) is written to file "', trim(outputFileName),'".'
    open(unit=c_MODEL_DATA_UNIT, file=outputFileName, status="REPLACE", &
        iostat=errIO)
    ! Write header
    write (unit=c_MODEL_DATA_UNIT, fmt=9200)
    if (n == 5) then
        ! "Present-day (PD) case no. 1"
        write (unit=c_MODEL_DATA_UNIT, fmt='("% ", 5G16.8, 3f10.4)') ep_hocn, ep_alw,              &
                                                                     ep_diff0, ep_diff2, ep_diff4, &
                                                                     fc_reg, fc_sse, fc
    else if (n == 6) then
        ! "Present-day (PD) case no. 2"
        write (unit=c_MODEL_DATA_UNIT, fmt='("% ", 6G16.8, 3f10.4)') ep_hocn, ep_alw, ep_blw,      &
                                                                     ep_diff0, ep_diff2, ep_diff4, &
                                                                     fc_reg, fc_sse, fc
    else if (n == 1) then
        ! "LGM case no. 1"
        write (unit=c_MODEL_DATA_UNIT, fmt='("% ",  G16.8, 3f10.4)') ep_dqco2x2, &
                                                                     fc_reg, fc_sse, fc
    else if (n == 4) then
        ! "LGM case no. 2"
        write (unit=c_MODEL_DATA_UNIT, fmt='("% ", 4G16.8, 3f10.4)') ep_dqco2x2,                   &
                                                                     ep_diff0, ep_diff2, ep_diff4, &
                                                                     fc_reg, fc_sse, fc
    end if
    write (unit=c_MODEL_DATA_UNIT, fmt=9300)
    ! Write to output file (data on T grid)
    do j=2,jmt-1
        write (unit=c_MODEL_DATA_UNIT, fmt=9100)          &
            eg_yt(j),                                     &
            cv_longTermMeansTGrid(j, cp_iFeb, cp_itsfc ), &
            cv_longTermMeansTGrid(j, cp_iAug, cp_itsfc )
    end do
    close(c_MODEL_DATA_UNIT)    

end subroutine ebm1DWriteCosts




