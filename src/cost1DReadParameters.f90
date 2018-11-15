subroutine cost1DReadParameters()
    use Constants
    use Cost1DParameters
    implicit none
    ! Read Cost1D parameters from file "Cost1DParameters.nml"

    cp_lastInterval = 10.0*c_DAYS_PER_YEAR*c_SECONDS_PER_DAY
    ! AP 2011-01-4: new variable names
    cp_longitudeBeg(cp_iFeb) = -48.780691
    cp_longitudeEnd(cp_iFeb) = -20.483465
    cp_longitudeBeg(cp_iAug) = 127.968121
    cp_longitudeEnd(cp_iAug) = 157.795565

    ! AP 2011-01-4: old variable names
    ! cp_startLongitude(cp_iFeb) = -48.780691
    ! cp_endLongitude(cp_iFeb)   = -20.483465
    ! cp_startLongitude(cp_iAug) = 127.968121
    ! cp_endLongitude(cp_iAug)   = 157.795565

end subroutine cost1DReadParameters
