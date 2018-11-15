subroutine readInitialConditions ! move to ebm1DReadParameters?
    use Sizes
    use Constants
    use Cost1DParameters
    use RunParameters
    use InitialConditions ! move to Ebm1DParameters?
    implicit none

    ! Local variables
    integer :: errIO, &
               i, j, iTimePeriods
    real, dimension(1:jmt) :: yic
    real, dimension(1:jmt, 1:cp_nLongTermPeriods) :: tic
    character(LEN=80) :: record

    ! Format statements
    9000 format (1X, 10A)
      
    ! Open initial conditions file
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        "Initial conditions are read from file '", trim(rp_initialConditionsFileName), "'"
    open(unit=c_MODEL_DATA_UNIT, file=rp_initialConditionsFileName, status="old", iostat=errIO)
    j = 1
    do i=1,100
        read  (unit=c_MODEL_DATA_UNIT, fmt='(a)', iostat=errIO) record
        if (errIO == 0 .AND. record(1:1) /= "%") then
            j = j + 1
            read  (unit=record, FMT=*) yic(j), (tic(j, iTimePeriods), iTimePeriods=1,cp_nLongTermPeriods)
            ! write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(2(a, i2, a, f8.2))') &
            !     " yic(", j, ") = ",yic(j), &
            !     " tic(", j, ") = ",tic(j, cp_iAnn)
        end if
    end do
    close(unit=c_MODEL_DATA_UNIT)

    ! Copy to module variable
    ic_tsfc = tic(1:jmt, cp_iAnn)

end subroutine readInitialConditions
