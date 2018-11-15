subroutine readObservations ! move to cost1DReadParameters?
    use Sizes
    use Constants
    use Cost1DParameters
    use RunParameters
    use Observations ! move to Cost1DParameters?
    implicit none

    ! Local variables
    integer :: errIO, &
               i, j, iTimePeriods
    real, dimension(1:jmt) :: yobs
    real, dimension(1:jmt, 1:cp_nLongTermPeriods) :: tobs
    character(LEN=80) :: record

    ! Format statements
    9000 format (1X,10A)
      
    ! Open observations file
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        "Observations are read from file '", trim(rp_observationsFileName), "'"
    open(unit=c_MODEL_DATA_UNIT, file=rp_observationsFileName, status="old", iostat=errIO)
    if (errIO /= 0) then
        write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
           "Error in reading observations from file '", trim(rp_observationsFileName), "'"
        stop
    end if
    j = 1
    do i=1,100
        read  (unit=c_MODEL_DATA_UNIT, fmt='(a)', iostat=errIO) record
        if (errIO == 0 .AND. record(1:1) /= "%") then
            j = j + 1
            read  (unit=record, FMT=*) yobs(j), (tobs(j, iTimePeriods), iTimePeriods=1,cp_nLongTermPeriods)
            ! write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(4(a, i2, a, f8.2))') &
            !     " yobs(", j, ") = ",yobs(j), &
            !     " tobs(", j, ", ifeb) = ",tobs(j, cp_iFeb), &
            !     " tobs(", j, ", iaug) = ",tobs(j, cp_iAug), &
            !     " tobs(", j, ", iann) = ",tobs(j, cp_iAnn)
        end if
    end do
    close(unit=c_MODEL_DATA_UNIT)

    ! Copy to module variable
    ob_tsfc = tobs
    
end subroutine readObservations
