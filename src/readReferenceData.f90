subroutine readReferenceData ! move to cost1DReadParameters?
    use Sizes
    use Constants
    use Cost1DParameters
    use RunParameters
    use ReferenceData ! move to Cost1DParameters?
    implicit none

    ! Local variables
    integer :: errIO, &
               i, j, iTimePeriods
    real, dimension(1:jmt) :: yref
    real, dimension(1:jmt, 1:cp_nLongTermPeriods) :: tref
    character(LEN=80) :: record

    ! Format statements
    9000 format (1X, 10A)
      
    ! Open reference data file
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        "Reference data are read from file '", trim(rp_referenceDataFileName), "'"
    open(unit=c_MODEL_DATA_UNIT, file=rp_referenceDataFileName, status="old", iostat=errIO)
    j = 1
    do i=1,100
        read  (unit=c_MODEL_DATA_UNIT, fmt='(a)', iostat=errIO) record
        if (errIO == 0 .AND. record(1:1) /= "%") then
            j = j + 1
            read  (unit=record, FMT=*) yref(j), (tref(j, iTimePeriods), iTimePeriods=1,cp_nLongTermPeriods)
            ! write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(4(a, i2, a, f8.2))') &
            !     " yref(", j, ") = ",yref(j), &
            !     " tref(", j, ", ifeb) = ",tref(j, cp_iFeb), &
            !     " tref(", j, ", iaug) = ",tref(j, cp_iAug), &
            !     " tref(", j, ", iann) = ",tref(j, cp_iAnn)
        end if
    end do
    close(unit=c_MODEL_DATA_UNIT)

    ! Copy to module variable
    rf_tsfc = tref
    
end subroutine readReferenceData
