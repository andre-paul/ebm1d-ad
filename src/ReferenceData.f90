module ReferenceData
    use Sizes
    use Cost1DParameters
    implicit none

    ! Reference data
    real, dimension(1:jmt, 1:cp_nLongTermPeriods) :: rf_tsfc ! surface temperature/degC

    ! Better move to Cost1DParameters and initialize in cost1DInitializeFixed?
                              

end module ReferenceData
