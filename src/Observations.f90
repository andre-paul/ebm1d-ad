module Observations
    use Sizes
    use Cost1DParameters
    implicit none

    ! Observed variables and fields
    real, dimension(1:jmt, 1:cp_nLongTermPeriods) :: ob_tsfc ! surface temperature/degC

    ! Better move to Cost1DParameters and initialize in cost1DInitializeFixed?
                              

end module Observations
