module Cost1DVariables
    use Sizes
    use Cost1DParameters
    implicit none

    ! Variables and fields for computing the cost function
    real, dimension(1:cp_nLongTermPeriods) :: cv_seasonBeg, & ! beginnning day of year for taking long-term (seasonal) means
                                              cv_seasonEnd    ! ending     day of year for taking long-term (seasonal) means

    real, dimension(1:cp_nLongTermPeriods)                                  :: cv_longTermTimes
    real, dimension(1:cp_nLongTermPeriods, 1:cv_nLongTermMeans)             :: cv_longTermMeans
    real, dimension(1:jmt, 1:cp_nLongTermPeriods, 1:cv_nLongTermMeansTGrid) :: cv_longTermMeansTGrid
    real, dimension(1:jmt, 1:cp_nLongTermPeriods, 1:cv_nLongTermMeansUGrid) :: cv_longTermMeansUGrid
 
end module Cost1DVariables
