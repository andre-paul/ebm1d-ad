subroutine cost1DInitializeVariables()
    ! Initialize variable cost function part
    use Cost1DVariables
    implicit none

    ! Set initial conditions
    cv_longTermTimes      = 0.0 ! duration of periods for computing cost function
    cv_longTermMeans      = 0.0 ! long-term mean values for computing cost function
    cv_longTermMeansTGrid = 0.0
    cv_longTermMeansUGrid = 0.0

end subroutine cost1DInitializeVariables
