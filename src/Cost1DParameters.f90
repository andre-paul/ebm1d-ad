module Cost1DParameters
    implicit none

    ! Parameters for computing the cost function

    ! Number of periods for taking long-term means 
    integer, parameter :: cp_nLongTermPeriods = 3

    ! IDs for time periods
    integer, parameter :: &
        cp_iFeb = 1, & ! Northern Hemisphere winter (February)
        cp_iAug = 2, & ! Northern Hemisphere summer (August)
        cp_iAnn = 3    ! annual mean

    ! IDs for variables
    integer, parameter :: &
        cp_iyice_s    = 1, & ! scalar values
        cp_iyice_n    = 2, &
        cp_isolin     = 1, & ! on T grid
        cp_idiv_heddy = 2, &
        cp_iapln      = 3, &
        cp_inetswpln  = 4, &
        cp_inetlwpln  = 5, &
        cp_itsfc      = 6, & 
        cp_iheddy     = 1    ! on U grid  

    ! Dimensions (no. of variables) for taking long-term means  
    integer, parameter :: &
        cv_nLongTermMeans      = 2, &
        cv_nLongTermMeansTGrid = 6, &
        cv_nLongTermMeansUGrid = 1 

    ! AP 2011-01-4: new variable names
    real, dimension(1:cp_nLongTermPeriods) :: cp_longitudeBeg, & ! beginnning true longitude for taking long-term means
                                              cp_longitudeEnd    ! ending     true longitude for taking long-term means
    ! AP 2011-01-4: old variable names
    ! real, dimension(1:cp_nLongTermPeriods) :: cp_startLongitude, & ! beginnning true longitude for taking long-term means
    !                                           cp_endLongitude      ! ending     true longitude for taking long-term means
    real :: cp_lastInterval ! length of interval for accumulating long-term mean

end module Cost1DParameters
