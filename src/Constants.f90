module Constants
    implicit none

    ! Set named constants
    real, parameter :: c_pi       = 3.14159265358979

    ! Conversion factors
    real, parameter :: c_DEG2RAD  = c_pi/180.0      ! degrees to radians 
    real, parameter :: c_RAD2DEG  = 1.0/c_DEG2RAD   ! radians to degrees
    real, parameter :: c_DEG2DIST = 111194.9        ! distance that corresponds
                                                    ! to one degree of latitude/m
    real, parameter :: c_SEC2RAD = c_DEG2RAD/3600.0 ! seconds to radians
   
    real, parameter :: c_DAYS_PER_YEAR   =   365.0
    real, parameter :: c_SECONDS_PER_DAY = 86400.0
    integer, parameter :: c_MAX_LEN_FILENAME      = 512, & ! maximum length
                                                           ! of a file name 
                          c_STANDARD_MESSAGE_UNIT = 6,   &
                          c_MODEL_DATA_UNIT       = 14,  &
                          c_ERROR_MESSAGE_UNIT    = 15
end module Constants
