   REAL FUNCTION orbTime2Longitude(ecc,omegap,cday)
      use Constants
      use OrbParameters
      implicit none
      ! This function determines the true longitude (deg)
      ! from the number of a day in a year with 365 days.

      ! Dummy arguments
      REAL, INTENT(IN) :: ecc,     & ! eccentricity
                          omegap,  & ! longitude of the perihelion 
                                     ! as measured from the moving 
                                     ! winter solstice (deg)
                          cday       ! calendar day including fraction

      ! Local parameter
      ! REAL, PARAMETER :: step = 360.0/365.25 ! tropical year
      REAL, PARAMETER :: step = 360.0/365.0    ! "model" year

      ! Local variables
      REAL :: ecc2,ecc3, &
                      beta,      &
                      omegap1,   &
                      omega,     & ! longitude of the perihelion 
                                   ! as measured from the moving 
                                   ! vernal equinox (heliocentric)
                      lambda_m,  & ! mean longitude for specified day 
                      lambda_m0, & ! mean longitude for 21.0 March 1950
                      nu_m,      & ! mean anomaly
                      lambda       ! true longitude of the Earth
                                   ! as measured from the moving 
                                   ! vernal equinox
      
      ! For convenience
      ecc2 = ecc*ecc
      ecc3 = ecc2*ecc
      beta = SQRT(1.0 - ecc2)
 
      ! Compute longitude of the perihelion as measured 
      ! from the moving winter solstice (heliocentric)
      omegap1 = omegap*c_DEG2RAD
      omega   = omegap1 + 3.0*c_pi/2.0

      ! Determine mean longitude for 21.0 March 1950 
      ! from a formula by Brouwer and Clemence (1961)
      lambda_m0 = (ecc/2.0 + ecc3/8.0)*(1.0 + beta)*SIN(omega)     &
                  - ecc2/4.0*(0.5 + beta)*SIN(2.0*omega)           &
                  + ecc*ecc2/8.0*(1.0/3.0 + beta)*SIN(3.0*omega)
      lambda_m0 = 2.0*lambda_m0
      
      ! Determine mean longitude and true anomaly
      ! lambda_m = lambda_m0 + (cday - 80.5)*step*c_DEG2RAD
      lambda_m = lambda_m0 + (cday - 80.0)*step*c_DEG2RAD
      nu_m = lambda_m - omega

      ! Compute true longitude
      lambda = lambda_m + (2.0*ecc-ecc3/4.0)*SIN(nu_m) &
               + 5.0/4.0*ecc2*SIN(2.0*nu_m)          &
               + 13.0/12.0*ecc3*SIN(3.0*nu_m)
      
      ! Change origin of true longitude to winter solstice
      lambda = lambda + c_pi/2.0
      IF (lambda > 2.0*c_pi) THEN 
        orbTime2Longitude = (lambda - 2.0*c_pi)*c_RAD2DEG
      ELSE IF (lambda < 0.0) THEN
        orbTime2Longitude = (lambda + 2.0*c_pi)*c_RAD2DEG
      ELSE
        orbTime2Longitude = lambda*c_RAD2DEG
      END IF 

   END FUNCTION orbTime2Longitude
