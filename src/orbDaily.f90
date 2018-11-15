   SUBROUTINE orbDaily(scon,eccen,obliq,perihp,lambdap,phi,wdaily,cosz)
      use Constants
      use OrbParameters
      implicit none
      !-------------------------------------------------------------------------
      ! This subroutine computes the daily insolation which is equal to
      ! the instantaneous insolation integrated over 24 hours of true solar
      ! time. Furthermore it provides the daily cosine of solar zenith angle.
      !
      ! Input arguments:
      !    scon    = solar constant (W m^(-2))
      !    eccen   = actual numerical eccentricity of the Earth's orbit
      !    obliq   = actual obliquity of the Earth's axis of rotation (deg)
      !    perihp  = actual longitude of the perihelion with respect to the 
      !              moving winter solstice (deg)
      !    lambdap = actual longitude of the Earth with respect to the 
      !              moving winter solstice (deg)
      !    phi     = geographical latitude (deg)
      ! 
      ! Output arguments:
      !    wdaily  = daily insolation
      !    cosz    = daily cosine of solar zenith angle, averaged with respect
      !              to insolation
      !
      !    According to Hartmann (1994), it is appropriate to weight the average
      !    cosine of solar zenith angle with respect to insolation, rather than
      !    time.
      !
      ! Reference:
      !    Milankovitch (1930)
      !    Hartmann (1994)
      !-------------------------------------------------------------------------
      
      ! Dummy arguments
      REAL, INTENT(IN)    :: scon,eccen,perihp,obliq,lambdap,phi  
      REAL, INTENT(OUT)   :: wdaily,cosz
      
      ! Local variables
      REAL :: delta, & ! geocentric declination of the Sun
                      hour0, & ! absolute value of the hour angle of the Sun
                               ! at sunrise and sunset
                      nu,    & ! true anomaly of the Earth
                      rho,   & ! normalized distance of the Earth from the Sun
                      winteg   ! integral of solar zenith angle
      REAL :: perihp1,obliq1,lambdap1,phi1
      
      ! Convert orbital elements from degrees to radians
      perihp1  = perihp*c_DEG2RAD
      obliq1   = obliq*c_DEG2RAD
      lambdap1 = lambdap*c_DEG2RAD
      phi1     = phi*c_DEG2RAD
   
      ! Compute declination
      delta = -ASIN(SIN(obliq1)*COS(lambdap1))

      IF ((ABS(phi1) >= c_pi/2.0 - ABS(delta)) .AND. (phi1*delta < 0.0)) THEN
         ! Case 1: at latitudes where the Sun never rises
         wdaily = 0.0
         cosz   = 0.0

      ELSE
         ! Compute true anomaly
         nu = lambdap1 - perihp1
         
         ! Compute Earth-Sun distance
         rho = (1.0 - eccen**2)/(1.0 + eccen*COS(nu))

         IF ((ABS(phi1) >= c_pi/2.0 - ABS(delta)) .AND. (phi1*delta > 0.0)) THEN
            ! Case 2: at latitudes where the Sun never sets
            winteg = SIN(phi1)*SIN(delta)
            wdaily = scon/rho**2*winteg
            cosz   = &
               (SIN(phi1)**2*SIN(delta)**2 + 0.5*COS(phi1)**2*COS(delta)**2) &
               /winteg

         ELSE
            ! Case 3: at latitudes where there is daily sunrise and sunset 
            ! Compute hour angle 
            hour0 = ACOS(-TAN(phi1)*TAN(delta))

            winteg = hour0*SIN(phi1)*SIN(delta) + COS(phi1)*COS(delta)*SIN(hour0)
            wdaily = scon/(c_pi*rho**2)*winteg
            cosz   = &
               (hour0*(SIN(phi1)**2*SIN(delta)**2 +0.5*COS(phi1)**2*COS(delta)**2) &
                + 2.0*SIN(phi1)*SIN(delta)*COS(phi1)*COS(delta)*SIN(hour0)         &
                + 0.25*COS(phi1)**2*COS(delta)**2*SIN(2.0*hour0))                  &
               /winteg

         END IF
      END IF 
             
   END SUBROUTINE orbDaily
