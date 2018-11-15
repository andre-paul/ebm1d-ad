   SUBROUTINE orbInitializeElements()
      use Constants
      use OrbParameters
      implicit none
      !------------------------------------------------------------------------
      ! This subroutine converts the mean rates and phases in the sine and 
      ! cosine expansions of the orbital elements from arc seconds/year and
      ! degrees, respectively, to radian.
      !
      ! Input arguments:
      !
      ! Output arguments:
      !
      ! Reference:
      !     A. Berger: A simple algorithm to compute long term variations
      !     of daily or monthly insolation. Institut d'Astronomique  et de 
      !     Geophysique, Universite Catholique de Louvain. Contribution 
      !     No. 18 (1978a).
      !
      ! A second reference is:
      !     A. Berger: Long-Term Variations of Daily Insolation and 
      !     Quaternary Climate Changes. J. Atmos. Sci. 35, 2362-2367 (1978b). 
      !                                  
      !------------------------------------------------------------------------
       
      ! Dummy arguments
      
      ! Local variables
      INTEGER :: errIO, &
                 i, j
      character(len=c_MAX_LEN_FILENAME) :: eccentricityDataFileName      
      character(LEN=80) :: record

      ! Format statements
      9000 format (1X, 10A)

      ! Read eccentricity data
      eccentricityDataFileName = "../input/eccentricity.dat"
      write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
          "Eccentricity data are read from file '", trim(eccentricityDataFileName), "'"
      open(unit=c_MODEL_DATA_UNIT, file=eccentricityDataFileName, status="old", &
           iostat=errIO)
      j = 0
      do i=1,100
          read  (unit=c_MODEL_DATA_UNIT, fmt='(a)', iostat=errIO) record
          if (errIO == 0 .AND. record(1:1) /= "%") then
              j = j + 1
              read  (unit=record, FMT=*) ae(j), be(j), ce(j)
              ! write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(3(a, i2, a, f12.8))') &
              !     " ae(", j, ") = ",ae(j), &
              !     " be(", j, ") = ",be(j), &
              !     " ce(", j, ") = ",ce(j)
        end if
      end do
      close(unit=c_MODEL_DATA_UNIT)

      ! Read obliquity data
      eccentricityDataFileName = "../input/obliquity.dat"
      write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
          "Obliquity data are read from file '", trim(eccentricityDataFileName), "'"
      open(unit=c_MODEL_DATA_UNIT, file=eccentricityDataFileName, status="old", &
           iostat=errIO)
      j = 0
      do i=1,100
          read  (unit=c_MODEL_DATA_UNIT, fmt='(a)', iostat=errIO) record
          if (errIO == 0 .AND. record(1:1) /= "%") then
              j = j + 1
              read  (unit=record, FMT=*) aob(j), bob(j), cob(j)
              ! write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(3(a, i2, a, f14.8))') &
              !     " aob(", j, ") = ",aob(j), &
              !     " bob(", j, ") = ",bob(j), &
              !     " cob(", j, ") = ",cob(j)
        end if
      end do
      close(unit=c_MODEL_DATA_UNIT)

      ! Read precession data
      eccentricityDataFileName = "../input/precession.dat"
      write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
          "Precession data are read from file '", trim(eccentricityDataFileName), "'"
      open(unit=c_MODEL_DATA_UNIT, file=eccentricityDataFileName, status="old", &
           iostat=errIO)
      j = 0
      do i=1,100
          read  (unit=c_MODEL_DATA_UNIT, fmt='(a)', iostat=errIO) record
          if (errIO == 0 .AND. record(1:1) /= "%") then
              j = j + 1
              read  (unit=record, FMT=*) aop(j), bop(j), cop(j)
              ! write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(3(a, i2, a, f14.8))') &
              !     " aop(", j, ") = ",aop(j), &
              !     " bop(", j, ") = ",bop(j), &
              !     " cop(", j, ") = ",cop(j)
        end if
      end do
      close(unit=c_MODEL_DATA_UNIT)

      ! Convert eccentricity data
      DO i=1,nef
         be(i)=be(i)*c_SEC2RAD
         ce(i)=ce(i)*c_DEG2RAD
      END DO

      ! Convert obliquity data
      DO i=1,nob
         bob(i)=bob(i)*c_SEC2RAD
         cob(i)=cob(i)*c_DEG2RAD
      END DO

      ! Convert precession data
      DO i=1,nop
         bop(i)=bop(i)*c_SEC2RAD
         cop(i)=cop(i)*c_DEG2RAD
      END DO
 
   END SUBROUTINE orbInitializeElements










