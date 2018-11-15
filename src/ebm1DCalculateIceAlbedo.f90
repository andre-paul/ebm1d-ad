subroutine ebm1DCalculateIceAlbedo(dyt,dyu,yt,yu,tcrit,asfc,aice,t,apln,jice_s,jice_n,yice_s,yice_n)
    use Sizes
    use Constants
    use RunParameters, only: rp_myIter
    implicit none
    !-----------------------------------------------------------------
    ! This subroutine computes the planetary albedo of the latitude 
    ! zones.
    !
    ! Input arguments:
    !    dyt    = T grid cell height (m)
    !    yt     = T grid cell latitude (deg N)
    !    dyu    = U grid cell height (m)
    !    yu     = U grid cell latitude (deg N)
    !    tcrit  = temperature at which the surface becomes ice covered
    !    apln   = planetary albedo
    !    asfc   = surface albedo
    !    aice   = albedo of snow and ice
    !    t      = temperature (deg C)
    !
    ! Output arguments:
    !    apln   = planetary albedo (actual value)
    !    yice_s = latitude of ice boundary (southern hemisphere)
    !    yice_n = latitude of ice boundary (northern hemisphere)
    !    jice_s = latitudinal index of ice boundary (southern hemisphere)
    !    jice_n = latitudinal index of ice boundary (northern hemisphere)
    !
    ! References:
    !    Henderson-Sellers and McGuffie (1987)
    !-----------------------------------------------------------------

    ! Dummy arguments
    REAL, DIMENSION(1:jmt) :: dyt,yt
    REAL, DIMENSION(1:jmt) :: dyu,yu
    REAL                   :: tcrit,aice
    REAL, DIMENSION(1:jmt) :: asfc,t
    REAL, DIMENSION(1:jmt) :: apln
    INTEGER                :: jice_s,jice_n
    REAL                   :: yice_s,yice_n

    ! Local variables
    ! j      = latitudinal index
    ! jmtm1  = jmt - 1
    ! dphi   = partial grid step
    ! ysouth = southern boundary of partially ice-covered grid cell 
    ! ynorth = northern boundary of partially ice-covered grid cell
    ! wice   = weight of ice-covered albedo
    ! amix_s = albedo of partially ice-covered grid cell 
    !             (southern hemisphere)
    ! amix_n = albedo of partially ice-covered grid cell 
    !             (northern hemisphere)
    INTEGER :: j,jmtm1
    REAL :: dphi,ysouth,ynorth,wice,amix_s,amix_n
    ! TAMC stuff
    INTEGER :: ikey

    !-----------------------------------------------------------------
    ! Compute planetary albedo of zones
    !-----------------------------------------------------------------

    jmtm1 = jmt - 1
    
    !-----------------------------------------------------------------
    ! Southern Hemisphere
    !-----------------------------------------------------------------

    ! Find latitude of ice boundary
    jice_s = -1
    DO j=2,jmt/2 ! starting at South Pole
       ikey = (rp_myIter - 1)*jmt + j
!adj store jice_s = tape1, key = ikey
       IF (jice_s .LT. 0) THEN
       IF (t(j) < tcrit .AND. t(j+1) >= tcrit) THEN
          ! Actual grid cell colder than critical temperature, 
          ! but grid cell to the north not: partial ice cover
          ! (assumes temperature monotonous function of latitude) 
          dphi = (-tcrit + t(j+1))/(t(j+1) - t(j))*0.1745/c_DEG2RAD
          ! dphi = (-tcrit + t(j+1))/(t(j+1) - t(j))*dyu(j)/deg2dist
          yice_s   = yt(j+1) - dphi
          IF (yice_s > yu(j)) THEN
             ! Ice boundary less than half-way between grid points
             ysouth = yu(j  )
             ynorth = yu(j+1)
             wice = (SIN(yice_s*c_DEG2RAD) - SIN(ysouth*c_DEG2RAD)) &
                   /(SIN(ynorth*c_DEG2RAD) - SIN(ysouth*c_DEG2RAD))
             amix_s = asfc(j+1)*(1.0 - wice) + aice*wice
             jice_s = j + 1
          ELSE
             ! Ice boundary more than half-way between grid points
             ysouth = yu(j-1)
             ynorth = yu(j  )
             wice = (SIN(yice_s*c_DEG2RAD) - SIN(ysouth*c_DEG2RAD)) &
                   /(SIN(ynorth*c_DEG2RAD) - SIN(ysouth*c_DEG2RAD))
             amix_s = asfc(j)*(1.0 - wice) + aice*wice
             jice_s = j
          END IF
          ! EXIT statement not handled by TAMC
          ! EXIT 
          ! GOTO is not allowed either
          ! GOTO 10
       END IF
       ENDIF
    END DO
    10 CONTINUE
    ! Set planetary albedo of zones
    IF (t(2) >  tcrit) THEN
       ! Totally ice-free case
       jice_s = 0
       yice_s = -90.0
       DO j=2,jmt/2
          apln(j) = asfc(j)
       END DO      
    ELSE IF (t(jmt/2) <= tcrit) THEN
       ! Totally ice-covered case
       jice_s = jmt/2
       yice_s = 0.0
       DO j=2,jmt/2
          apln(j) = aice
       END DO
    ELSE
       ! Partially ice-covered case
       DO j=2,jice_s-1
          apln(j) = aice
       END DO
       apln(jice_s) = amix_s
       DO j=jice_s+1,jmt/2
          apln(j) = asfc(j)
       END DO
    END IF
 
    !-----------------------------------------------------------------
    ! Northern Hemisphere
    !-----------------------------------------------------------------

    ! Find latitude of ice boundary
    jice_n = -1
    DO j=jmtm1,jmt/2+1,-1 ! starting at North Pole
       ikey = (rp_myIter - 1)*jmt + j
!adj store jice_n = tape1, key = ikey
       IF (jice_n .LT. 0) THEN
       IF (t(j) < tcrit .AND. t(j-1) >= tcrit) THEN
          ! Actual grid cell colder than critical temperature, 
          ! but grid cell to the south not: partial ice cover
          ! (assumes temperature monotonous function of latitude) 
          dphi = (-tcrit + t(j-1))/(t(j-1) - t(j))*0.1745/c_DEG2RAD
          ! dphi = (-tcrit + t(j-1))/(t(j-1) - t(j))*dyu(j-1)/deg2dist
          yice_n   = yt(j-1) + dphi
          IF (yice_n < yu(j-1)) THEN
             ! Ice boundary less than half-way between grid points
             ysouth = yu(j-2)
             ynorth = yu(j-1)
             wice = (SIN(ynorth*c_DEG2RAD) - SIN(yice_n*c_DEG2RAD)) &
                   /(SIN(ynorth*c_DEG2RAD) - SIN(ysouth*c_DEG2RAD))
             amix_n = asfc(j-1)*(1.0 - wice) + aice*wice
             jice_n = j - 1
          ELSE
             ! Ice boundary more than half-way between grid points
             ysouth = yu(j-1)
             ynorth = yu(j  )
             wice = (SIN(ynorth*c_DEG2RAD) - SIN(yice_n*c_DEG2RAD)) &
                   /(SIN(ynorth*c_DEG2RAD) - SIN(ysouth*c_DEG2RAD))
             amix_n = asfc(j)*(1.0 - wice) + aice*wice
             jice_n = j
          END IF
          ! EXIT statement not handled by TAMC
          ! EXIT
          ! goto is not allowed either
          ! GOTO 20
       END IF
       ENDIF
    END DO
    20 CONTINUE
    ! Set planetary albedo of zones
    IF (t(jmtm1) >  tcrit) THEN
       ! Totally ice-free case
       jice_n = 0
       yice_n = 90.0
       DO j=jmtm1,jmt/2+1,-1 
          apln(j) = asfc(j)
       END DO      
    ELSE IF (t(jmt/2+1) <= tcrit) THEN
       ! Totally ice-covered case
       jice_n = jmt/2
       yice_n = 0.0
       DO j=jmtm1,jmt/2+1,-1 
          apln(j) = aice
       END DO
    ELSE
       ! Partially ice-covered case
       DO j=jmtm1,jice_n+1,-1 
          apln(j) = aice
       END DO
       apln(jice_n) = amix_n
       DO j=jice_n-1,jmt/2+1,-1 
          apln(j) = asfc(j)
       END DO
    END IF

end subroutine ebm1DCalculateIceAlbedo
