subroutine ebm1DInitializeGrids
    use Constants
    use RunParameters
    use Ebm1DParameters
    use Ebm1DGrids
    implicit none

    ! Local variables
    integer :: errIO, &
               jmd,   &
               j
    real    :: dxtdeg, &
               dxudeg, &
               dxt,    &
               dxu
    real, dimension(1:jmt) :: dytdeg, &
                              dyudeg, &
                              yt,     &
                              yu,     &
                              dyt,    &
                              dyu,    &
                              cst,    &
                              csu,    &
                              sint,   &
                              sinu
     
    ! Format statements
    9000 format (1x, 10a)

    ! Open grids file
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt=9000) & 
        'Ebm1D grids are read from file "',TRIM(ep_gridFileName),'"'
    open(unit=c_MODEL_DATA_UNIT, file=ep_gridFileName, status="OLD", iostat=errIO)
    read (unit=c_MODEL_DATA_UNIT, fmt=*) jmd
    read (unit=c_MODEL_DATA_UNIT, fmt=*) dxtdeg
    read (unit=c_MODEL_DATA_UNIT, fmt=*) dxudeg
    read (unit=c_MODEL_DATA_UNIT, fmt=*) dytdeg(1:jmt)
    read (unit=c_MODEL_DATA_UNIT, fmt=*) dyudeg(1:jmt)
    read (unit=c_MODEL_DATA_UNIT, fmt=*) yt(1:jmt)
    read (unit=c_MODEL_DATA_UNIT, fmt=*) yu(1:jmt)
    close(c_MODEL_DATA_UNIT)
      
    ! Set up longitudinal grids
    dxt = dxtdeg*c_DEG2DIST
    dxu = dxudeg*c_DEG2DIST

    ! Set up latitudinal grids
    do j=1,jmt
        dyt(j) = dytdeg(j)*c_DEG2DIST
        dyu(j) = dyudeg(j)*c_DEG2DIST
    end do
    ! Compute arrays derived from the latitudinal grid spacing
    do j=1,jmt
        cst(j)  = cos(yt(j)*c_DEG2RAD)
        csu(j)  = cos(yu(j)*c_DEG2RAD)
        sint(j) = sin(yt(j)*c_DEG2RAD)
        sinu(j) = sin(yu(j)*c_DEG2RAD)
    end do

    ! Copy to module variables
    eg_jmt  = jmt
    eg_dxt  = dxt
    eg_dxu  = dxu
    eg_dyt  = dyt
    eg_dyu  = dyu
    eg_yt   = yt
    eg_yu   = yu
    eg_cst  = cst
    eg_csu  = csu
    eg_sint = sint
    eg_sinu = sinu

end subroutine ebm1DInitializeGrids
