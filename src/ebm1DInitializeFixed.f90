subroutine ebm1DInitializeFixed()
    use Sizes
    use Constants
    use Ebm1DParameters
    use Ebm1DGrids
    implicit none

    ! Complete setup of Ebm1D

    ! Local variables
    integer :: j       ! latitudinal index
    integer :: nFields ! no. of fields
    real, dimension(1:jmt) :: lat

    ! Format statements
    9000 format (1X,10A)

    ! Initialize land fraction
    nFields = 1
    ! call readTextFile(ep_fracLandFileName, "Land fractions", nFields, lat, ep_fland)
    ! Initialize ocean fraction
    ! do j=2,jmt-1
    !     ep_focean(j) = 1.0 - ep_fland(j)
    ! end do

    ! Compute effective heat capacity of the atmosphere-ocean system,
    ! by assuming that the heat capacity of land is 1/hcrat times that of ocean
    ! ep_ceff(1:jmt) = 0.0
    ! do j=2,jmt-1
    !     ep_ceff(j) = ep_cp0*ep_rhowat*ep_hocn* &
    !         (ep_focean(j) + ep_fland(j)/ep_hcrat)
    ! end do

    ! Initialize Earth's orbital elements
    call orbInitializeElements()
    ! Compute Earth's orbital elements
    call orbComputeElements(ep_pyear, ep_eccen, ep_perih, ep_obliq, ep_clipr)
    ep_perihp  = ep_perih  - 90.0
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "pyear   = ", ep_pyear
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "eccen   = ", ep_eccen
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "perih   = ", ep_perih
    write (unit=c_STANDARD_MESSAGE_UNIT, fmt='(1x, a, f10.3, a)') "obliq   = ", ep_obliq
       
end subroutine ebm1DInitializeFixed
