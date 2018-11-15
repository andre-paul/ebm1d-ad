module Ebm1DGrids
    use Sizes
    implicit none

    ! Ebm1D grids
    integer :: eg_jmt
    real    :: eg_dxt, &
               eg_dxu
    ! real, allocatable, dimension(:) :: eg_dyt, &
    real, dimension(1:jmt) :: eg_dyt,  &
                              eg_dyu,  &
                              eg_yt,   &
                              eg_yu,   &
                              eg_cst,  &
                              eg_csu,  &
                              eg_sint, &
                              eg_sinu

end module Ebm1DGrids
