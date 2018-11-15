subroutine calculateGlobalAverage(weight, value, globalAverage)
    use Sizes
    use Constants
    use Ebm1DParameters
    use Ebm1DGrids
    use Ebm1DVariables
    implicit none

    ! Dummy arguments
    real, dimension(1:jmt) :: weight, value
    real :: globalAverage

    ! Local variables
    integer :: j
    real    :: sumOfWeights, weightedSum
    
    sumOfWeights = 0.0
    weightedSum  = 0.0
    do j=2,jmt-1
         sumOfWeights = sumOfWeights + weight(j)
         weightedSum  = weightedSum  + weight(j)*value(j)
    end do
    globalAverage = weightedSum/sumOfWeights

end subroutine calculateGlobalAverage
