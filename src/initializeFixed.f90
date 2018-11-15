subroutine initializeFixed()
    use Constants
    use RunParameters
    use Ebm1DParameters
    implicit none
      
    ! Set fixed model parameters and arrays

    !--------------------------------------------------------------------------
    ! Set general model parameters
    !--------------------------------------------------------------------------
 
    ! Set run parameters
    call readRunParameters()

    !--------------------------------------------------------------------------
    ! Set Ebm1D-specfic parameters
    !--------------------------------------------------------------------------
       
    ! Set parameters
    call ebm1DReadParameters()
 
    ! Set grid
    ! (grid arrays are held and described in Ebm1DGrids.f90)
    call ebm1DInitializeGrids()
 
    ! Set fields to complete setup of Ebm1D
    call ebm1DInitializeFixed()

    !--------------------------------------------------------------------------
    ! Set parameters specific to computation of cost function
    !--------------------------------------------------------------------------
       
    ! Set parameters
    call cost1DReadParameters()

end subroutine initializeFixed
