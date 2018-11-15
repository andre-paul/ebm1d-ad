PROGRAM prgcost
   IMPLICIT NONE

!=========================================
! declaration
!=========================================
   integer n

!==============================================================
! get the number of control variables
!==============================================================
   call numbmod( n )

!-----------------------------------------
! call the subroutine
!-----------------------------------------
   call docost( n )

END PROGRAM prgcost


   SUBROUTINE docost( N )

      IMPLICIT NONE

      INTEGER N
      REAL    FC

      REAL   X(N)

!==============================================================
! initialize the model
! and set the first guess of the control variables
!==============================================================
      call initmod( n, x )

!===============================================
! forward model
! first call to do possible initializations
!===============================================

      call model( n, x, fc )
      print *
      print '(a,f10.4)', ' The cost function is : fc = ', fc

   END SUBROUTINE docost

