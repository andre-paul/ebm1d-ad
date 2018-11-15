PROGRAM prgtest
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

END PROGRAM prgtest


   SUBROUTINE docost( N )
      use OptimizationParameters    
      IMPLICIT NONE

      INTEGER N,i
      REAL    FC, adfc, fcp, fcm

      REAL   X(N), adx(n), dx(n), x0(n), x_rescaled(n), x0_rescaled(n)
      
      integer impres,io,mode,niter,nsim,iz(5),nrz,izs(1)
      real dxmin,df1,epsg,rzs(1)
      double precision dzs(1)
      REAL, ALLOCATABLE, dimension(:) :: rz
      ! external simul,euclid,ctonbe,ctcabe

!==============================================================
! initialize the model
! and set the first guess of the control variables
!==============================================================

      call initmod( n, x )

!===============================================
! forward model
! first call to do possible initializations
!===============================================

      print '(a)', ' >>> First running forward model'
      call model( n, x, fc )
      print '(a, f10.4)', ' >>> The cost function is: fc = ', fc

!===============================================
! forward model
! first call to do possible initializations
!===============================================

      DO i=1,n
         adx(i)=0.
      ENDDO
      adfc = 1.
      print '(a)', ' >>> Now running adjoint model'
      ! call admodel( n, x, fc, adx, adfc )
      call model_ad( n, x, adx, fc, adfc ) ! note different calling sequence for TAF
      print '(a, f10.4)', ' >>> The cost function is: fc = ', fc
      print '(a)', ' >>> The gradient with respect to the control variables is:'
      do i=1,n
         print '(a, i2, a, i2, a, e12.6)', " i = ", i, &
                ", adx(", i, ") = ", adx(i)
      end do
       
      print '(a)', ' >>> Checking gradients'

      do i=1, n
          print '(a, i2)', " >>> Working on control variable i = ", i
          dx = 0.
          dx(i) = 1.e-6
          call model(n, x + dx, fcp)
          print '(a, f10.4)', ' >>> The cost function is: fcp = ', fcp
          call model(n, x - dx, fcm)
          print '(a, f10.4)', ' >>> The cost function is: fcm = ', fcm
          print '(a, i2, a)', &
              " >>> Finite-difference derivative with respect to control variable i = ", & 
              i, ":"
          print '(3(a, e12.6))', &
              "  fdx = ", (fcp - fcm)/(2.0*dx(i)), &
              ", adx = ", adx(i),                  &
              ", fdx - adx = ", (fcp - fcm)/(2.*dx(i)) - adx(i)
          if (adx(i) .ne. 0.) &
              print '(a, e12.6)', " Relative error: (fdx - adx)/adx  = ", &
              ((fcp - fcm)/(2.0*dx(i)) - adx(i))/adx(i)
      end do

   END SUBROUTINE docost
    


