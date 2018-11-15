PROGRAM prgopti
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

END PROGRAM prgopti


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
      external simul,euclid,ctonbe,ctcabe

!==============================================================
! initialize the model
! and set the first guess of the control variables
!==============================================================

      print '(a)', ' >>> Set a first guess of the parameter vector'
      call initmod( n, x )

!===============================================
! forward model
! first call to do possible initializations
!===============================================

      print '(a)', ' >>> First running forward model'
      call model( n, x, fc )
      print '(a, f10.4)', ' >>> The cost function is: fc = ', fc

!===============================================
! adjoint model
!===============================================

      DO i=1,n
         adx(i)=0.
      ENDDO
      adfc = 1.
      x0 = x
      print '(a)', ' >>> Now running adjoint model'
      ! call admodel( n, x, fc, adx, adfc )
      call model_ad( n, x, adx, fc, adfc ) ! note different calling sequence for TAF
      print '(a, f10.4)', ' >>> The cost function is: fc = ', fc
      print '(a)', ' >>> The gradient with respect to the control variables is:'
      do i=1,n
          print '(a, i2, a, i2, a, e12.6)', " i = ", i, ", adx(", i, ") = ", adx(i)
      end do

      dxmin = 1.e-30 ! absolute precision on x
      ! dxmin = 1.e-6 ! absolute precision on x
      df1 = 0.1*fc   ! expected decrease for f
      ! epsg = 1.e-3   ! relative precision on g
                       ! (precision of stopping criterion based on norm of gradient)
      epsg = 1.e-4   ! relative precision on g
      ! epsg = 5.e-4   ! relative precision on g
      ! epsg = 1.e-5   ! relative precision on g
      ! epsg = 1.e-6   ! relative precision on g
      ! epsg = 1.e-7   ! relative precision on g
      impres = 3     ! control of output to channel io
      io = 6         ! channel number for output
      mode = 0
      niter = 1000   ! maximum number of iterations
      nsim  = 100000 ! maximum number of simulations
      iz = 0
      nrz = 6*n+2
      izs(1) = 1
      rzs(1) = 1.
      dzs(1) = 1.D0
      if (allocated(rz)) deallocate(rz)
      allocate(rz(nrz))
      CALL m1qn3 (simul,euclid,ctonbe,ctcabe,n,x,fc,adx,dxmin,df1, &
           epsg,impres,io,mode,niter,nsim,iz,rz,nrz,izs,rzs,dzs)
      deallocate(rz)
      PRINT *, 'x0 scaled   =', x0
      PRINT *, 'x  scaled   =', x
      do i=1,n
         x0_rescaled(i) = x0(i)*cond(i)
         x_rescaled(i)  = x(i)*cond(i)
      end do
      PRINT *, 'x0 rescaled =', x0_rescaled
      PRINT *, 'x  rescaled =', x_rescaled
      PRINT '(a,f10.4)', 'The cost function is   : ', fc
      PRINT '(a)', '>>> Rescaled control variables and cost function:'  
      print '(1X, 20G16.8)', (x_rescaled(i), i=1,n), fc

!===============================================
! forward model
!===============================================

      print '(a)', ' >>> Running forward model again'
      call model( n, x, fc )
      print '(a, f10.4)', ' >>> The final cost function is: fc = ', fc

   END SUBROUTINE docost

   SUBROUTINE simul (indic,n,x,f,g,izs,rzs,dzs)
     IMPLICIT NONE
     INTEGER indic,n,izs(1)
     REAL x(n),f,g(n),rzs(1)
     DOUBLE PRECISION dzs(1)

     REAL adf
     IF ( indic == 4 ) THEN
        adf = 1.
        g = 0.
        ! CALL admodel( n, x, f, g, adf )
        CALL model_ad( n, x, g, f, adf ) ! note different calling sequence for TAF
     ELSEIF (indic == 1) THEN
        CALL model( n, x, f )
     ENDIF

   END SUBROUTINE simul
     


