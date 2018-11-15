MODULE OrbParameters
   USE Constants
   IMPLICIT NONE

   ! Constant declarations (Berger 1978)  
   ! Numbers of terms available for computation of orbital elements
   INTEGER, PARAMETER :: nef=19, nob=47, nop=78  
   ! INTEGER, PARAMETER :: nef=19, nob=18, nop=9
   ! Numbers of terms kept for computation of orbital elements
   INTEGER, PARAMETER :: neff=19, nobb=47, nopp=78  
   ! INTEGER, PARAMETER :: neff=19, nobb=18, nopp=9
   ! Some reference values
   REAL, PARAMETER :: xod=23.320556, xop=3.392506, prm=50.439273 

   ! Global variable declarations
   ! op_scon0 = solar constant/(W m^(-2))
   ! op_eccen = numerical eccentricity of the Earth's orbit (past value)
   ! op_perih = longitude of the perihelion with respect to the 
   !            moving vernal equinox (past value/deg)
   ! op_obliq = obliquity of the Earth's axis of rotation (past value/deg)
   ! op_latit   = latitude/degN
   REAL :: op_scon0, op_eccen, op_perih, op_obliq, op_latit

   ! Cosine series data for computation of obliquity:
   ! amplitude (arc seconds), rate (arc seconds/year), phase (degrees).
   REAL, DIMENSION(nob) :: aob
   REAL, DIMENSION(nob) :: bob
   REAL, DIMENSION(nob) :: cob

   ! Cosine/sine series data for computation of eccentricity and
   ! fixed vernal equinox longitude of perihelion (fvelp):
   ! amplitude, rate (arc seconds/year), phase (degrees).
   REAL, DIMENSION(nef) :: ae
   REAL, DIMENSION(nef) :: be
   REAL, DIMENSION(nef) :: ce
 
   ! Sine series data for computation of moving vernal equinox
   ! longitude of perihelion:
   ! amplitude (arc seconds), rate (arc seconds/year), phase (degrees).      
   REAL, DIMENSION(nop) :: aop
   REAL, DIMENSION(nop) :: bop
   REAL, DIMENSION(nop) :: cop
   
END MODULE OrbParameters


































