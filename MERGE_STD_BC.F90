!===================================================================================================
! File: MERGE_STD.F
!
! Function: Provides subroutine BCON_OUT to merge the output from geoschem with std
!           CMAQ profile.
!
! Revision History:
!   1. Created By Yun-Fat Lam, April 30 16:36:04 EDT, 2010  (original code given by David Wong)
!      at the University of Tennessee, Knoxville.
!
!   2. Reformatted to Fortran 90 format. -- By Cheng-En Yang,  Aug. 3, 2017.
!===================================================================================================
SUBROUTINE MERGE_STD_BC(IN_DATA, OUT_DATA, OUT_NAME, OUT_UNITS, OUT_DES, NVARS_LEFT)
IMPLICIT NONE

INCLUDE SUBST_IOPARMS
INCLUDE SUBST_IOFDESC
INCLUDE SUBST_IODECL

INCLUDE SUBST_VGRD_ID
INCLUDE SUBST_COORD_ID
INCLUDE SUBST_CMAQ_ID

REAL*4,   INTENT(IN)  :: IN_DATA( 2*NTHIK*( NCOLS + NROWS + 2*NTHIK ), NLAYS, NVARS)
REAL*4,   INTENT(OUT) :: OUT_DATA( 2*NTHIK*( NCOLS + NROWS + 2*NTHIK ), NLAYS, NVARS_BUFF)
INTEGER,  INTENT(OUT) :: NVARS_LEFT
CHARACTER*16, INTENT(OUT) :: OUT_NAME(NVARS_BUFF),  OUT_UNITS(NVARS_BUFF)
CHARACTER*80, INTENT(OUT) :: OUT_DES(NVARS_BUFF)

CHARACTER*16, ALLOCATABLE :: standard_vname(:), standard_units(:)
CHARACTER*80, ALLOCATABLE :: standard_vdesc(:)

CHARACTER*16, ALLOCATABLE :: geos_vname(:), geos_units(:), left_vname(:), left_units(:)
CHARACTER*80, ALLOCATABLE :: geos_vdesc(:), left_vdesc(:)

INTEGER :: extra_nvar, left_nvar
INTEGER :: stat, nbndy, n, v, i, standard_jdate, standard_jtime, geos_jdate, geos_jtime,   &
           standard_nvar, geos_nvar, total_nvar, standard_nsteps, geos_nsteps, geos_tsteps

INTEGER, ALLOCATABLE :: left_index(:), standard_index(:)
