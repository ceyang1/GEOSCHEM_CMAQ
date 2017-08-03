!===================================================================================================
! File: MERGE_STD_IC.F90
!
! Function: Provides subroutine ICON_OUT to merge the output from geoschem with std CMAQ profile.
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

REAL (kind=4), INTENT(IN)  :: IN_DATA( NCOLS, NROWS, NLAYS, NVARS)
REAL (kind=4), INTENT(OUT) :: OUT_DATA( NCOLS, NROWS, NLAYS, NVARS_BUFF)
INTEGER,  INTENT(OUT) :: NVARS_LEFT
CHARACTER (LEN=16), INTENT(OUT) :: OUT_NAME(NVARS_BUFF),  OUT_UNITS(NVARS_BUFF)
CHARACTER (LEN=80), INTENT(OUT) :: OUT_DES(NVARS_BUFF)

CHARACTER (LEN=16), ALLOCATABLE :: standard_vname(:), standard_units(:)
CHARACTER (LEN=80), ALLOCATABLE :: standard_vdesc(:)

CHARACTER (LEN=16), ALLOCATABLE :: geos_vname(:), geos_units(:), left_vname(:), left_units(:)
CHARACTER (LEN=80), ALLOCATABLE :: geos_vdesc(:), left_vdesc(:)

INTEGER :: extra_nvar, left_nvar
INTEGER :: stat, nbndy, n, v, i, standard_jdate, standard_jtime, geos_jdate, geos_jtime,     &
           standard_nvar, geos_nvar, total_nvar, standard_nsteps, geos_nsteps, geos_tsteps

INTEGER, ALLOCATABLE :: left_index(:), standard_index(:)
REAL, ALLOCATABLE :: data(:,:,:,:), data_left(:,:,:,:)
INTEGER, EXTERNAL :: index1

CHARACTER (LEN=256), PARAMETER :: PNAME='MAIN'
CHARACTER (LEN=16), PARAMETER :: STANDARD_FILENAME='STD_PROFILE_IN'

! Open standard ICON file generated from CMAQ

OUT_DATA(:,:,:,:) = 0

if (.not. open3( STANDARD_FILENAME, FSREAD3, PNAME ) ) then
  write(6,*)
  write(6,*) ' could not open file:', STANDARD_FILENAME
  write(6,*)
  stop
end if

if ( .not. desc3(STANDARD_FILENAME) ) then
  write(6,*)
  write(6,*) ' could not get file description:', STANDARD_FILENAME
  write(6,*)
  stop
end if

standard_nvar = nvars3d

!------------------------------------------------------
allocate (standard_vname(standard_nvar), standard_units(standard_nvar), standard_vdesc(standard_nvar), &
          stat=stat)

standard_vname = vname3d
standard_units = units3d
standard_vdesc = vdesc3d
standard_jdate = sdate3d
standard_jtime = stime3d

allocate (standard_index(standard_nvar), stat=stat)

!do i = 1, standard_nvar
!  print *, "Check #1, ",  standard_vname(i)
!end do

!------------------------------------------------------
allocate (geos_vname(NVARS), geos_units(NVARS), geos_vdesc(NVARS), stat=stat)

geos_nvar = NVARS
geos_vname = VNAME
geos_units = UNITS
geos_vdesc = VDESC

!do i = 1, NVARS
!  print *, "Check #2, ", geos_vname(i)
!end do

! Compare variable lists

left_nvar = 0

do v = 1, standard_nvar
  standard_index(v) = index1(standard_vname(v), geos_nvar, geos_vname)
  if (standard_index(v) .eq. 0) then
    left_nvar = left_nvar + 1
  end if
end do

allocate (left_vname(left_nvar), left_units(left_nvar), left_vdesc(left_nvar), left_index(left_nvar), &
          stat=stat)

i = 1
do v = 1, standard_nvar
  if (standard_index(v) .eq. 0) then
    left_vname(i) = standard_vname(v)
    left_units(i) = standard_units(v)
    left_vdesc(i) = standard_vdesc(v)
    i = i + 1
  end if
end do

!do i = 1, left_nvar
!  print *, "Check #3, ", left_vname(i)
!end do

! Allocate data storage
allocate (data(NCOLS, NROWS, NLAYS, standard_nvar), stat=stat)
allocate (data_left(NCOLS, NROWS, NLAYS, left_nvar), stat=stat)

if (stat .ne. 0) then
  print *, ' Error allocating data'
  stop
end if

! Setup up the output file
if ( .not. read3(STANDARD_FILENAME, allvar3, allays3, standard_jdate, standard_jtime, data(:,:,:,:) )) then
  print *, 'could not read from ', STANDARD_FILENAME
  stop
end if

i = 1
do v = 1, standard_nvar
  if (standard_index(v) .eq. 0) then
    data_left(:,:,:,i) = data(:,:,:,v)
    i = i + 1
  end if
end do

NVARS_LEFT = i - 1

OUT_NAME(1:geos_nvar)  = geos_vname(1:geos_nvar)
OUT_UNITS(1:geos_nvar) = geos_units(1:geos_nvar)
OUT_DES(1:geos_nvar)   = geos_vdesc(1:geos_nvar)
OUT_DATA(:,:,:,1:geos_nvar) = IN_DATA(:,:,:,1:geos_nvar)

OUT_NAME(geos_nvar+1:geos_nvar+left_nvar)  = left_vname(1:left_nvar)
OUT_UNITS(geos_nvar+1:geos_nvar+left_nvar) = left_units(1:left_nvar)
OUT_DES(geos_nvar+1:geos_nvar+left_nvar)   = left_vdesc(1:left_nvar)
OUT_DATA(:,:,:,geos_nvar+1:geos_nvar+left_nvar) = data_left(:,:,:,1:left_nvar)

deallocate (left_vname)
deallocate (left_units)
deallocate (left_vdesc)
deallocate (data_left)
deallocate (data)

RETURN
END
