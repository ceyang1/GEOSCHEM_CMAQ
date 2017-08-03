!===================================================================================================
! File: SPECIES_MAPPING_OUTUNIT_PPMV.F90
!
! Function: Mapping the species between GEOSCHEM and CMAQ. Output units are ppmV for all species.
!
! Returned Values:
!     CONC_GEOS~ GEOSCHEM DATA ALREADY READIN
!     STATUS ~ .true. if read data , .false. if no data read
!     CONC_GEOS_SPECS_MAPPED~ Have the mapped species although in GEOSCHEM grids
!
! Note: Make sure the species map is right before use this subroutine. Pay attention to the units of
!       the GEOSCHEM results. This version is fo GEOSCHEM output with units v/v
!
! Fact: When do the mapping, the unit of the species are vC/v rather than ppbC or ppbv
!       So, be cautious on the units of CONC_GEOS, CONC_GEOS_SPECS_MAPPED and AIR_DENSITY.

! Revision History:
!  1. Created by Zuopan Li, The University of Tennessee, Knoxville. Sat Feb 19 15:44:05 EST 2005.
!  1.2 Assign 10ppbv to negative mapped values, might be invalid for your case
!  1.3 Modified for 2002 GEOSCHEM results.  Thu Jul 13 14:16:08 EDT 2006
!
!  2. Modified to accept 2006 GEOSCHEM with 54 species. -- By Yun-Fat Lam, April 30 16:36:04 EDT, 2010
!            (original code given by David Wong) at the University of Tennessee, Knoxville.
!
!  3. Updated to GEOS-Chem 11-01. -- By Cheng-En Yang, Jul. 20, 2017
!  3.1 Reformatted to Fortran 90 format. -- By Cheng-En Yang,  Aug. 3, 2017.
!  3.2 All species are required in the GEOSCHEM output, or you must modify the reading and mapping
!      table.
!===================================================================================================
!Tracer #1   :   1   NO      30.0
!Tracer #2   :   2   O3      48.0
!Tracer #3   :   3   PAN    121.0
!Tracer #4   :   4   CO      28.0   (CO)
!Tracer #5   :   5   ALK4    58.0   (4C)
!Tracer #6   :   6   ISOP    12.0   (5C)
!Tracer #7   :   7   HNO3    63.0
!Tracer #8   :   8   H2O2    34.0
!Tracer #9   :   9   ACET    58.0   (3C)
!Tracer #10  :  10   MEK     12.0   (4C)
!Tracer #11  :  11   ALD2    44.0   (2C)
!Tracer #12  :  12   RCHO    58.0
!Tracer #13  :  13   MVK     70.0
!Tracer #14  :  14   MACR    70.0
!Tracer #15  :  15   PMN    147.0
!Tracer #16  :  16   PPN    135.0
!Tracer #17  :  17   R4N2   119.0
!Tracer #18  :  18   PRPE    12.0   (3C)
!Tracer #19  :  19   C3H8    12.0   (3C)
!Tracer #20  :  20   CH2O    30.0   (CH2O)
!Tracer #21  :  21   C2H6    12.0   (2C)
!Tracer #22  :  22   N2O5   108.0
!Tracer #23  :  23   HNO4    79.0
!Tracer #24  :  24   MP      48.0
!Tracer #25  :  25   DMS     62.0
!Tracer #26  :  26   SO2     64.0
!Tracer #27  :  27   SO4     96.0
!Tracer #28  :  28   SO4s    31.4
!Tracer #29  :  29   MSA     96.0
!Tracer #30  :  30   NH3     17.0
!Tracer #31  :  31   NH4     18.0
!Tracer #32  :  32   NIT     62.0
!Tracer #33  :  33   NITs    31.4
!Tracer #34  :  34   BCPI    12.0
!Tracer #35  :  35   OCPI    12.0
!Tracer #36  :  36   BCPO    12.0
!Tracer #37  :  37   OCPO    12.0
!Tracer #38  :  38   DST1    29.0
!Tracer #39  :  39   DST2    29.0
!Tracer #40  :  40   DST3    29.0
!Tracer #41  :  41   DST4    29.0
!Tracer #42  :  42   SALA    31.4
!Tracer #43  :  43   SALC    31.4
!Tracer #44  :  44   Br2    126.0
!Tracer #45  :  45   Br      63.0
!Tracer #46  :  46   BrO    142.0
!Tracer #47  :  47   HOBr   143.0
!Tracer #48  :  48   HBr     64.0
!Tracer #49  :  49   BrNO2  126.0
!Tracer #50  :  50   BrNO3  142.0
!Tracer #51  :  51   CHBr3  202.0
!Tracer #52  :  52   CH2Br2 140.0
!Tracer #53  :  53   CH3Br   78.0
!Tracer #54  :  54   MPN     93.0
!Tracer #55  :  55   ISOPND 147.0
!Tracer #56  :  56   ISOPNB 147.0
!Tracer #57  :  57   MOBA   114.0
!Tracer #58  :  58   PROPNN 119.0
!Tracer #59  :  59   HAC     74.0
!Tracer #60  :  60   GLYC    60.0
!Tracer #61  :  61   MVKN   149.0
!Tracer #62  :  62   MACRN  149.0
!Tracer #63  :  63   RIP    118.0
!Tracer #64  :  64   IEPOX  118.0
!Tracer #65  :  65   MAP     76.0
!Tracer #66  :  66   NO2     46.0
!Tracer #67  :  67   NO3     62.0
!Tracer #68  :  68   HNO2    47.0
!Tracer #69  :  69   MTPA   136.23
!Tracer #70  :  70   LIMO   136.23
!Tracer #71  :  71   MTPO   136.23
!Tracer #72  :  72   TSOG1  150.0
!Tracer #73  :  73   TSOG2  150.0
!Tracer #74  :  74   TSOG3  150.0
!Tracer #75  :  75   TSOG0  150.0
!Tracer #76  :  76   TSOA1  150.0
!Tracer #77  :  77   TSOA2  150.0
!Tracer #78  :  78   TSOA3  150.0
!Tracer #79  :  79   TSOA0  150.0
!Tracer #80  :  80   ISOG1  150.0
!Tracer #81  :  81   ISOG2  150.0
!Tracer #82  :  82   ISOG3  150.0
!Tracer #83  :  83   ISOA1  150.0
!Tracer #84  :  84   ISOA2  150.0
!Tracer #85  :  85   ISOA3  150.0
!Tracer #86  :  86   BENZ    78.0   (6C)
!Tracer #87  :  87   TOLU    12.0   (7C)
!Tracer #88  :  88   XYLE    12.0   (8C)
!Tracer #89  :  89   ASOG1  150.0
!Tracer #90  :  90   ASOG2  150.0
!Tracer #91  :  91   ASOG3  150.0
!Tracer #92  :  92   ASOAN  150.0
!Tracer #93  :  93   ASOA1  150.0
!Tracer #94  :  94   ASOA2  150.0
!Tracer #95  :  95   ASOA3  150.0
!Tracer #1   :  96   Hg0    201.0
!Tracer #2   :  97   Hg2    201.0
!Tracer #3   :  98   HgP    201.0
!===================================================================================================

SUBROUTINE SPECIES_MAPPING_OUTUNIT_PPMV (NI, NJ, NL, NP, CONC_GEOS, NCOLS, NROWS, NLAYS, NVARS, CONC_GEOS_SPECS_MAPPED, STATUS)

IMPLICIT NONE
INCLUDE SUBST_CONST

INTEGER, INTENT(IN) :: NI, NJ, NL, NP
INTEGER, INTENT(IN) :: NCOLS, NROWS, NVARS, NLAYS
REAL (kind=4), INTENT(IN) :: CONC_GEOS( NI, NJ, NL, NP )
!REAL, INTENT(IN) :: AIR_DENSITY (NI, NJ, NL)           ! Air density from GEOSCHEM
REAL (kind=4), INTENT(OUT) :: CONC_GEOS_SPECS_MAPPED(NI, NJ, NL, NVARS)
LOGICAL :: STATUS

INTEGER :: I, J, L, SPC

!...... Subroutine starts here
STATUS = .FALSE.

!! =======================================================================================================
!! Starting gas phase mapping, note the jumps between numbers if you skip some species in GEOS-Chem output
!! =======================================================================================================
! [ NO2     ] (CMAQ  1) = [ NO2     ] (GS 58)
CONC_GEOS_SPECS_MAPPED(:,:,:, 1) = CONC_GEOS(:,:,:,58)

! [ NO      ] (CMAQ  2) = [ NO      ] (GC  1)
CONC_GEOS_SPECS_MAPPED(:,:,:, 2) = CONC_GEOS(:,:,:, 1)

! [ O3      ] (CMAQ  3) = [ O3      ] (GS  2) + [ NO3     ] (GC 59)
!                       - [ NO      ] (GC  1) - [ HNO2    ] (GC 60)
CONC_GEOS_SPECS_MAPPED(:,:,:, 3) = CONC_GEOS(:,:,:, 2) + CONC_GEOS(:,:,:,59)  &
                                 - CONC_GEOS(:,:,:, 1) - CONC_GEOS(:,:,:,60)
!! [ O3      ] (CMAQ  3) = [ O3      ] (GC  2) - [ NO      ] (GC  1)
! CONC_GEOS_SPECS_MAPPED(:,:,:, 3) = CONC_GEOS(:,:,:, 2) - CONC_GEOS(:,:,:, 1)

! [ NO3     ] (CMAQ  4) = [ NO3     ] (GC 59)
CONC_GEOS_SPECS_MAPPED(:,:,:, 4) = CONC_GEOS(:,:,:,59)

! [ N2O5    ] (CMAQ  5) = [ N2O5    ] (GC 22)
CONC_GEOS_SPECS_MAPPED(:,:,:, 5) = CONC_GEOS(:,:,:,22)

! [ HNO3    ] (CMAQ  6) = [ HNO3    ] (GS  7)
CONC_GEOS_SPECS_MAPPED(:,:,:, 6) = CONC_GEOS(:,:,:, 7)

! [ HONO    ] (CMAQ  7) = [ HNO2    ] (GC 60)
CONC_GEOS_SPECS_MAPPED(:,:,:, 7) = CONC_GEOS(:,:,:,60)

! [ PNA     ] (CMAQ  8) = [ HNO4    ] (GC 23)
CONC_GEOS_SPECS_MAPPED(:,:,:, 8) = CONC_GEOS(:,:,:,23)

! [ H2O2    ] (CMAQ  9) = [ H2O2    ] (GC  8)
CONC_GEOS_SPECS_MAPPED(:,:,:, 9) = CONC_GEOS(:,:,:, 8)

! [ ALD2    ] (CMAQ 10) = [ ALD2    ] (GS 11) / 2.
CONC_GEOS_SPECS_MAPPED(:,:,:,10) = CONC_GEOS(:,:,:,11) / 2.

! [ IOLE    ] (CMAQ 11) = [ PRPE    ] (GC 18) / 2. /4.
CONC_GEOS_SPECS_MAPPED(:,:,:,11) = CONC_GEOS(:,:,:,18) /2./4.

! [ FORM    ] (CMAQ 12) = [ CH2O    ] (GC 20)
CONC_GEOS_SPECS_MAPPED(:,:,:,12) = CONC_GEOS(:,:,:,20)

! [ CO      ] (CMAQ 13) = [ CO      ] (GC  4)
CONC_GEOS_SPECS_MAPPED(:,:,:,13) = CONC_GEOS(:,:,:, 4)

! [ MEPX    ] (CMAQ 14) = [ MP      ] (GC 24)
CONC_GEOS_SPECS_MAPPED(:,:,:,14) = CONC_GEOS(:,:,:,24)

! [ PAN     ] (CMAQ 15) = [ PAN     ] (GC 3)
CONC_GEOS_SPECS_MAPPED(:,:,:,15) = CONC_GEOS(:,:,:, 3)

! [ PANX    ] (CMAQ 16) = [ PMN     ] (GC 15) + [ PPN     ] (GC 16)
CONC_GEOS_SPECS_MAPPED(:,:,:,16) = CONC_GEOS(:,:,:,15) + CONC_GEOS(:,:,:,16)

! [ OLE     ] (CMAQ 17) = [ PRPE    ] (GC 18) / 2. / 2.
CONC_GEOS_SPECS_MAPPED(:,:,:,17) = CONC_GEOS(:,:,:,18) /2./2.

! [ ISPD    ] (CMAQ 18) = [ MVK     ] (GC 13) + [ MACR    ] (GC 14)
CONC_GEOS_SPECS_MAPPED(:,:,:,18) = CONC_GEOS(:,:,:,13) + CONC_GEOS(:,:,:,14)

! [ NTR     ] (CMAQ 19) = [ R4N2    ] (GC 17)
CONC_GEOS_SPECS_MAPPED(:,:,:,19) = CONC_GEOS(:,:,:,17)

! [ ALDX    ] (CMAQ 20) = [ RCHO    ] (GC 12)
CONC_GEOS_SPECS_MAPPED(:,:,:,20) = CONC_GEOS(:,:,:,12)

! [ TOL     ] (CMAQ 21) = [ TOLU    ] (GC 79) / 7.
CONC_GEOS_SPECS_MAPPED(:,:,:,21) = CONC_GEOS(:,:,:,79) / 7.

! [ XYLMN   ] (CMAQ 22) = [ XYLE    ] (GC 80) / 8.
CONC_GEOS_SPECS_MAPPED(:,:,:,22) = CONC_GEOS(:,:,:,80) / 8.

! [ SO2     ] (CMAQ 23) = [ SO2     ] (GC 26)
CONC_GEOS_SPECS_MAPPED(:,:,:,23) = CONC_GEOS(:,:,:,26)

! [ ETHA    ] (CMAQ 24) = [ C2H6    ] (GC 21) / 2.
CONC_GEOS_SPECS_MAPPED(:,:,:,24) = CONC_GEOS(:,:,:,21) / 2.

! [ NH3     ] (CMAQ 25) = [ NH3     ] (GC 30)
CONC_GEOS_SPECS_MAPPED(:,:,:,25) = CONC_GEOS(:,:,:,30)

! [ PAR     ] (CMAQ 26) = [ C3H8    ] (GC 19) *1.5/3. + [ ALK4    ] (GC  5) *4./4.
!                       + [ ACET    ] (GC  9) *3./3.  + [ MEK     ] (GC 10) *4./4.
!                       + [ BENZ    ] (GC 78) / 6.
CONC_GEOS_SPECS_MAPPED(:,:,:,26) = CONC_GEOS(:,:,:,19) / 2. + CONC_GEOS(:,:,:, 5)   &
                                 + CONC_GEOS(:,:,:, 9)      + CONC_GEOS(:,:,:,10)   &
                                 + CONC_GEOS(:,:,:,78) / 6.

! [ ISOP    ] (CMAQ 19) = [ ISOP    ] (GC  6) / 5.
CONC_GEOS_SPECS_MAPPED(:,:,:,27) = CONC_GEOS(:,:,:, 6) / 5.

! [ BENZENE ] (CMAQ 28) = [ BENZ    ] (GC 78) / 6.
CONC_GEOS_SPECS_MAPPED(:,:,:,28) = CONC_GEOS(:,:,:,78) / 6.

!! ===========================
!! Starting aerosol mapping
!! ===========================

! [ ASOIL   ] (CMAQ 29) = 0.95995 * ( [ DST2 ] (GC 39) + [ DST3 ] (GC 40) + [ DST4 ] (GC 41) )
CONC_GEOS_SPECS_MAPPED(:,:,:,29) = 0.95995*( CONC_GEOS(:,:,:,39) + CONC_GEOS(:,:,:,40)  &
                                           + CONC_GEOS(:,:,:,41) )
! CYZ
! print *, 'TEST 1: ',CONC_GEOS_SPECS_MAPPED(31,65,1,29)
! print *, 'DST2', CONC_GEOS(31,65,1,39)
! print *, 'DST3', CONC_GEOS(31,65,1,40)
! print *, 'DST4', CONC_GEOS(31,65,1,41)
! CYZ
!! [ ASOIL   ] (CMAQ 29) = [ DST1 ] (GC 38) + [ DST2 ] (GC 39) + [ DST3 ] (GC 40)
! CONC_GEOS_SPECS_MAPPED(:,:,:,29) = CONC_GEOS(:,:,:,38) + CONC_GEOS(:,:,:,39)  &
!                                  + CONC_GEOS(:,:,:,40)

! [ ASO4K   ] (CMAQ 30) = 0.0776 * [ SALC ] (GC 43)
!                       + 0.02655 *( [ DST2 ] (GC 39) + [ DST3 ] (GC 40) + [ DST4 ] (GC 41) )
!                       + [ SO4s ] (GC 28)
CONC_GEOS_SPECS_MAPPED(:,:,:,30) = 0.0776 * CONC_GEOS(:,:,:,43)
                                 + 0.02655* ( CONC_GEOS(:,:,:,39) + CONC_GEOS(:,:,:,40)  &
                                            + CONC_GEOS(:,:,:,41) )
                                 + CONC_GEOS(:,:,:,28)

! [ ASO4J   ] (CMAQ 31) = 0.99 * [ SO4 ] (GC 27) + 0.0776 * [ SALA ] (GC 42) + 0.0225 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,31) = 0.99 * CONC_GEOS(:,:,:,27) + 0.0776 * CONC_GEOS(:,:,:,42)  &
                                 + 0.0225 * CONC_GEOS(:,:,:,38)

! [ ASO4I   ] (CMAQ 32) = 0.01 * [ SO4 ] (GC 27)
CONC_GEOS_SPECS_MAPPED(:,:,:,32) = 0.01 * CONC_GEOS(:,:,:,27)

! [ ANH4J   ] (CMAQ 33) = 0.99 * [ NH4 ] (GC 31) + 0.00005 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,33) = 0.99 * CONC_GEOS(:,:,:,31) + 0.00005 * CONC_GEOS(:,:,:,38)

! [ ANH4I   ] (CMAQ 34) = 0.01 * [ NH4 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,34) = 0.01 * CONC_GEOS(:,:,:,31)

! [ ANO3J   ] (CMAQ 35) = 0.99 * [ NIT ] (GC 32) + 0.0002 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,35) = 0.99 * CONC_GEOS(:,:,:,32) + 0.0002 * CONC_GEOS(:,:,:,38)

! [ ANO3I   ] (CMAQ 36) = 0.01 * [ NIT ]
CONC_GEOS_SPECS_MAPPED(:,:,:,36) = 0.01 * CONC_GEOS(:,:,:,32)

! [ ACLK    ] (CMAQ 37) = 0.5538 * [ SALC ] (GC 43)  &
!                       + 0.0119 * ( [ DST2 ] (GC 39) + [ DST3 ] (GC 40) + [ DST4 ] (GC 41) )
CONC_GEOS_SPECS_MAPPED(:,:,:,37) = 0.5538 * CONC_GEOS(:,:,:,43)
                                 + 0.0119 * ( CONC_GEOS(:,:,:,39) + CONC_GEOS(:,:,:,40)  &
                                            + CONC_GEOS(:,:,:,41) )

! [ ACLJ    ] (CMAQ 38) = 0.5538 * [ SALA ] (GC 42) + 0.00945 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,38) = 0.5538 * CONC_GEOS(:,:,:,42) + 0.00945 * CONC_GEOS(:,:,:,38)

! [ ANAJ    ] (CMAQ 39) = 0.3086 * [ SALA ] (GC 42) + 0.03935 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,39) = 0.3086 * CONC_GEOS(:,:,:,42) + 0.03935 * CONC_GEOS(:,:,:,38)

! [ AMGJ    ] (CMAQ 40) = 0.0368 * [ SALA ] (GC 42)
CONC_GEOS_SPECS_MAPPED(:,:,:,40) = 0.0368 * CONC_GEOS(:,:,:,42)

! [ AKJ     ] (CMAQ 41) = 0.0114 * [ SALA ] (GC 42) + 0.0377 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,41) = 0.0114 * CONC_GEOS(:,:,:,42) + 0.0377 * CONC_GEOS(:,:,:,38)

! [ ACAJ    ] (CMAQ 42) = 0.0118 * [ SALA ] (GC 42) + 0.0794 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,42) = 0.0118 * CONC_GEOS(:,:,:,42) + 0.0794 * CONC_GEOS(:,:,:,38)

! [ AFEJ    ] (CMAQ 43) = 0.03355 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,43) = 0.03355 * CONC_GEOS(:,:,:,38)

! [ AALJ    ] (CMAQ 44) = 0.05695 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,44) = 0.05695 * CONC_GEOS(:,:,:,38)

! [ ASIJ    ] (CMAQ 45) = 0.19435 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,45) = 0.19435 * CONC_GEOS(:,:,:,38)

! [ ATIJ    ] (CMAQ 46) = 0.0028 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,46) = 0.0028 * CONC_GEOS(:,:,:,38)

! [ AMNJ    ] (CMAQ 47) = 0.00115 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,47) = 0.00115 * CONC_GEOS(:,:,:,38)

! [ AOTHRJ  ] (CMAQ 48) = 0.50219 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,48) = 0.50219 * CONC_GEOS(:,:,:,38)

! [ AECJ    ] (CMAQ 49) = 0.999 * ( [ BCPI ] (GC 34) + [ BCPO ] (GC 36) )
CONC_GEOS_SPECS_MAPPED(:,:,:,49) = 0.999 * ( CONC_GEOS(:,:,:,34) + CONC_GEOS(:,:,:,36) )

! [ AECI    ] (CMAQ 50) = 0.001 * ( [ BCPI ] (GC 34) + [ BCPO ] (GC 36) )
CONC_GEOS_SPECS_MAPPED(:,:,:,50) = 0.001 * ( CONC_GEOS(:,:,:,34) + CONC_GEOS(:,:,:,36) )

! [ APOCJ   ] (CMAQ 51) = 0.9 * ( [ OCPO ] (GC 37) + [ OCPI ] (GC 35) ) + 0.01075 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,51) = 0.9*( CONC_GEOS(:,:,:,37) + CONC_GEOS(:,:,:,35) )  &
                                 + 0.01075 * CONC_GEOS(:,:,:,38)

! [ APOCI   ] (CMAQ 52) = 0.1 * ( [ OCPO ] (GC 37) + [ OCPI ] (GC 35) )
CONC_GEOS_SPECS_MAPPED(:,:,:,52) = 0.9*( CONC_GEOS(:,:,:,37) + CONC_GEOS(:,:,:,35) )

! [ APNCOMJ ] (CMAQ 53) = 0.4*0.9*( [ OCPO ] (GC 37) + [ OCPI ] (GC 35) ) + 0.0043 * [ DST1 ] (GC 38)
CONC_GEOS_SPECS_MAPPED(:,:,:,53) = 0.4*0.9*( CONC_GEOS(:,:,:,35)+CONC_GEOS(:,:,:,98)   &
                                           + CONC_GEOS(:,:,:,37)+CONC_GEOS(:,:,:,97) ) &
                                 + 0.0043 * CONC_GEOS(:,:,:,38)

! [ APNCOMI ] (CMAQ 54) = 0.4*0.1*( [ OCPO ] (GC 37) + [ OCPI ] (GC 35) )
CONC_GEOS_SPECS_MAPPED(:,:,:,54) = 0.4*0.1*( CONC_GEOS(:,:,:,35)+CONC_GEOS(:,:,:,98)   &
                                           + CONC_GEOS(:,:,:,37)+CONC_GEOS(:,:,:,97) )

! [ ANO3K   ] (CMAQ 55) = [ NITs ] + 0.0016 * ( [ DST2 ] (GC 39) + [ DST3 ] (GC 40) + [ DST4 ] (GC 41) )
CONC_GEOS_SPECS_MAPPED(:,:,:,55) = CONC_GEOS(:,:,:,33)
                                 + 0.0016 * ( CONC_GEOS(:,:,:,39) + CONC_GEOS(:,:,:,40)  &
                                            + CONC_GEOS(:,:,:,41))

! [ ASEACAT ] (CMAQ 56) = 0.3685 * [ SALC ] (GC 43)
CONC_GEOS_SPECS_MAPPED(:,:,:,56) = 0.3685 * CONC_GEOS(:,:,:,43)

! [ ABNZ1J ] (CMAQ 57) = 0.12 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85) + [ASOA2] (GC 86) + [ASOA3] (GC 87) )
CONC_GEOS_SPECS_MAPPED(:,:,:,57) = 0.12* ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                         + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ ABNZ2J ] (CMAQ 58) = 0.04 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85) + [ASOA2] (GC 86) + [ASOA3] (GC 87) )
CONC_GEOS_SPECS_MAPPED(:,:,:,58) = 0.04* ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                         + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ ABNZ3J ] (CMAQ 59) = 0.32 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85) + [ASOA2] (GC 86) + [ASOA3] (GC 87) )
CONC_GEOS_SPECS_MAPPED(:,:,:,59) = 0.32 * ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                          + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ AISO1J ] (CMAQ 60) = 0.75 * ( [ISOA1] (GC 75) + [ISOA2] (GC 76) + [ISOA3] (GC 77) )
CONC_GEOS_SPECS_MAPPED(:,:,:,60) = 0.75 * ( CONC_GEOS(:,:,:,75) + CONC_GEOS(:,:,:,76)  &
                                          + CONC_GEOS(:,:,:,77) )

! [ AISO2J ] (CMAQ 61) = 0.25 * ( [ISOA1] (GC 75) + [ISOA2] (GC 76) + [ISOA3] (GC 77) )
CONC_GEOS_SPECS_MAPPED(:,:,:,61) = 0.25 * ( CONC_GEOS(:,:,:,75) + CONC_GEOS(:,:,:,76)  &
                                          + CONC_GEOS(:,:,:,77) )

! [ ASQTJ ] (CMAQ 62) = 0.33 * ( [TSOA0] (GC 68) + [TSOA1] (GC 69) + [TSOA2] (GC 70) + [TSOA3] (GC 71))
CONC_GEOS_SPECS_MAPPED(:,:,:,62) = 0.33 * ( CONC_GEOS(:,:,:,68) + CONC_GEOS(:,:,:,69)  &
                                          + CONC_GEOS(:,:,:,70) + CONC_GEOS(:,:,:,71) )

! [ ATOL1J ] (CMAQ 63) = 0.04 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85) + [ASOA2] (GC 86) + [ASOA3] (GC 87))
CONC_GEOS_SPECS_MAPPED(:,:,:,63) = 0.04 * ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                          + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ ATOL2J ] (CMAQ 64) = 0.04 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85)
!                               + [ASOA2] (GC 86) + [ASOA3] (GC 87))
CONC_GEOS_SPECS_MAPPED(:,:,:,64) = 0.04 * ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                          + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ ATOL3J ] (CMAQ 65) = 0.29 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85) + [ASOA2] (GC 86) + [ASOA3] (GC 87) )
CONC_GEOS_SPECS_MAPPED(:,:,:,65) = 0.29 * ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                          + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ ATRP1J ] (CMAQ 66) = 0.33 * ( [TSOA0] (GC 68) + [TSOA1] (GC 69) + [TSOA2] (GC 70) + [TSOA3] (GC 71) )
CONC_GEOS_SPECS_MAPPED(:,:,:,66) = 0.33 * ( CONC_GEOS(:,:,:,68) + CONC_GEOS(:,:,:,69)  &
                                          + CONC_GEOS(:,:,:,70) + CONC_GEOS(:,:,:,71) )

! [ ATRP2J ] (CMAQ 67) = 0.34 * ( [TSOA0] (GC 68) + [TSOA1] (GC 69) + [TSOA2] (GC 70) + [TSOA3] (GC 71) )
CONC_GEOS_SPECS_MAPPED(:,:,:,67) = 0.34 * ( CONC_GEOS(:,:,:,68) + CONC_GEOS(:,:,:,69)  &
                                          + CONC_GEOS(:,:,:,70) + CONC_GEOS(:,:,:,71) )

! [ AXYL1J ] (CMAQ 68) = 0.03 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85) + [ASOA2] (GC 86) + [ASOA3] (GC 87) )
CONC_GEOS_SPECS_MAPPED(:,:,:,68) = 0.03 * ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                          + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ AXYL2J ] (CMAQ 69) = 0.01 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85) + [ASOA2] (GC 86) + [ASOA3] (GC 87) )
CONC_GEOS_SPECS_MAPPED(:,:,:,69) = 0.01 * ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                          + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ AXYL3J ] (CMAQ 70) = 0.11 * ( [ASOAN] (GC 84) + [ASOA1] (GC 85) + [ASOA2] (GC 86) + [ASOA3] (GC 87) )
CONC_GEOS_SPECS_MAPPED(:,:,:,70) = 0.11 * ( CONC_GEOS(:,:,:,84) + CONC_GEOS(:,:,:,85)  &
                                          + CONC_GEOS(:,:,:,86) + CONC_GEOS(:,:,:,87) )

! [ SV_BNZ1 ] (CMAQ 71) = 0.06 * ( [ASOG1] (GC 81) + [ASOG2] (GC 82) + [ASOG3] (GC 83) )
CONC_GEOS_SPECS_MAPPED(:,:,:,71) = 0.06 * ( CONC_GEOS(:,:,:,81) + CONC_GEOS(:,:,:,82)  &
                                          + CONC_GEOS(:,:,:,83) )

! [ SV_BNZ2 ] (CMAQ 72) = 0.23 * ( [ASOG1] (GC 81) + [ASOG2] (GC 82) + [ASOG3] (GC 83) )
CONC_GEOS_SPECS_MAPPED(:,:,:,72) = 0.23 * ( CONC_GEOS(:,:,:,81) + CONC_GEOS(:,:,:,82)  &
                                          + CONC_GEOS(:,:,:,83) )

! [ SV_TOL1 ] (CMAQ 73) = 0.23 * ( [ASOG1] (GC 81) + [ASOG2] (GC 82) + [ASOG3] (GC 83) )
CONC_GEOS_SPECS_MAPPED(:,:,:,73) = 0.23 * ( CONC_GEOS(:,:,:,81) + CONC_GEOS(:,:,:,82)  &
                                          + CONC_GEOS(:,:,:,83) )

! [ SV_TOL2 ] (CMAQ 74) = 0.23 * ([ASOG1] (GC 81) + [ASOG2] (GC 82) + [ASOG3] (GC 83) )
CONC_GEOS_SPECS_MAPPED(:,:,:,74) = 0.23 * ( CONC_GEOS(:,:,:,81) + CONC_GEOS(:,:,:,82)  &
                                          + CONC_GEOS(:,:,:,83) )

! [ SV_XYL1 ] (CMAQ 75) = 0.19 * ( [ASOG1] (GC 81) + [ASOG2] (GC 82) + [ASOG3] (GC 83) )
CONC_GEOS_SPECS_MAPPED(:,:,:,75) = 0.19 * ( CONC_GEOS(:,:,:,81) + CONC_GEOS(:,:,:,82)  &
                                          + CONC_GEOS(:,:,:,83) )

! [ SV_XYL2 ] (CMAQ 76) = 0.06 * ( [ASOG1] (GC 81) + [ASOG2] (GC 82) + [ASOG3] (GC 83) )
CONC_GEOS_SPECS_MAPPED(:,:,:,76) = 0.06 * ( CONC_GEOS(:,:,:,81) + CONC_GEOS(:,:,:,82)  &
                                          + CONC_GEOS(:,:,:,83) )

! [ SV_ISO1 ] (CMAQ 77) = 0.75 * ( [ISOG1] (GC 72) + [ISOG2] (GC 73) + [ISOG3] (GC 74) )
CONC_GEOS_SPECS_MAPPED(:,:,:,77) = 0.75*( CONC_GEOS(:,:,:,72) + CONC_GEOS(:,:,:,73)  &
                                        + CONC_GEOS(:,:,:,74) )

! [ SV_ISO2 ] (CMAQ 78) = 0.25 * ( [ISOG1] (GC 72) + [ISOG2] (GC 73) + [ISOG3] (GC 74) )
CONC_GEOS_SPECS_MAPPED(:,:,:,78) = 0.25 * ( CONC_GEOS(:,:,:,72) + CONC_GEOS(:,:,:,73)  &
                                          + CONC_GEOS(:,:,:,74) )

! [ SV_SQT ] (CMAQ 79) = 0.33 * ( [TSOG0] (GC 64) + [TSOG1] (GC 65) + [TSOG2] (GC 66) + [TSOG3] (GC 67) )
CONC_GEOS_SPECS_MAPPED(:,:,:,79) = 0.33*( CONC_GEOS(:,:,:,64) + CONC_GEOS(:,:,:,65)  &
                                        + CONC_GEOS(:,:,:,66) + CONC_GEOS(:,:,:,67) )

! [ SV_TRP1 ] (CMAQ 80)  = 0.33 * ( [TSOG0] (GC 64) + [TSOG1] (GC 65) + [TSOG2] (GC 66) + [TSOG3] (GC 67) )
CONC_GEOS_SPECS_MAPPED(:,:,:,80) = 0.33 * ( CONC_GEOS(:,:,:,64) + CONC_GEOS(:,:,:,65)  &
                                          + CONC_GEOS(:,:,:,66) + CONC_GEOS(:,:,:,67) )

! [ SV_TRP2 ] (CMAQ 81)  = 0.34 * ( [TSOG0] (GC 64) + [TSOG1] (GC 65) + [TSOG2] (GC 66) + [TSOG3] (GC 67) )
CONC_GEOS_SPECS_MAPPED(:,:,:,81) = 0.34 * ( CONC_GEOS(:,:,:,64) + CONC_GEOS(:,:,:,65)  &
                                          + CONC_GEOS(:,:,:,66) + CONC_GEOS(:,:,:,67) )

! [ HG      ] (CMAQ 82) = [ Hg0  ] (GC 1)
CONC_GEOS_SPECS_MAPPED(:,:,:, 82) = CONC_GEOS(:,:,:, 95)

! [ HGIIGAS ] (CMAQ 83) = [ Hg2  ] (GC 2)
CONC_GEOS_SPECS_MAPPED(:,:,:, 83) = CONC_GEOS(:,:,:, 96)

! [ HGIIAER ] (CMAQ 84) = [ HgP  ] (GC 3)
CONC_GEOS_SPECS_MAPPED(:,:,:, 84) = CONC_GEOS(:,:,:, 97)

!===================================================================================================
! Convert unit of the ASO4J, ANH4J and ANO3J from v/v to microgram/m**3
! Other variables's units are converted from v/v to ppmv
!
! We have a magic number here, i.e., 3,  the number of the variables using microgram/m**3.
! Be sure to modify accordingly when having different output vars and different orders
! Note The data read from GEOSCHEM bpch file is v/v, so the ppmv should be applied below

DO I = 1, NI
  DO J = 1, NJ
    DO L = 1, NL

! Dealing with negative derived values, assign 10ppbv , i.e. 1e-8 v/v ., before unit convert to ppmv
      DO SPC = 1, NVARS
        IF( CONC_GEOS_SPECS_MAPPED(I,J,L,SPC) .LT. 0 ) THEN
          WRITE(*,*) 'Negative Values found '
          WRITE(*,*) 'In GEOSCHEM Spc Mapped: I, J, L, SPC and conc mapped is '
          WRITE(*,*) I, J, L, SPC, CONC_GEOS_SPECS_MAPPED(I,J,L,SPC)
          WRITE(*,*) 'Assign 10ppbV to the grid cell before converting to ppmV.'
          CONC_GEOS_SPECS_MAPPED(I,J,L,SPC) = 1.0E-8
        END IF
      ENDDO

! Unit conversion , v/v to ppm(1.E+6), multiply by 1.E+6
      DO SPC = 1, NVARS
        CONC_GEOS_SPECS_MAPPED(I,J,L,SPC) = CONC_GEOS_SPECS_MAPPED(I,J,L,SPC)*1.E+6
      ENDDO
    ENDDO
  ENDDO
ENDDO

STATUS = .TRUE.

! CYZ
! print *, 'TEST 3: ',CONC_GEOS_SPECS_MAPPED(31,65,1,29)
! CYZ

END SUBROUTINE SPECIES_MAPPING_OUTUNIT_PPMV
