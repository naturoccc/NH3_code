MODULE WRSNH3_MOD

INTEGER, PUBLIC               :: fid_49
INTEGER, PUBLIC               :: fid_3
CHARACTER(LEN=255), PUBLIC    :: AD32NC_FILENAME
CHARACTER(LEN=255), PUBLIC    :: ND49NC_FILENAME


!-------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! !DESCRIPTION: Subroutine DIAG3 prints out diagnostics to the BINARY PUNCH
!  format file.

SUBROUTINE DIAG3NC

  USE DIAG_MOD,     ONLY : AD32_fe,             AD32_so
  USE TIME_MOD,     ONLY : GET_NYMDb,           GET_NHMSb

! !DESCRIPTION: Function GET\_NYMDb returns the NYMDb value (YYYYMMDD at the 
!  beginning of the run).
! !DESCRIPTION: Function GET\_NHMSb returns the NHMSb value (HHMMSS at the 
!  beginning of the run) to the calling program. (bmy, 3/21/03)

  USE TIME_MOD,     ONLY : ITS_TIME_FOR_EXIT,   EXPAND_DATE, GET_CT_EMIS

! !DESCRIPTION: Function ITS\_TIME\_FOR\_EXIT returns TRUE if it is the end of
!  the GEOS-Chem simulation (i.e. TAU >= TAUe), or FALSE otherwise.
! !DESCRIPTION: Subroutine EXPAND\_DATE replaces "YYYYMMDD" and "hhmmss" 
!  tokens within a filename string with the actual values.
! !DESCRIPTION: Function GET\_CT\_CHEM returns the emissions timestep counter
!  to the calling program.

  USE NETCDF_MOD

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'
# include "CMN_SIZE"      ! Size parameters

  INTEGER                      :: it
  INTEGER                      :: rcode
  CHARACTER(LEN=255)           :: AD32NC_OUTPUT_FILE,  AD32NC_OUTPUT_DIR
  LOGICAL, SAVE                :: FIRST   = .TRUE.
  INTEGER, SAVE                :: N_diag3 = 0
  REAL*8                       :: SCALESRCE
  REAL*4                       :: ARRAY(IIPAR,JJPAR)


!  AD32NC_OUTPUT_FILE = './output/ctm.YYYYMMDD.nc'
  AD32NC_OUTPUT_DIR = './output/'

  !=================================================================
  ! Open a new netCDF file (first pass only)
  !=================================================================
  
  ! First-time only initialization
  IF ( FIRST ) THEN
         
    ! Reset First-time flag
    FIRST = .FALSE.

    AD32NC_OUTPUT_FILE = 'SNH3_cn36_ctm_' // 'YYYYMMDD'

    ! Expand date tokens in the file name
    CALL EXPAND_DATE( AD32NC_OUTPUT_FILE, GET_NYMDb(), GET_NHMSb() )
    AD32NC_FILENAME = TRIM( AD32NC_OUTPUT_DIR ) // TRIM( AD32NC_OUTPUT_FILE )

    ! Echo info
    WRITE( 6, 150 ) TRIM( AD32NC_FILENAME )
150 FORMAT( '     - DIAG3: Opening file : ', a )
 
 
!  netcdf.inc
!  integer         nf_create
! !                         (character*(*)       path,
! !                          integer             cmode,
! !                          integer             ncid)

    rcode = nf_create(AD32NC_FILENAME,NF_CLOBBER, fid_3) 

!  netcdf.inc
! !     no error 
!       integer nf_noerr
!       parameter (nf_noerr = 0)
!       parameter(ncnoerr = nf_noerr)


!       integer nf_clobber
! !       parameter (nf_clobber = 0)
! !       parameter(ncclob = nf_clobber)

    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,*) 'error in 2d_var_real write, nf_create'
    ENDIF

!netcdf_mod90
!Subroutine def_diag3_cdf

    CALL def_diag3_cdf (fid_3, IIPAR, JJPAR, rcode)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,*) 'error in 2d_var_real write, def_2d_cdf'
    ENDIF

  ENDIF

  !=================================================================
  ! Save this data block to the file
  !=================================================================

  ! Echo output
  WRITE( 6, '(a)' ) '     - DIAG3: Diagnostics written to bpch!'

  N_diag3 = N_diag3 + 1
  it = N_diag3

  SCALESRCE  = DBLE( GET_CT_EMIS() ) + 1d-32

  ARRAY(:,:) = AD32_fe(:,:) / SCALESRCE
  CALL put_var_2d_real_cdf (fid_3, 'NH3_FERT', ARRAY, IIPAR, JJPAR, it, rcode)
  IF ( rcode /= nf_noerr ) THEN
    WRITE (6,*) 'error in 2d_var_real write, put_var_2d_real_cdf'
  ENDIF

  ARRAY(:,:) = AD32_so(:,:) / SCALESRCE
  CALL put_var_2d_real_cdf (fid_3, 'NH3_SOIL', ARRAY, IIPAR, JJPAR, it, rcode)
  IF ( rcode /= nf_noerr ) THEN
    WRITE (6,*) 'error in 2d_var_real write, put_var_2d_real_cdf'
  ENDIF

  !=================================================================
  ! If it is time for exit, close the file
  !=================================================================

  IF ( ITS_TIME_FOR_EXIT() ) THEN

    ! Echo info
    WRITE( 6, 160 ) TRIM( AD32NC_FILENAME )
160 FORMAT( '     - DIAG3: Closing file : ', a )

    rcode = nf_close (fid_3)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,*) 'error in 2d_var_real write, nf_close'
    ENDIF

  ENDIF

END SUBROUTINE DIAG3NC

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! !MODULE: diag49_mod
!
! !DESCRIPTION: Module DIAG49\_MOD contains variables and routines to save 
!  out 3-D instantaneous timeseries output to disk.

SUBROUTINE DIAG49NC

  USE NETCDF_MOD
  USE TIME_MOD,     ONLY : GET_NYMD,           GET_NHMS
  USE TIME_MOD,     ONLY : YMD_EXTRACT
  USE TIME_MOD,     ONLY : GET_NYMD_DIAG
  USE TIME_MOD,     ONLY : EXPAND_DATE,        TIMESTAMP_STRING
  USE TIME_MOD,     ONLY : GET_DAY_OF_YEAR

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'
# include "CMN_SIZE"      ! Size parameters
# include "commsoil.h"    ! SOILNH3

  INTEGER                      :: it
  INTEGER                      :: rcode
  INTEGER                      :: NYMD, NHMS, TS_DIAG
  INTEGER                      :: HH, MM, SS
  CHARACTER(LEN=255)           :: ND49NC_OUTPUT_FILE,  ND49NC_OUTPUT_DIR
  CHARACTER(LEN=16)            :: STAMP
  INTEGER, SAVE                :: N_diag49
  INTEGER                      :: DAY_OF_YEAR
  CHARACTER(LEN=3)             :: DOY


  REAL                         :: SNH3         ( IIPAR, JJPAR )
!###Hong Start
  SNH3(:,:) = INST_SOIL(:,:) + INST_FERT(:,:)
!###Hong End
!  ND49NC_OUTPUT_FILE = './output/tsYYYYMMDD.nc'
  ND49NC_OUTPUT_DIR = './output/'

! added by hong
#if   defined( GEOS_5 )
      TS_DIAG = 30000
#endif

#if   defined( WRF )
      TS_DIAG = 10000
#endif


  !=================================================================
  ! If it's a new day, open a new netCDF file and write file header
  !=================================================================

  NHMS = GET_NHMS()

  IF ( NHMS == TS_DIAG ) THEN

    ! Initialize N_diag49 counters
    N_diag49 = 0  


    DAY_OF_YEAR   = GET_DAY_OF_YEAR()
    WRITE( DOY , '(I3)' ) DAY_OF_YEAR

    ND49NC_OUTPUT_FILE = 'SNH3_cn36_' // 'YYYY' // DOY

    ! Expand date tokens in the file name
    CALL EXPAND_DATE( ND49NC_OUTPUT_FILE, GET_NYMD(), GET_NHMS() )
    ND49NC_FILENAME = TRIM( ND49NC_OUTPUT_DIR ) // TRIM( ND49NC_OUTPUT_FILE )

    ! Echo info
    WRITE( 6, 100 ) TRIM( ND49NC_FILENAME )
100 FORMAT( '     - DIAG49: Opening file ', a )

    rcode = nf_create(ND49NC_FILENAME, NF_CLOBBER, fid_49)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,*) 'error in 2d_var_real write, nf_create'
    ENDIF

    CALL def_diag49_cdf (fid_49, IIPAR, JJPAR, rcode)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,*) 'error in 2d_var_real write, def_2d_cdf'
    ENDIF

  ENDIF

  !=================================================================
  ! Save this data block to the ND49 timeseries file
  !=================================================================

  ! Split NHMS into hours, mins, seconds
!  CALL YMD_EXTRACT( NHMS, HH, MM, SS )
!  it = HH + 1

  N_diag49 = N_diag49 + 1
  it = N_diag49

    ! Echo info
    STAMP = TIMESTAMP_STRING()
    WRITE( 6, 110 ) STAMP
110 FORMAT( '     - DIAG49: Saving timeseries at ', a )

  CALL put_var_2d_real_cdf (fid_49, 'NH3_SOIL', SNH3, IIPAR, JJPAR, it, rcode)
  IF ( rcode /= nf_noerr ) THEN
    WRITE (6,*) 'error in 2d_var_real write, put_var_2d_real_cdf'
  ENDIF

  !=================================================================
  ! Close the file at the proper time
  !=================================================================

  IF ( ITS_TIME_TO_CLOSE_FILE() ) THEN

    ! Echo info
    WRITE( 6, 120 ) TRIM( ND49NC_FILENAME )
120 FORMAT( '     - DIAG49: Closing file : ', a )

    rcode = nf_close (fid_49)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,*) 'error in 2d_var_real write, nf_close'
    ENDIF
  ENDIF

END SUBROUTINE DIAG49NC

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
 
     FUNCTION ITS_TIME_TO_CLOSE_FILE() RESULT( ITS_TIME )
!
! !USES:
      USE TIME_MOD, ONLY : GET_HOUR
      USE TIME_MOD, ONLY : GET_MINUTE

! !RETURN VALUE:
      LOGICAL :: ITS_TIME

! !RETURN VALUE:
      REAL*8 :: HR1

      ! Current hour
      HR1      = GET_HOUR() + ( GET_MINUTE() / 60d0 )

      ITS_TIME = ( INT( HR1 ) == 00 )

      END FUNCTION ITS_TIME_TO_CLOSE_FILE

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

END MODULE WRSNH3_MOD
