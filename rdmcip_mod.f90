!------------------------------------------------------------------------------
! MODULE RDMCIP_MOD
!
! DESCRIPTION: get met fields from netcdf in directionary WRF
!! OUTPUT:
!       TS = TEMP2  'TEMPERATURE AT 2M'
!       GWETTOP = SOIM1  'VOLUMETRIC SOIL MOISTURE IN TOP CM'
!       SFCWINDSQR = WSPD10  'WIND SPEED AT 10M'
!       CLDFRC = CFRAC  'TOTAL CLOUD FRACTION' 
!       RADSWG = RGRND  'SOLAR RAD REACHING SFC'
!       PRECON = RC  'CONVECTIVE PCPN PER MET TSTEP'
!       PRELSC = RN  'NONCONVEC. PCPN PER MET TSTEP'
!       SNOCOV = SNOCOV 'SNOW COVER(1=YES, 0=NO)'
!-------------------------------------------------------------------------------
       
MODULE RDMCIP_MOD

  INTEGER, PUBLIC       :: fid_a1

!-------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE RDMCIP( NYMD, NHMS )

  USE DIRECTORY_MOD,  ONLY : DATA_DIR
  USE DAO_MOD,        ONLY : CLDFRC, GWETTOP, RADSWG, SFCWINDSQR, TS, SNOCOV
  USE DAO_MOD,        ONLY : PREACC, PRECON, PRELSC
  USE TIME_MOD,       ONLY : EXPAND_DATE
  USE TIME_MOD,       ONLY : GET_DAY_OF_YEAR,  TIMESTAMP_STRING
  USE TIME_MOD,       ONLY : YMD_EXTRACT,      ITS_A_NEW_DAY
  USE NETCDF_MOD


  IMPLICIT NONE

  INCLUDE 'netcdf.inc'
# include "CMN_SIZE"      ! Size parameters

! !INPUT PARAMETERS: 
!
  INTEGER, INTENT(IN) :: NYMD   ! YYYY/MM/DD
  INTEGER, INTENT(IN) :: NHMS   !  and hh:mm:ss of desired data
!
! !LOCAL VARIABLES:
!
  INTEGER                      :: it
  INTEGER                      :: rcode
  CHARACTER(LEN=255)           :: FILENAME
  REAL, SAVE, ALLOCATABLE      :: dum2d      ( : , : )
  INTEGER, SAVE, ALLOCATABLE   :: dum2d_i    ( : , : )
  INTEGER                      :: I,  J

  CHARACTER(LEN=255)           :: WRF_DIR
  CHARACTER(LEN=255)           :: MC2_STR
  INTEGER                      :: DAY_OF_YEAR
  CHARACTER(LEN=3)             :: DOY
  INTEGER                      :: HH, MM, SS
  CHARACTER(LEN=16)            :: STAMP


  IF ( .NOT. ALLOCATED ( dum2d   ) )  &
    ALLOCATE ( dum2d   ( IIPAR, JJPAR )  )  ! 2D array on cross points
  IF ( .NOT. ALLOCATED ( dum2d_i ) )  &
    ALLOCATE ( dum2d_i ( IIPAR, JJPAR )  )  ! 2D integer array on cross points

  !=================================================================
  ! If it's a new day, open a new met fields file
  !=================================================================

  IF ( ITS_A_NEW_DAY() ) THEN 

    DAY_OF_YEAR   = GET_DAY_OF_YEAR()
    WRITE( DOY , '(I3.3)' ) DAY_OF_YEAR

    ! Strings for directory & filename
    WRF_DIR = 'WRF/YYYY/'
    MC2_STR = 'METCRO2D_cn36_' // 'YYYY' // DOY

    ! Replace date tokens
    CALL EXPAND_DATE( WRF_DIR, NYMD, 000000 )
    CALL EXPAND_DATE( MC2_STR, NYMD, 000000 )

    FILENAME = TRIM( DATA_DIR ) // TRIM( WRF_DIR ) // TRIM( MC2_STR )

    ! Echo info
    WRITE( 6, 100 ) TRIM( FILENAME )
100 FORMAT( '     - Opening: ', a )

    rcode = nf_open ( FILENAME , nf_nowrite, fid_a1)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,*) 'error in 2d_var_real read, nf_open'    
    ENDIF

  ENDIF

  !=================================================================
  ! Read the met fields data
  !=================================================================

  ! Split NHMS into hours, mins, seconds
  CALL YMD_EXTRACT( NHMS, HH, MM, SS )
  it = HH + 1


  CALL get_var_2d_real_cdf (fid_a1, 'TEMP2' , dum2d, IIPAR, JJPAR, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    TS(:,:) = dum2d(:,:)
  ELSE
    WRITE (6,*) 'error in 2d_var_real read, get_var_2d_real_cdf'
  ENDIF

  CALL get_var_2d_real_cdf (fid_a1, 'SOIM1' , dum2d, IIPAR, JJPAR, it, rcode)
  GWETTOP(:,:) = dum2d(:,:)
  ! Convert GWETTOP from volumetric soil moisture content(v/v) to WFPS
  ! WFPS(water filled pore space) [unitless]
  ! is the ratio of the volumetric soil moisture content to the porosity
  ! Soil porosity = 1 - soil bulk density / soil particle density
  ! Assume soil bulk density is 1.4 Mg/m3, soil particle density is 2.65 Mg/m3

  GWETTOP(:,:) = GWETTOP(:,:) * 2
!  GWETTOP(:,:) = GWETTOP(:,:) / ( 1d0 - 1.4d0 / 2.65d0 )
  DO J = 1, JJPAR
  DO I = 1, IIPAR
    IF ( GWETTOP(I,J) > 1 ) THEN
      GWETTOP(I,J) = 1
    ENDIF
  ENDDO
  ENDDO

  CALL get_var_2d_real_cdf (fid_a1, 'WSPD10', dum2d, IIPAR, JJPAR, it, rcode)
  SFCWINDSQR(:,:) = dum2d(:,:)

  CALL get_var_2d_real_cdf (fid_a1, 'CFRAC' , dum2d, IIPAR, JJPAR, it, rcode)
  CLDFRC(:,:) = dum2d(:,:)

  CALL get_var_2d_real_cdf (fid_a1, 'RGRND' , dum2d, IIPAR, JJPAR, it, rcode)
  RADSWG(:,:) = dum2d(:,:)

  CALL get_var_2d_real_cdf (fid_a1, 'RC'    , dum2d, IIPAR, JJPAR, it, rcode)
  PRECON(:,:) = dum2d(:,:)
  ! Convert PRECON from [cm/(1-hr met timestep)] to [mm/day]
  PRECON(:,:) = PRECON(:,:) * 10d0 * 24d0

  CALL get_var_2d_real_cdf (fid_a1, 'RN'    , dum2d, IIPAR, JJPAR, it, rcode)
  PRELSC(:,:) = dum2d(:,:)
  ! Convert PRELSC from [cm/(1-hr met timestep)] to [mm/day]
  PRELSC(:,:) = PRELSC(:,:) * 10d0 * 24d0

  PREACC(:,:) = PRECON(:,:) + PRELSC(:,:)

  CALL get_var_2d_int_cdf  (fid_a1, 'SNOCOV', dum2d_i, IIPAR, JJPAR, it, rcode)
  SNOCOV(:,:) = dum2d_i(:,:)

    ! Echo info
    STAMP = TIMESTAMP_STRING( NYMD, NHMS )
    WRITE( 6, 210 ) STAMP
210 FORMAT( '     - Found all met fields for ', a )

  !=================================================================
  ! Close the file at the proper time
  !=================================================================

  IF ( ITS_TIME_TO_CLOSE_FILE() ) THEN
    rcode = nf_close (fid_a1)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,*) 'error in 2d_var_real read, nf_close'
    ENDIF
  ENDIF

END SUBROUTINE RDMCIP

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

      ITS_TIME = ( INT( HR1 ) == 23 )

      END FUNCTION ITS_TIME_TO_CLOSE_FILE

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

END MODULE RDMCIP_MOD
