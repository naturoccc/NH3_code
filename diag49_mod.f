!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: diag49_mod
!
! !DESCRIPTION: Module DIAG49\_MOD contains variables and routines to save 
!  out 3-D instantaneous timeseries output to disk.
!\\
!\\
! !INTERFACE: 
!
      MODULE DIAG49_MOD
!
! !USES:
!
      IMPLICIT NONE
      PRIVATE
!
! !PUBLIC DATA MEMBERS:
!
      LOGICAL, PUBLIC :: DO_SAVE_DIAG49
!
! !PUBLIC MEMBER FUNCTIONS:
! 
      PUBLIC  :: DIAG49
      PUBLIC  :: ITS_TIME_FOR_DIAG49
      PUBLIC  :: INIT_DIAG49
!
! !PRIVATE MEMBER FUNCTIONS:
! 
      PRIVATE :: ITS_TIME_TO_CLOSE_FILE
      PRIVATE :: GET_I
!
! !REMARKS:
!  ND49 tracer numbers:
!  ============================================================================
!  1 - N_TRACERS : GEOS-CHEM transported tracers            [v/v        ]
!
!
! !PRIVATE TYPES:
!
      !=================================================================
      ! MODULE VARIABLES
      !
      ! I0               : Offset between global & nested grid
      ! J0               : Offset between global & nested grid
      ! IOFF             : Longitude offset
      ! JOFF             : Latitude offset
      ! LOFF             : Altitude offset
      ! ND49_IMIN        : Minimum latitude  index for DIAG51 region
      ! ND49_IMAX        : Maximum latitude  index for DIAG51 region
      ! ND49_JMIN        : Minimum longitude index for DIAG51 region
      ! ND49_JMAX        : Maximum longitude index for DIAG51 region
      ! ND49_LMIN        : Minimum altitude  index for DIAG51 region
      ! ND49_LMAX        : Minimum latitude  index for DIAG51 region
      ! ND49_NI          : Number of longitudes in DIAG51 region 
      ! ND49_NJ          : Number of latitudes  in DIAG51 region
      ! ND49_NL          : Number of levels     in DIAG51 region
      ! ND49_N_TRACERS   : Number of tracers for DIAG51
      ! ND49_OUTPUT_FILE : Name of bpch file w  timeseries data
      ! ND49_TRACERS     : Array of DIAG51 tracer numbers
      ! HALFPOLAR        : Used for bpch file output
      ! CENTER180        : Used for bpch file output
      ! LONRES           : Used for bpch file output
      ! LATRES           : Used for bpch file output
      ! MODELNAME        : Used for bpch file output
      ! RESERVED         : Used for bpch file output
      !=================================================================

      INTEGER            :: IOFF,           JOFF,   LOFF
      INTEGER            :: I0,             J0
      INTEGER            :: ND49_N_TRACERS, ND49_TRACERS(120)
      INTEGER            :: ND49_IMIN,      ND49_IMAX
      INTEGER            :: ND49_JMIN,      ND49_JMAX
      INTEGER            :: ND49_LMIN,      ND49_LMAX
      INTEGER            :: ND49_FREQ,      ND49_NI
      INTEGER            :: ND49_NJ,        ND49_NL
      INTEGER            :: HALFPOLAR
      INTEGER, PARAMETER :: CENTER180=1 
      REAL*4             :: LONRES,         LATRES
      CHARACTER(LEN=20)  :: MODELNAME
      CHARACTER(LEN=40)  :: RESERVED = ''
      CHARACTER(LEN=80)  :: TITLE
      CHARACTER(LEN=255) :: ND49_OUTPUT_FILE

      CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: diag49 
!
! !DESCRIPTION: Subroutine DIAG49 produces time series (instantaneous fields) 
!  for a geographical domain from the information read in timeseries.dat.  
!  Output will be in binary punch (BPCH) format.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE DIAG49
!
! !USES:
!
      USE BPCH2_MOD,    ONLY : BPCH2,   OPEN_BPCH2_FOR_WRITE
      USE FILE_MOD,     ONLY : IU_ND49
      USE GRID_MOD,     ONLY : GET_XOFFSET,        GET_YOFFSET
      USE TIME_MOD,     ONLY : EXPAND_DATE
      USE TIME_MOD,     ONLY : GET_NYMD,           GET_NHMS
      USE TIME_MOD,     ONLY : GET_NYMD_DIAG,      GET_TS_DIAG
      USE TIME_MOD,     ONLY : GET_TAU,            GET_HOUR
      USE TIME_MOD,     ONLY : ITS_A_NEW_DAY,      TIMESTAMP_STRING
!     USE LOGICAL_MOD,  ONLY : LSOILNOX, LFERTILIZERNOX
      USE LOGICAL_MOD,  ONLY : LSOILNH3, LFERTILIZERNH3

#     include "CMN_SIZE"        ! Size parameters
#     include "commsoil.h"      ! SOILNH3
!#    include "commsoil.h"      ! SOILNOX
!
! !REVISION HISTORY: 
!  09 Apr 1999 - I. Bey, R. Martin, R. Yantosca - Initial version
!  (1 ) Now bundled into "diag49_mod.f".  Now reference STT from 
!        "tracer_mod.f".  Now scale aerosol & dust OD's to 400 nm.  
!        (bmy, rvm, aad, 7/9/04)
!  (2 ) Updated tracer # for NO2 (bmy, 10/25/04)
!  (3 ) Remove reference to "CMN".  Also now get PBL heights in meters and 
!        model layers from GET_PBL_TOP_m and GET_PBL_TOP_L of "pbl_mix_mod.f".
!        (bmy, 2/16/05)
!  (4 ) Now reference CLDF and BXHEIGHT from "dao_mod.f".  Now save 3-D cloud 
!        fraction as tracer #79 and box height as tracer #93.  Now remove 
!        reference to PBL from "dao_mod.f"(bmy, 4/20/05)
!  (5 ) Remove references to TRCOFFSET because it is always zero (bmy, 6/24/05)
!  (6 ) Now do not save SLP data if it is not allocated (bmy, 8/2/05)
!  (7 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  (8 ) Now references XNUMOLAIR from "tracer_mod.f".  Bug fix: now must sum
!        aerosol OD's over all RH bins.  Also zero Q array. (bmy, 11/1/05)
!  (9 ) Bug fix: accumulate into Q(X,Y,K) for dust OD (qli, bmy, 4/30/07)
!  (10) Bug fix: UNIT should be "levels" for tracer 77.  Also RH should be
!        tracer #17 under "TIME-SER" category. (cdh, bmy, 2/11/08)
!  (11) Bug fix: replace "PS-PTOP" with "PEDGE-$" (bmy, phs, 10/7/08)
!  (12) Change the new day condition to open a new file. (ccc, 8/12/09)
!  (13) Change the timestamp for the filename when closing (ccc, 8/12/09)
!  (14) Add outputs for EMISS_BVOC (10 tracers), TS, PARDR, PARDF and ISOLAI
!        (mpb, 11/19/09)
!  02 Dec 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL, SAVE            :: FIRST  = .TRUE.
!     LOGICAL, SAVE            :: IS_FULLCHEM, IS_NOx,     IS_Ox 
      LOGICAL, SAVE            :: IS_FULLCHEM, IS_NH3,     IS_Ox 
      LOGICAL, SAVE            :: IS_NOy,      IS_CLDTOPS, IS_OPTD
      LOGICAL, SAVE            :: IS_SEASALT,  IS_SLP
      INTEGER                  :: IOS,  GMTRC, GMNL, I, J, K, L 
      INTEGER                  :: N,    R,     H,    W, X, Y
      INTEGER                  :: NHMS, TS_DIAG
      REAL*8                   :: TAU, TMP,   SCALEAODnm
      REAL*8                   :: Q( ND49_NI, ND49_NJ, ND49_NL )
      CHARACTER(LEN=16)        :: STAMP
      CHARACTER(LEN=40)        :: CATEGORY
      CHARACTER(LEN=40)        :: UNIT
      CHARACTER(LEN=255)       :: FILENAME

      ! Aerosol types (rvm, aad, bmy, 7/20/04)
      INTEGER                  :: IND(6) = (/ 22, 29, 36, 43, 50, 15 /)

      !=================================================================
      ! DIAG49 begins here!
      !=================================================================

      ! Set logical flags on first timestep
      IF ( FIRST ) THEN
         FIRST       = .FALSE.
      ENDIF

      !=================================================================
      ! If it's a new day, open a new BPCH file and write file header
      ! We need to check if it's a new day + 1 ND49 time step (ccc, 8/12/09)
      !=================================================================
!--- Previous to (ccc, 8/12/09)
!      IF ( ITS_A_NEW_DAY() ) THEN
      NHMS    = GET_NHMS()
      TS_DIAG = ND49_FREQ

      ! To change TS_DIAG to NHMS format
      TS_DIAG = TS_DIAG/60 * 10000 + (TS_DIAG - (TS_DIAG/60)*60) * 100  

      IF ( NHMS == TS_DIAG ) THEN     ! It's a new day for diagnostics.

         ! Expand date tokens in the file name
         FILENAME = TRIM( ND49_OUTPUT_FILE )
         CALL EXPAND_DATE( FILENAME, GET_NYMD(), GET_NHMS() )

         ! Echo info
         WRITE( 6, 100 ) TRIM( FILENAME )
 100     FORMAT( '     - DIAG49: Opening file ', a )
        
         ! Open bpch file and write top-of-file header
         CALL OPEN_BPCH2_FOR_WRITE( IU_ND49, FILENAME, TITLE )
      ENDIF

      !=================================================================
      ! Save tracers to timeseries file
      !=================================================================

      ! Echo info
      STAMP = TIMESTAMP_STRING()
      WRITE( 6, 110 ) STAMP
 110  FORMAT( '     - DIAG49: Saving timeseries at ', a )

      ! Time for BPCH file
      TAU  = GET_TAU()

         ! Zero summing array
         Q = 0d0

         ! Test by tracer number

!	modified by chenchuchu  2013/5/24

!        IF ( LSOILNOX .and. LFERTILIZERNOX ) THEN
            !-------------------------------------
            ! SOILNOX  [molec NOx/cm2/s]
            !-------------------------------------
!           CATEGORY = 'NOX-SOIL'
!           UNIT     = 'molec/cm2/s'
!           GMNL     = 1
!           GMTRC    = 1

      WRITE(6,*) LSOILNH3, LFERTILIZERNH3

      IF ( LSOILNH3 .and. LFERTILIZERNH3 ) THEN
            !-------------------------------------
            ! SOILNH3  [molec NH3/cm2/s]
            !-------------------------------------
           CATEGORY = 'NH3-SOIL'
           UNIT     = 'molec/cm2/s'
           GMNL     = 1
           GMTRC    = 1
!$OMP PARALLEL DO
!$OMP+DEFAULT( SHARED )
!$OMP+PRIVATE( I, J, L, X, Y, K )


            DO K = 1, ND49_NL
               L = LOFF + K
            DO Y = 1, ND49_NJ
               J = JOFF + Y
            DO X = 1, ND49_NI
               I = GET_I( X )
!###Hong Start
               Q(X,Y,K) = INST_SOIL(I,J) + INST_FERT(I,J)
!###Hong End
            ENDDO
            ENDDO
            ENDDO
!$OMP END PARALLEL DO

         !==============================================================
         ! Save this data block to the ND49 timeseries file
         !==============================================================
         CALL BPCH2( IU_ND49,      MODELNAME,    LONRES,   
     &               LATRES,       HALFPOLAR,    CENTER180, 
     &               CATEGORY,     GMTRC,        UNIT,      
     &               TAU,          TAU,          RESERVED,  
     &               ND49_NI,      ND49_NJ,      GMNL,  
     &               ND49_IMIN+I0, ND49_JMIN+J0, ND49_LMIN, 
     &               REAL( Q(1:ND49_NI, 1:ND49_NJ, 1:GMNL) ) )

         ENDIF

            
      !=================================================================
      ! Close the file at the proper time
      !=================================================================
      IF ( ITS_TIME_TO_CLOSE_FILE() ) THEN

         ! Expand date tokens in the file name
         FILENAME = TRIM( ND49_OUTPUT_FILE )
!--- Previous to (ccc, 8/12/09)
!         CALL EXPAND_DATE( FILENAME, GET_NYMD(), GET_NHMS() )
         CALL EXPAND_DATE( FILENAME, GET_NYMD_DIAG(), GET_NHMS() )

         ! Echo info
         WRITE( 6, 120 ) TRIM( FILENAME )
 120     FORMAT( '     - DIAG49: Closing file : ', a )

         ! Close file
         CLOSE( IU_ND49 ) 
      ENDIF

      END SUBROUTINE DIAG49
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: its_time_to_close_file
!
! !DESCRIPTION: Function ITS\_TIME\_TO\_CLOSE\_FILE returns TRUE if it's 
!  time to close the ND49 bpch file before the end of the day.
!\\
!\\
! !INTERFACE:
!
      FUNCTION ITS_TIME_TO_CLOSE_FILE() RESULT( ITS_TIME )
!
! !USES:
!
      USE TIME_MOD, ONLY : GET_HOUR
      USE TIME_MOD, ONLY : GET_MINUTE
!
! !RETURN VALUE:
!
      LOGICAL :: ITS_TIME
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) The time is already updated to the next time step (ccc, 8/12/09)
!  02 Dec 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      REAL*8 :: HR1

      !=================================================================
      ! ITS_TIME_TO_CLOSE_FILE begins here!
      !=================================================================

      ! Current hour
      HR1      = GET_HOUR() + ( GET_MINUTE() / 60d0 )

!--- Previous to (ccc, 8/12/09)
!      ! Hour at the next dynamic timestep
!      HR2      = HR1        + ( ND49_FREQ / 60d0 )

      ! If the next dyn step is the start of a new day, return TRUE
!--- Previous to (ccc, 11/11/10)
!       HR1 varies between 00 and 23:59. So compares to 00 not 24 anymore.
!      ITS_TIME = ( INT( HR1 ) == 24 )

      ITS_TIME = ( INT( HR1 ) == 00 )

      END FUNCTION ITS_TIME_TO_CLOSE_FILE
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: its_time_for_diag49
!
! !DESCRIPTION: Function ITS\_TIME\_FOR\_DIAG49 returns TRUE if ND49 is 
!  turned on and it is time to call DIAG49 -- or FALSE otherwise.
!\\
!\\
! !INTERFACE:
!
      FUNCTION ITS_TIME_FOR_DIAG49() RESULT( ITS_TIME )
!
! !USES:
!
      USE TIME_MOD,  ONLY : GET_ELAPSED_MIN
      USE TIME_MOD,  ONLY : GET_TS_DIAG
      USE ERROR_MOD, ONLY : GEOS_CHEM_STOP
!
! !RETURN VALUE:
!
      LOGICAL :: ITS_TIME
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Add a check on the output frequency for validity compared to time 
!        steps used. (ccc, 5/21/09)
!  02 Dec 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER       :: XMIN, TS_DIAG
      LOGICAL, SAVE :: FIRST = .TRUE.

      !=================================================================
      ! ITS_TIME_FOR_DIAG49 begins here!
      !=================================================================

      IF ( DO_SAVE_DIAG49 ) THEN
         IF ( FIRST ) THEN
            TS_DIAG = GET_TS_DIAG()
            
            ! Check if ND49_FREQ is a multiple of TS_DIAG
            IF ( MOD( ND49_FREQ, TS_DIAG ) /= 0 ) THEN
               WRITE( 6, 100 ) 'ND49', ND49_FREQ, TS_DIAG
 100           FORMAT( 'The ',a,' output frequency must be a multiple '
     &              'of the largest time step:', i5, i5 )
               CALL GEOS_CHEM_STOP
            ENDIF
            FIRST = .FALSE.
         ENDIF
         
         ! Time already elapsed in this run
         XMIN     = GET_ELAPSED_MIN()
         
         ! Is the elapsed time a multiple of ND49_FREQ?
         ITS_TIME = ( DO_SAVE_DIAG49 .and. MOD( XMIN, ND49_FREQ ) == 0 )
      ELSE
         ITS_TIME = DO_SAVE_DIAG49
      ENDIF
            
      END FUNCTION ITS_TIME_FOR_DIAG49
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_i
!
! !DESCRIPTION: Function GET\_I returns the absolute longitude index (I), 
!  given the relative longitude index (X).
!\\
!\\
! !INTERFACE:
!
      FUNCTION GET_I( X ) RESULT( I )
!
! !USES:
!
#     include "CMN_SIZE"         ! Size parameters
!
! !INPUT PARAMETERS: 
!
      INTEGER, INTENT(IN) :: X   ! Relative longitude index (used by Q array)
!
! !RETURN VALUE:
!
      INTEGER             :: I   ! Absolute longitude index
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  02 Dec 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
      !=================================================================
      ! GET_I begins here!
      !=================================================================

      ! Add the offset to X to get I  
      I = IOFF + X

      ! Handle wrapping around the date line, if necessary
      IF ( I > IIPAR ) I = I - IIPAR

      END FUNCTION GET_I
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_diag49
!
! !DESCRIPTION: Subroutine INIT\_DIAG49 allocates and zeroes all module 
!  arrays.  It also gets values for module variables from "input\_mod.f". 
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE INIT_DIAG49( DO_ND49, N_ND49, TRACERS, IMIN,    
     &                        IMAX,    JMIN,   JMAX,    LMIN,    
     &                        LMAX,    FREQ,   FILE )
!
! !USES:
!
      USE BPCH2_MOD, ONLY : GET_MODELNAME
      USE BPCH2_MOD, ONLY : GET_HALFPOLAR
      USE GRID_MOD,  ONLY : GET_XOFFSET
      USE GRID_MOD,  ONLY : GET_YOFFSET
      USE GRID_MOD,  ONLY : ITS_A_NESTED_GRID
      USE ERROR_MOD, ONLY : ERROR_STOP

#     include "CMN_SIZE" ! Size parameters
!
! !INPUT PARAMETERS: 
!
      ! DO_ND49 : Switch to turn on ND49 timeseries diagnostic
      ! N_ND50  : Number of ND49 read by "input_mod.f"
      ! TRACERS : Array w/ ND49 tracer #'s read by "input_mod.f"
      ! IMIN    : Min longitude index read by "input_mod.f"
      ! IMAX    : Max longitude index read by "input_mod.f" 
      ! JMIN    : Min latitude index read by "input_mod.f" 
      ! JMAX    : Min latitude index read by "input_mod.f" 
      ! LMIN    : Min level index read by "input_mod.f" 
      ! LMAX    : Min level index read by "input_mod.f" 
      ! FREQ    : Frequency for saving to disk [min]
      ! FILE    : ND49 output file name read by "input_mod.f"
      LOGICAL,            INTENT(IN) :: DO_ND49
      INTEGER,            INTENT(IN) :: N_ND49, TRACERS(100)
      INTEGER,            INTENT(IN) :: IMIN,   IMAX 
      INTEGER,            INTENT(IN) :: JMIN,   JMAX      
      INTEGER,            INTENT(IN) :: LMIN,   LMAX 
      INTEGER,            INTENT(IN) :: FREQ
      CHARACTER(LEN=255), INTENT(IN) :: FILE
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Now get I0 and J0 correctly for nested grid simulations (bmy, 11/9/04)
!  (2 ) Now call GET_HALFPOLAR from "bpch2_mod.f" to get the HALFPOLAR flag 
!        value for GEOS or GCAP grids. (bmy, 6/28/05)
!  (3 ) Now allow ND49_IMIN to be equal to ND49_IMAX and ND49_JMIN to be
!        equal to ND49_JMAX.  This will allow us to save out longitude
!        or latitude transects.  (cdh, bmy, 11/30/06)
!  02 Dec 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      CHARACTER(LEN=255) :: LOCATION
      
      !=================================================================
      ! INIT_DIAG49 begins here!
      !=================================================================

      ! Initialize
      LOCATION               = 'INIT_DIAG49 ("diag49_mod.f")'
      ND49_TRACERS(:)        = 0

      ! Get values from "input_mod.f"
      DO_SAVE_DIAG49         = DO_ND49 
      ND49_N_TRACERS         = N_ND49
      ND49_TRACERS(1:N_ND49) = TRACERS(1:N_ND49)
      ND49_IMIN              = IMIN
      ND49_IMAX              = IMAX
      ND49_JMIN              = JMIN
      ND49_JMAX              = JMAX
      ND49_LMIN              = LMIN
      ND49_LMAX              = LMAX
      ND49_FREQ              = FREQ
      ND49_OUTPUT_FILE       = FILE
     
      ! Return if we are not saving ND49 diagnostics
      IF ( .not. DO_SAVE_DIAG49 ) RETURN

      !=================================================================
      ! Compute lon, lat, alt extents and check for errors
      !=================================================================

      ! Get grid offsets for error checking
      IF ( ITS_A_NESTED_GRID() ) THEN
         I0 = GET_XOFFSET()
         J0 = GET_YOFFSET()
      ELSE
         I0 = GET_XOFFSET( GLOBAL=.TRUE. )
         J0 = GET_YOFFSET( GLOBAL=.TRUE. )
      ENDIF

      !-----------
      ! Longitude
      !-----------

      ! Error check ND49_IMIN
      IF ( ND49_IMIN+I0 < 1 .or. ND49_IMIN+I0 > IGLOB ) THEN
         CALL ERROR_STOP( 'Bad ND49_IMIN value!', LOCATION )
      ENDIF

      ! Error check ND49_IMAX
      IF ( ND49_IMAX+I0 < 1 .or. ND49_IMAX+I0 > IGLOB ) THEN
         CALL ERROR_STOP( 'Bad ND49_IMAX value!', LOCATION )
      ENDIF

      ! Compute longitude limits to write to disk 
      ! Also handle wrapping around the date line
      IF ( ND49_IMAX >= ND49_IMIN ) THEN
         ND49_NI = ( ND49_IMAX - ND49_IMIN ) + 1
      ELSE 
         ND49_NI = ( IIPAR - ND49_IMIN ) + 1 + ND49_IMAX
         WRITE( 6, '(a)' ) 'We are wrapping over the date line!'
      ENDIF

      ! Make sure that ND49_NI <= IIPAR
      IF ( ND49_NI > IIPAR ) THEN
         CALL ERROR_STOP( 'Too many longitudes!', LOCATION )
      ENDIF

      !-----------
      ! Latitude
      !-----------
      
      ! Error check JMIN_AREA
      IF ( ND49_JMIN+J0 < 1 .or. ND49_JMIN+J0 > JGLOB ) THEN
         CALL ERROR_STOP( 'Bad ND49_JMIN value!', LOCATION)
      ENDIF
     
      ! Error check JMAX_AREA
      IF ( ND49_JMAX+J0 < 1 .or.ND49_JMAX+J0 > JGLOB ) THEN
         CALL ERROR_STOP( 'Bad ND49_JMAX value!', LOCATION)
      ENDIF

      ! Compute latitude limits to write to disk (bey, bmy, 3/16/99)
      IF ( ND49_JMAX >= ND49_JMIN ) THEN      
         ND49_NJ = ( ND49_JMAX - ND49_JMIN ) + 1
      ELSE
         CALL ERROR_STOP( 'ND49_JMAX < ND49_JMIN!', LOCATION )
      ENDIF     
  
      !-----------
      ! Altitude
      !-----------

      ! Error check ND49_LMIN, ND49_LMAX
      IF ( ND49_LMIN < 1 .or. ND49_LMAX > LLPAR ) THEN 
         CALL ERROR_STOP( 'Bad ND49 altitude values!', LOCATION )
      ENDIF

      ! # of levels to save in ND49 timeseries
      IF ( ND49_LMAX >= ND49_LMIN ) THEN  
         ND49_NL = ( ND49_LMAX - ND49_LMIN ) + 1
      ELSE
         CALL ERROR_STOP( 'ND49_LMAX < ND49_LMIN!', LOCATION )
      ENDIF

      !-----------
      ! Offsets
      !-----------
      IOFF      = ND49_IMIN - 1
      JOFF      = ND49_JMIN - 1
      LOFF      = ND49_LMIN - 1

      !-----------
      ! For bpch
      !-----------
      TITLE     = 'GEOS-CHEM DIAG49 instantaneous timeseries'
      LONRES    = DISIZE
      LATRES    = DJSIZE
      MODELNAME = GET_MODELNAME()
      HALFPOLAR = GET_HALFPOLAR()
      
      ! Reset grid offsets to global values for bpch write
      I0        = GET_XOFFSET( GLOBAL=.TRUE. )
      J0        = GET_YOFFSET( GLOBAL=.TRUE. )      

      END SUBROUTINE INIT_DIAG49
!EOC
      END MODULE DIAG49_MOD
