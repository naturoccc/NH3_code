      PROGRAM GEOS_CHEM
!
! !USES:
!
  
      USE DAO_MOD,           ONLY : INIT_DAO
      USE INPUT_MOD,         ONLY : READ_INPUT_FILE
      USE EMISSIONS_MOD,     ONLY : DO_EMISSIONS

      USE LOGICAL_MOD,       ONLY : LPRT
      USE LOGICAL_MOD,       ONLY : LSVGLB
      USE LOGICAL_MOD,       ONLY : LSOILNH3

      USE SOILNH3_RESTART_MOD, ONLY: MAKE_SOILNH3_RESTART
      USE DIAG49_MOD,        ONLY : DIAG49,          ITS_TIME_FOR_DIAG49
      USE ERROR_MOD,         ONLY : DEBUG_MSG
      USE FILE_MOD,          ONLY : CLOSE_FILES
      USE RDMCIP_MOD,        ONLY : RDMCIP

      USE TIME_MOD,          ONLY : GET_NYMDb,        GET_NHMSb
      USE TIME_MOD,          ONLY : GET_NYMD,         GET_NHMS
      USE TIME_MOD,          ONLY : GET_TAU,          GET_TAUb
      USE TIME_MOD,          ONLY : GET_FIRST_A1_TIME
      USE TIME_MOD,          ONLY : GET_TS_DYN
      USE TIME_MOD,          ONLY : SET_CURRENT_TIME
      USE TIME_MOD,          ONLY : GET_YEAR,         GET_MONTH
      USE TIME_MOD,          ONLY : GET_HOUR,         GET_MINUTE
      USE TIME_MOD,          ONLY : GET_DAY_OF_YEAR,  ITS_A_NEW_DAY
      USE TIME_MOD,          ONLY : GET_ELAPSED_SEC
      USE TIME_MOD,          ONLY : GET_SEASON
      USE TIME_MOD,          ONLY : GET_A1_TIME

      USE TIME_MOD,          ONLY : SET_DIAGb,        SET_DIAGe

      USE TIME_MOD,          ONLY : ITS_TIME_FOR_BPCH,ITS_TIME_FOR_EXIT
      USE TIME_MOD,          ONLY : ITS_TIME_FOR_A1
      USE TIME_MOD,          ONLY : ITS_TIME_FOR_EMIS
      USE TIME_MOD,          ONLY : SET_CT_EMIS
      USE TIME_MOD,          ONLY : SET_ELAPSED_MIN
      USE TIME_MOD,          ONLY : TIMESTAMP_DIAG

      USE WRSNH3_MOD,        ONLY : DIAG3NC,          DIAG49NC

      IMPLICIT NONE
      
#     include "CMN_SIZE"          ! Size parameters
#     include "CMN_DIAG"          ! Diagnostic switches, NJDAY
!
!
! !LOCAL VARIABLES:
!
      LOGICAL            :: FIRST = .TRUE.
      LOGICAL            :: LXTRA 
      INTEGER            :: I,             IOS,   J,         K,      L
      INTEGER            :: N,             JDAY,  NDIAGTIME, N_DYN,  NN
      INTEGER            :: N_DYN_STEPS,   NSECb, N_STEP,    DATE(2)
      INTEGER            :: YEAR,          MONTH, DAY,       DAY_OF_YEAR
      INTEGER            :: SEASON,        NYMD,  NYMDb,     NHMS
      INTEGER            :: ELAPSED_SEC,   NHMSb, RC
      INTEGER            :: ELAPSED_TODAY, HOUR,  MINUTE
      REAL*8             :: TAU,           TAUb         
      REAL*8             :: HGPFRAC(IIPAR,JJPAR,LLPAR)
      CHARACTER(LEN=255) :: ZTYPE

      !=================================================================
      !            ***** I N I T I A L I Z A T I O N *****
      !=================================================================

      ! Read input file and call init routines from other modules
      CALL READ_INPUT_FILE

      ! Initialize met field arrays from "dao_mod.f"
      CALL INIT_DAO
      IF ( LPRT ) CALL DEBUG_MSG( '### MAIN: a INIT_DAO' )

      ! Initialize diagnostic arrays and counters
      CALL INITIALIZE( 2 )
      CALL INITIALIZE( 3 )
 
      ! Define time variables for use below
      NHMS  = GET_NHMS()
      NHMSb = GET_NHMSb()
      NYMD  = GET_NYMD()
      NYMDb = GET_NYMDb()
      TAU   = GET_TAU()
      TAUb  = GET_TAUb()


! added by hong
#if   defined( WRF )
      DATE = GET_FIRST_A1_TIME()
      CALL RDMCIP( DATE(1), DATE(2) )
#endif

      ! Read land types and fractions from "vegtype.global"
      CALL RDLAND   

      !=================================================================
      !       *****  I N I T I A L   C O N D I T I O N S *****
      !=================================================================

      !=================================================================
      !      ***** 6 - H O U R   T I M E S T E P   L O O P  *****
      !=================================================================

      ! Echo message before first timestep
      WRITE( 6, '(a)' )
      WRITE( 6, '(a)' ) REPEAT( '*', 44 )
      WRITE( 6, '(a)' ) '* B e g i n   T i m e   S t e p p i n g !! *'
      WRITE( 6, '(a)' ) REPEAT( '*', 44 )
      WRITE( 6, '(a)' ) 

      ! NSTEP is the number of dynamic timesteps w/in a 6-h interval
      N_DYN_STEPS = 360 / GET_TS_DYN()

      ! Start a new 6-h loop
      DO 

      ! Compute time parameters at start of 6-h loop
      CALL SET_CURRENT_TIME

      ! NSECb is # of seconds (measured from 00 GMT today) 
      ! at the start of this 6-hr timestepping loop.
      ! NOTE: Assume we start at the head of each minute (i.e. SECONDS=0)
      HOUR   = GET_HOUR()
      HOUR   = ( HOUR / 6 ) * 6
      MINUTE = GET_MINUTE()
      NSECb  = ( HOUR * 3600 ) + ( MINUTE * 60 )

      ! Get dynamic timestep in seconds
      N_DYN  = 60d0 * GET_TS_DYN()

      !=================================================================
      !     ***** D Y N A M I C   T I M E S T E P   L O O P *****
      !=================================================================
      DO N_STEP = 1, N_DYN_STEPS
    
         ! Compute & print time quantities at start of dyn step
         CALL SET_CURRENT_TIME

         ! Set time variables for dynamic loop
         DAY_OF_YEAR   = GET_DAY_OF_YEAR()
         ELAPSED_SEC   = GET_ELAPSED_SEC()
         MONTH         = GET_MONTH()
         NHMS          = GET_NHMS()
         NYMD          = GET_NYMD()
         HOUR          = GET_HOUR()
         MINUTE        = GET_MINUTE()
         TAU           = GET_TAU()
         YEAR          = GET_YEAR()
         SEASON        = GET_SEASON()
         ELAPSED_TODAY = ( HOUR * 3600 ) + ( MINUTE * 60 )


         !==============================================================
         !   ***** W R I T E   D I A G N O S T I C   F I L E S *****
         !==============================================================
         IF ( ITS_TIME_FOR_BPCH() ) THEN
            
            ! Set time at end of diagnostic timestep
            CALL SET_DIAGe( TAU )

            ! Write bpch file
            CALL DIAG3  

            ! added by hong, 2012/4/24
            CALL DIAG3NC

            !===========================================================
            !    *****  W R I T E   R E S T A R T   F I L E S  *****
            !===========================================================
            IF ( LSVGLB ) THEN

               !###Start Rynda
               ! Make soil restart file
               !!! modified by ccc  LSOILNOX -> LSOILNH3
               IF ( LSOILNH3 ) THEN
                  CALL MAKE_SOILNH3_RESTART( NYMD, NHMS, TAU )
               ENDIF
                !###End Rynda

               !### Debug
               IF ( LPRT ) THEN
                  CALL DEBUG_MSG( '### MAIN: a MAKE_RESTART_FILE' )
               ENDIF
            ENDIF

            ! Set time at beginning of next diagnostic timestep
            CALL SET_DIAGb( TAU )

            !===========================================================
            !        ***** Z E R O   D I A G N O S T I C S *****
            !===========================================================
            CALL INITIALIZE( 2 ) ! Zero arrays
            CALL INITIALIZE( 3 ) ! Zero counters
         ENDIF

         !==============================================================
         !       ***** T E S T   F O R   E N D   O F   R U N *****
         !==============================================================
         IF ( ITS_TIME_FOR_EXIT() ) GOTO 9999

! added by hong
#if   defined( WRF )
         IF ( ITS_TIME_FOR_A1() ) THEN
            DATE = GET_A1_TIME()
            CALL RDMCIP( DATE(1), DATE(2) )
         ENDIF
#endif


         !==============================================================
         !              ***** D A I L Y   D A T A *****
         !==============================================================
         IF ( ITS_A_NEW_DAY() ) THEN 

            ! Read leaf-area index
         ! !  CALL RDLAI( DAY_OF_YEAR, MONTH, YEAR )

            ! Read soil-type info
            CALL RDSOIL 
         ENDIF


         !-------------------------------
         ! Test for emission timestep
         !-------------------------------
         IF ( ITS_TIME_FOR_EMIS() ) THEN

            ! Increment emission counter
            CALL SET_CT_EMIS( INCREMENT=.TRUE. )

            !========================================================
            !         ***** D R Y   D E P O S I T I O N *****
            !========================================================
            ! NOT FOR NH3 EMISSION
            ! CALL GET_CANOPY_NOx
            ! GET_CANOPY_NOX RETURN CANOPYNOX(IJLOOP,LDT)

            !========================================================
            !             ***** E M I S S I O N S *****
            !========================================================
            CALL DO_EMISSIONS

         ENDIF

         !==============================================================
         !   ***** I N C R E M E N T   E L A P S E D   T I M E *****
         !============================================================== 
         ! Moved before diagnostics to count the last timestep as done.
         ! Need to save timestamps for filenames.
         ! (ccc, 5/13/09)
 

         CALL TIMESTAMP_DIAG
         
         
         CALL SET_ELAPSED_MIN
         CALL SET_CURRENT_TIME

         !==============================================================
         !   ***** T I M E S E R I E S   D I A G N O S T I C S  *****
         !============================================================== 

         ! 3-D timeseries
         IF ( ITS_TIME_FOR_DIAG49() ) CALL DIAG49

         ! added by hong, 2012/4/24
         IF ( ITS_TIME_FOR_DIAG49() ) CALL DIAG49NC
          
      ENDDO

      ENDDO

      !=================================================================
      !         ***** C L E A N U P   A N D   Q U I T *****
      !=================================================================
 9999 CONTINUE


      ! Close all files
      CALL CLOSE_FILES

      ! Deallocate dynamic module arrays
      CALL CLEANUP
      
      !Program exited normally
      WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
      WRITE( 6, '(a,/)' ) 'G E O S - C H E M   E X I T E D !'
 
      END
