!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: input_mod
!
! !DESCRIPTION: Module INPUT\_MOD contains routines that read the GEOS-Chem 
!  input file at the start of the run and pass the information to several 
!  other GEOS-Chem F90 modules.
!\\
!\\
! !INTERFACE:
!
      MODULE INPUT_MOD
!
! !USES:
!
      IMPLICIT NONE
      PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!  
      PUBLIC  :: READ_INPUT_FILE
!
! !PRIVATE MEMBER FUNCTIONS:
!
      PRIVATE :: READ_ONE_LINE
      PRIVATE :: SPLIT_ONE_LINE        
      PRIVATE :: READ_SIMULATION_MENU
!      PRIVATE :: READ_TRACER_MENU  
!      PRIVATE :: READ_AEROSOL_MENU     
      PRIVATE :: READ_EMISSIONS_MENU
!      PRIVATE :: READ_FUTURE_MENU
!      PRIVATE :: READ_CHEMISTRY_MENU
      PRIVATE :: READ_TRANSPORT_MENU
!      PRIVATE :: READ_CONVECTION_MENU
!      PRIVATE :: READ_DEPOSITION_MENU
      PRIVATE :: READ_OUTPUT_MENU
      PRIVATE :: READ_DIAGNOSTIC_MENU
      PRIVATE :: SET_TINDEX
      PRIVATE :: READ_ND49_MENU      
!      PRIVATE :: READ_ND50_MENU  
!      PRIVATE :: READ_ND51_MENU  
!      PRIVATE :: READ_ND51b_MENU  
!      PRIVATE :: READ_PROD_LOSS_MENU 
      PRIVATE :: READ_UNIX_CMDS_MENU
!      PRIVATE :: READ_NESTED_GRID_MENU
!      PRIVATE :: READ_ARCHIVED_OH_MENU
!      PRIVATE :: READ_O3PL_MENU
!      PRIVATE :: READ_BENCHMARK_MENU  
!      PRIVATE :: READ_CH4_MENU
!      PRIVATE :: VALIDATE_DIRECTORIES  
!      PRIVATE :: CHECK_DIRECTORY
      PRIVATE :: CHECK_TIME_STEPS 
      PRIVATE :: IS_LAST_DAY_GOOD
      PRIVATE :: INIT_INPUT
! 
!BOC
!
! !PRIVATE TYPES:
!
      LOGICAL            :: VERBOSE  = .FALSE.
      INTEGER, PARAMETER :: FIRSTCOL = 26
      INTEGER, PARAMETER :: MAXDIM   = 255
      INTEGER            :: TS_CHEM
      INTEGER            :: TS_DYN
      INTEGER            :: TS_CONV
      INTEGER            :: TS_EMIS
      INTEGER            :: TS_UNIT
      INTEGER            :: CT1, CT2, CT3
      CHARACTER(LEN=255) :: FILENAME = 'input.geos'
      CHARACTER(LEN=255) :: TOPTITLE
      CHARACTER(LEN=255) :: BPCH_FILE
      CHARACTER(LEN=255) :: DIAGINFO  
      CHARACTER(LEN=255) :: TRACERINFO

      CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_input_file
!
! !DESCRIPTION: Subroutine READ\_INPUT\_FILE is the driver program for 
!  reading the GEOS-Chem input file "input.geos" from disk. 
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE READ_INPUT_FILE
!
! !USES:
!
      USE CHARPAK_MOD, ONLY : STRREPL
      USE FILE_MOD,    ONLY : IU_GEOS, IOERROR
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Now call DO_GAMAP from "gamap_mod.f" to create "diaginfo.dat" and
!        "tracerinfo.dat" files after all diagnostic menus have been read in
!  (2 ) Now call NDXX_setup from this routine (phs, 11/18/08)
!  (3 ) Now call READ_ND51b_MENU (amv, bmy, 12/18/09)
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL            :: EOF
      INTEGER            :: IOS
      CHARACTER(LEN=1)   :: TAB   = ACHAR(9)
      CHARACTER(LEN=1)   :: SPACE = ' '
      CHARACTER(LEN=255) :: LINE

      !=================================================================
      ! READ_INPUT_FILE begins here!
      !=================================================================  

      ! Echo output
      WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
      WRITE( 6, '(a,/)' ) 'G E O S - C H E M   U S E R   I N P U T'
      WRITE( 6, 100   ) TRIM( FILENAME )
 100  FORMAT( 'READ_INPUT_FILE: Reading ', a )

      ! Initialize directory & logical variables
      CALL INIT_INPUT

      ! Open file
      OPEN( IU_GEOS, FILE=TRIM( FILENAME ), STATUS='OLD', IOSTAT=IOS )
      IF ( IOS /= 0 ) CALL IOERROR( IOS, IU_GEOS, 'read_input_file:1' )

      ! Read TOPTITLE for binary punch file
      TOPTITLE = READ_ONE_LINE( EOF  )
      IF ( EOF ) RETURN

      ! Loop until EOF
      DO 
         
         ! Read a line from the file, exit if EOF
         LINE = READ_ONE_LINE( EOF ) 
         IF ( EOF ) EXIT
         
         ! Replace tab characters in LINE (if any) w/ spaces
         CALL STRREPL( LINE, TAB, SPACE )

         !=============================================================
         ! Call individual subroutines to read sections of the file
         ! 
         ! NOTE: You are pretty flexible in setting the order of the
         ! menus in the input file; however, a few guidelines apply:
         !
         ! (1) SIMULATION MENU should be listed first.
         ! (2) TRACER MENU should be listed second.
         ! (3) EMISSIONS, AEROSOL, CHEMISTRY, TRANSPORT, CONVECTION, 
         !      and DEPOSITION menus (in any order) should follow.
         ! (4) Diagnostic menus, including OUTPUT, DIAGNOSTIC,
         !      PLANEFLIGHT, ND48, ND49, ND50, ND51, and PROD_LOSS
         !      menus (in any order) should follow next.
         ! (5) The following menus have no other restriction and
         !      can be placed anywhere (but by convention we will
         !      place them after the diagnostic menu): NESTED GRID
         !      UNIX CMDS, ARCHIVED OH, and O3PL menus.
         !=============================================================
         IF      ( INDEX( LINE, 'SIMULATION MENU'  ) > 0 ) THEN
            CALL READ_SIMULATION_MENU             
                                     
         ELSE IF ( INDEX( LINE, 'EMISSIONS MENU'   ) > 0 ) THEN
            CALL READ_EMISSIONS_MENU              
                                                  
         ELSE IF ( INDEX( LINE, 'TRANSPORT MENU'   ) > 0 ) THEN
            CALL READ_TRANSPORT_MENU
                                                  
         ELSE IF ( INDEX( LINE, 'OUTPUT MENU'      ) > 0 ) THEN
            CALL READ_OUTPUT_MENU                 
                                                  
         ELSE IF ( INDEX( LINE, 'DIAGNOSTIC MENU'  ) > 0 ) THEN
            CALL READ_DIAGNOSTIC_MENU                              
                                    
         ELSE IF ( INDEX( LINE, 'ND49 MENU'        ) > 0 ) THEN
            CALL READ_ND49_MENU  

         ELSE IF ( INDEX( LINE, 'UNIX CMDS MENU'   ) > 0 ) THEN 
            CALL READ_UNIX_CMDS_MENU 

         ELSE IF ( INDEX( LINE, 'END OF FILE'      ) > 0 ) THEN 
            EXIT

         ENDIF  
      ENDDO

      ! Close input file
      CLOSE( IU_GEOS )

      ! Allocate diagnostic arrays (phs, 11/18/08)
      CALL NDXX_SETUP

      !=================================================================
      ! Further error-checking and initialization
      !=================================================================

      ! Check GEOS-CHEM timesteps
      CALL CHECK_TIME_STEPS

      ! Echo output
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )

      END SUBROUTINE READ_INPUT_FILE
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_one_line
!
! !DESCRIPTION: Subroutine READ\_ONE\_LINE reads a line from the input file.  
!  If the global variable VERBOSE is set, the line will be printed to stdout.  
!  READ\_ONE\_LINE can trap an unexpected EOF if LOCATION is passed.  
!  Otherwise, it will pass a logical flag back to the calling routine, 
!  where the error trapping will be done.
!\\
!\\
! !INTERFACE:
!
      FUNCTION READ_ONE_LINE( EOF, LOCATION ) RESULT( LINE )
!
! !USES:
!
      USE FILE_MOD, ONLY : IU_GEOS, IOERROR
!
! !INPUT PARAMETERS: 
!
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: LOCATION    ! Msg to display
!
! !OUTPUT PARAMETERS:
!
      LOGICAL,          INTENT(OUT)          :: EOF         ! Denotes EOF 
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER            :: IOS
      CHARACTER(LEN=255) :: LINE, MSG

      !=================================================================
      ! READ_ONE_LINE begins here!
      !=================================================================

      ! Initialize
      EOF = .FALSE.

      ! Read a line from the file
      READ( IU_GEOS, '(a)', IOSTAT=IOS ) LINE

      ! IO Status < 0: EOF condition
      IF ( IOS < 0 ) THEN
         EOF = .TRUE.

         ! Trap unexpected EOF -- stop w/ error msg if LOCATION is passed
         ! Otherwise, return EOF to the calling program
         IF ( PRESENT( LOCATION ) ) THEN
            MSG = 'READ_ONE_LINE: error at: ' // TRIM( LOCATION )
            WRITE( 6, '(a)' ) MSG
            WRITE( 6, '(a)' ) 'Unexpected end of file encountered!'
            WRITE( 6, '(a)' ) 'STOP in READ_ONE_LINE (input_mod.f)'
            WRITE( 6, '(a)' ) REPEAT( '=', 79 )
            STOP
         ELSE
            RETURN
         ENDIF
      ENDIF

      ! IO Status > 0: true I/O error condition
      IF ( IOS > 0 ) CALL IOERROR( IOS, IU_GEOS, 'read_one_line:1' )

      ! Print the line (if necessary)
      IF ( VERBOSE ) WRITE( 6, '(a)' ) TRIM( LINE )

      END FUNCTION READ_ONE_LINE
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: split_one_line
!
! !DESCRIPTION: Subroutine SPLIT\_ONE\_LINE reads a line from the input file 
!  (via routine READ\_ONE\_LINE), and separates it into substrings.
!\\
!\\
!  SPLIT\_ONE\_LINE also checks to see if the number of substrings found is 
!  equal to the number of substrings that we expected to find.  However, if
!  you don't know a-priori how many substrings to expect a-priori, 
!  you can skip the error check.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE SPLIT_ONE_LINE( SUBSTRS, N_SUBSTRS, N_EXP, LOCATION ) 
!
! !USES:
!
      USE CHARPAK_MOD, ONLY: STRSPLIT
!
! !INPUT PARAMETERS: 
!
      ! Number of substrings we expect to find
      INTEGER,            INTENT(IN)  :: N_EXP

      ! Name of routine that called SPLIT_ONE_LINE
      CHARACTER(LEN=*),   INTENT(IN)  :: LOCATION 
!
! !OUTPUT PARAMETERS:
!
      ! Array of substrings (separated by " ")
      CHARACTER(LEN=255), INTENT(OUT) :: SUBSTRS(MAXDIM)

      ! Number of substrings actually found
      INTEGER,            INTENT(OUT) :: N_SUBSTRS
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL                         :: EOF
      CHARACTER(LEN=255)              :: LINE, MSG

      !=================================================================
      ! SPLIT_ONE_LINE begins here!
      !=================================================================      

      ! Create error msg
      MSG = 'SPLIT_ONE_LINE: error at ' // TRIM( LOCATION )

      !=================================================================
      ! Read a line from disk
      !=================================================================
      LINE = READ_ONE_LINE( EOF )

      ! STOP on End-of-File w/ error msg
      IF ( EOF ) THEN
         WRITE( 6, '(a)' ) TRIM( MSG )
         WRITE( 6, '(a)' ) 'End of file encountered!' 
         WRITE( 6, '(a)' ) 'STOP in SPLIT_ONE_LINE (input_mod.f)!'
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         STOP
      ENDIF

      !=================================================================
      ! Split the lines between spaces -- start at column FIRSTCOL
      !=================================================================
      CALL STRSPLIT( LINE(FIRSTCOL:), ' ', SUBSTRS, N_SUBSTRS )

      ! Sometimes we don't know how many substrings to expect,
      ! if N_EXP is greater than MAXDIM, then skip the error check
      IF ( N_EXP < 0 ) RETURN

      ! Stop if we found the wrong 
      IF ( N_EXP /= N_SUBSTRS ) THEN
         WRITE( 6, '(a)' ) TRIM( MSG )
         WRITE( 6, 100   ) N_EXP, N_SUBSTRS
         WRITE( 6, '(a)' ) 'STOP in SPLIT_ONE_LINE (input_mod.f)!'
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         STOP
 100     FORMAT( 'Expected ',i2, ' substrs but found ',i3 )
      ENDIF
       
      END SUBROUTINE SPLIT_ONE_LINE
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_simulation_menu
!
! !DESCRIPTION: Subroutine READ\_SIMULATION\_MENU reads the SIMULATION MENU 
!  section of the GEOS-Chem input file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE READ_SIMULATION_MENU
!
! !USES:
!
      USE DIRECTORY_MOD, ONLY : DATA_DIR,    DATA_DIR_1x1, GCAP_DIR
      USE DIRECTORY_MOD, ONLY : GEOS_3_DIR,  GEOS_4_DIR,   GEOS_5_DIR
      USE DIRECTORY_MOD, ONLY : MERRA_DIR
      USE DIRECTORY_MOD, ONLY : RUN_DIR
      USE DIRECTORY_MOD, ONLY : TEMP_DIR   
      USE GRID_MOD,      ONLY : SET_XOFFSET, SET_YOFFSET,  COMPUTE_GRID
      USE LOGICAL_MOD,   ONLY : LSVGLB,      LUNZIP,       LWAIT
      USE LOGICAL_MOD,   ONLY : LVARTROP,    LLINOZ
      USE TIME_MOD,      ONLY : SET_BEGIN_TIME,   SET_END_TIME 
      USE TIME_MOD,      ONLY : SET_CURRENT_TIME, SET_DIAGb
      USE TIME_MOD,      ONLY : SET_NDIAGTIME,    GET_TAU
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Bug fix: Read LSVGLB w/ the * format and not w/ '(a)'. (bmy, 2/23/05)
!  (2 ) Now read GEOS_5_DIR and GCAP_DIR from input.geos (swu, bmy, 5/25/05)
!  (3 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  (4 ) Now references DATA_DIR_1x1 for 1x1 emissions files (bmy, 10/24/05)
!  (5 ) Now read switch for using variable tropopause or not (phs, 9/14/06)
!  (6 ) Remove references to GEOS-1 and GEOS-STRAT run dirs.  Now calls 
!        INIT_TRANSFER (bmy, 11/5/07)
!  (7 ) Fix typo in "print to screen" section  (phs, 6/1/08)
!  (8 ) Call INIT_TRANSFER w/ (0,0) instead of (I0,J0) (phs, 6/17/08)
!  (10) Now read LLINOZ switch from input.geos file (dbm, bmy, 10/16/09)
!  13 Aug 2010 - R. Yantosca - Now read MERRA_DIR
!  19 Aug 2010 - R. Yantosca - Set LUNZIP=F for MERRA met fields.
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER            :: I0,    J0
      INTEGER            :: N,     NDIAGTIME
      INTEGER            :: NYMDb, NHMSb 
      INTEGER            :: NYMDe, NHMSe
      CHARACTER(LEN=255) :: SUBSTRS(MAXDIM)
      CHARACTER(LEN=255) :: IN_RST_FILE
      CHARACTER(LEN=255) :: OUT_RST_FILE

      !=================================================================
      ! READ_SIMULATION_MENU begins here!
      !=================================================================

      ! Start YYYYMMDD, HHMMSS
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_simulation_menu:1' )
      READ( SUBSTRS(1:N), * ) NYMDb, NHMSb

      ! End YYYYMMDD, HHMMSS
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_simulation_menu:2' )
      READ( SUBSTRS(1:N), * ) NYMDe, NHMSe

      ! Run directory
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:3' )
      READ( SUBSTRS(1:N), '(a)' ) RUN_DIR

      ! Input restart file
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:4' )
      READ( SUBSTRS(1:N), '(a)' ) IN_RST_FILE

      ! Make new restart file?
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:5' )
      READ( SUBSTRS(1:N), * ) LSVGLB

      ! Output restart file(s)
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:6' )
      READ( SUBSTRS(1:N), '(a)' ) OUT_RST_FILE

      ! Root data dir
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:7' )
      READ( SUBSTRS(1:N), '(a)' ) DATA_DIR

      ! GCAP subdir
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:8' )
      READ( SUBSTRS(1:N), '(a)' ) GCAP_DIR

      ! GEOS-3 subdir
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:9' )
      READ( SUBSTRS(1:N), '(a)' ) GEOS_3_DIR

      ! GEOS-4 subdir
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:10' )
      READ( SUBSTRS(1:N), '(a)' ) GEOS_4_DIR

      ! GEOS-5 subdir
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:11' )
      READ( SUBSTRS(1:N), '(a)' ) GEOS_5_DIR

      ! MERRA subdir
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:12' )
      READ( SUBSTRS(1:N), '(a)' ) MERRA_DIR

      ! Temp dir
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:13' )
      READ( SUBSTRS(1:N), '(a)' ) DATA_DIR_1x1

      ! Temp dir
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:14' )
      READ( SUBSTRS(1:N), '(a)' ) TEMP_DIR

      ! Unzip met fields
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:15' )
      READ( SUBSTRS(1:N), *     ) LUNZIP

      ! Wait for met fields?
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:16' )
      READ( SUBSTRS(1:N), *     ) LWAIT

      ! Variable Tropopause
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:17' )
      READ( SUBSTRS(1:N), *     ) LVARTROP

      ! LINOZ chemistry in the stratosphere
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:18' )
      READ( SUBSTRS(1:N), *     ) LLINOZ  

      ! I0, J0
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 2, 'read_simulation_menu:19' )
      READ( SUBSTRS(1:N), *     ) I0, J0

      ! Separator line
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_simulation_menu:20' )

      !=================================================================
      ! Add safety checks for logical switches
      !=================================================================
#if   defined( MERRA )
      ! Turn unzipping off for MERRA met fields
      LUNZIP = .FALSE. 
#endif

      !=================================================================
      ! Print to screen
      !=================================================================
      WRITE( 6, '(/,a)' ) 'SIMULATION MENU'
      WRITE( 6, '(  a)' ) '---------------'
      WRITE( 6, 100     ) 'Start time of run           : ', NYMDb, NHMSb
      WRITE( 6, 100     ) 'End time of run             : ', NYMDe, NHMSe
      WRITE( 6, 110     ) 'Run directory               : ',
     &                     TRIM( RUN_DIR )
      WRITE( 6, 110     ) 'Data Directory              : ',
     &                     TRIM( DATA_DIR )
      WRITE( 6, 110     ) 'GCAP       sub-directory    : ', 
     &                     TRIM( GCAP_DIR )
      WRITE( 6, 110     ) 'GEOS-3     sub-directory    : ', 
     &                     TRIM( GEOS_3_DIR )
      WRITE( 6, 110     ) 'GEOS-4     sub-directory    : ', 
     &                     TRIM( GEOS_4_DIR )
      WRITE( 6, 110     ) 'GEOS-5     sub-directory    : ', 
     &                     TRIM( GEOS_5_DIR )
      WRITE( 6, 110     ) 'MERRA      sub-directory    : ', 
     &                     TRIM( MERRA_DIR )
      WRITE( 6, 110     ) '1x1 Emissions etc Data Dir  : ',
     &                     TRIM( DATA_DIR_1x1 )
      WRITE( 6, 110     ) 'Temporary Directory         : ', 
     &                     TRIM( TEMP_DIR )
      WRITE( 6, 110     ) 'Input restart file          : ', 
     &                     TRIM( IN_RST_FILE )
      WRITE( 6, 120     ) 'Create restart file?        : ', LSVGLB
      WRITE( 6, 110     ) 'Output restart file(s)      : ', 
     &                     TRIM( OUT_RST_FILE )
      WRITE( 6, 120     ) 'Unzip met fields?           : ', LUNZIP
      WRITE( 6, 120     ) 'Wait for met fields?        : ', LWAIT
      WRITE( 6, 120     ) 'Use variable tropopause?    : ', LVARTROP
      WRITE( 6, 120     ) 'Use LINOZ strat chemistry?  : ', LLINOZ
      WRITE( 6, 130     ) 'Global offsets I0, J0       : ', I0, J0

      ! Format statements
 100  FORMAT( A, I8.8, 1X, I6.6 )
 110  FORMAT( A, A              )
 120  FORMAT( A, L5             )
 130  FORMAT( A, 2I5            )

      !=================================================================
      ! Call setup routines from other GEOS-CHEM modules
      !=================================================================

      ! Set start time of run in "time_mod.f"
      CALL SET_BEGIN_TIME( NYMDb, NHMSb )

      ! Set end time of run in "time_mod.f"
      CALL SET_END_TIME( NYMDe, NHMSe )

      ! Set the current time
      CALL SET_CURRENT_TIME()

      ! Set the time of day for writing bpch files
      NDIAGTIME = NHMSe !### test
      CALL SET_NDIAGTIME( NDIAGTIME )

      ! Set the start of the 1st diagnostic interval
      CALL SET_DIAGb( GET_TAU() )
     

      ! Set global offsets
      CALL SET_XOFFSET( I0 )
      CALL SET_YOFFSET( J0 )

      ! Compute lat/lon/surface area variables
      CALL COMPUTE_GRID


      ! Set counter
      CT1 = CT1 + 1

      END SUBROUTINE READ_SIMULATION_MENU
!EOC
      SUBROUTINE READ_EMISSIONS_MENU 
!
! !USES:
!
      USE ERROR_MOD,   ONLY : ERROR_STOP
      USE LOGICAL_MOD, ONLY : LAIRNOX,    LANTHRO,   LAVHRRLAI, LBBSEA    
      USE LOGICAL_MOD, ONLY : LBIOFUEL,   LBIOGENIC, LBIOMASS,  LBIONOX
      USE LOGICAL_MOD, ONLY : LCOOKE
      USE LOGICAL_MOD, ONLY : LEMIS,      LFOSSIL,   LLIGHTNOX, LMONOT    
!##!modified by chenchuchu, add LSOILNH3 & LFERTILIZERNH3     
      USE LOGICAL_MOD, ONLY : LNEI99,     LSHIPSO2,  LSOILNOX,  LTOMSAI 
      USE LOGICAL_MOD, ONLY : LSOILNH3,   LFERTILIZERNH3  
      USE LOGICAL_MOD, ONLY : LWOODCO,    LMEGAN,    LMEGANMONO,LEMEP
      USE LOGICAL_MOD, ONLY : LFERTILIZERNOX
      USE LOGICAL_MOD, ONLY : LOTDLOC
      USE LOGICAL_MOD, ONLY : LBRAVO,    LEDGAR    
      USE LOGICAL_MOD, ONLY : LEDGARNOx,  LEDGARCO,  LEDGARSOx 
      USE LOGICAL_MOD, ONLY : LEDGARSHIP, LSTREETS,  LCAC,      LVISTAS
      USE LOGICAL_MOD, ONLY : LARCSHIP,   LEMEPSHIP, LICARTT,   LGFED2BB 
      USE LOGICAL_MOD, ONLY : LICOADSSHIP,LNEI05 
      USE LOGICAL_MOD, ONLY : L8DAYBB,    L3HRBB,    LSYNOPBB
      USE LOGICAL_MOD, ONLY : LMODISLAI , LPECCA  !(mpb,2009)


#     include "CMN_SIZE"      ! Size parameters
! 
!
! !LOCAL VARIABLES:
!
      INTEGER            :: N
      CHARACTER(LEN=255) :: SUBSTRS(MAXDIM), MSG, LOC

      !=================================================================
      ! READ_EMISSIONS_MENU begins here!
      !=================================================================

      ! Location for error messages
      LOC = 'READ_EMISSIONS_MENU ("input_mod.f")'

      ! Turn on emissions?
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:1' )
      READ( SUBSTRS(1:N), * ) LEMIS

      ! Emissions timestep
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:2' )
      READ( SUBSTRS(1:N), * ) TS_EMIS

      ! Include anthropogenic emissions?
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:3' )
      READ( SUBSTRS(1:N), * ) LANTHRO

      ! Use soil NOx
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:32' )
      READ( SUBSTRS(1:N), * ) LSOILNOX

      ! separate use fertilizer and soil NOx (fp, 06/09)
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:33' )
      READ( SUBSTRS(1:N), * ) LFERTILIZERNOX
!##! modified by chenchuchu
      ! Use soil NH3
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:34' )
      READ( SUBSTRS(1:N), * ) LSOILNH3

      ! separate use fertilizer and soil NH3 (fp, 06/09)
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:35' )
      READ( SUBSTRS(1:N), * ) LFERTILIZERNH3

      ! Use AVHRR-derived LAI fields?
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:42' )
      READ( SUBSTRS(1:N), * ) LAVHRRLAI

      ! Use MODIS-derived LAI fields? (mpb,2009)
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_emissions_menu:43' )
      READ( SUBSTRS(1:N), * ) LMODISLAI

      !=================================================================
      ! Print to screen
      !=================================================================
      WRITE( 6, '(/,a)' ) 'EMISSIONS MENU'
      WRITE( 6, '(  a)' ) '--------------'
      WRITE( 6, 100     ) 'Turn on emissions?          : ', LEMIS
      WRITE( 6, 110     ) 'Emissions timestep [min]    : ', TS_EMIS
      WRITE( 6, 100     ) 'Turn on ANTHRO emissions    : ', LANTHRO
      WRITE( 6, 100     ) 'Turn on SOIL NOx?           : ', LSOILNOX
      WRITE( 6, 100     ) 'Turn on Fertilizer NOx?     : ', 
     &                     LFERTILIZERNOX
!##! modified by chenchuchu
      WRITE( 6, 100     ) 'Turn on SOIL NH3?           : ', LSOILNH3
      WRITE( 6, 100     ) 'Turn on Fertilizer NH3?     : ',
     &                     LFERTILIZERNH3
      WRITE( 6, 100     ) 'Turn on AVHRR-derived LAI?  : ', LAVHRRLAI
      WRITE( 6, 100     ) 'Turn on MODIS-derived LAI?  : ', LMODISLAI

      ! FORMAT statements
 100  FORMAT( A, L5 )
 110  FORMAT( A, I5 )

      END SUBROUTINE READ_EMISSIONS_MENU
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_transport_menu
!
! !DESCRIPTION: Subroutine READ\_TRANSPORT\_MENU reads the TRANSPORT MENU 
!  section of the GEOS-Chem input file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE READ_TRANSPORT_MENU
!
! !USES:
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Now define MAX_DYN for 1 x 1.25 grid (bmy, 12/1/04)
!  (2 ) Update text in error message (bmy, 2/23/05)
!  (3 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  (4 ) Don't stop run if TS_DYN > MAX_DYN but transport is turned off
!        (cdh, bmy, 7/7/08)
!  (5 ) Set MAX_DYN for the 0.5 x 0.666 nested grid (yxw, dan, bmy, 11/6/08)
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER            :: N, IORD, JORD, KORD, J1, KS, MAX_DYN
      CHARACTER(LEN=255) :: SUBSTRS(MAXDIM), MSG, LOCATION

      !=================================================================
      ! READ_TRANSPORT_MENU begins here!
      !=================================================================

      ! Transport timestep
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_transport_menu:5' )
      READ( SUBSTRS(1:N), * ) TS_DYN

      !=================================================================
      ! Print to screen
      !=================================================================
      WRITE( 6, '(/,a)' ) 'TRANSPORT MENU'
      WRITE( 6, '(  a)' ) '--------------'
      WRITE( 6, 120     ) 'Transport timestep [min]    : ', TS_DYN
 120  FORMAT( A, I5  )

      END SUBROUTINE READ_TRANSPORT_MENU
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_output_menu
!
! !DESCRIPTION: Subroutine READ\_OUTPUT\_MENU reads the OUTPUT MENU section of 
!  the GEOS-Chem input file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE READ_OUTPUT_MENU
!
! !USES:
!
      USE FILE_MOD, ONLY : IU_GEOS, IOERROR
      
#     include "CMN_SIZE" ! Size parameters
#     include "CMN_DIAG" ! NJDAY
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER :: IOS

      !=================================================================
      ! READ_OUTPUT_MENU begins here!
      !=================================================================
      READ( IU_GEOS, 100, IOSTAT=IOS ) NJDAY
 100  FORMAT( 26x, 31i1, /  26x, 29i1, /, 26x, 31i1, /, 26x, 30i1, /, 
     &        26x, 31i1, /, 26x, 30i1, /, 26x, 31i1, /, 26x, 31i1, /,
     &        26x, 30i1, /  26x, 31i1, /, 26x, 30i1, /, 26x, 31i1 )

      ! Error check
      IF ( IOS /= 0 ) CALL IOERROR( IOS, IU_GEOS, 'read_output_menu:1' )

      !=================================================================
      ! Print to screen
      !=================================================================

      WRITE( 6, '(/,a)' ) 'OUTPUT MENU'
      WRITE( 6, '(  a)' ) '-----------'
      WRITE( 6, 110     )
      WRITE( 6, 120     )
      WRITE( 6, 130     )
      WRITE( 6, 140     ) NJDAY

      ! FORMAT statements
 110  FORMAT( '              1111111111222222222233' )
 120  FORMAT( '     1234567890123456789012345678901' )
 130  FORMAT( '     -------------------------------' )
 140  FORMAT( 'JAN--', 31i1, /, 'FEB--', 29i1, /, 'MAR--', 31i1, /, 
     &        'APR--', 30i1, /, 'MAY--', 31i1, /, 'JUN--', 30i1, /, 
     &        'JUL--', 31i1, /, 'AUG--', 31i1, /, 'SEP--', 30i1, /,
     &        'OCT--', 31i1, /, 'NOV--', 30i1, /, 'DEC--', 31i1 )

      ! Make sure we have output at end of run
      CALL IS_LAST_DAY_GOOD

      END SUBROUTINE READ_OUTPUT_MENU
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_diagnostic_menu
!
! !DESCRIPTION: Subroutine READ\_DIAGNOSTIC\_MENU reads the DIAGNOSTIC MENU 
!  section of the GEOS-Chem input file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE READ_DIAGNOSTIC_MENU
!
! !USES:
!

      USE BPCH2_MOD,    ONLY : OPEN_BPCH2_FOR_WRITE
      USE ERROR_MOD,    ONLY : ERROR_STOP
      USE FILE_MOD,     ONLY : IU_BPCH
      USE LOGICAL_MOD,  ONLY : LBIOMASS,  LBIOFUEL,  LCARB, LCONV    
      USE LOGICAL_MOD,  ONLY : LDRYD,     LDUST,     LPRT,  LSULF    
      USE LOGICAL_MOD,  ONLY : LSSALT,    LTURB,     LWETD, LGFED2BB  
      USE TIME_MOD,     ONLY : GET_NYMDb, GET_NHMSb, EXPAND_DATE

#     include "CMN_SIZE"     ! Size parameters
#     include "CMN_DIAG"     ! NDxx flags
!
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Now reference IU_BPCH from "file_mod.f" and OPEN_BPCH2_FOR_WRITE
!        from "bpch2_mod.f".  Now opens the bpch file for output here
!        instead of w/in "main.f" (bmy, 2/3/05)
!  (2 ) Now references "diag03_mod.f" and "diag41_mod.f".  Now turn off ND38
!        when both LWETD=F and LCONV=F.  Now calls EXPAND_DATE to replace
!        YYYYMMDD and HHMMSS tokens in the bpch file name with the actual
!        starting date & time of the run. (bmy, 3/25/05)
!  (3 ) Now get diag info for ND09 for HCN/CH3CN sim (bmy, 6/27/05)
!  (4 ) Now references "diag04_mod.f" (bmy, 7/26/05)
!  (5 ) Now make sure all USE statements are USE, ONLY.  Also remove reference
!        to DIAG_MOD, it's not needed. (bmy, 10/3/05)
!  (6 ) Now remove reference to NBIOTRCE; Replace w/ NBIOMAX. (bmy, 4/5/06)
!  (7 ) Now reference ND56, PD56, INIT_DIAG56 from "diag56_mod.f" 
!        (bmy, 5/10/06)
!  (8 ) Now reference ND42, PD42, INIT_DIAG42 from "diag42_mod.f"
!        (dkh, bmy, 5/22/06)
!  (9 ) Now set max dimension for GFED2 or default biomass (bmy, 9/22/06)
!  (10) Bug fix: Should use ND52 in call to SET_TINDEX (cdh, bmy, 2/11/08)
!  (11) Remove call to NDXX_SETUP; this is now called in READ_INPUT_FILE.
!        (phs, 11/18/08)
!  (12) Now set TINDEX with PD45=NNPAR+1 tracers instead of N_TRACERS.
!        (tmf, 2/10/09)
!  (13) NBIOMAX now in CMN_SIZE (fp, 6/2009)
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!  26 May 2011 - R. Yantosca - For ND17, ND18, ND37, ND38, ND39, we need to
!                              set N_TMP = N_TRACERS, or else wetdep tracers 
!                              with indices higher than #32 won't print out.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER            :: M, N, N_TMP
      CHARACTER(LEN=255) :: SUBSTRS(MAXDIM), MSG, LOCATION

      !=================================================================
      ! READ_DIAGNOSTIC_MENU begins here!
      !=================================================================

      ! Location for ERROR_STOP
      LOCATION = 'READ_DIAGNOSTIC_MENU ("input_mod.f")'

      ! Binary punch file name
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1,  'read_diagnostic_menu:1' )
      READ( SUBSTRS(1:N), '(a)' ) BPCH_FILE

      !--------------------------
      ! ND32: NOx sources
      !--------------------------
      CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_diagnostic_menu:33' )
      READ( SUBSTRS(1), * ) ND32
      CALL SET_TINDEX( 32, ND32, SUBSTRS(2:N), N-1, PD32 )

      !--------------------------
      ! ND66: DAO 3-D fields
      !--------------------------
      CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_diagnostic_menu:64' )
      READ( SUBSTRS(1), * ) ND66
      CALL SET_TINDEX( 66, ND66, SUBSTRS(2:N), N-1, PD66 )

      !--------------------------
      ! ND67: DAO 2-D fields
      !--------------------------
      CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_diagnostic_menu:65' )
      READ( SUBSTRS(1), * ) ND67
      CALL SET_TINDEX( 67, ND67, SUBSTRS(2:N), N-1, PD67 )

      !--------------------------
      ! ND70: Debug info
      !--------------------------
      CALL SPLIT_ONE_LINE( SUBSTRS, N, -1, 'read_diagnostic_menu:68' )
      READ( SUBSTRS(1), * ) ND70
      LPRT = ( ND70 > 0 )
      CALL SET_TINDEX( 70, ND70, SUBSTRS(2:N), N-1, PD70 )
     
      ! Expand YYYYMMDD tokens in the bpch file name
      CALL EXPAND_DATE( BPCH_FILE, GET_NYMDb(), GET_NHMSb() )

      ! Open the binary punch file for output 
      CALL OPEN_BPCH2_FOR_WRITE( IU_BPCH, BPCH_FILE )

      END SUBROUTINE READ_DIAGNOSTIC_MENU
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: set_tindex
!
! !DESCRIPTION: Subroutine SET\_TINDEX sets the TINDEX and TMAX arrays, 
!  which determine how many tracers to print to the punch file. 
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE SET_TINDEX( N_DIAG, L_DIAG, SUBSTRS, N, NMAX )
!
! !USES:
!
#     include "CMN_SIZE"  ! Size parameters
#     include "CMN_DIAG"  ! TMAX, TINDEX
!
! !INPUT PARAMETERS: 
!
      INTEGER,            INTENT(IN) :: N_DIAG      ! GEOS-Chem diagnostic #
      INTEGER,            INTENT(IN) :: N           ! # of valid substrs passed
      INTEGER,            INTENT(IN) :: NMAX        ! Max # of tracers allowed
      INTEGER,            INTENT(IN) :: L_DIAG      ! # of levels to save
      CHARACTER(LEN=255), INTENT(IN) :: SUBSTRS(N)  ! Substrs passed from
                                                    !  READ_DIAGNOSTIC_MENU
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Bug fix: now do not drop the last tracer number if "all" is not
!        explicitly specified (tmf, bmy, 11/15/04)
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL, SAVE :: FIRST = .TRUE.
      LOGICAL       :: IS_ALL 
      INTEGER       :: M

      !=================================================================
      ! SET_TINDEX begins here!
      !=================================================================     

      ! Error check
      IF ( N < 1 ) THEN
         WRITE( 6, '(a)' ) 'ERROR: N must be 1 or greater!'
         WRITE( 6, '(a)' ) 'STOP in SET_TINDEX (input_mod.f)'
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         STOP
      ENDIF

      !=================================================================
      ! If the word "all" is present, then set TMAX, TINDEX to all
      ! available tracers for the given diagnostic.  Otherwise, just
      ! use the tracers that were read in from the line
      !=================================================================
      IF ( TRIM( SUBSTRS(1) ) == 'all'  .or. 
     &     TRIM( SUBSTRS(1) ) == 'ALL' ) THEN 

         ! TMAX is the max # of tracers to print out
         TMAX(N_DIAG) = NMAX 

         ! Fill TINDEX with all possible diagnostic tracer numbers
         DO M = 1, TMAX(N_DIAG)
            TINDEX(N_DIAG,M) = M
         ENDDO

         ! Set flag
         IS_ALL = .TRUE. 

      ELSE 

         ! Otherwise, set TMAX, TINDEX to the # of tracers
         ! listed in "input.ctm" -- need some error checks too
         TMAX(N_DIAG) = N

         ! Use explicit DO-loop
         DO M = 1, N
            READ( SUBSTRS(M:M), * ) TINDEX(N_DIAG,M)
         ENDDO

         ! Set flag
         IS_ALL = .FALSE.

      ENDIF

      !=================================================================
      ! Print to screen
      !=================================================================

      ! First-time printing only
      IF ( FIRST ) THEN 
         WRITE( 6, '(/,a)' ) 'DIAGNOSTIC MENU'
         WRITE( 6, '(  a)' ) '---------------'
         WRITE( 6, '(  a)' ) 'Diag    L   Tracers being saved to disk'
         FIRST = .FALSE.
      ENDIF

      ! Test if all tracers are being printed out
      IF ( IS_ALL ) THEN

         ! Print abbreviated output string
         IF ( L_DIAG > 0 ) THEN
            WRITE( 6, 100 ) N_DIAG, L_DIAG, 1, TMAX(N_DIAG)
 100        FORMAT( 'ND', i2.2, 2x, i3, 1x, i3, ' -', i3 ) 
         ENDIF

      ELSE

         ! Or just list each tracer
         ! Print each diagnostic and # of tracers that will print out
         IF ( L_DIAG > 0 ) THEN 
            WRITE( 6, 110 ) N_DIAG, L_DIAG, 
     &                      ( TINDEX(N_DIAG,M), M=1,TMAX(N_DIAG) )
 110        FORMAT( 'ND', i2, 2x, i3, 1x, 100i3 ) 
         ENDIF

      ENDIF

      END SUBROUTINE SET_TINDEX
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_nd49_menu
!
! !DESCRIPTION: Subroutine READ\_ND49\_MENU reads the ND49 MENU section of 
!  the GEOS-Chem input file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE READ_ND49_MENU
!
! !USES:
!
      USE DIAG49_MOD, ONLY : INIT_DIAG49
      USE ERROR_MOD,  ONLY : ERROR_STOP

#     include "CMN_SIZE"   ! Size parameters
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL             :: DO_ND49
      INTEGER             :: N,    I,         AS
      ! Increased to 121 from 100 (mpb,2009)
      INTEGER             :: ND49, N_TRACERS, TRACERS(121)
      INTEGER             :: IMIN, IMAX,      FREQ
      INTEGER             :: JMIN, JMAX,      N_ND49
      INTEGER             :: LMIN, LMAX
      CHARACTER(LEN=255)  :: SUBSTRS(MAXDIM), MSG
      CHARACTER(LEN=255)  :: FILE

      !=================================================================
      ! READ_ND49_MENU begins here!
      !=================================================================

      ! Initialize
      ND49       = 0
      TRACERS(:) = 0

      ! Turn on ND49 diagnostic
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1,  'read_nd49_menu:1' )
      READ( SUBSTRS(1:N), * ) DO_ND49

      ! Instantaneous 3-D timeseries file
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1,  'read_nd49_menu:2' )
      READ( SUBSTRS(1:N), '(a)' ) FILE

      ! Tracers to include
      CALL SPLIT_ONE_LINE( SUBSTRS, N_ND49, -1, 'read_nd49_menu:3' )
      DO N = 1, N_ND49
         READ( SUBSTRS(N), * ) TRACERS(N)
      ENDDO

      ! FREQ
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1,  'read_nd49_menu:4' )
      READ( SUBSTRS(1:N), * ) FREQ

      ! IMIN, IMAX
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 2,  'read_nd49_menu:5' )
      READ( SUBSTRS(1:N), * ) IMIN, IMAX

      ! JMIN, JMAX
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 2,  'read_nd49_menu:6' )
      READ( SUBSTRS(1:N), * ) JMIN, JMAX

      ! LMIN, LMAX
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 2,  'read_nd49_menu:7' )
      READ( SUBSTRS(1:N), * ) LMIN, LMAX

      ! Separator line
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1,  'read_nd49_menu:8' )

      !=================================================================
      ! Print to screen
      !=================================================================
      WRITE( 6, '(/,a)' ) 'ND49 3-D INSTANTANEOUS TIMESERIES MENU'
      WRITE( 6, '(  a)' ) '--------------------------------------'
      WRITE( 6, 100     ) 'Turn on ND49 timeseries?    : ', DO_ND49
      WRITE( 6, 110     ) 'ND49 timeseries file name   : ', TRIM( FILE )
      WRITE( 6, 120     ) 'ND49 timeseries tracers     : ', 
     &                     ( TRACERS(N), N=1, N_ND49 )
      WRITE( 6, 130     ) 'ND49 save frequency [min]   : ', FREQ
      WRITE( 6, 130     ) 'ND49 longitude limits       : ', IMIN, IMAX
      WRITE( 6, 130     ) 'ND49 latitude  limits       : ', JMIN, JMAX
      WRITE( 6, 130     ) 'ND49 level     limits       : ', LMIN, LMAX

      ! FORMAT statements
 100  FORMAT( A, L5    )
 110  FORMAT( A, A     )
 120  FORMAT( A, 100I3 )
 130  FORMAT( A, 2I5   )

      !=================================================================
      ! Call setup routines from other F90 modules
      !=================================================================

      ! Initialize for ND49 timeseries
      CALL INIT_DIAG49( DO_ND49, N_ND49, TRACERS, IMIN, 
     &                  IMAX,    JMIN,   JMAX,    LMIN,    
     &                  LMAX,    FREQ,   FILE )

      END SUBROUTINE READ_ND49_MENU
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_unix_cmds_menu
!
! !DESCRIPTION: Subroutine READ\_UNIX\_CMDS\_MENU reads the UNIX CMDS MENU 
!  section of the GEOS-Chem input file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE READ_UNIX_CMDS_MENU
!
! !USES:
!
      USE CHARPAK_MOD,   ONLY : STRSQUEEZE
      USE UNIX_CMDS_MOD, ONLY : BACKGROUND, REDIRECT,  REMOVE_CMD 
      USE UNIX_CMDS_MOD, ONLY : SEPARATOR,  SPACE,     UNZIP_CMD
      USE UNIX_CMDS_MOD, ONLY : WILD_CARD,  ZIP_SUFFIX 
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL            :: EOF
      INTEGER            :: N
      CHARACTER(LEN=255) :: SUBSTRS(MAXDIM)

      !=================================================================
      ! READ_UNIX_CMDS_MENU begins here!
      !=================================================================

      ! Background
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_unix_cmds_menu:1' )
      READ( SUBSTRS(1:N), '(a)' ) BACKGROUND

      ! Redirect
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_unix_cmds_menu:2' )
      READ( SUBSTRS(1:N), '(a)' ) REDIRECT

      ! Remove command
      REMOVE_CMD = READ_ONE_LINE( EOF,    'read_unix_cmds_menu:3' ) 
      REMOVE_CMD = REMOVE_CMD(FIRSTCOL:)
      CALL STRSQUEEZE( REMOVE_CMD )

      ! Separator
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_unix_cmds_menu:4' )
      READ( SUBSTRS(1:N), '(a)' ) SEPARATOR

      ! Wild Card
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_unix_cmds_menu:5' )
      READ( SUBSTRS(1:N), '(a)' ) WILD_CARD

      ! Unzip command
      UNZIP_CMD = READ_ONE_LINE( EOF,     'read_unix_cmds_menu:6' ) 
      UNZIP_CMD = UNZIP_CMD(FIRSTCOL:)
      CALL STRSQUEEZE( UNZIP_CMD )

      ! Zip suffix
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_unix_cmds_menu:7' )
      READ( SUBSTRS(1:N), '(a)' ) ZIP_SUFFIX

      ! Separator line
      CALL SPLIT_ONE_LINE( SUBSTRS, N, 1, 'read_unix_cmds_menu:8' )

      ! Just hardwire the SPACE character
      SPACE = ' '

      !=================================================================
      ! Print to screen
      !=================================================================
      WRITE( 6, '(/,a)' ) 'UNIX CMDS MENU'
      WRITE( 6, '(  a)' ) '---------------'            
      WRITE( 6, 100     ) 'Unix BACKGROUND  command    : ', 
     &                    TRIM( BACKGROUND )
      WRITE( 6, 100     ) 'Unix REDIRECT    command    : ', 
     &                    TRIM( REDIRECT   )
      WRITE( 6, 100     ) 'Unix REMOVE      command    : ',
     &                    TRIM( REMOVE_CMD )
      WRITE( 6, 100     ) 'Unix SEPARATOR   command    : ',
     &                    TRIM( SEPARATOR  )
      WRITE( 6, 100     ) 'Unix WHITE SPACE command    : ',
     &                    TRIM( SPACE      )
      WRITE( 6, 100     ) 'Unix WILD CARD   command    : ',
     &                    TRIM( WILD_CARD  )
      WRITE( 6, 100     ) 'Unix UNZIP       command    : ',
     &                    TRIM( UNZIP_CMD  )
      
      ! FORMAT statements
 100  FORMAT( A, A )

      END SUBROUTINE READ_UNIX_CMDS_MENU
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: check_time_steps
!
! !DESCRIPTION: Subroutine CHECK\_TIME\_STEPS computes the smallest dynamic 
!  time step for the model, based on which operation are turned on.  This 
!  is called from routine READ\_INPUT\_FILE, after all of the timesteps and 
!  logical flags have been read from "input.geos".
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE CHECK_TIME_STEPS
!
! !USES:
!
      USE TIME_MOD,    ONLY : SET_TIMESTEPS
! 
!
! !LOCAL VARIABLES:
!
      INTEGER :: I,           J,       K,       L
      INTEGER :: TS_SMALLEST, TS_DIAG, TS_SUN_2     

      ! Compute NSMALLEST as the minimum of NDYN, NCONV, NSRCE, NCHEM 
      TS_CONV = TS_DYN
      TS_EMIS = TS_DYN
      TS_CHEM = TS_DYN
      TS_DIAG = TS_DYN
      TS_UNIT = MAX( TS_DYN, TS_CONV )
      TS_SUN_2= TS_CHEM / 2

      ! Initialize timesteps in "time_mod.f"
      CALL SET_TIMESTEPS( CHEMISTRY  = TS_CHEM, EMISSION  = TS_EMIS, 
     &                    DYNAMICS   = TS_DYN,  UNIT_CONV = TS_UNIT,
     &                    CONVECTION = TS_CONV, DIAGNOS   = TS_DIAG,
     &                    SUNCOS     = TS_SUN_2 )

      

      END SUBROUTINE CHECK_TIME_STEPS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: is_last_day_good
!
! !DESCRIPTION: Suborutine IS\_LAST\_DAY\_GOOD tests to see if there is 
!  output scheduled on the last day of the run. 
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE IS_LAST_DAY_GOOD
!
! !USES:
!
      USE ERROR_MOD,  ONLY : ERROR_STOP
      USE JULDAY_MOD, ONLY : JULDAY
      USE TIME_MOD,   ONLY : GET_NYMDe, ITS_A_LEAPYEAR, YMD_EXTRACT

#     include "CMN_SIZE"   ! Size parameters
#     include "CMN_DIAG"   ! NJDAY
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Moved to "input_mod.f" from "main.f" (bmy, 1/11/05)
!  (2 ) Now call ITS_A_LEAPYEAR with FORCE=.TRUE. to always return whether
!        the year Y would be a leap year, regardless of met field type.
!        (swu, bmy, 4/24/06)
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL :: IS_LEAPYEAR
      INTEGER :: NYMDe, Y, M, D, LASTDAY
      REAL*8  :: JD, JD0

      !=================================================================
      ! IS_LAST_DAY_GOOD begins here!
      !=================================================================

      ! Astronomical Julian Day corresponding to NYMDe
      NYMDe = GET_NYMDe()
      CALL YMD_EXTRACT( NYMDe, Y, M, D )
      JD = JULDAY( Y, M, DBLE( D ) )

      ! Astronomical Julian Day corresponding to the 1st of the year
      JD0 = JULDAY( Y, 1, 0d0 )

      ! LASTDAY is the day of year corresponding to NYMDe      
      LASTDAY = JD - JD0

      ! Skip past the element of NJDAY for Feb 29, if necessary
      IF ( .not. ITS_A_LEAPYEAR( Y, .TRUE. ) .and. LASTDAY > 59 ) THEN
         LASTDAY = LASTDAY + 1
      ENDIF

      ! Stop w/ error if THIS_NJDAY = 0 
      IF ( NJDAY(LASTDAY) == 0 ) THEN
         CALL ERROR_STOP( 'No output scheduled on last day of run!',
     &                    'IS_LAST_DAY_GOOD ("input_mod.f")' )
      ENDIF
     
      END SUBROUTINE IS_LAST_DAY_GOOD
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_input
!
! !DESCRIPTION: Subroutine INIT\_INPUT initializes all variables from 
!  "directory\_mod.f" and "logical\_mod.f" for safety's sake.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE INIT_INPUT
!
! !USES:
!
      USE DIRECTORY_MOD, ONLY : DATA_DIR,   GEOS_1_DIR, GEOS_S_DIR 
      USE DIRECTORY_MOD, ONLY : GEOS_3_DIR, GEOS_4_DIR, TEMP_DIR   
      USE DIRECTORY_MOD, ONLY : RUN_DIR,    OH_DIR,     O3PL_DIR   
      USE DIRECTORY_MOD, ONLY : TPBC_DIR,   DATA_DIR_1x1
      USE LOGICAL_MOD,   ONLY : LATEQ,      LAVHRRLAI,  LCARB      
      USE LOGICAL_MOD,   ONLY : LDEAD,      LDUST,      LSULF      
      USE LOGICAL_MOD,   ONLY : LSOA,       LSSALT,     LCHEM      
      USE LOGICAL_MOD,   ONLY : LCONV,      LDBUG      
      USE LOGICAL_MOD,   ONLY : LDIAG,      LPRT,       LSTDRUN    
      USE LOGICAL_MOD,   ONLY : LDRYD,      LAIRNOX,    LANTHRO    
      USE LOGICAL_MOD,   ONLY : LBIONOX,    LBIOMASS,   LBIOFUEL   
      USE LOGICAL_MOD,   ONLY : LBIOGENIC,  LBBSEA,     LEMIS      
      USE LOGICAL_MOD,   ONLY : LFFNOX,     LFOSSIL,    LLIGHTNOX  
      USE LOGICAL_MOD,   ONLY : LMONOT,     LNEI99,     LSHIPSO2   
      USE LOGICAL_MOD,   ONLY : LSOILNOX,   LTOMSAI,    LWOODCO     
!##!modified by chenchuchu
      USE LOGICAL_MOD,   ONLY : LSOILNH3
      USE LOGICAL_MOD,   ONLY : LFILL,      LMFCT,      LTRAN      
      USE LOGICAL_MOD,   ONLY : LTPFV,      LUPBD,      LWINDO     
      USE LOGICAL_MOD,   ONLY : LUNZIP,     LWAIT,      LTURB      
      USE LOGICAL_MOD,   ONLY : LSVGLB,     LSPLIT,     LWETD 
      USE LOGICAL_MOD,   ONLY : LMEGAN,     LMEGANMONO, LDYNOCEAN
      USE LOGICAL_MOD,   ONLY : LGFED2BB,   LFUTURE,    LEDGAR
      USE LOGICAL_MOD,   ONLY : LEDGARNOx,  LEDGARCO,   LEDGARSHIP
      USE LOGICAL_MOD,   ONLY : LEDGARSOx,  LVARTROP
      USE LOGICAL_MOD,   ONLY : LOTDLOC
      USE LOGICAL_MOD,   ONLY : LEMEP
      USE LOGICAL_MOD,   ONLY : LNEI05,     LPREINDHG
      USE LOGICAL_MOD,   ONLY : LSVCSPEC 
      USE LOGICAL_MOD,   ONLY : LLINOZ
      USE LOGICAL_MOD,   ONLY : LMODISLAI,   LPECCA
      USE LOGICAL_MOD,   ONLY : LGENFF,      LANNFF,      LMONFF
      USE LOGICAL_MOD,   ONLY : LSEASBB,     LBIODAILY,   LBIODIURNAL 
      USE LOGICAL_MOD,   ONLY : LBIONETORIG, LBIONETCLIM
      USE LOGICAL_MOD,   ONLY : LOCN1997,    LOCN2009ANN, LOCN2009MON
      USE LOGICAL_MOD,   ONLY : LFFBKGRD
      USE LOGICAL_MOD,   ONLY : LBIOSPHTAG,  LFOSSILTAG
      USE LOGICAL_MOD,   ONLY : LSHIPEDG,    LSHIPICO,    LPLANE
      USE LOGICAL_MOD,   ONLY : LSHIPSCALE,  LPLANESCALE
      USE LOGICAL_MOD,   ONLY : LSHIPTAG,    LPLANETAG
      USE LOGICAL_MOD,   ONLY : LCHEMCO2
! 
! !REVISION HISTORY: 
!  20 Jul 2004 - R. Yantosca - Initial version
!  (1 ) Now also initialize LNEI99 from "logical_mod.f" (bmy, 11/5/04)
!  (2 ) Now also initialize LAVHRRLAI from "logical_mod.f" (bmy, 12/20/04)
!  (3 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  (4 ) Now also initialize LMEGAN switch (tmf, bmy, 10/20/05)
!  (5 ) Now also initialize LEMEP, LGFED2BB switches and DATA_DIR_1x1
!        directory (bmy, 4/5/06)
!  (6 ) Now also intitialize LFUTURE (swu, bmy, 6/1/06)
!  (7 ) Now reference the EDGAR logical switches from "logical_mod.f"
!        (avd, bmy, 7/11/06)
!  (8 ) Now initialize the LVARTROP switch (phs, 9/14/06)
!  (9 ) Now initialize LOTDREG, LOTDLOC, LCTH, LMFLUX, LPRECON (bmy, 1/31/07)
!  (10) Now initialize LOTDSCALE (ltm, bmy, 9/24/07)
!  (11) Add MEGAN Monoterpenes switch (ccc, 2/2/09)
!  16 Oct 2009 - R. Yantosca - Now initialize LLINOZ
!  19 Nov 2009 - C. Carouge  - Initialize LMODISLAI and LPECCA
!  01 Dec 2009 - C. Carouge  - Initialize LNEI05 
!  27 Aug 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
      !=================================================================
      ! INIT_INPUT begins here!
      !=================================================================

      ! Initialize directories
      DATA_DIR     = ''
      DATA_DIR_1x1 = ''
      GEOS_1_DIR   = ''
      GEOS_S_DIR   = ''
      GEOS_3_DIR   = ''
      GEOS_4_DIR   = ''
      TEMP_DIR     = ''
      RUN_DIR      = ''
      OH_DIR       = ''
      O3PL_DIR     = ''
      TPBC_DIR     = ''

      ! Initialize logicals
      LATEQ        = .FALSE.
      LAVHRRLAI    = .FALSE.
      LCARB        = .FALSE.
      LDEAD        = .FALSE.
      LDUST        = .FALSE.
      LSULF        = .FALSE.
      LSOA         = .FALSE.
      LSSALT       = .FALSE.
      LCHEM        = .FALSE.
      LCONV        = .FALSE.
      LDBUG        = .FALSE.
      LDIAG        = .FALSE.
      LPRT         = .FALSE.
      LSTDRUN      = .FALSE.
      LDRYD        = .FALSE.
      LAIRNOX      = .FALSE.
      LANTHRO      = .FALSE.
      LBIONOX      = .FALSE.
      LBIOMASS     = .FALSE.
      LBIOFUEL     = .FALSE.
      LBIOGENIC    = .FALSE.
      LBBSEA       = .FALSE.
      LDYNOCEAN    = .FALSE.
      LEMEP        = .FALSE.
      LEMIS        = .FALSE.
      LEDGAR       = .FALSE.
      LEDGARNOx    = .FALSE. 
      LEDGARCO     = .FALSE. 
      LEDGARSHIP   = .FALSE. 
      LEDGARSOx    = .FALSE. 
      LFFNOX       = .FALSE.
      LFOSSIL      = .FALSE.
      LFUTURE      = .FALSE.
      LGFED2BB     = .FALSE.
      LLIGHTNOX    = .FALSE.
      LMEGAN       = .FALSE.
      LMEGANMONO   = .FALSE.
      LMONOT       = .FALSE.
      LNEI99       = .FALSE.
      LNEI05       = .FALSE.
      LOTDLOC      = .FALSE.
      LSHIPSO2     = .FALSE.
      LSOILNOX     = .FALSE.
!##!modified by chenchuchu
      LSOILNH3     = .FALSE.
      LTOMSAI      = .FALSE.
      LWOODCO      = .FALSE.
      LFILL        = .FALSE.
      LMFCT        = .FALSE.
      LTRAN        = .FALSE.
      LTPFV        = .FALSE.
      LUPBD        = .FALSE.
      LWINDO       = .FALSE.
      LUNZIP       = .FALSE.
      LWAIT        = .FALSE.
      LTURB        = .FALSE.
      LSVGLB       = .FALSE.
      ! >> (dkh, 02/12/09) 
      LSVCSPEC     = .FALSE.
      ! << 
      LSPLIT       = .FALSE.
      LWETD        = .FALSE.
      LVARTROP     = .FALSE.
      LLINOZ       = .FALSE.
      LPREINDHG    = .FALSE.

      !Specifically for CO2 simulation (R Nassar, 2009-03-02)
      LGENFF       = .FALSE.
      LANNFF       = .FALSE.
      LMONFF       = .FALSE.
      LSEASBB      = .FALSE.
      LBIONETORIG  = .FALSE.
      LBIONETCLIM  = .FALSE.
      LBIODAILY    = .FALSE.
      LBIODIURNAL  = .FALSE.
      LOCN1997     = .FALSE.
      LOCN2009ANN  = .FALSE.
      LOCN2009MON  = .FALSE.
      LFFBKGRD     = .FALSE.
      LBIOSPHTAG   = .FALSE.
      LFOSSILTAG   = .FALSE.
      LSHIPEDG     = .FALSE.
      LSHIPICO     = .FALSE.
      LSHIPSCALE   = .FALSE.
      LSHIPTAG     = .FALSE.
      LPLANE       = .FALSE.
      LPLANESCALE  = .FALSE.
      LPLANETAG    = .FALSE.
      LCHEMCO2     = .FALSE.
      
      ! Initialize counters
      CT1          = 0
      CT2          = 0
      CT3          = 0

      END SUBROUTINE INIT_INPUT
!EOC
      END MODULE INPUT_MOD
