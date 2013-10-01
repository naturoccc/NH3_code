! $Id: emissdr.f,v 1.20 2009/05/06 15:33:26 phs Exp $
      SUBROUTINE EMISSDR

      ! References to F90 modules
      !###Rynda Start
!     USE CANOPY_NOX_MOD,    ONLY : GET_CANOPY_NOX
!     USE DAO_MOD,           ONLY : SUNCOS
      USE DIAG_MOD,          ONLY : AD32_fe,     AD32_so
      USE DAO_MOD,           ONLY : PREACC, PRECON, TS, U10M, V10M
      USE DAO_MOD,           ONLY : BXHEIGHT,  GWETTOP !! Top soil wetness
!###Hong Start
      USE DAO_MOD,           ONLY : SFCWINDSQR
!###Hong End
      USE GRID_MOD,          ONLY : GET_XOFFSET,   GET_YOFFSET
      USE GRID_MOD,          ONLY : GET_YMID
      USE GRID_MOD,          ONLY : GET_XMID ! Just for testing 
!     USE LOGICAL_MOD,       ONLY : LSOILNOX  
!     USE SOIL_NOX_MOD,      ONLY : SOIL_NOX_EMISSION 
      USE LOGICAL_MOD,       ONLY : LSOILNH3
      USE SOIL_NH3_MOD,      ONLY : SOIL_NH3_EMISSION 
!###Hong Start
!     USE GET_NDEP_MOD,      ONLY : GET_DEP_N 
!###Hong End
      USE TIME_MOD,          ONLY : GET_MONTH
      USE TIME_MOD,          ONLY : GET_TS_EMIS
      USE TIME_MOD,          ONLY : GET_DAY_OF_YEAR
      !###Rynda End  
 
      IMPLICIT NONE

#     include "CMN_SIZE"     ! Size parameters
#     include "CMN_DIAG"     ! Diagnostic arrays and switches
#     include "CMN_NH3"      ! GEMISNH32
!#    include "CMN_NOX"      ! GEMISNOX2

      !###Rynda Start
#     include "CMN_DEP"  ! IREG, ILAND, IUSE, etc.
#     include "commsoil.h" !Soil Variables
      !###Rynda End

      ! Local variables
      LOGICAL, SAVE          :: FIRSTEMISS = .TRUE. 
      INTEGER                :: I, J, L, N, IJLOOP 
      INTEGER                :: I0, J0, IOFF, JOFF, IREF, JREF
      !###Rynda Start
      INTEGER                :: DOY, MONTH, RC, M
      REAL*8                 :: LAT, TS_EMIS, FERTDIAG
      ! Just for testing
      REAL*8                 :: LON
      ! REAL*8                 :: TOTALPULSE, SOILPRC(2)
      REAL*8                 :: SOILFRT
      REAL*8                 :: DEP_FERT, TEMP_FERT
      !###Rynda End ERROR! CHECK THI
!###Hong Start
      REAL*8                 :: WINDSQR
!###Hong End

!
!******************************************************************************
!  EMISSDR begins here!
!******************************************************************************
!
      ! Get nested-grid offsets
      I0 = GET_XOFFSET()
      J0 = GET_YOFFSET()


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! ### Rynda Start
! SOIL EMISSIONS NH3 [molecules/cm3/s]
! Pulled call to soil NH3 mod out of I,J, Loop to just loop over land boxes

       WRITE(6,*) '# DYNTS' , TEST_DYNTS

      IF ( LSOILNH3 ) THEN
               
!###Hong Start
         ! Just for now initialize Pfactor and DrySpell this way 
         ! first time only!
         IF ( FIRSTEMISS ) THEN
           DO J = 1, JGLOB
           DO I = 1, IGLOB
             PFACTOR(I,J)   = 0.0 ! Pulse Factor
             DRYPERIOD(I,J) = 0.0 ! Dry period length
             GWET_PREV(I,J) = 0.0 ! Pulse Factor
             INST_SOIL(I,J) = 0.0 ! instantaneous soil nh3
             INST_FERT(I,J) = 0.0 ! instantaneous fert nh3
             FIRSTEMISS     = .FALSE.
           ENDDO
           ENDDO
         ENDIF
!###Hong End

         !***************************************************************
         !first zero the arrays in which emissions will be stored
         !GEMISNOX2(I,J)   = Array which stores NOx emissions from soils.
         !                    Units are [molec NOx/cm3/s].
         !GEMISNH32(I,J)   = Array which stores NH3 emissions from soils.
         !                    Units are [molec NH3/cm3/s].
         GEMISNH32   = 0d0
         !***************************************************************

         ! Now need to call GET_CANOPY_NOX to break ugly dependency between
         ! drydep and soil NOx emissions. (bmy, 6/22/09) 
         ! Compute the CANOPYNOX array in commsoil.h
!        CALL GET_CANOPY_NOX
               
         !Get day of year/month and timestep information
         DOY     = GET_DAY_OF_YEAR() 
         TS_EMIS = GET_TS_EMIS()  !returns the emission timestep in minutes         
         MONTH   = GET_MONTH()

         ! Loop over each land grid-box
         DO M = 1, NLAND
            IREF   = INDEXSOIL(1,M)
            JREF   = INDEXSOIL(2,M)
            I      = IREF - I0
            J      = JREF - J0
            IJLOOP = ( (J-1) * IIPAR ) + I

            ! Get Lat/Lon
            LAT     = GET_YMID( J )
            LON     = GET_XMID( I ) ! Just for testing

!###Hong Start
            ! Get Deposited Fertilizer
            !CALL GET_DEP_N( I, J, TS_EMIS, DEP_FERT )
            !Ryn Change for testing without deposition
            DEP_FERT = 0.d0
            CALL FLUSH(6)
!###Hong End

            ! Get New Fertilizer 
            !SOILFRT =  SOILFERT(M) 
            SOILFRT =  SOILFERT( GET_DAY_OF_YEAR() + (M-1)*365) 

            ! Just this month and previous month
!           SOILPRC(1) =  SOILPREP(1,M) 
!           SOILPRC(2) =  SOILPREP(2,M) 

            !Put in constraint if dry period gt 1 yr, keep at 1yr to
            !avoid unrealistic pulse
!           IF ( DRYPERIOD(I,J) .GT. 8760.) DRYPERIOD(I,J) = 8760.

!###Hong Start
            ! Surface wind speed, squared
#     include "define.h"
#if   defined( GEOS_5 )
            WINDSQR = U10M(I,J)**2 + V10M(I,J)**2
#endif
#if   defined( WRF )
            WINDSQR = SFCWINDSQR(I,J)
#endif

            ! Return NOx emission from soils [molec/cm2/s]
            CALL SOIL_NH3_EMISSION( DOY,                
     &                              LAT,      
     &                              MONTH,               
     &                              TS_EMIS,
     &                              IREG(I,J),     
     &                              ILAND(I,J,:),
     &                              IUSE(I,J,:),                
     &                              XLAI(I,J,:),
     &                              PRECON(I,J),          
     &                              PREACC(I,J),
     &                              TS(I,J),               
!     &                              SUNCOS(IJLOOP),
     &                              GWETTOP(I,J),
!     &                              U10M(I,J),             
!     &                              V10M(I,J),
     &                              WINDSQR,
!     &                              CANOPYNOX(IJLOOP,:), 
     &                              SOILFRT,  
     &                              GWET_PREV(I,J),
     &                              DRYPERIOD(I,J),
     &                              PFACTOR(I,J),
     &                              GEMISNH32(I,J),
     &                              DEP_FERT, 
     &                              FERTDIAG, 
     &                              CLIM(M),
     &                              RC , 
     &                              I, 
     &                              J)
!###Hong End

            ! Need to add this
            !Archive emissions
            IF ( ND32 > 0 ) THEN
! Soil NOx diag is only Soil NOx (fertilizer NOx is separate)(Hong)
              AD32_so(I,J) = AD32_so(I,J) + GEMISNH32(I,J)
              AD32_fe(I,J) = AD32_fe(I,J) + FERTDIAG
            ENDIF
            IF ( I == 69 .and. J == 51 ) THEN 
            WRITE(6,*) LON, LAT,  ' SOIL TOT = ', GEMISNH32(I,J)
            WRITE(6,*) LON, LAT,  ' DIAG TOT = ', AD32_so(I,J)
            WRITE(6,*) LON, LAT,  ' FERT TOT = ', AD32_fe(I,J)
            WRITE(6,*) LON, LAT,  ' DEP FERT = ', DEP_FERT
            ENDIF
!###Hong Start
! INST_SOIL is only Soil NOx (fertilizer NOx is separate)(Hong)
            INST_SOIL(I,J) = GEMISNH32(I,J) !###Rynda Diag49
            INST_FERT(I,J) = FERTDIAG !###Rynda Diag49

!            GEMISNOX2(I,J) = GEMISNOX2(I,J) / 
!     &                        ( BXHEIGHT(I,J,1) * 100d0 )
!###Hong End

           
         ENDDO


         !Zero arrays. This code assumes that ts_chem = ts_emiss
         DRY_HNO3 = 0.d0
         DRY_NH4  = 0.d0
         DRY_NH3  = 0.d0
         DRY_NIT  = 0.d0
         DRY_NO2  = 0.d0
         DRY_PAN  = 0.d0

         WET_HNO3 = 0.d0
         WET_NH4  = 0.d0
         WET_NH3  = 0.d0
         WET_NIT  = 0.d0


         TEST_DYNTS = 0


      ENDIF

! ###Rynda End


      ! Return to calling program
      END SUBROUTINE EMISSDR
