! $Id: soil_nh3_mod_loop.f,v 1.2 2013/05/27  ccc Exp $
!BOP
!
! !MODULE: soil_nh3_mod.f
!
! !DESCRIPTION: Module containing GEOS-Chem soil NH3 emissions routines.
!
!
! !INTERFACE: 
!188424.00
      MODULE SOIL_NH3_MOD
! 
! !USES:
!
      IMPLICIT NONE
      PRIVATE
   
#     include "lai_land_info.h"
#     include "smv_errcode.h"
#     include "CMN_SIZE"
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC  :: SOIL_NH3_EMISSION
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PRIVATE :: SOILTEMP
!      PRIVATE :: SOILPH
      PRIVATE :: SOILWIND
      PRIVATE :: FERTADD
       
!fertilizer file name
!  New:
!  ENOx   = f( T, WIND, PH, Fert)  x Pulse(dryspell) x canopy uptake 
!
!  ENH3   = f( T, WIND, PH, Fert)  x Method  x
!  2 - Update to Fertilizer:  new fertilizer maps including chemical and 
!  manure fertilizer from Potter et al., [2010] distributed using MODIS EVI 
!  seasonality, online-N deposition as a fertilizer source, and N-fertilizer
!  source subject to T, WFPS, and pulsing like other N (impact = +1.3 Tg N/yr)
!
!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !DEFINED PARAMETERS:
!
      ! Conversion factor from [ng N/m2/s] to [molec/cm2/s]
      !REAL*8,  PARAMETER :: UNITCONV = 4.3d9
      ! Conversion factor from [ng N/m2/s] to [kg/m2/yr]
      ! REAL*8,  PARAMETER :: UNITCONV = 441.504
      REAL*8,  PARAMETER :: UNITCONV = 31.536

      ! Number of soil biome types
      INTEGER, PARAMETER :: NSOIL    = 11

      ! Number of soil pulsing types (excluding "no pulsing")
!     INTEGER, PARAMETER :: NPULSE   = 3  
                                                                             .
      ! Soil biome types corresponding to each of the 74 Olson land types.  
      ! These were contained in the input file "soiltype.dat", but we can 
      ! hard-code these here as these are not likely to change anytime soon.
      INTEGER, PARAMETER :: NCONSOIL(N_OLSON_TYPES) =               (/ 
     &   1, 1, 7, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 6, 1, 1, 1,
     &   3, 3, 3, 3, 5, 5, 5, 3, 6, 5, 8, 8, 4, 2, 1, 1, 9, 8, 8, 8,
     &   7, 7, 7, 7,11,11, 6, 6, 6, 6, 1, 1, 6,10, 2, 6, 5, 5, 6, 6,
     &   3, 3, 3, 7, 7, 1, 1, 1, 1, 1, 1, 1,11, 1                   /)

      !  There are 11 soil land biomes:======================
      !     (1 ) No soils (e.g. water, desert, ice)
      !     (2 ) Tropical rainforest
      !     (3 ) Coniferous trees
      !     (4 ) Dry deciduous trees
      !     (5 ) Other deciduous trees
      !     (6 ) Woodland
      !     (7 ) Grassland
      !     (8 ) Agriculture (other than rice)
      !     (9 ) Rice paddies
      !     (10)!  \begin{itemize} Wetland
      !     (11) Tundra
   
      !=================================================================
      ! Variables which define quantities for each soil biome type
      ! (cf Yienger & Levy [1995], Sec 4.2ff)
      !=================================================================

      ! "A" coefficients for converting surface temp to soil temp
      ! for each of the 11 soil biomes
      REAL*8,  PARAMETER :: SOILTA(NSOIL)     = (/ 0.d0,    0.84d0,  
     &                                             0.84d0,  0.84d0,  
     &                                             0.84d0,  0.66d0, 
     &                                             0.66d0,  1.03d0,  
     &                                             1.03d0,  0.92d0,  
     &                                             0.66d0           /)

      ! "B" coefficients for converting surface temp to soil temp
      ! for each of the 11 soil biomes
      REAL*8,  PARAMETER :: SOILTB(NSOIL)     = (/ 0.d0,    3.6d0,   
     &                                             3.6d0,   3.6d0,   
     &                                             3.6d0,   8.8d0,     
     &                                             8.8d0,   2.9d0,   
     &                                             2.9d0,   4.4d0,   
     &                                             8.8d0            /)
  

      ! Canopy wind extinction coefficients 
      ! (cf. Yienger & Levy [1995], Sec 5)
      REAL*8,  PARAMETER :: SOILEXC(NSOIL)    = (/ 0.1d0,   4.d0,    
     &                                             4.d0,    4.d0,    
     &                                             4.d0,    2.d0,  
     &                                             1.d0,    2.d0,    
     &                                             2.d0,    0.5d0,   
     &                                             0.1d0            /)

 
      ! Only wet biome coefficients used  and scaled based on WFPS
      ! Wet biome coefficient
      REAL*8,  PARAMETER :: SOILAW(NSOIL)     = (/ 0.d0,    0.2d0,   
     &                                             0.03d0,  0.06d0,  
     &                                             0.03d0,  0.17d0,     
     &                                             0.36d0,  0.36d0,  
     &                                             0.36d0,  0.003d0, 
     &                                             0.05d0           /)


      ! Scale factor so that fertilizer emission = 1.8 Tg N/yr 
      ! before canopy reduction
#if   defined( GRID4x5  ) 
      REAL*8, PARAMETER :: FERT_SCALE = 1.  ! not correct
#elif defined( GRID2x25 )
      REAL*8, PARAMETER :: FERT_SCALE =  0.025 / 4.06
!###Hong Start
#elif defined( GRID05x0666 )
      REAL*8, PARAMETER :: FERT_SCALE =  0.025 / 4.06
#elif defined( GRID36x36 )
      REAL*8, PARAMETER :: FERT_SCALE =  0.025 / 4.06
      !REAL*8, PARAMETER :: FERT_SCALE =  1
!###Hong End
#endif

      CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: soil_nox_emission
!
! !DESCRIPTION: Subroutine SOIL_NOX_EMISSION computes the emission of soil and
!  fertilizer NOx for the GEOS-Chem model.
!\\
!\\
! !INTERFACE:
!
!###Hong Start
      SUBROUTINE SOIL_NH3_EMISSION( DOY,       LAT,      MONTH, 
     &                              TS_EMIS,   IREG,     ILAND,     
     &                              IUSE,      LAI,      PRECCON, 
     &                              PRECTOT,   TK,       
!     &							  SUNCOS,
!     &                              GWET,      U10M,     V10M,  
     &                              GWET,      WINDSQR,
!     &                              R_CANOPY, 
     &                              SOILFRT,  GWET_PREV, 
     &                              DRYPERIOD, PFACTOR,  SOILNH3, 
     &                              DEPN,      FERTDIAG, 
     &                              CLIM, RC, I, J )
!###Hong End
!
! !INPUT PARAMETERS: 

      INTEGER, INTENT(IN)  :: DOY         ! Day of year (0-365 or 0-366)
      REAL*8,  INTENT(IN)  :: LAT         ! Latitude [degrees]
      INTEGER, INTENT(IN)  :: MONTH       ! Current month
      REAL*8,  INTENT(IN)  :: TS_EMIS     ! Emission timestep [min]
      INTEGER, INTENT(IN)  :: IREG        ! # of Olson land types per grid box
      INTEGER, INTENT(IN)  :: ILAND(:)    ! Olson land type indices in grid box
      INTEGER, INTENT(IN)  :: IUSE(:)     ! Fraction of Olson land type usage
      REAL*8,  INTENT(IN)  :: LAI(:)      ! Leaf area indices [cm2/cm2]
      REAL*8,  INTENT(IN)  :: PRECCON     ! Convective precip @ ground [mm/day]
      REAL*8,  INTENT(IN)  :: PRECTOT     ! Total precip @ ground [mm/day]
      REAL*8,  INTENT(IN)  :: TK          ! surface temperature [K]
!      REAL*8,  INTENT(IN)  :: SUNCOS      ! Cosine of solar zenith angle
      REAL*8,  INTENT(IN)  :: GWET        ! Top soil wetness [unitless]
!###Hong Start
!      REAL*8,  INTENT(IN)  :: U10M        ! E/W wind speed @ 10m altitude [m]
!      REAL*8,  INTENT(IN)  :: V10M        ! N/S wind speed @ 10m altitude [m]
      REAL*8,  INTENT(IN)  :: WINDSQR     ! wind speed, squared [m2/s2]
!###Hong End
!      REAL*8,  INTENT(IN)  :: R_CANOPY(:) ! Resist. of canopy to NOx [1/s]
      REAL*8,  INTENT(IN)  :: DEPN        ! Dry Dep Fert term [ng N/m2/s]
      REAL*8,  INTENT(IN)  :: SOILFRT     ! Fertilizer emissions [ng N/m2/s]
      INTEGER, INTENT(IN)  :: CLIM        ! Arid/SemiArid = 1
      INTEGER, INTENT(IN)  :: I ! Temporary
      INTEGER, INTENT(IN)  :: J ! Temporary       

! !OUTPUT PARAMETERS:
!
!      REAL*8,  INTENT(OUT)   :: SOILNOx   ! Soil NOx emissions [molec/cm2/s]
      REAL*8,  INTENT(OUT)   :: SOILNH3   ! Soil NH3 emissions [molec/cm2/s]
      REAL*4,  INTENT(INOUT) :: GWET_PREV ! Soil Moisture Prev timestep
      REAL*4,  INTENT(INOUT) :: DRYPERIOD ! Dry period length in hours
      REAL*4,  INTENT(INOUT) :: PFACTOR   ! Pulsing Factor
      REAL*8,  INTENT(OUT)   :: FERTDIAG  ! Fert emissions [molec/cm2/s]
      INTEGER, INTENT(OUT)   :: RC        ! Return code
!
! !REMARKS:
!  R_CANOPY is computed in routine GET_CANOPY_NOX of "canopy_nox_mod.f". 
!  This was originally in the GEOS-Chem dry deposition code, but was split 
!  off in order to avoid an ugly code dependency between the dry deposition
!  and soil NOx codes.
!
! !REVISION HISTORY: 
!  17 Aug 2009 - R. Yantosca - Columnized and cleaned up
!  17 Aug 2009 - R. Yantosca - Added ProTeX headers
!  31 Jan 2011 - R. Hudman   - New Model added
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Scalars
      INTEGER :: K,         MONTH_P,   NN
!BOP
!
!      REAL*8  :: BASE_TERM, CRF_TERM,  PULSE
!###Hong Start
!      REAL*8  :: TC,        TEMP_TERM, WINDSQR   
      REAL*8  :: TC,        TEMP_TERM, WIND_TERM
!###Hong End
      REAL*8  :: FERT_AW

      !=================================================================
      ! Initialize
      !=================================================================

      ! Assume successful return
      RC             = SMV_SUCCESS

      ! Initialize
!      SOILNOX        = 0d0
      SOILNH3        = 0d0
      FERTDIAG       = 0d0
 
      ! Surface temperature [C]
      TC             = TK - 273.15d0

!###Hong Start
      ! Surface wind speed, squared
!      WINDSQR        = U10M**2 + V10M**2
!###Hong End

      !=================================================================
      ! Compute soil NH3 emissions
      !=================================================================

      ! Cumulative multiplication factor (over baseline emissions) 
      ! that accounts for soil pulsing
!      PULSE = PULSING( GWET, TS_EMIS, GWET_PREV, PFACTOR, DRYPERIOD )
      
      ! Loop over all Olson land types in the box
      DO K = 1, IREG

         ! Soil biome index [1-11] for this Olson land type
         NN        = NCONSOIL( ILAND(K)+1 )

         ! Temperature-dependent term of soil NH3 emissions [unitless]
         !TEMP_TERM = SOILTEMP( NN, TC, SOIL_PULSING(1) )
         !Use GWET instead of climo wet/dry
         TEMP_TERM = SOILTEMP( NN, TC , GWET )
         !CHANGE RYN

         ! Wind-dependent term of soil NH3 emissions [unitless]
         ! WIND_TERM = SOILWIND( WINDSQR)
         WIND_TERM = SOILWIND( WINDSQR )
         
!!!!!!!PH	   !
         !PH_TERM = SOILPH( PH )

         ! Fertilizer emission 
         FERT_AW = FERTADD( NN, MONTH, LAT, SOILFRT , DEPN)


         ! soil moisture scaling of soil NOx emissions 
         !WET_TERM = SOILWET( NN, GWET , CLIM)

         ! Canopy reduction factor term
         !CRF_TERM  = SOILCRF( NN, LAI(K), R_CANOPY(K), WINDSQR, SUNCOS )

         ! Total soil NOx emissions for each Olson land type [molec/cm2/s]
         ! Sum into SOILNOX variable for overall soil NOx emissions


!###Hong Start
! SOILNOX is only Soil NOx (fertilizer NOx is separate)(Hong)
!         SOILNOX   = SOILNOX 
!     &              + ( SOILAW(NN) + FERT_AW )
!     &             + ( SOILAW(NN) )
!     &             * ( TEMP_TERM * WET_TERM * PULSE ) 
!     &             * ( 1d0                       - CRF_TERM  ) 
!     &             + PULSE
!     &             * ( DBLE( IUSE(K) )           / 1000d0    )
! SOILNH3 is only Soil NH3 (fertilizer NH3 is seperate)
         SOILNH3 = SOILNH3+( FERT_AW )
!     &             * ( TEMP_TERM * WIND_TERM ) ! * PH_TERM
     &             * ( DBLE( IUSE(K) )           / 1000d0    )

!###Hong End


         FERTDIAG  = FERTDIAG +
     &             (  WIND_TERM ) ! * PH_TERM 
     &             * ( DBLE( IUSE(K) )           / 1000d0    )


           IF ( I  .EQ. 100 .AND. J .EQ. 61 ) THEN 

           WRITE(6,*) '==================================='
           WRITE(6,*) 'IX = ', I, 'JX =', J
           WRITE(6,*) 'SOILNH3 = ',  SOILNH3 
           WRITE(6,*) 'SOILTEMP = ', TEMP_TERM 
           WRITE(6,*) 'SOILWIND = ', WIND_TERM 
           WRITE(6,*) 'LANDTYPE',   NN
           WRITE(6,*) 'DEP = ',   DEPN 
           WRITE(6,*) 'FRT = ',   SOILFRT 
           WRITE(6,*) 'FERTAW = ',   FERT_AW
      !     WRITE(6,*) 'SOILAW = ',   SOILAW(NN)
      !     WRITE(6,*) 'CRF =', 1d0 - CRF_TERM 
           WRITE(6,*) 'IUSE % = ',  DBLE( IUSE(K) )/ 1000d0 
           WRITE(6,*) 'IUSE # = ', IUSE(K)
      !     WRITE(6,*) 'PULSE = ', PULSE
      !     WRITE(6,*) 'PRECTOT ', PRECTOT
      !     WRITE(6,*) 'PRECCON ', PRECCON
           WRITE(6,*) 'TS_EMIS ', TS_EMIS
           WRITE(6,*) '==================================='

           ENDIF


      ENDDO

       

      END SUBROUTINE SOIL_NH3_EMISSION

!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: soiltemp
!
! !DESCRIPTION: Function SOILTEMP computes the temperature-dependent term
!  of the soil NOx emissions in ng N/m2/s and converts to molec/cm2/s
!\\
!\\
! !INTERFACE:
!
       FUNCTION SOILTEMP( NN, TC, GWET ) RESULT( SOIL_TEMP )
!
! !INPUT PARAMETERS: 
!
      INTEGER, INTENT(IN) :: NN            ! Soil biome type 
      REAL*8,  INTENT(IN) :: TC            ! Surface air temperature [C]
      REAL*8,  INTENT(IN) :: GWET          ! Top soil moisture

! !RETURN VALUE:
!
      REAL*8              :: SOIL_TEMP     ! Temperature-dependent term of
                                           ! soil NOx emissions [unitless]
!
! !REMARKS:
!
!    Based on Sjoth 2005:        
!                     
!                           
!         f(T) =  exp( 0.0223 * T ) 
!           in ng N/m2/s    
!                      
!                                                                             
!     where T is the temperature in degrees Celsius....Below 
!     0 C, we assume emissions are zero because they are insignificant 
!     for the purposes of this global source. ...
!                                                                             
!
! !REFERENCES:
!  \begin{itemize}
!  \item Ormeci, B., S. L. Sanin, and J. J. Pierce, Laboratory study of 
!         NO flux from agricultural soil: Effects of soil moisture, pH, 
!         and temperature, J. Geophys. Res., 104 ,16211629, 1999.
!  \item Otter, L. B., W. X. Yang, M. C. Scholes, and F. X. Meixner, 
!         Nitric oxide emissions from a southern African savanna, J. 
!         Geophys. Res., 105 , 20,69720,706, 1999.
!  \item Yienger, J.J, and H. Levy, \emph{Empirical model of global 
!        soil-biogenic NOx emissions}, \underline{J. Geophys. Res.}, 
!        \textbf{100}, D6, 11,447-11464, June 20, 1995.
!  \end{itemize}
! !REVISION HISTORY: 
!  17 Aug 2009 - R. Yantosca - Initial Version
!  17 Aug 2009 - R. Yantosca - Added ProTeX headers
!  31 Jan 2011 - R. Hudman   - Added new soil T dependance 
!  31 Jan 2011 - R. Hudman   - Updated headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES
!     
      REAL*8  :: TMMP

      !==============================================================
      ! 1) Convert from Surface Temp  --> Soil Temp 
      !   
      !==============================================================

      ! Save surface air temp in shadow variable TMMP
      TMMP   = TC

 
      ! NO SOILS (e.g. water, desert, ice) = no emissions
      ! IF ( NN < 8 .OR. NN > 9 ) THEN
!BOP
!
   
      !   SOIL_TEMP = 1d0

      !ELSE      

      ! DRY
      !IF ( GWET < 0.3d0 ) THEN 
     
         ! Convert surface air temperature to model temperature
         ! by adding 5 degrees C to model temperature
      !   TMMP = TMMP + 5d0

      ! WET
      !ELSE

      !   TMMP = SOILTA(NN) * TMMP + SOILTB(NN)
      !ELSE 
        
        SOIL_TEMP = EXP( 0.0223 * TMMP )

      !ENDIF

      !==============================================================
      ! 2) Compute Temperature Dependence
      !==============================================================

      ! Compute the soil temperature dependence term according 
      ! to equations 9b, 9a of Yienger & Levy [1995].
      ! We now assume that soil response is exponential 0-30C
      ! based on latest observations, caps at 30C

      !IF ( TMMP >= 36.d0 ) THEN

      !   TMMP = 36.d0

      !ENDIF

      !IF ( TMMP <= -5.6d0 ) THEN

         ! No soil emissions if temp below freezing
      !   SOIL_TEMP = 0d0

      !ELSE 

         ! Caps temperature response at 36C
      
      !   SOIL_TEMP =  EXP( 0.0223 * TMMP )

      !ENDIF


      ! Convert soil NH3  emissions to [molec/cm2/s]
      SOIL_TEMP = SOIL_TEMP * UNITCONV 

!
      END FUNCTION SOILTEMP
!EOC

!ADDED BY CCC

! ! IROUTINE : SOILWIND
! ! DESCRIPTION: Function SOILWIND computes the wind-dependent term
!  of the soil NH3 emissions in ng N/m2/s and converts to molec/cm2/s
!\\
!\
! !INTERFACE:
!
       FUNCTION SOILWIND( WINDSQR ) RESULT( SOIL_WIND )
!
! !INPUT PARAMETERS: 
!
      REAL*8,  INTENT(IN) :: WINDSQR          ! WIND SPEED

! !RETURN VALUE:
!
      REAL*8              :: SOIL_WIND     ! Wind-dependent term of
                                           ! soil NH3 emissions [unitless]
!! !REMARKS:
!
!    Based on Sjoth 2005:        
!                     
!         f(T) =  exp*( 0.0419 * WIND )
!           in ng N/m2/s    
!     
      REAL*8  :: WIND

      WIND = WINDSQR

      !IF (WIND>=9.d0) THEN

      !   WIND = 9.d0

      !ENDIF

      !IF (WIND<=0d0) THEN

      !  SOIL_WIND = 1d0

      !ELSE

        SOIL_WIND = EXP(0.0419 * WIND)

      !ENDIF

      ! Convert soil NH3  emissions to [molec/cm2/s]
      SOIL_WIND = SOIL_WIND * UNITCONV 

      END FUNCTION SOILWIND

!-----------------------------------------------------------------------------

!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: fertadd
!
! !DESCRIPTION: Function FERTADD 

!\\
! !INTERFACE:
!
      FUNCTION FERTADD( NN, MONTH, LAT, SOILFERT, DEPN ) 
     &         RESULT( FERT_ADD )
!
! !INPUT PARAMETERS: 
!
      INTEGER, INTENT(IN) :: NN         ! Soil biome index [1-11]
      INTEGER, INTENT(IN) :: MONTH      ! Current month
      REAL*8,  INTENT(IN) :: LAT        ! Latitude [degrees] 
      REAL*8,  INTENT(IN) :: DEPN       ! N emissions from deposition
      REAL*8,  INTENT(IN) :: SOILFERT   ! N emissions from fertilizers
                                        !  read in from disk and passed
                                        !  here as an argument [ng N/m2/s]
!
! !RETURN_VALUE:
! 
      REAL*8              :: FERT_ADD   ! Total Fert emissions

      REAL*8,  PARAMETER :: SECPERYEAR    = 86400.d0 * 365.
!
!
! !REMARKS:
!
!  We use a new spatially explicit data set of chemical and manure fert
!  (native resolution 0.5°x0.5°) from Potter et al., [2010] 
!  distributed using MODIS EVI seasonality as described in 
!  N.E. Moore thesis, and Hudman et al., in prep.
!    
!  In previous model, fertilizer emissions were emitted instantaneously as 
!  2.5% of applied fertilizer, independent of soil moisture/soil temperature, 
!  so that they were constant over the growing season. 

!  Similar to the YL  parameterization, we now treat fertilizer emissions 
!  as part of the Aw. If we treat the wet biome coefficient as a measure of 
!  available N multiplied by a mean emission rate, we can treat fertilizer 
!  N in the same manner. 
!
!  AW = SOILAW(Biome) + N available in soil  x mean emission rate
! 
! Instead of choosing an emission rate for each box equivalent to 2.5% 
! of applied N yearly as done in the YL scheme, we chose the mean emission 
! rate so that the total global above canopy SNOx due to fertilizer matches 
! observed estimates of fertilizer emissions of 1.8 Tg N yr-1 from Stehfest 
! and Bouman [2006].  This treatment allows for interannual and daily 
! variability in the strength  of response to temperature and precipitation. 
! Note: this scaling must be set for each resolution. 

!  References:
!  \begin{itemize}
!  \item Potter, P., Ramankutty, N., Bennett, 
!BOP
!E.,  and Donner, S.: 
!        Characterizing the Spatial Patterns of Global Fertilizer 
!        Application and Manure Production, Earth Interactions, 
!        in press, 2010.
!  \item Stehfest, E. and L. Bouwman, N2O and NO emission from 
!        agricultural fields and soils under natural vegetation: 
!        summarizing available measurement data and modeling
!        of global annual emissions, Nutrient Cycling in Agroecosystems 
!        (2006), 74:207-228 DOI 10.1007/s10705-006-9000-7.
!  \end{itemize}

! !REVISION HISTORY: 
!  17 Aug 2009 - R. Yantosca - Columnized and cleaned up
!  17 Aug 2009 - R. Yantosca - Added ProTeX headers
!  31 Jan 2011 - R. Hudman   - Rewrote pulsing scheme
!  31 Jan 2011 - R. Hudman   - Updated ProTex headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
      ! Initialize
      FERT_ADD = 0d0

      ! Soil fert and dep [ ng N /m2 ], a measure of N avail. in soil
      FERT_ADD = SOILFERT  + DEPN


       ! Convert [ng N/m2] --> [ng N /m2/s] (scale needed to force
       ! fert emiss of 1.8 Tg N/yr w/o canopy uptake.
      FERT_ADD = FERT_ADD / SECPERYEAR * FERT_SCALE

    
      ! Section 4.2.1 of Yienger & Levy [1995] states that over rice 
      ! paddies the NOx emissions from fertilizers should be cut by a 
      ! factor of 30.  This is because the very wet soil in rice paddies 
      ! impedes NOx emission.
      IF ( NN == 9 ) FERT_ADD = FERT_ADD / 30d0      


      END FUNCTION FERTADD
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: pulsing
!
! !DESCRIPTION: Function PULSING calculates the increase (or "pulse") of 
!  soil NOx emission that happens after preciptiation falls on dry soil.  
!\\
!\\
!  According to  Yan et al., [2005] , this pulsing process is thought to  
!  be due to a release of inorganic nitrogen trapped on top of the dry soil 
!  and a subsequent reactivation of water-stressed bacteria, which then 
!  metabolize the excess nitrogen. This can happen in seasonally dry
!  grasslands and savannahs or over freshly fertilized fields.
!\\
!\\
! !INTERFACE:
!
      FUNCTION PULSING( GWET,      TS_EMIS, 
     &                  GWET_PREV, PFACTOR,
     &                  DRYPERIOD         ) RESULT( THE_PULSING )
!
! !INPUT PARAMETERS: 
!
      REAL*8, INTENT(IN)    :: GWET        ! Soil Moisture 
      REAL*8, INTENT(IN)    :: TS_EMIS     ! Emissions timestep [min]

! !INPUT/OUTPUT PARAMETERS:
!
      REAL*4,  INTENT(INOUT) :: GWET_PREV   ! soil moisture from prev. timestep
      REAL*4,  INTENT(INOUT) :: PFACTOR     ! pulsing factor
      REAL*4,  INTENT(INOUT) :: DRYPERIOD   ! dry period in # timesteps
                                           
!  
! !RETURN VALUE:
!
      REAL*8                :: THE_PULSING ! Factor to multiply baseline 
!                                          ! emissions by to account for
!                                          ! soil pulsing of all types
!
! !REMARKS:
!  Soil NOx emissions consist of baseline emissions plus discrete "pulsing"
!  episodes.  We follow thw Yan et al., [2005] algorithm, where the pulse
!  (relative to the flux prewetting) is determined by the antecedent dry 
!  period, with a simple logarithmic relationship,
!
!  PFACTOR = 13.01 ln ( DRYPERIOD ) -  53.6
!
!  ,where PFACTOR is the magnitude of peak flux relative to prewetting flux, 
!  and DRYPERIOD  is the length of the antecedent dry period in hours.
! 
!  The pulse decays with 
!
!  PFACTOR = PFACTOR * EXP( -0.068d0 * DTSRCE )       
!                                                                  
!  References:
!  \begin{itemize}
!  \item Yan, X., T. Ohara, and H. Akimoto (2005), Statistical modeling of 
!        global soil NOx emissions, Global Biogeochem. Cycles, 19, GB3019, 
!        doi:10.1029/2004GB002276.Section 2.3.3
!  \end{itemize}
!
! !REVISION HISTORY: 
!  17 Aug 2009 - R. Yantosca - Columnized and cleaned up
!  17 Aug 2009 - R. Yantosca - Added ProTeX headers
!  31 Jan 2011 - R. Hudman   - Rewrote pulsing scheme
!  31 Jan 2011 - R. Hudman   - Updated ProTex header
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      REAL*8  :: DTSRCE, GDIFF 

      !=================================================================
      ! PULSING begins here!
      !=================================================================

      ! Emission timestep [min --> hours]
      DTSRCE = TS_EMIS / 60d0

      ! If soil moisture less than 0.3 and no pulse is taking place
      IF ( GWET < 0.3D0 .and. PFACTOR == 1.D0) THEN

         ! Get change in soil moisture since previous timestep
         GDIFF = ( GWET - GWET_PREV )

         ! If change in soil moisture is > 0.01 (rains)
         IF ( GDIFF > 0.01 ) THEN

             !Initialize new pulse factor (dry period hours)
             PFACTOR = 13.01 * LOG( DRYPERIOD ) - 53.6

             ! If dry period < ~3 days then no pulse
             IF ( PFACTOR < 1.0 ) PFACTOR = 1.0

             ! Reinitialize dry period
             DRYPERIOD = 0

         ! If no rain (i.e.,  change in soil moisture is < 0.01)
         ELSE
             ! Add one timestep to dry period
             DRYPERIOD = DRYPERIOD + DTSRCE

         ENDIF

      ! If box is already pulsing , then decay pulse one timestep
      ELSEIF ( PFACTOR /= 1.d0) THEN

         ! decay pulse
         PFACTOR   = PFACTOR * EXP( -0.068d0 * DTSRCE )

         ! Update dry period
         IF ( GWET < 0.3D0 ) DRYPERIOD = DRYPERIOD + DTSRCE

         ! If end of pulse
         IF ( PFACTOR < 1.d0 ) PFACTOR = 1.d0
      
      ENDIF

      ! Update soil moisture holder for previous timestep
      GWET_PREV = GWET

      ! Return the pulsing factor
      THE_PULSING = PFACTOR

      ! Return to calling program
      END FUNCTION PULSING
!EOC


      END MODULE SOIL_NH3_MOD

