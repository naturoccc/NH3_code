!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: ndxx_setup
!
! !DESCRIPTION: Subroutine NDXX\_SETUP dynamically allocates memory for 
!  certain diagnostic arrays that  are declared allocatable in "diag\_mod.f". 
!\\
!\\
!  This allows us to reduce the amount of memory that needs to be declared 
!  globally.  We only allocate memory for arrays if the corresponding 
!  diagnostic is turned on.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE NDXX_SETUP
!
! !USES:
!
      ! References to F90 modules
      !NBIOMAX moved to CMN_SIZE (fp, 6/2009)
      !USE BIOMASS_MOD,     ONLY : NBIOMAX
      USE DIAG_MOD,        ONLY : AD01,        AD02,        AD05    
      USE DIAG_MOD,        ONLY : AD06,        AD07,        AD07_BC
      USE DIAG_MOD,        ONLY : AD07_OC,     AD07_HC,     AD08
      USE DIAG_MOD,        ONLY : AD07_SOAGM
      USE DIAG_MOD,        ONLY : AD09,        AD09_em,     AD11
      USE DIAG_MOD,        ONLY : AD12,        AD13_DMS,    AD13_SO2_ac 
      USE DIAG_MOD,        ONLY : AD13_SO2_an, AD13_SO2_bb, AD13_SO2_bf
      USE DIAG_MOD,        ONLY : AD13_SO2_ev, AD13_SO2_nv, AD13_SO4_an
      USE DIAG_MOD,        ONLY : AD13_SO4_bf, AD13_SO2_sh, AD13_NH3_an
      USE DIAG_MOD,        ONLY : AD13_NH3_na, AD13_NH3_bb, AD13_NH3_bf
      USE DIAG_MOD,        ONLY : CONVFLUP,    TURBFLUP,    AD16
      USE DIAG_MOD,        ONLY : CT16,        AD17,        CT17
      USE DIAG_MOD,        ONLY : AD18,        CT18,        AD21
      USE DIAG_MOD,        ONLY : AD21_cr,     AD22,        LTJV
      USE DIAG_MOD,        ONLY : CTJV,        MASSFLEW,    MASSFLNS
      USE DIAG_MOD,        ONLY : MASSFLUP,    AD28,        AD29
      USE DIAG_MOD,        ONLY : AD30,        AD31
      !FP_ISOP potential temperature diag (6/2009)
      USE DIAG_MOD,        ONLY : AD57
      !
      USE DIAG_MOD,        ONLY : AD32_ac,     AD32_an,     AD32_bb
      USE DIAG_MOD,        ONLY : AD32_bf,     AD32_fe,     AD32_li
      USE DIAG_MOD,        ONLY : AD32_so,     AD32_ub,     AD33
      USE DIAG_MOD,        ONLY : AD34,        AD35,        AD36
      USE DIAG_MOD,        ONLY : AD37,        AD38,        AD39
      USE DIAG_MOD,        ONLY : AD43,        LTNO
      USE DIAG_MOD,        ONLY : CTNO,        LTOH,        CTOH
      USE DIAG_MOD,        ONLY : LTHO2,       CTHO2,       LTNO2
      USE DIAG_MOD,        ONLY : CTNO2,       LTNO3,       CTNO3
      ! update for arom (dkh, 06/21/07)  
      USE DIAG_MOD,        ONLY : CTLBRO2H,    CTLBRO2N
      USE DIAG_MOD,        ONLY : CTLTRO2H,    CTLTRO2N
      USE DIAG_MOD,        ONLY : CTLXRO2H,    CTLXRO2N
      USE DIAG_MOD,        ONLY : LTLBRO2H,    LTLBRO2N
      USE DIAG_MOD,        ONLY : LTLTRO2H,    LTLTRO2N
      USE DIAG_MOD,        ONLY : LTLXRO2H,    LTLXRO2N
      USE DIAG_MOD,        ONLY : AD44,        AD45,        LTOTH
      USE DIAG_MOD,        ONLY : CTOTH,       AD46,        AD47
      USE DIAG_MOD,        ONLY : AD52,        AD54
      USE DIAG_MOD,        ONLY : AD19,        AD58,        AD60
      USE DIAG_MOD,        ONLY : AD55,        AD66,        AD67
      USE DIAG_MOD,        ONLY : AD68,        AD69,        CTO3
      USE DIAG_MOD,        ONLY : AD10,        AD10em,      CTO3_24h
      ! Add O3 for ND45 diag. (ccc, 8/12/09)
      USE DIAG_MOD,        ONLY : LTO3
      USE ERROR_MOD,       ONLY : ALLOC_ERR,   ERROR_STOP
      USE LOGICAL_MOD,     ONLY : LDUST, LCARB, LSSALT, LCRYST, LDRYD
      ! Added for mercury simulation. (ccc, 6/4/10)
      USE LOGICAL_MOD,     ONLY : LGTMM


      IMPLICIT NONE

#     include "CMN_SIZE"   ! Size parameters
#     include "CMN_DIAG"   ! Diagnostic switches & arrays
! 
!
! !LOCAL VARIABLES:
!
      INTEGER :: NMAX, AS, NEMISS, LMAX

      !=================================================================
      ! ND32: Sources of NOx [molec/cm2/s]
      !       (aircraft, biomass, biofuel, lightning, 
      !        stratosphere, soils, fertilizer, anthropogenic) 
      !       --> Uses AD32_xx arrays (allocatable)
      !=================================================================

      !=================================================================
      ! ND32: Sources of NH3 [molec/cm2/s]
      !       (aircraft, biomass, biofuel, lightning, 
      !        stratosphere, soils, fertilizer, anthropogenic) 
      !       --> Uses AD32_xx arrays (allocatable)
      !=================================================================

      IF ( ND32 > 0 ) THEN

         ! For fertilizer NOx
	   ! For fertilizer NH3
         ALLOCATE( AD32_fe( IIPAR, JJPAR ), STAT=AS )
         IF ( AS /= 0 ) CALL ALLOC_ERR( 'AD32_fe' ) 

         ! For soil NOx
	   ! For soil NH3
         ALLOCATE( AD32_so( IIPAR, JJPAR ), STAT=AS )
         IF ( AS /= 0 ) CALL ALLOC_ERR( 'AD32_so' ) 

      ENDIF

 
      END SUBROUTINE NDXX_SETUP
!EOC
