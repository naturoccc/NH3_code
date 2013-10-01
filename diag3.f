!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: diag3
!
! !DESCRIPTION: Subroutine DIAG3 prints out diagnostics to the BINARY PUNCH
!  format file.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE DIAG3                                                    
!
! !USES:
!
      USE BPCH2_MOD
      ! NBIOMAX now refers to the maximum number of possible
      ! BB species (hotp 7/31/09)
      ! NBIOTRCE is the number for a given simulation and
      ! set of tracers
      ! NBIOMAX in CMN_SIZE (FP)
      USE DIAG_MOD,     ONLY : AD01,        AD02,        AD05    
      USE DIAG_MOD,     ONLY : AD06,        AD07,        AD07_BC
      USE DIAG_MOD,     ONLY : AD07_SOAGM
      USE DIAG_MOD,     ONLY : AD07_OC,     AD07_HC,     AD08
      USE DIAG_MOD,     ONLY : AD09,        AD09_em,     AD11
      USE DIAG_MOD,     ONLY : AD12,        AD13_DMS,    AD13_SO2_ac 
      USE DIAG_MOD,     ONLY : AD13_SO2_an, AD13_SO2_bb, AD13_SO2_bf
      USE DIAG_MOD,     ONLY : AD13_SO2_ev, AD13_SO2_nv, AD13_SO4_an
      USE DIAG_MOD,     ONLY : AD13_SO4_bf, AD13_SO2_sh, AD13_NH3_an
      USE DIAG_MOD,     ONLY : AD13_NH3_na, AD13_NH3_bb, AD13_NH3_bf
      USE DIAG_MOD,     ONLY : CONVFLUP,    TURBFLUP,    AD16
      USE DIAG_MOD,     ONLY : CT16,        AD17,        CT17
      USE DIAG_MOD,     ONLY : AD18,        CT18,        AD21
      USE DIAG_MOD,     ONLY : AD21_cr,     AD22,        LTJV
      USE DIAG_MOD,     ONLY : CTJV,        MASSFLEW,    MASSFLNS
      USE DIAG_MOD,     ONLY : MASSFLUP,    AD28,        AD29
      USE DIAG_MOD,     ONLY : AD30,        AD31
      ! potential temperature (hotp 7/31/09)
      USE DIAG_MOD,     ONLY : AD57
      USE DIAG_MOD,     ONLY : AD32_ac,     AD32_an,     AD32_bb
      USE DIAG_MOD,     ONLY : AD32_bf,     AD32_fe,     AD32_li
      USE DIAG_MOD,     ONLY : AD32_so,     AD32_ub,     AD33
      USE DIAG_MOD,     ONLY : AD34,        AD35,        AD36
      USE DIAG_MOD,     ONLY : AD37,        AD38,        AD39
      USE DIAG_MOD,     ONLY : AD43,        LTNO
      USE DIAG_MOD,     ONLY : CTNO,        LTOH,        CTOH
      USE DIAG_MOD,     ONLY : LTHO2,       CTHO2,       LTNO2
      USE DIAG_MOD,     ONLY : CTNO2,       LTNO3,       CTNO3
      ! update for arom (dkh, 06/21/07)  
      ! to save the amount of RO2 consumed by HO2 (*H) or NO (*N)
      ! CTLxRO2x : # of times a grid box was in the ND43 time range between
      ! the last .bpch write and current .bpch write
      USE DIAG_MOD,     ONLY : CTLBRO2H,      CTLBRO2N
      USE DIAG_MOD,     ONLY : CTLTRO2H,      CTLTRO2N
      USE DIAG_MOD,     ONLY : CTLXRO2H,      CTLXRO2N
      USE DIAG_MOD,     ONLY : AD44,        AD45,        LTOTH
      USE DIAG_MOD,     ONLY : CTOTH,       AD46,        AD47
      USE DIAG_MOD,     ONLY : AD52
      USE DIAG_MOD,     ONLY : AD54,        CTO3,        CTO3_24h
      USE DIAG_MOD,     ONLY : AD19,        AD58,        AD60
      USE DIAG_MOD,     ONLY : AD55,        AD66,        AD67
      USE DIAG_MOD,     ONLY : AD68,        AD69
      USE DIAG_MOD,     ONLY : AD10,        AD10em
      USE FILE_MOD,     ONLY : IU_BPCH
      USE GRID_MOD,     ONLY : GET_AREA_M2, GET_XOFFSET, GET_YOFFSET
      USE LOGICAL_MOD,  ONLY : LCARB,       LCRYST,      LDUST    
      USE LOGICAL_MOD,  ONLY : LSHIPSO2,    LSOA,        LSSALT
      USE LOGICAL_MOD,  ONLY : LEDGARSHIP,  LARCSHIP,    LEMEPSHIP
      USE LOGICAL_MOD,  ONLY : LICOADSSHIP, LGTMM
      USE TIME_MOD,     ONLY : GET_DIAGb,   GET_DIAGe,   GET_CT_A3   
      USE TIME_MOD,     ONLY : GET_CT_A6,   GET_CT_CHEM, GET_CT_CONV 
      USE TIME_MOD,     ONLY : GET_CT_DYN,  GET_CT_EMIS, GET_CT_I6   
      USE TIME_MOD,     ONLY : GET_CT_DIAG, GET_CT_A1


      IMPLICIT NONE

#     include "CMN_SIZE"     ! Size parameters
#     include "CMN_DIAG"     ! Diagnostic switches & arrays
! 
!
! !LOCAL VARIABLES:
!
      INTEGER            :: I, IREF, J, JREF, L, M, MM, MMB, LMAX
      INTEGER            :: N, NN, NMAX, NTEST, IDTNOX
      INTEGER            :: IE, IN, IS, IW, ITEMP(3)
      REAL*8             :: SCALE_TMP(IIPAR,JJPAR)
      REAL*8             :: SCALE_I6,   SCALE_A6,   SCALE_A3  
      REAL*8             :: SCALE_A1,   SCALED,     SCALEDYN
      REAL*8             :: SCALECONV,  SCALESRCE,  SCALECHEM  
      REAL*8             :: SCALEDIAG,  SCALE_ND66, SCALE_ND67 
      REAL*8             :: SCALEX,     SECONDS,    PMASS      
      REAL*8             :: PRESSX,     FDTT,       AREA_M2
      REAL*8             :: DIAGb,      DIAGe
      
      ! For binary punch file, version 2.0
      CHARACTER (LEN=40) :: CATEGORY 
      REAL*4             :: ARRAY(IIPAR,JJPAR,LLPAR+1)
      REAL*4             :: LONRES, LATRES
      INTEGER            :: IFIRST, JFIRST, LFIRST
      INTEGER            :: HALFPOLAR
      INTEGER, PARAMETER :: CENTER180 = 1
      CHARACTER (LEN=20) :: MODELNAME 
      CHARACTER (LEN=40) :: UNIT
      CHARACTER (LEN=40) :: RESERVED = ''
!
!******************************************************************************
!  DIAG3 begins here!
!
!  Define scale factors for division.  
!  Add a small number (e.g. 1d-32) to prevent division by zero errors.
!******************************************************************************
!
      ! Now use counter variables from "time_mod.f" (bmy, 3/27/03)
      DIAGb      = GET_DIAGb()
      DIAGe      = GET_DIAGe()
      SECONDS    = ( DIAGe - DIAGb ) * 3600d0
      SCALED     = 1d0
      SCALEDYN   = DBLE( GET_CT_DYN()  ) + 1d-32
      SCALECONV  = DBLE( GET_CT_CONV() ) + 1d-32
      SCALESRCE  = DBLE( GET_CT_EMIS() ) + 1d-32
      SCALECHEM  = DBLE( GET_CT_CHEM() ) + 1d-32
      SCALE_A1   = DBLE( GET_CT_A1()   ) + 1d-32
      SCALE_A3   = DBLE( GET_CT_A3()   ) + 1d-32
      SCALE_A6   = DBLE( GET_CT_A6()   ) + 1d-32
      SCALE_I6   = DBLE( GET_CT_I6()   ) + 1d-32
      SCALEDIAG  = DBLE( GET_CT_DIAG() ) + 1d-32
!
!******************************************************************************
!  Setup for binary punch file:
!
!  IFIRST, JFIRST, LFIRST = I, J, L indices of the starting grid box 
!  LONRES                 = DISIZE, cast to REAL*4
!  LATRES                 = DJSIZE, cast to REAL*4
!******************************************************************************
!
      IFIRST = GET_XOFFSET( GLOBAL=.TRUE. ) + 1
      JFIRST = GET_YOFFSET( GLOBAL=.TRUE. ) + 1
      LFIRST = 1
      LONRES = DISIZE
      LATRES = DJSIZE

      ! Get the proper model name and HALFPOLAR setting for the bpch file
      MODELNAME = GET_MODELNAME()
      HALFPOLAR = GET_HALFPOLAR()
!
!******************************************************************************
!  ND32: NOx source diagnostic
!
!  Levels        : Field                  : Units       : Scale Factor
!  -------------------------------------------------------------------------
!  1 - LLTROP    : Aircraft NOx           : molec/cm2/s : SCALESRCE
!  1 - NOXEXTENT : Anthropogenic NOx      : molec/cm2/s : SCALESRCE
!  Surface       : Biomass Burning NOx    : molec/cm2/s : SCALESRCE
!  Surface       : Biofuel Burning NOx    : molec/cm2/s : SCALESRCE
!  Surface       : Fertilizer NOx         : molec/cm2/s : SCALESRCE
!  1 - LLCONVM   : Lightning NOx          : molec/cm2/s : SCALESRCE
!  Surface       : Soil NOx               : molec/cm2/s : SCALESRCE
!  Above TP      : NOx from upper boundary: molec/cm2/s : SCALEDYN
!
!  Print out all of the types of NOx, for all levels.
!
!  NOTES:
!  (1) Only print out ND32 if for an O3 chemistry run ( NSRCX == 3 ),
!       and if NOx is a defined tracer ( IDTNOX > 0 ). (bmy, 5/26/99)
!  (2) ND32 now uses allocatable arrays instead of AIJ. (bmy 3/16/00)
!  (3) Added biofuel burning to ND32 diagnostic (bmy, 9/12/00)
!******************************************************************************
!
      
         IDTNOX=1
         
         ! All categories of NOx are in molec/cm2/s
         UNIT = 'molec/cm2/s'

         !==============================================================
         ! Fertilizer NOx
         !==============================================================
         CATEGORY     = 'NOX-FERT'
         ARRAY(:,:,1) = AD32_fe(:,:) / SCALESRCE
            
         CALL BPCH2( IU_BPCH,   MODELNAME, LONRES,   LATRES,     
     &               HALFPOLAR, CENTER180, CATEGORY, IDTNOX,    
     &               UNIT,      DIAGb,     DIAGe,    RESERVED,   
     &               IIPAR,     JJPAR,     1,        IFIRST,     
     &               JFIRST,    LFIRST,    ARRAY(:,:,1) )

         !==============================================================
         ! Soil NOx
         !==============================================================
         CATEGORY     = 'NOX-SOIL'
         ARRAY(:,:,1) = AD32_so(:,:) / SCALESRCE
            
         CALL BPCH2( IU_BPCH,   MODELNAME, LONRES,   LATRES,     
     &               HALFPOLAR, CENTER180, CATEGORY, IDTNOX,    
     &               UNIT,      DIAGb,     DIAGe,    RESERVED,   
     &               IIPAR,     JJPAR,     1,        IFIRST,     
     &               JFIRST,    LFIRST,    ARRAY(:,:,1) )
   

      END SUBROUTINE DIAG3    
!EOC
