!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: cleanup
!
! !DESCRIPTION: Subroutine CLEANUP deallocates the memory assigned to 
!  dynamically allocatable arrays just before exiting a GEOS-Chem simulation.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE CLEANUP
!
! !USES:
!
      USE DIAG_MOD,                ONLY : CLEANUP_DIAG

      IMPLICIT NONE

#     include "define.h"
!
      !=================================================================
      ! CLEANUP begins here!
      !=================================================================

      ! Echo info
      WRITE( 6, 100 ) 
 100  FORMAT( '     - CLEANUP: deallocating arrays now...' )

      ! Call cleanup routines from individual F90 modules
      CALL CLEANUP_DIAG

      END SUBROUTINE CLEANUP
!EOC
