      MODULE EMISSIONS_MOD
! 
      IMPLICIT NONE
      PRIVATE
!
      PUBLIC :: DO_EMISSIONS

      CONTAINS
!
      SUBROUTINE DO_EMISSIONS
!

#     include "CMN_SIZE" 

         ! NOx-Ox-HC (w/ or w/o aerosols)
         CALL EMISSDR

      END SUBROUTINE DO_EMISSIONS

      END MODULE EMISSIONS_MOD
