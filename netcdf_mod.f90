MODULE NETCDF_MOD

!-------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_2d_real_cdf (cdfid, var, dataout, i1, i2, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER,       INTENT(IN)    :: cdfid
  REAL,          INTENT(OUT)   :: dataout    ( i1, i2 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( ( i1  /= idims(1) ) .OR.  &
       ( i2  /= idims(2) ) .OR.  &
       ( time > idims(4) ) ) THEN

    WRITE (6,*) 'error in 2d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) time, idims(4)
    WRITE (6,*) 'error stop'
    rcode = -9999
    RETURN

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = 1     ;  iend(3) = 1
  istart(4) = time  ;  iend(4) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, dataout)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE get_var_2d_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_2d_int_cdf (cdfid, var, dataout, i1, i2, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER,       INTENT(IN)    :: cdfid
  INTEGER,       INTENT(OUT)   :: dataout    ( i1, i2 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( ( i1   /= idims(1) ) .OR.  &
       ( i2   /= idims(2) ) .OR.  &
       ( time >  idims(4) ) )  THEN

    WRITE (6,*) 'error in 2d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) time, idims(4)
    WRITE (6,*) 'error stop'
    rcode = -9999
    RETURN

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = 1     ;  iend(3) = 1
  istart(4) = time  ;  iend(4) = 1

  rcode = nf_get_vara_int (cdfid, id_data, istart, iend, dataout)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE get_var_2d_int_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE put_var_2d_real_cdf (cdfid, var, datain, i1, i2, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER,       INTENT(IN)    :: cdfid
  REAL,          INTENT(IN)    :: datain     ( i1, i2 )
  INTEGER                      :: dimids     ( 10 )
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  rcode = nf_inq_varid (cdfid, var, id_data)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= nf_noerr ) RETURN

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= nf_noerr ) RETURN
  ENDDO

  ! Check the dimensions.

  IF ( ( i1   /= idims(1) ) .OR.  &
       ( i2   /= idims(2) ) )  THEN

    WRITE (6,*) 'error in 2d_var_real write, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) 'error stop'
    rcode = -9999
    RETURN

  ENDIF

  ! Put the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = 1     ;  iend(3) = 1
  istart(4) = time  ;  iend(4) = 1

  rcode = nf_put_vara_real (cdfid, id_data, istart, iend, datain)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE put_var_2d_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE def_diag3_cdf (cdfid, i1, i2, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER,       INTENT(IN)    :: cdfid
  INTEGER                      :: dimids     ( 4  )
  INTEGER                      :: dimid_col
  INTEGER                      :: dimid_row
  INTEGER                      :: dimid_lay
  INTEGER                      :: dimid_tst
  INTEGER                      :: id_data    ( 10 )
  INTEGER,       PARAMETER     :: ndims = 4
  INTEGER,       INTENT(OUT)   :: rcode


  rcode = nf_def_dim (cdfid, 'COL'  , i1, dimid_col)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_dim (cdfid, 'ROW'  , i2, dimid_row)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_dim (cdfid, 'LAY'  , 1, dimid_lay)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_dim (cdfid, 'TSTEP', nf_unlimited, dimid_tst)
  IF ( rcode /= nf_noerr ) RETURN

  dimids = (/ dimid_col, dimid_row, dimid_lay, dimid_tst /)

!  rcode = nf_def_var (cdfid, 'NOX_FERT', nf_float, ndims, dimids, id_data(1))
!  IF ( rcode /= nf_noerr ) RETURN

!  rcode = nf_def_var (cdfid, 'NOX_SOIL', nf_float, ndims, dimids, id_data(2))
!  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_var (cdfid, 'NH3_FERT', nf_float, ndims, dimids, id_data(1))
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_var (cdfid, 'NH3_SOIL', nf_float, ndims, dimids, id_data(2))
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_enddef (cdfid)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE def_diag3_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


SUBROUTINE def_diag49_cdf (cdfid, i1, i2, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER,       INTENT(IN)    :: cdfid
  INTEGER                      :: dimids     ( 4  )
  INTEGER                      :: dimid_col
  INTEGER                      :: dimid_row
  INTEGER                      :: dimid_lay
  INTEGER                      :: dimid_tst
  INTEGER                      :: id_data    ( 10 )
  INTEGER,       PARAMETER     :: ndims = 4
  INTEGER,       INTENT(OUT)   :: rcode


  rcode = nf_def_dim (cdfid, 'COL'  , i1, dimid_col)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_dim (cdfid, 'ROW'  , i2, dimid_row)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_dim (cdfid, 'LAY'  , 1, dimid_lay)
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_dim (cdfid, 'TSTEP', nf_unlimited, dimid_tst)
  IF ( rcode /= nf_noerr ) RETURN

  dimids = (/ dimid_col, dimid_row, dimid_lay, dimid_tst /)

!  rcode = nf_def_var (cdfid, 'NOX_SOIL', nf_float, ndims, dimids, id_data(1))
!  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_def_var (cdfid, 'NH3_SOIL', nf_float, ndims, dimids, id_data(1))
  IF ( rcode /= nf_noerr ) RETURN

  rcode = nf_enddef (cdfid)
  IF ( rcode /= nf_noerr ) RETURN

END SUBROUTINE def_diag49_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

END MODULE NETCDF_MOD
