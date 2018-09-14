PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 1 for using the function parser module
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE FortranParser_parameters, ONLY: rn
  USE FortranParser,    ONLY: EquationParser
  IMPLICIT NONE

  type(EquationParser) :: eqParser
  INTEGER,                             PARAMETER :: nfunc = 1
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ '-x' /)
  INTEGER,                             PARAMETER :: nvar = 1
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'x'  /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  2.  /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  REAL(rn)                                       :: x


  WRITE(*,*)'==> Bytecode evaluation:'

  DO i=1,nfunc
     eqParser = EquationParser(func(i), var)
     res = eqParser%evaluate(val)
     WRITE(*,*) func(i),'=',res
  END DO

  WRITE(*,*)'==> Direct evaluation:'
  x  = val(1)
  WRITE(*,*)'-x=',-x
  !
END PROGRAM fptest
