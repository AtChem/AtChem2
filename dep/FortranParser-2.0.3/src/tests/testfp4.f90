PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 4 for using the function parser module
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE FortranParser_parameters, ONLY: rn
  USE FortranParser,    ONLY: EquationParser
  IMPLICIT NONE
  type(EquationParser) :: eqParser
  INTEGER,                             PARAMETER :: nfunc = 1
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ '1.0e0 + 5.e1' /)
  INTEGER,                             PARAMETER :: nvar = 0
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = 'a'
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = 0._rn
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  DO i=1,nfunc
     eqParser = EquationParser(func(i), var)
     res = eqParser%evaluate(val)
     WRITE(*,*) func(i),'=',res
  END DO

  !
END PROGRAM fptest
