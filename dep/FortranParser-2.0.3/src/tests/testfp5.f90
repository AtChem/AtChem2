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
  INTEGER,                             PARAMETER :: nfunc = 4
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ '-1.0*x        ',  &
                                                              '-x            ',  &
                                                              'A*COS(B*x)+5  ',  &
                                                              'A*COS(B*x)+5.0' /)
  INTEGER,                             PARAMETER :: nvar = 3
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'x', &
                                                              'A', &
                                                              'B'  /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  2., 3., 4. /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  REAL(rn)                                       :: x,A,B
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  WRITE(*,*)'==> Bytecode evaluation:'
  DO i=1,nfunc
     eqParser = EquationParser(func(i), var)
     res = eqParser%evaluate(val)
     WRITE(*,*) func(i),'=',res
  END DO

  WRITE(*,*)'==> Direct evaluation:'
  x  = val(1)
  A  = val(2)
  B  = val(3)
  WRITE(*,*)'-1.0*x        =',-1.0*x
  WRITE(*,*)'-x            =',-x
  WRITE(*,*)'A*COS(B*x)+5  =',A*COS(B*x)+5
  WRITE(*,*)'A*COS(B*X)+5.0=',A*COS(B*X)+5.0
  !
END PROGRAM fptest
