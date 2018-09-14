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
  INTEGER,                             PARAMETER :: nfunc = 3
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ 'a0*b0 ', &
                                                              'a1/b1 ', &
                                                              'a3**b3' /)
  INTEGER,                             PARAMETER :: nvar = 6
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'a0', &
                                                              'b0', &
                                                              'a1', &
                                                              'b1', &
                                                              'a3', &
                                                              'b3' /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  1., 2., 3., 0., 5., 6. /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  REAL(rn)                                       :: a0,b0,a1,b1,a3,b3
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  WRITE(*,*)'==> Bytecode evaluation:'
  DO i=1,nfunc
     eqParser = EquationParser(func(i), var)
     res = eqParser%evaluate(val)
     WRITE(*,*) func(i),'=',res
  END DO

  WRITE(*,*)'==> Direct evaluation:'
  a0 = val(1)
  b0 = val(2)
  a1 = val(3)
  b1 = val(4)
  a3 = val(5)
  b3 = val(6)
  WRITE(*,*)'res=',a0*b0
  WRITE(*,*)'res=',a1/b1
  WRITE(*,*)'res=',a3**b3
  !
END PROGRAM fptest
