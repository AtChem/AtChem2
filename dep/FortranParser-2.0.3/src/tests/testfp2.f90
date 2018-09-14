PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 2 for using the function parser module
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE FortranParser_parameters, ONLY: rn
  USE FortranParser,    ONLY: EquationParser
  IMPLICIT NONE
  type(EquationParser) :: eqParser
  INTEGER,                             PARAMETER :: nfunc = 3
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ 'vel*COS(beta)           ', &
                                                              'vel*SIN(beta)*COS(alpha)', &
                                                              'vel*SIN(beta)*SIN(alpha)' /)
  INTEGER,                             PARAMETER :: nvar = 3
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'vel  ', &
                                                              'alpha', &
                                                              'beta ' /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  10., 1.5, 2.0  /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i
  REAL(rn)                                       :: vel,alpha,beta
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  WRITE(*,*)'==> Bytecode evaluation:'
  DO i=1,nfunc
     eqParser = EquationParser(func(i), var)
     res = eqParser%evaluate(val)
     WRITE(*,*) func(i),'=',res
  END DO

  WRITE(*,*)'==> Direct evaluation:'
  vel   = val(1)
  alpha = val(2)
  beta  = val(3)
  WRITE(*,*)'res=',vel*COS(beta)
  WRITE(*,*)'res=',vel*SIN(beta)*COS(alpha)
  WRITE(*,*)'res=',vel*SIN(beta)*SIN(alpha)
  !
END PROGRAM fptest
