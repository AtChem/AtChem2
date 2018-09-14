PROGRAM fptest
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  ! Example program 3 for using the function parser module:
  !
  ! Assessing how fast the interpreter is compared against a direct evaluation
  !
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  USE FortranParser_parameters, ONLY: rn
  USE FortranParser,    ONLY: EquationParser
  IMPLICIT NONE
  type(EquationParser), allocatable :: eqParser(:)
  INTEGER,                             PARAMETER :: neval = 10000000
  INTEGER,                             PARAMETER :: nfunc = 3
  CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ 'vel*COS(beta)           ', &
                                                              'vel*SIN(beta)*COS(alpha)', &
                                                              'vel*SIN(beta)*SIN(alpha)' /)
  INTEGER,                             PARAMETER :: nvar = 3
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'vel  ', &
                                                              'alpha', &
                                                              'beta ' /)
  REAL(rn),          DIMENSION(nvar, neval)      :: val
  REAL(rn)                                       :: res
  INTEGER                                        :: i,n
  REAL                                           :: rt1,rt2,rt3
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  allocate(eqParser(nfunc))
  call random_number(val)

  CALL CPU_TIME (rt1)
  DO i=1,nfunc
    eqParser(i) = EquationParser(func(i), var)
    DO n=1,neval
       res = eqParser(i)%evaluate(val(:,i))
    END DO
  END DO
  CALL CPU_TIME (rt2)

  DO n=1,neval
     res = val(1,i)*COS(val(3,i))
     res = val(1,i)*SIN(val(3,i))*COS(val(2,i))
     res = val(1,i)*SIN(val(3,i))*SIN(val(2,i))
  END DO
  CALL CPU_TIME (rt3)
  WRITE(*,*)'Function evaluation:'
  WRITE(*,*)'- Bytecode interpreter cpu time = ',rt2-rt1
  WRITE(*,*)'- Machine code         cpu time = ',rt3-rt2,' = ',(rt3-rt2)/(rt2-rt1)*100.,'%'
  !
END PROGRAM fptest
