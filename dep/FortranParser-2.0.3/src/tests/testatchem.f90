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
  USE input_functions_mod, only : count_lines_in_file
  IMPLICIT NONE
  type(EquationParser) :: eqParser
  INTEGER,                             PARAMETER :: neval = 1000000
  INTEGER                                        :: nfunc, ngeneric
  ! CHARACTER (LEN=*), DIMENSION(nfunc), PARAMETER :: func = (/ 'vel*COS(beta)           ', &
  !                                                             'vel*SIN(beta)*COS(alpha)', &
  !                                                             'vel*SIN(beta)*SIN(alpha)' /)
  INTEGER,                             PARAMETER :: nvar = 4
  CHARACTER (LEN=*), DIMENSION(nvar),  PARAMETER :: var  = (/ 'O2  ', &
                                                              'N2  ', &
                                                              'p1  ', &
                                                              'TEMP' /)
  REAL(rn),          DIMENSION(nvar),  PARAMETER :: val  = (/  10., 10., 1.5, 2.0  /)
  REAL(rn)                                       :: res
  INTEGER                                        :: i,n, ierr
  REAL                                           :: rt1,rt2,rt3
  REAL(rn)                                       :: o2,n2,temp
  CHARACTER (LEN=50)                             :: c
  CHARACTER (LEN=50), allocatable                :: func(:)
  CHARACTER (LEN=50), allocatable                :: generic(:)
  CHARACTER (LEN=50), allocatable                :: generic_coeff(:)
  ! Read in generic rate coefficients
  write (*,*) 'start'
  ngeneric = count_lines_in_file('generic.txt')
  allocate(generic(ngeneric), generic_coeff(ngeneric))
  write(*,*) 'counted'
  open (10, file='generic.txt', status='old')
  i = 0
  read (10,'(A50)', iostat=ierr) c
  write(*,*) c
  do while ( ierr == 0 )
    i = i + 1
    call split_string(c, generic_coeff(i), generic(i), '=')
    write(*,*) c
    read (10,'(A50)', iostat=ierr) c
  end do
  close (10, status='keep')
  write(*,*) 'read'
  do i=1,ngeneric
    write(*,*) generic_coeff(i), generic(i)
  end do

  CALL initf (ngeneric)                      ! Initialize function parser for nfunc functions
  write(*,*) 'inited'
  DO i=1,ngeneric
    write(*,*) 'parse'
     CALL parsef (i, trim(generic(i)), var)        ! Parse and bytecompile ith function string
  END DO

  ! Read in number of reactions
  write (*,*) 'start'
  nfunc = count_lines_in_file('coeffs.txt')
  allocate(func(nfunc))
  write(*,*) 'counted'
  open (10, file='coeffs.txt', status='old')
  i = 0
  read (10,'(A100)', iostat=ierr) c
  do while ( ierr == 0 )
    i = i + 1
    write(*,*) c
    func(i) = c
    read (10,'(A100)', iostat=ierr) c
  end do
  close (10, status='keep')
  write(*,*) 'read'
  do i=1,nfunc
    write(*,*) func(i)
  end do
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  !
  temp = 300.0
  o2 = 2.0
  n2 = 2.0
  CALL initf (nfunc)                      ! Initialize function parser for nfunc functions
  DO i=1,nfunc
     CALL parsef (i, trim(func(i)), var)        ! Parse and bytecompile ith function string
  END DO
  o2   = val(1)
  n2   = val(2)
  temp = val(3)
  CALL CPU_TIME (rt1)
  DO n=1,neval
     DO i=1,nfunc
        eqParser = EquationParser(trim(func(i)), var)
        res = evalf (i, val)              ! Interprete bytecode representation of ith function
        IF (EvalErrType > 0) WRITE(*,*)'*** Error: ',EvalErrMsg ()
     END DO
  END DO
  CALL CPU_TIME (rt2)
  DO n=1,neval
    res = 5.6D-34*N2*(TEMP/300)**(-2.6)*O2
    res = 6.0D-34*O2*(TEMP/300)**(-2.6)*O2
    res = 8.0D-12*EXP(-2060/TEMP)
  END DO
  CALL CPU_TIME (rt3)
  WRITE(*,*)'Function evaluation:'
  WRITE(*,*)'- Bytecode interpreter cpu time = ',rt2-rt1
  WRITE(*,*)'- Machine code         cpu time = ',rt3-rt2,' = ',(rt3-rt2)/(rt2-rt1)*100.,'% = ',(rt2-rt1)/(rt3-rt2),'x faster'
  !
END PROGRAM fptest

! split a string into 2 either side of a delimiter token
SUBROUTINE split_string(instring, string1, string2, delim)
  CHARACTER(50) :: instring
  CHARACTER(1)  :: delim
  CHARACTER(50), INTENT(OUT) :: string1,string2
  INTEGER :: index

  instring = TRIM(instring)

  index = SCAN(instring,delim)
  string1 = instring(1:index-1)
  string2 = instring(index+1:)

END SUBROUTINE split_string
