!
! Copyright (c) 2000-2008, Roland Schmehl. All rights reserved.
!
! This software is distributable under the BSD license. See the terms of the
! BSD license in the documentation provided with this software.
!
MODULE FortranParser
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  ! Fortran 2008 function parser
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  !
  ! This is an OOP Fortran 2008 version of the original fparser by Roland Schmehl. This simple class
  ! wrapping of the original fparser has been developed by Jacopo Chevallard, and it is available on
  ! the GitHub repository https://github.com/jacopo-chevallard/FortranParser.
  !
  ! For comments and bug reports, please open an issue on
  ! https://github.com/jacopo-chevallard/FortranParser/issues
  !
  ! This function parser module is intended for applications where a set of mathematical
  ! fortran-style expressions is specified at runtime and is then evaluated for a large
  ! number of variable values. This is done by compiling the set of function strings
  ! into byte code, which is interpreted efficiently for the various variable values.
  !
  ! The source code of the original fparser is available from http://fparser.sourceforge.net
  !
  ! Please send comments, corrections or questions realtive to the original fparser to its author:
  ! Roland Schmehl <roland.schmehl@alumni.uni-karlsruhe.de>
  !
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  ! The function parser concept is based on a C++ class library written by  Juha
  ! Nieminen <warp@iki.fi> available from http://warp.povusers.org/FunctionParser/
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  USE types_mod, ONLY: DP,NPI,SI              ! Import KIND parameters

  IMPLICIT NONE

  PUBLIC                     :: EquationParser

  !------- -------- --------- --------- --------- --------- --------- --------- -------
  PRIVATE

  INTEGER(kind=NPI),                        PARAMETER :: cImmed   = 1,          &
                                                         cNeg     = 2,          &
                                                         cAdd     = 3,          &
                                                         cSub     = 4,          &
                                                         cMul     = 5,          &
                                                         cDiv     = 6,          &
                                                         cPow     = 7,          &
                                                         cAbs     = 8,          &
                                                         cExp     = 9,          &
                                                         cLog10   = 10,         &
                                                         cLog     = 11,         &
                                                         cSqrt    = 12,         &
                                                         cSinh    = 13,         &
                                                         cCosh    = 14,         &
                                                         cTanh    = 15,         &
                                                         cSin     = 16,         &
                                                         cCos     = 17,         &
                                                         cTan     = 18,         &
                                                         cAsin    = 19,         &
                                                         cAcos    = 20,         &
                                                         cAtan    = 21,         &
                                                         cQ       = 22,         &
                                                         VarBegin = 23

  CHARACTER (LEN=1), DIMENSION(cAdd:cPow),  PARAMETER :: Ops      = (/ '+',     &
                                                                       '-',     &
                                                                       '*',     &
                                                                       '/',     &
                                                                       '^' /)

  CHARACTER (LEN=5), DIMENSION(cAbs:cQ), PARAMETER :: Funcs       = (/ 'abs  ', &
                                                                       'exp  ', &
                                                                       'log10', &
                                                                       'log  ', &
                                                                       'sqrt ', &
                                                                       'sinh ', &
                                                                       'cosh ', &
                                                                       'tanh ', &
                                                                       'sin  ', &
                                                                       'cos  ', &
                                                                       'tan  ', &
                                                                       'asin ', &
                                                                       'acos ', &
                                                                       'atan ', &
                                                                       'q    ' /)

  INTEGER, parameter  :: MAX_FUN_LENGTH = 1024

  TYPE EquationParser

    INTEGER(NPI), POINTER :: ByteCode(:) => null()
    INTEGER(NPI)          :: ByteCodeSize = 0_NPI
    REAL(DP),     POINTER :: Immed(:) => null()
    INTEGER(NPI)          :: ImmedSize = 0_NPI
    REAL(DP),     POINTER :: Stack(:) => null()
    INTEGER(NPI)          :: StackSize = 0
    INTEGER(NPI)          :: StackPtr = 0

    character(len=MAX_FUN_LENGTH) :: funcString = ''
    character(len=MAX_FUN_LENGTH) :: funcStringOrig = ''
    character(len=MAX_FUN_LENGTH), allocatable :: variableNames(:)
    contains

      private

      procedure, public :: evaluate
      procedure :: parse
      procedure :: Compile
      procedure :: AddCompiledByte
      procedure :: CompileSubstr
      procedure :: MathItemIndex
      procedure :: CheckSyntax

      final :: finalize

  END TYPE EquationParser

  ! Class constructor
  interface EquationParser
    procedure constructor
  end interface EquationParser

CONTAINS

!*****************************************************************************************
  type (EquationParser) function constructor(FuncStr, Var)

    CHARACTER (LEN=*),               INTENT(in) :: FuncStr   ! Function string
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Var       ! Array with variable names

    constructor%ByteCode => null()
    constructor%Immed => null()
    constructor%Stack => null()

    constructor%ByteCodeSize = 0_NPI
    constructor%ImmedSize = 0_NPI
    constructor%StackSize = 0_NPI
    constructor%StackPtr = 0_NPI

    constructor%funcString = FuncStr
    constructor%funcStringOrig = FuncStr

    allocate(constructor%variableNames(size(Var)))

    constructor%variableNames(:) = Var(:)

    call constructor%parse()

  end function constructor

!*****************************************************************************************
  subroutine finalize(this)

    type(EquationParser) :: this

    if (associated(this%ByteCode))  nullify(this%ByteCode)
    if (associated(this%Immed))     nullify(this%Immed)
    if (associated(this%Stack))     nullify(this%Stack)

  end subroutine finalize

!*****************************************************************************************
  SUBROUTINE parse(this)
    ! Parse ith function string FuncStr and compile it into bytecode
    class(EquationParser) :: this

    CALL Replace('**','^ ', this%funcString)                            ! Exponent into 1-Char. format

    CALL RemoveSpaces(this%funcString)                                 ! Condense function string

    CALL this%CheckSyntax()

    CALL this%Compile()                                ! Compile into bytecode

  END SUBROUTINE parse

!*****************************************************************************************
  FUNCTION evaluate(this, Val, q) RESULT (res)
    ! Evaluate bytecode of ith function for the values passed in array Val(:)
    USE types_mod
    class(EquationParser) :: this
    REAL(kind=DP), DIMENSION(:), INTENT(in) :: Val, q             ! Variable values

    REAL(kind=DP)                          :: res                ! Result
    INTEGER(kind=NPI)                      :: IPo,              & ! Instruction pointer
                                              DPo,              & ! Data pointer
                                              SPo                 ! Stack pointer
    REAL(kind=DP),               PARAMETER :: zero = 0._DP
    INTEGER(kind=SI)                       :: EvalErrType

    DPo = 1_NPI
    SPo = 0_NPI
    EvalErrType=0_SI

    DO IPo=1_NPI,this%ByteCodeSize

       SELECT CASE (this%ByteCode(IPo))

       CASE (cImmed); SPo=SPo+1_NPI; this%Stack(SPo)=this%Immed(DPo); DPo=DPo+1_NPI

       CASE   (cNeg); this%Stack(SPo)=-this%Stack(SPo)

       CASE   (cAdd); this%Stack(SPo-1_NPI)=this%Stack(SPo-1_NPI)+this%Stack(SPo); SPo=SPo-1_NPI

       CASE   (cSub); this%Stack(SPo-1_NPI)=this%Stack(SPo-1_NPI)-this%Stack(SPo); SPo=SPo-1_NPI

       CASE   (cMul); this%Stack(SPo-1_NPI)=this%Stack(SPo-1_NPI)*this%Stack(SPo); SPo=SPo-1_NPI

       CASE   (cDiv)

         IF (this%Stack(SPo)==0._DP) THEN
           EvalErrType=1_SI
           res=zero
           exit
         ENDIF
         this%Stack(SPo-1_NPI)=this%Stack(SPo-1_NPI)/this%Stack(SPo); SPo=SPo-1_NPI

       CASE   (cPow); this%Stack(SPo-1_NPI)=this%Stack(SPo-1_NPI)**this%Stack(SPo); SPo=SPo-1_NPI

       CASE   (cAbs); this%Stack(SPo)=ABS(this%Stack(SPo))

       CASE   (cExp); this%Stack(SPo)=EXP(this%Stack(SPo))

       CASE (cLog10)

         IF (this%Stack(SPo)<=0._DP) THEN
           EvalErrType=3_SI
           res=zero
           exit
         ENDIF
         this%Stack(SPo)=LOG10(this%Stack(SPo))

       CASE   (cLog)

         IF (this%Stack(SPo)<=0._DP) THEN
           EvalErrType=3_SI
           res=zero
           exit
         ENDIF
         this%Stack(SPo)=LOG(this%Stack(SPo))

       CASE  (cSqrt)

         IF (this%Stack(SPo)<0._DP) THEN
           EvalErrType=3_SI
           res=zero
           exit
         ENDIF
         this%Stack(SPo)=SQRT(this%Stack(SPo))

       CASE  (cSinh); this%Stack(SPo)=SINH(this%Stack(SPo))

       CASE  (cCosh); this%Stack(SPo)=COSH(this%Stack(SPo))

       CASE  (cTanh); this%Stack(SPo)=TANH(this%Stack(SPo))

       CASE   (cSin); this%Stack(SPo)=SIN(this%Stack(SPo))

       CASE   (cCos); this%Stack(SPo)=COS(this%Stack(SPo))

       CASE   (cTan); this%Stack(SPo)=TAN(this%Stack(SPo))

       CASE  (cAsin)

         IF ((this%Stack(SPo)<-1._DP) .OR. (this%Stack(SPo)>1._DP)) THEN
           EvalErrType=4_SI
           res=zero
           exit
         ENDIF
         this%Stack(SPo)=ASIN(this%Stack(SPo))

       CASE  (cAcos);
         IF ((this%Stack(SPo)<-1._DP).OR.(this%Stack(SPo)>1._DP)) THEN
           EvalErrType=4_SI
           res=zero
           exit
         ENDIF
         this%Stack(SPo)=ACOS(this%Stack(SPo))

       CASE  (cAtan); this%Stack(SPo)=ATAN(this%Stack(SPo))

       CASE     (cQ); this%Stack(SPo)=q(INT(this%Stack(SPo)))

       CASE  DEFAULT; SPo=SPo+1_NPI; this%Stack(SPo)=Val(this%ByteCode(IPo)-VarBegin+1_NPI)

       END SELECT

    END DO

    IF (EvalErrType > 0_SI) then
      WRITE(*,*)'*** Error: ',EvalErrMsg(EvalErrType)
    else
      res = this%Stack(1)
    endif

  END FUNCTION evaluate

!*****************************************************************************************
  SUBROUTINE CheckSyntax(this)
    ! Check syntax of function string,  returns 0 if syntax is ok
    class(EquationParser) :: this
    INTEGER(kind=NPI)                           :: n
    CHARACTER(LEN=1)                            :: c
    REAL(kind=DP)                               :: r
    LOGICAL                                     :: err
    INTEGER(kind=SI)                            :: ParCnt ! Parenthesis counter
    INTEGER(kind=NPI)                           :: ib, in, j
    INTEGER(kind=NPI)                           :: lFunc

    j = 1_NPI
    ParCnt = 0_SI
    lFunc = LEN_TRIM(this%funcString, KIND(1_NPI))
    step: DO
       IF (j > lFunc) CALL ParseErrMsg (j, this%funcStringOrig)
       c = this%funcString(j:j)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Check for valid operand (must appear)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       IF (c == '-' .OR. c == '+') THEN                      ! Check for leading - or +
          j = j+1_NPI
          IF (j > lFunc) CALL ParseErrMsg (j, this%funcStringOrig, 'Missing operand')
          c = this%funcString(j:j)
          IF (ANY(c == Ops)) CALL ParseErrMsg (j, this%funcStringOrig, 'Multiple operators')
       END IF
       n = MathFunctionIndex (this%funcString(j:))
       IF (n > 0_NPI) THEN                                       ! Check for math function
          j = j+LEN_TRIM(Funcs(n))
          IF (j > lFunc) CALL ParseErrMsg (j, this%funcStringOrig, 'Missing function argument')
          c = this%funcString(j:j)
          IF (c /= '(') CALL ParseErrMsg (j, this%funcStringOrig, 'Missing opening parenthesis')
       END IF
       IF (c == '(') THEN                                    ! Check for opening parenthesis
          ParCnt = ParCnt+1_SI
          j = j+1_NPI
          CYCLE step
       END IF
       IF (SCAN(c,'0123456789.') > 0) THEN                   ! Check for number
          r = RealNum (this%funcString(j:),ib,in,err)
          IF (err) CALL ParseErrMsg (j, this%funcStringOrig, 'Invalid number format:  '//this%funcString(j+ib-1:j+in-2))
          j = j+in-1_NPI
          IF (j > lFunc) EXIT
          c = this%funcString(j:j)
       ELSE                                                  ! Check for variable
          n = VariableIndex (this%funcString(j:),this%variableNames,ib,in)
          IF (n == 0_NPI) CALL ParseErrMsg (j, this%funcStringOrig, 'Invalid element: '//this%funcString(j+ib-1:j+in-2))
          j = j+in-1_NPI
          IF (j > lFunc) EXIT
          c = this%funcString(j:j)
       END IF
       DO WHILE (c == ')')                                   ! Check for closing parenthesis
          ParCnt = ParCnt-1_SI
          IF (ParCnt < 0_SI) CALL ParseErrMsg (j, this%funcStringOrig, 'Mismatched parenthesis')
          IF (this%funcString(j-1_NPI:j-1_NPI) == '(') CALL ParseErrMsg (j-1_NPI, this%funcStringOrig, 'Empty parentheses')
          j = j+1_NPI
          IF (j > lFunc) EXIT
          c = this%funcString(j:j)
       END DO
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Now, we have a legal operand: A legal operator or end of string must follow
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       IF (j > lFunc) EXIT
       IF (ANY(c == Ops)) THEN                               ! Check for multiple operators
          IF (j+1_NPI > lFunc) CALL ParseErrMsg (j, this%funcStringOrig)
          IF (ANY(this%funcString(j+1_NPI:j+1_NPI) == Ops)) CALL ParseErrMsg (j+1_NPI, this%funcStringOrig, 'Multiple operators')
       ELSE                                                  ! Check for next operand
          CALL ParseErrMsg (j, this%funcStringOrig, 'Missing operator')
       END IF
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Now, we have an operand and an operator: the next loop will check for another
       ! operand (must appear)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       j = j+1_NPI
    END DO step
    IF (ParCnt > 0_SI) CALL ParseErrMsg (j, this%funcStringOrig, 'Missing )')
  END SUBROUTINE CheckSyntax

!*****************************************************************************************
  FUNCTION EvalErrMsg(EvalErrType) RESULT (msg)
    ! Return error message
    INTEGER(kind=SI), INTENT(in) :: EvalErrType
    CHARACTER (LEN=*), DIMENSION(4), PARAMETER :: m = (/ 'Division by zero                ', &
                                                         'Argument of SQRT negative       ', &
                                                         'Argument of LOG negative        ', &
                                                         'Argument of ASIN or ACOS illegal' /)
    CHARACTER (LEN=LEN(m))                     :: msg
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IF (EvalErrType < 1_SI .OR. EvalErrType > SIZE(m)) THEN
       msg = ''
    ELSE
       msg = m(EvalErrType)
    ENDIF

  END FUNCTION EvalErrMsg

!*****************************************************************************************
  SUBROUTINE ParseErrMsg (j, FuncStr, Msg)
    ! Print error message and terminate program
    INTEGER(kind=NPI),           INTENT(in) :: j
    CHARACTER (LEN=*),           INTENT(in) :: FuncStr       ! Original function string
    CHARACTER (LEN=*), OPTIONAL, INTENT(in) :: Msg

    IF (PRESENT(Msg)) THEN
       WRITE(*,*) '*** Error in syntax of function string: '//Msg
    ELSE
       WRITE(*,*) '*** Error in syntax of function string:'
    ENDIF

    WRITE(*,*)
    WRITE(*,'(A)') ' '//FuncStr

    WRITE(*,'(A)') '?'
    STOP
  END SUBROUTINE ParseErrMsg

!*****************************************************************************************
  FUNCTION OperatorIndex (c) RESULT (n)
    ! Return operator index
    CHARACTER (LEN=1), INTENT(in) :: c
    INTEGER(NPI)                  :: n,j

    n = 0

    DO j=cAdd,cPow
       IF (c == Ops(j)) THEN
          n = j
          EXIT
       END IF
    END DO

  END FUNCTION OperatorIndex

!*****************************************************************************************
  FUNCTION MathFunctionIndex (str) RESULT (n)
    ! Return index of math function beginnig at 1st position of string str
    CHARACTER (LEN=*), INTENT(in) :: str

    INTEGER(kind=NPI)             :: n, j, k
    CHARACTER (LEN=LEN(Funcs))    :: fun

    n = 0_NPI

    DO j=cAbs,cQ                                             ! Check all math functions
       k = MIN(LEN_TRIM(Funcs(j)), LEN(str))
       CALL LowCase (str(1:k), fun)
       IF (fun == Funcs(j)) THEN                             ! Compare lower case letters
          n = j                                              ! Found a matching function
          EXIT
       END IF
    END DO

  END FUNCTION MathFunctionIndex

!*****************************************************************************************
  FUNCTION VariableIndex (str, Var, ibegin, inext) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return index of variable at begin of string str (returns 0 if no variable found)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),               INTENT(in) :: str       ! String
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Var       ! Array with variable names
    INTEGER(kind=NPI)                           :: n         ! Index of variable
    INTEGER(kind=NPI), OPTIONAL,    INTENT(out) :: ibegin, & ! Start position of variable name
                                                   inext     ! Position of character after name
    INTEGER(kind=NPI)                           :: j, ib, in, lstr
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0_NPI
    lstr = LEN_TRIM(str)
    IF (lstr > 0_NPI) THEN
       DO ib=1_NPI,lstr                                          ! Search for first character in str
          IF (str(ib:ib) /= ' ') EXIT                        ! When lstr>0 at least 1 char in str
       END DO
       DO in=ib,lstr                                         ! Search for name terminators
          IF (SCAN(str(in:in),'+-*/^) ') > 0) EXIT
       END DO
       DO j=1_NPI,SIZE(Var)
          IF (str(ib:in-1_NPI) == Var(j)) THEN
             n = j                                           ! Variable name found
             EXIT
          END IF
       END DO
    END IF
    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in
  END FUNCTION VariableIndex

!*****************************************************************************************
  SUBROUTINE RemoveSpaces (str)
    ! Remove Spaces from string, remember positions of characters in old string
    CHARACTER (LEN=*), INTENT(inout) :: str

    INTEGER(kind=NPI)                :: k,lstr

    lstr = LEN_TRIM(str,KIND(1_NPI))

    k = 1_NPI

    DO WHILE (str(k:lstr) /= ' ')
       IF (str(k:k) == ' ') THEN
          str(k:lstr)  = str(k+1_NPI:lstr)//' '                  ! Move 1 character to left
          k = k-1_NPI
       END IF
       k = k+1_NPI
    END DO

  END SUBROUTINE RemoveSpaces

!*****************************************************************************************
  SUBROUTINE Replace (ca,cb,str)
    ! Replace ALL appearances of character set ca in string str by character set cb
    CHARACTER (LEN=*),       INTENT(in) :: ca
    CHARACTER (LEN=LEN(ca)), INTENT(in) :: cb                ! LEN(ca) must be LEN(cb)
    CHARACTER (LEN=*),    INTENT(inout) :: str

    INTEGER(kind=NPI)                   :: j,lca

    lca = LEN(ca,KIND(1_NPI))

    DO j=1_NPI,LEN_TRIM(str)-lca+1_NPI
       IF (str(j:j+lca-1_NPI) == ca) str(j:j+lca-1_NPI) = cb
    END DO

  END SUBROUTINE Replace

!*****************************************************************************************
  SUBROUTINE Compile(this)
    ! Compile i-th function string F into bytecode
    class(EquationParser) :: this
    INTEGER                                     :: istat

    IF (ASSOCIATED(this%ByteCode)) DEALLOCATE ( this%ByteCode, &
                                                this%Immed,    &
                                                this%Stack     )
    this%ByteCodeSize = 0_NPI
    this%ImmedSize    = 0_NPI
    this%StackSize    = 0_NPI
    this%StackPtr     = 0_NPI

    CALL this%CompileSubstr(1_NPI,INT(LEN_TRIM(this%funcString),KIND(1_NPI)))

    ALLOCATE ( this%ByteCode(this%ByteCodeSize), &
               this%Immed(this%ImmedSize),       &
               this%Stack(this%StackSize),       &
               STAT = istat                            )
    IF (istat /= 0) THEN
       WRITE(*,*) '*** Parser error: Memmory allocation for byte code failed'
       STOP
    ELSE
       this%ByteCodeSize = 0_NPI
       this%ImmedSize    = 0_NPI
       this%StackSize    = 0_NPI
       this%StackPtr     = 0_NPI
       CALL this%CompileSubstr(1_NPI,INT(LEN_TRIM(this%funcString),KIND(1_NPI)))
    END IF

  END SUBROUTINE Compile

!*****************************************************************************************
  SUBROUTINE AddCompiledByte(this, b)
    ! Add compiled byte to bytecode
    class(EquationParser) :: this
    INTEGER(kind=NPI), INTENT(in) :: b                             ! Value of byte to be added

    this%ByteCodeSize = this%ByteCodeSize + 1_NPI

    IF (ASSOCIATED(this%ByteCode)) then
      this%ByteCode(this%ByteCodeSize) = b
    endif

  END SUBROUTINE AddCompiledByte

!*****************************************************************************************
  FUNCTION MathItemIndex(this, b, e) RESULT (n)
    ! Return math item index, if item is real number, enter it into Comp-structure
    class(EquationParser) :: this
    INTEGER(kind=NPI), INTENT(in) :: b, e      ! First and last pos. of substring
    INTEGER(kind=NPI)             :: n         ! Byte value of math item

    n = 0_NPI

    IF (SCAN(this%funcString(b:b),'0123456789.') > 0_NPI) THEN                 ! Check for begin of a number
       this%ImmedSize = this%ImmedSize + 1_NPI
       IF (ASSOCIATED(this%Immed)) this%Immed(this%ImmedSize) = RealNum(this%funcString(b:e))
       n = cImmed
    ELSE                                                     ! Check for a variable
       n = VariableIndex(this%funcString(b:e), this%variableNames)
       IF (n > 0_NPI) n = VarBegin+n-1_NPI
    END IF

  END FUNCTION MathItemIndex

!*****************************************************************************************
  FUNCTION CompletelyEnclosed (F, b, e) RESULT (res)
    ! Check if function substring F(b:e) is completely enclosed by a pair of parenthesis
    CHARACTER (LEN=*), INTENT(in) :: F                       ! Function substring
    INTEGER(kind=NPI), INTENT(in) :: b,e                     ! First and last pos. of substring

    LOGICAL                       :: res
    INTEGER(kind=NPI)             :: j
    INTEGER(kind=SI)              :: k

    res=.false.

    IF (F(b:b) == '(' .AND. F(e:e) == ')') THEN
       k = 0_SI
       DO j = b + 1_NPI, e - 1_NPI
          IF     (F(j:j) == '(') THEN
             k = k + 1_SI
          ELSEIF (F(j:j) == ')') THEN
             k = k - 1_SI
          END IF
          IF (k < 0_SI) EXIT
       END DO
       IF (k == 0_SI) res=.true.                                ! All opened parenthesis closed
    END IF

  END FUNCTION CompletelyEnclosed

!*****************************************************************************************
  RECURSIVE SUBROUTINE CompileSubstr(this, b, e)
    ! Compile i-th function string funcString into bytecode
    class(EquationParser) :: this
    INTEGER(kind=NPI),               INTENT(in) :: b, e      ! Begin and end position substring

    INTEGER(kind=NPI)                           :: n
    INTEGER(kind=NPI)                           :: b2, j, k
    INTEGER(kind=SI)                            :: io
    CHARACTER (LEN=*),                PARAMETER :: calpha = 'abcdefghijklmnopqrstuvwxyz'// &
                                                            'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ! Check for special cases of substring

    IF     (this%funcString(b:b) == '+') THEN                              ! Case 1: funcString(b:e) = '+...'
!      WRITE(*,*)'1. funcString(b:e) = "+..."'
       CALL this%CompileSubstr(b+1_NPI, e)
       RETURN
    ELSEIF (CompletelyEnclosed (this%funcString, b, e)) THEN               ! Case 2: funcString(b:e) = '(...)'
!      WRITE(*,*)'2. funcString(b:e) = "(...)"'
       CALL this%CompileSubstr(b+1_NPI, e-1_NPI)
       RETURN
    ELSEIF (SCAN(this%funcString(b:b), calpha) > 0) THEN
       n = MathFunctionIndex(this%funcString(b:e))
       IF (n > 0_NPI) THEN
          b2 = b+INDEX(this%funcString(b:e),'(')-1_NPI
          IF (CompletelyEnclosed(this%funcString, b2, e)) THEN             ! Case 3: funcString(b:e) = 'fcn(...)'
!            WRITE(*,*)'3. funcString(b:e) = "fcn(...)"'
             CALL this%CompileSubstr(b2+1, e-1)
             CALL this%AddCompiledByte(n)
             RETURN
          END IF
       END IF

    ELSEIF (this%funcString(b:b) == '-') THEN
       IF (CompletelyEnclosed(this%funcString, b+1_NPI, e)) THEN              ! Case 4: this%funcString(b:e) = '-(...)'
!         WRITE(*,*)'4. this%funcString(b:e) = "-(...)"'
          CALL this%CompileSubstr(b+2_NPI, e-1_NPI)
          CALL this%AddCompiledByte(cNeg)
          RETURN
       ELSEIF (SCAN(this%funcString(b+1_NPI:b+1_NPI),calpha) > 0) THEN
          n = MathFunctionIndex(this%funcString(b+1_NPI:e))
          IF (n > 0_NPI) THEN
             b2 = b+INDEX(this%funcString(b+1_NPI:e),'(')
             IF (CompletelyEnclosed(this%funcString, b2, e)) THEN          ! Case 5: this%funcString(b:e) = '-fcn(...)'
!               WRITE(*,*)'5. this%funcString(b:e) = "-fcn(...)"'
                CALL this%CompileSubstr(b2+1_NPI, e-1_NPI);
                CALL this%AddCompiledByte(n)
                CALL this%AddCompiledByte(cNeg)
                RETURN
             END IF
          END IF
       ENDIF
    END IF

    ! Check for operator in substring: check only base level (k=0), exclude expr. in ()

    DO io=cAdd,cPow                                          ! Increasing priority +-*/^
       k = 0_NPI
       DO j=e,b,-1_NPI
          IF     (this%funcString(j:j) == ')') THEN
             k = k+1_NPI
          ELSEIF (this%funcString(j:j) == '(') THEN
             k = k-1_NPI
          END IF
          IF (k == 0_NPI .AND. this%funcString(j:j) == Ops(io) .AND. IsBinaryOp (j, this%funcString)) THEN
             IF (ANY(this%funcString(j:j) == Ops(cMul:cPow)) .AND. this%funcString(b:b) == '-') THEN ! Case 6: this%funcString(b:e) = '-...Op...' with Op > -
!               WRITE(*,*)'6. this%funcString(b:e) = "-...Op..." with Op > -'
                CALL this%CompileSubstr(b+1_NPI, e)
                CALL this%AddCompiledByte(cNeg)
                RETURN
             ELSE                                                        ! Case 7: this%funcString(b:e) = '...BinOp...'
!               WRITE(*,*)'7. Binary operator',this%funcString(j:j)
                CALL this%CompileSubstr(b, j-1_NPI)
                CALL this%CompileSubstr(j+1_NPI, e)
                CALL this%AddCompiledByte(OperatorIndex(Ops(io)))
                this%StackPtr = this%StackPtr - 1_NPI
                RETURN
             END IF
          END IF
       END DO
    END DO

    ! Check for remaining items, i.e. variables or explicit numbers

    b2 = b

    IF (this%funcString(b:b) == '-') b2 = b2+1_NPI

    n = this%MathItemIndex(b2, e)

!   WRITE(*,*)'8. AddCompiledByte ',n
    CALL this%AddCompiledByte(n)

    this%StackPtr = this%StackPtr + 1_NPI
    IF (this%StackPtr > this%StackSize) this%StackSize = this%StackSize + 1_NPI

    IF (b2 > b) CALL this%AddCompiledByte(cNeg)

  END SUBROUTINE CompileSubstr

!*****************************************************************************************
  FUNCTION IsBinaryOp (j, F) RESULT (res)
    ! Check if operator F(j:j) in string F is binary operator
    ! Special cases already covered elsewhere:              (that is corrected in v1.1)
    ! - operator character F(j:j) is first character of string (j=1)
    INTEGER(kind=NPI), INTENT(in) :: j                       ! Position of Operator
    CHARACTER (LEN=*), INTENT(in) :: F                       ! String

    LOGICAL                       :: res                     ! Result
    INTEGER(kind=NPI)             :: k
    LOGICAL                       :: Dflag,Pflag

    res=.true.

    IF (F(j:j) == '+' .OR. F(j:j) == '-') THEN               ! Plus or minus sign:
       IF (j == 1_NPI) THEN                                      ! - leading unary operator ?
          res = .false.
  ELSEIF (SCAN(F(j-1_NPI:j-1_NPI),'+-*/^(') > 0) THEN           ! - other unary operator ?
          res = .false.
  ELSEIF (SCAN(F(j+1_NPI:j+1_NPI),'0123456789') > 0 .AND. &     ! - in exponent of real number ?
               SCAN(F(j-1_NPI:j-1_NPI),'eEdD')       > 0) THEN
          Dflag=.false.; Pflag=.false.
          k = j-1_NPI
          DO WHILE (k > 1_NPI)                                   !   step to the left in mantissa
             k = k-1_NPI
             IF     (SCAN(F(k:k),'0123456789') > 0) THEN
                Dflag=.true.
             ELSEIF (F(k:k) == '.') THEN
                IF (Pflag) THEN
                   EXIT                                      !   * EXIT: 2nd appearance of '.'
                ELSE
                   Pflag=.true.                              !   * mark 1st appearance of '.'
                ENDIF
             ELSE
                EXIT                                         !   * all other characters
             END IF
          END DO
          IF (Dflag .AND. (k == 1_NPI .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
       END IF
    END IF
  END FUNCTION IsBinaryOp

!*****************************************************************************************
  FUNCTION RealNum(str, ibegin, inext, error) RESULT (res)
    ! Get real number from string - Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
    CHARACTER (LEN=*),  INTENT(in) :: str                    ! String
    REAL(kind=DP)                  :: res                    ! Real number
    INTEGER(kind=NPI), OPTIONAL, INTENT(out) :: ibegin,    & ! Start position of real number
                                                inext        ! 1st character after real number
    LOGICAL, OPTIONAL, INTENT(out) :: error                  ! Error flag

    INTEGER(kind=NPI)              :: ib,in
    INTEGER                        :: istat
    LOGICAL                        :: Bflag,               & ! .T. at begin of number in str
                                      InMan,               & ! .T. in mantissa of number
                                      Pflag,               & ! .T. after 1st '.' encountered
                                      Eflag,               & ! .T. at exponent identifier 'eEdD'
                                      InExp,               & ! .T. in exponent of number
                                      DInMan,              & ! .T. if at least 1 digit in mant.
                                      DInExp,              & ! .T. if at least 1 digit in exp.
                                      err                    ! Local error flag
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    Bflag=.true.; InMan=.false.; Pflag=.false.; Eflag=.false.; InExp=.false.
    DInMan=.false.; DInExp=.false.
    ib   = 1_NPI
    in   = 1_NPI
    DO WHILE (in <= LEN_TRIM(str))
       SELECT CASE (str(in:in))
       CASE (' ')                                            ! Only leading blanks permitted
          ib = ib+1_NPI
          IF (InMan .OR. Eflag .OR. InExp) EXIT
       CASE ('+','-')                                        ! Permitted only
          IF     (Bflag) THEN
             InMan=.true.; Bflag=.false.                     ! - at beginning of mantissa
          ELSEIF (Eflag) THEN
             InExp=.true.; Eflag=.false.                     ! - at beginning of exponent
          ELSE
             EXIT                                            ! - otherwise STOP
          ENDIF
       CASE ('0':'9')                                        ! Mark
          IF     (Bflag) THEN
             InMan=.true.; Bflag=.false.                     ! - beginning of mantissa
          ELSEIF (Eflag) THEN
             InExp=.true.; Eflag=.false.                     ! - beginning of exponent
          ENDIF
          IF (InMan) DInMan=.true.                           ! Mantissa contains digit
          IF (InExp) DInExp=.true.                           ! Exponent contains digit
       CASE ('.')
          IF     (Bflag) THEN
             Pflag=.true.                                    ! - mark 1st appearance of '.'
             InMan=.true.; Bflag=.false.                     !   mark beginning of mantissa
          ELSEIF (InMan .AND..NOT.Pflag) THEN
             Pflag=.true.                                    ! - mark 1st appearance of '.'
          ELSE
             EXIT                                            ! - otherwise STOP
          END IF
       CASE ('e','E','d','D')                                ! Permitted only
          IF (InMan) THEN
             Eflag=.true.; InMan=.false.                     ! - following mantissa
          ELSE
             EXIT                                            ! - otherwise STOP
          ENDIF
       CASE DEFAULT
          EXIT                                               ! STOP at all other characters
       END SELECT
       in = in+1_NPI
    END DO
    err = (ib > in-1_NPI) .OR. (.NOT.DInMan) .OR. ((Eflag.OR.InExp).AND..NOT.DInExp)
    IF (err) THEN
       res = 0.0_DP
    ELSE
       READ(str(ib:in-1_NPI),*,IOSTAT=istat) res
       err = istat /= 0
    END IF
    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in
    IF (PRESENT(error))  error  = err
  END FUNCTION RealNum

!*****************************************************************************************
  SUBROUTINE LowCase (str1, str2)
    ! Transform upper case letters in str1 into lower case letters, result is str2
    IMPLICIT NONE
    CHARACTER (LEN=*),  INTENT(in) :: str1
    CHARACTER (LEN=*), INTENT(out) :: str2
    INTEGER                        :: j,k
    CHARACTER (LEN=*),   PARAMETER :: lc = 'abcdefghijklmnopqrstuvwxyz'
    CHARACTER (LEN=*),   PARAMETER :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    str2 = str1

    DO j=1,LEN_TRIM(str1)
       k = INDEX(uc,str1(j:j))
       IF (k > 0) str2(j:j) = lc(k:k)
    END DO

  END SUBROUTINE LowCase

  END MODULE FortranParser
