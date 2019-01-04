!
! Copyright (c) 2000-2008, Roland Schmehl. All rights reserved.
!
! This software is distributable under the BSD license. See the terms of the
! BSD license in the documentation provided with this software.
!
module FortranParser
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
  use types_mod, only : DP, SI              ! Import KIND parameters

  implicit none

  public :: EquationParser

  !------- -------- --------- --------- --------- --------- --------- --------- -------
  private

  integer(kind=SI), parameter :: cImmed   = 1, &
                                 cNeg     = 2, &
                                 cAdd     = 3, &
                                 cSub     = 4, &
                                 cMul     = 5, &
                                 cDiv     = 6, &
                                 cPow     = 7, &
                                 cAbs     = 8, &
                                 cExp     = 9, &
                                 cLog10   = 10, &
                                 cLog     = 11, &
                                 cSqrt    = 12, &
                                 cSinh    = 13, &
                                 cCosh    = 14, &
                                 cTanh    = 15, &
                                 cSin     = 16, &
                                 cCos     = 17, &
                                 cTan     = 18, &
                                 cAsin    = 19, &
                                 cAcos    = 20, &
                                 cAtan    = 21, &
                                 cQ       = 22, &
                                 cJ       = 23, &
                                 VarBegin = 24

  character(len=1), dimension(cAdd:cPow), parameter :: Ops = (/ '+', &
                                                                '-', &
                                                                '*', &
                                                                '/', &
                                                                '^' /)

  character(len=5), dimension(cAbs:cJ), parameter :: Funcs = (/ 'abs  ', &
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
                                                                'q    ', &
                                                                'j    ' /)

  integer(kind=SI), parameter :: MAX_FUN_LENGTH = 127_SI

  type EquationParser

    integer(SI), pointer :: ByteCode(:) => null()
    integer(SI) :: ByteCodeSize = 0_SI
    real(DP), pointer :: Immed(:) => null()
    integer(SI) :: ImmedSize = 0_SI
    real(DP), pointer :: Stack(:) => null()
    integer(SI) :: StackSize = 0
    integer(SI) :: StackPtr = 0

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

    procedure, public :: finalize

  end type EquationParser

  ! Class constructor
  interface EquationParser
    procedure constructor
  end interface EquationParser

contains

  !*****************************************************************************************
  type (EquationParser) function constructor(FuncStr, Var)

    character(len=*), intent(in) :: FuncStr   ! Function string
    character(len=*), dimension(:), intent(in) :: Var       ! Array with variable names

    constructor%ByteCode => null()
    constructor%Immed => null()
    constructor%Stack => null()

    constructor%ByteCodeSize = 0_SI
    constructor%ImmedSize = 0_SI
    constructor%StackSize = 0_SI
    constructor%StackPtr = 0_SI

    constructor%funcString = FuncStr
    constructor%funcStringOrig = FuncStr

    allocate(constructor%variableNames(size(Var)))

    constructor%variableNames(:) = Var(:)

    call constructor%parse()

  end function constructor

  !*****************************************************************************************
  subroutine finalize( this )

    class(EquationParser) :: this

    if (associated(this%ByteCode)) nullify(this%ByteCode)
    if (associated(this%Immed)) nullify(this%Immed)
    if (associated(this%Stack)) nullify(this%Stack)

  end subroutine finalize

  !*****************************************************************************************
  subroutine parse( this )
    ! Parse ith function string FuncStr and compile it into bytecode
    class(EquationParser) :: this

    call Replace( '**', '^ ', this%funcString ) ! Exponent into 1-Char. format

    call RemoveSpaces( this%funcString ) ! Condense function string

    call this%CheckSyntax()

    call this%Compile()! Compile into bytecode

  end subroutine parse

  !*****************************************************************************************
  function evaluate( this, Val, q, j) result (res )
    ! Evaluate bytecode of ith function for the values passed in array Val(:)
    use types_mod
    class(EquationParser) :: this
    real(kind=DP), dimension(:), intent(in) :: Val, q, j          ! Variable values

    real(kind=DP) :: res       ! Result
    integer(kind=SI) :: DPo    ! Data pointer
    integer(kind=SI) :: IPo, & ! Instruction pointer
                        SPo    ! Stack pointer
    real(kind=DP), parameter :: zero = 0._DP
    integer(kind=SI) :: EvalErrType

    DPo = 1_SI
    SPo = 0_SI
    EvalErrType=0_SI

    do IPo=1_SI, this%ByteCodeSize

      select case (this%ByteCode(IPo))

        case (cImmed); SPo=SPo+1_SI; this%Stack(SPo)=this%Immed(DPo); DPo=DPo+1_SI

        case   (cNeg); this%Stack(SPo)=-this%Stack(SPo)

        case   (cAdd); this%Stack(SPo-1_SI)=this%Stack(SPo-1_SI)+this%Stack(SPo); SPo=SPo-1_SI

        case   (cSub); this%Stack(SPo-1_SI)=this%Stack(SPo-1_SI)-this%Stack(SPo); SPo=SPo-1_SI

        case   (cMul); this%Stack(SPo-1_SI)=this%Stack(SPo-1_SI)*this%Stack(SPo); SPo=SPo-1_SI

        case   (cDiv)

          if (this%Stack(SPo)==0._DP) then
            EvalErrType=1_SI
            res=zero
            exit
          end if
          this%Stack(SPo-1_SI)=this%Stack(SPo-1_SI)/this%Stack(SPo); SPo=SPo-1_SI

        case   (cPow); this%Stack(SPo-1_SI)=this%Stack(SPo-1_SI)**this%Stack(SPo); SPo=SPo-1_SI

        case   (cAbs); this%Stack(SPo)=abs(this%Stack(SPo))

        case   (cExp); this%Stack(SPo)=exp(this%Stack(SPo))

        case (cLog10)

          if (this%Stack(SPo)<=0._DP) then
            EvalErrType=3_SI
            res=zero
            exit
          end if
          this%Stack(SPo)=log10(this%Stack(SPo))

        case   (cLog)

          if (this%Stack(SPo)<=0._DP) then
            EvalErrType=3_SI
            res=zero
            exit
          end if
          this%Stack(SPo)=LOG(this%Stack(SPo))

        case  (cSqrt)

          if (this%Stack(SPo)<0._DP) then
            EvalErrType=3_SI
            res=zero
            exit
          end if
          this%Stack(SPo)=SQRT(this%Stack(SPo))

        case  (cSinh); this%Stack(SPo)=SINH(this%Stack(SPo))

        case  (cCosh); this%Stack(SPo)=COSH(this%Stack(SPo))

        case  (cTanh); this%Stack(SPo)=TANH(this%Stack(SPo))

        case   (cSin); this%Stack(SPo)=SIN(this%Stack(SPo))

        case   (cCos); this%Stack(SPo)=COS(this%Stack(SPo))

        case   (cTan); this%Stack(SPo)=TAN(this%Stack(SPo))

        case  (cAsin)

          if ((this%Stack(SPo)<-1._DP) .or. (this%Stack(SPo)>1._DP)) then
            EvalErrType=4_SI
            res=zero
            exit
          end if
          this%Stack(SPo)=ASIN(this%Stack(SPo))

        case  (cAcos);
          if ((this%Stack(SPo)<-1._DP).or.(this%Stack(SPo)>1._DP)) then
            EvalErrType=4_SI
            res=zero
            exit
          end if
          this%Stack(SPo)=ACOS(this%Stack(SPo))

        case  (cAtan); this%Stack(SPo)=ATAN(this%Stack(SPo))

        case     (cQ); this%Stack(SPo)=q(INT(this%Stack(SPo)))

        case     (cJ); this%Stack(SPo)=j(INT(this%Stack(SPo)))

        case  DEFAULT; SPo=SPo+1_SI; this%Stack(SPo)=Val(this%ByteCode(IPo)-VarBegin+1_SI)

      end select

    end do

    if (EvalErrType > 0_SI) then
      write(*,*)'*** Error: ', EvalErrMsg(EvalErrType)
    else
      res = this%Stack(1)
    end if

  end function evaluate

  !*****************************************************************************************
  subroutine CheckSyntax( this )
    ! Check syntax of function string,  returns 0 if syntax is ok
    class(EquationParser) :: this
    integer(kind=SI) :: n
    character(len=1) :: c
    real(kind=DP) :: r
    logical :: err
    integer(kind=SI) :: ParCnt ! Parenthesis counter
    integer(kind=SI) :: ib, in, j
    integer(kind=SI) :: lFunc

    j = 1_SI
    ParCnt = 0_SI
    lFunc = LEN_trim(this%funcString, KIND(1_SI))
    step: do
      if (j > lFunc) call ParseErrMsg (this%funcStringOrig)
      c = this%funcString(j:j)
      !-- -------- --------- --------- --------- --------- --------- --------- -------
      ! Check for valid operand (must appear)
      !-- -------- --------- --------- --------- --------- --------- --------- -------
      if (c == '-' .or. c == '+') then                      ! Check for leading - or +
        j = j+1_SI
        if (j > lFunc) call ParseErrMsg (this%funcStringOrig, 'Missing operand')
        c = this%funcString(j:j)
        if (ANY(c == Ops)) call ParseErrMsg (this%funcStringOrig, 'Multiple operators')
      end if
      n = MathFunctionIndex (this%funcString(j:))
      if (n > 0_SI) then                                       ! Check for math function
        j = j+LEN_trim(Funcs(n), KIND(1_SI))
        if (j > lFunc) call ParseErrMsg (this%funcStringOrig, 'Missing function argument')
        c = this%funcString(j:j)
        if (c /= '(') call ParseErrMsg (this%funcStringOrig, 'Missing opening parenthesis')
      end if
      if (c == '(') then                                    ! Check for opening parenthesis
        ParCnt = ParCnt+1_SI
        j = j+1_SI
        cycle step
      end if
      if (SCAN(c, '0123456789.') > 0) then                   ! Check for number
        r = RealNum (this%funcString(j:), ib, in, err)
        if (err) call ParseErrMsg (this%funcStringOrig, 'Invalid number format:  '//this%funcString(j+ib-1:j+in-2))
        j = j+in-1_SI
        if (j > lFunc) exit
        c = this%funcString(j:j)
      else                                                  ! Check for variable
        n = VariableIndex (this%funcString(j:), this%variableNames, ib, in)
        if (n == 0_SI) call ParseErrMsg (this%funcStringOrig, 'Invalid element: '//this%funcString(j+ib-1:j+in-2))
        j = j+in-1_SI
        if (j > lFunc) exit
        c = this%funcString(j:j)
      end if
      do while (c == ')') ! Check for closing parenthesis
        ParCnt = ParCnt-1_SI
        if (ParCnt < 0_SI) call ParseErrMsg (this%funcStringOrig, 'Mismatched parenthesis')
        if (this%funcString(j-1_SI:j-1_SI) == '(') call ParseErrMsg (this%funcStringOrig, 'Empty parentheses')
        j = j+1_SI
        if (j > lFunc) exit
        c = this%funcString(j:j)
      end do
      !-- -------- --------- --------- --------- --------- --------- --------- -------
      ! Now, we have a legal operand: A legal operator or end of string must follow
      !-- -------- --------- --------- --------- --------- --------- --------- -------
      if (j > lFunc) exit
      if (ANY(c == Ops)) then                               ! Check for multiple operators
        if (j+1_SI > lFunc) call ParseErrMsg (this%funcStringOrig)
        if (ANY(this%funcString(j+1_SI:j+1_SI) == Ops)) call ParseErrMsg (this%funcStringOrig, 'Multiple operators')
      else                                                  ! Check for next operand
        call ParseErrMsg( this%funcStringOrig, 'Missing operator')
      end if
      !-- -------- --------- --------- --------- --------- --------- --------- -------
      ! Now, we have an operand and an operator: the next loop will check for another
      ! operand (must appear)
      !-- -------- --------- --------- --------- --------- --------- --------- -------
      j = j+1_SI
    end do step
    if (ParCnt > 0_SI) call ParseErrMsg (this%funcStringOrig, 'Missing )')
  end subroutine CheckSyntax

  !*****************************************************************************************
  function EvalErrMsg( EvalErrType) result (msg )
    ! Return error message
    integer(kind=SI), intent(in) :: EvalErrType
    character(len=*), dimension(4), parameter :: m = (/ 'Division by zero                ', &
                                                        'Argument of SQRT negative       ', &
                                                        'Argument of LOG negative        ', &
                                                        'Argument of ASIN or ACOS illegal' /)
    character(len=len(m)) :: msg
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (EvalErrType < 1_SI .or. EvalErrType > SIZE(m)) then
      msg = ''
    else
      msg = m(EvalErrType)
    end if

  end function EvalErrMsg

  !*****************************************************************************************
  subroutine ParseErrMsg( FuncStr, Msg )
    ! Print error message and terminate program
    character(len=*), intent(in) :: FuncStr       ! Original function string
    character(len=*), optional, intent(in) :: Msg

    if (PRESENT(Msg)) then
      write(*,*) '*** Error in syntax of function string: '//Msg
    else
      write(*,*) '*** Error in syntax of function string:'
    end if

    write(*,*)
    write(*, '(A)') ' '//FuncStr

    write(*, '(A)') '?'
    stop
  end subroutine ParseErrMsg

  !*****************************************************************************************
  function OperatorIndex( c) result (n )
    ! Return operator index
    character(len=1), intent(in) :: c
    integer(SI) :: n, j

    n = 0

    do j=cAdd, cPow
      if (c == Ops(j)) then
        n = j
        exit
      end if
    end do

  end function OperatorIndex

  !*****************************************************************************************
  function MathFunctionIndex( str) result (n )
    ! Return index of math function beginnig at 1st position of string str
    character(len=*), intent(in) :: str

    integer(kind=SI) :: n, j, k
    character(len=len(Funcs)) :: fun

    n = 0_SI

    do j=cAbs, cJ                                             ! Check all math functions
      k = MIN(LEN_trim(Funcs(j), KIND(1_SI)), len(str))
      call LowCase( str(1_SI:k), fun )
      if (fun == Funcs(j)) then                             ! Compare lower case letters
        n = j                                              ! Found a matching function
        exit
      end if
    end do

  end function MathFunctionIndex

  !*****************************************************************************************
  function VariableIndex( str, Var, ibegin, inext) result (n )
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return index of variable at begin of string str (returns 0 if no variable found)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    implicit none
    character(len=*), intent(in) :: str       ! String
    character(len=*), dimension(:), intent(in) :: Var       ! Array with variable names
    integer(kind=SI) :: n         ! Index of variable
    integer(kind=SI), optional, intent(out) :: ibegin, & ! Start position of variable name
                                             inext     ! Position of character after name
    integer(kind=SI) :: j, ib, in, lstr
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0_SI
    ib = 0_SI
    in = 0_SI
    lstr = LEN_trim(str, KIND(1_SI))
    if (lstr > 0_SI) then
      do ib=1_SI, lstr                                          ! Search for first character in str
        if (str(ib:ib) /= ' ') exit                        ! When lstr>0 at least 1 char in str
      end do
      do in=ib, lstr                                         ! Search for name terminators
        if (SCAN(str(in:in), '+-*/^) ') > 0) exit
      end do
      do j=1_SI, SIZE(Var, KIND(1_SI))
        if (str(ib:in-1_SI) == Var(j)) then
          n = j                                           ! Variable name found
          exit
        end if
      end do
    end if
    if (PRESENT(ibegin)) ibegin = ib
    if (PRESENT(inext)) inext  = in
  end function VariableIndex

  !*****************************************************************************************
  subroutine RemoveSpaces( str )
    ! Remove Spaces from string, remember positions of characters in old string
    character(len=*), intent(inout) :: str

    integer(kind=SI) :: k, lstr

    lstr = LEN_trim(str, KIND(1_SI))

    k = 1_SI

    do while (str(k:lstr) /= ' ')
      if (str(k:k) == ' ') then
        str(k:lstr) = str(k+1_SI:lstr)//' '                  ! Move 1 character to left
        k = k-1_SI
      end if
      k = k+1_SI
    end do

  end subroutine RemoveSpaces

  !*****************************************************************************************
  subroutine Replace( ca, cb, str )
    ! Replace ALL appearances of character set ca in string str by character set cb
    character(len=*), intent(in) :: ca
    character(len=len(ca)) , intent(in) :: cb                ! len(ca) must be len(cb)
    character(len=*), intent(inout) :: str

    integer(kind=SI) :: j, lca

    lca = len(ca, KIND(1_SI))

    do j=1_SI, LEN_trim(str, KIND(1_SI))-lca+1_SI
      if (str(j:j+lca-1_SI) == ca) str(j:j+lca-1_SI) = cb
    end do

  end subroutine Replace

  !*****************************************************************************************
  subroutine Compile( this )
    ! Compile i-th function string F into bytecode
    class(EquationParser) :: this
    integer :: istat

    if (ASSOCIATED(this%ByteCode)) DEALLOCATE ( this%ByteCode, &
                                              this%Immed, &
                                              this%Stack     )
    this%ByteCodeSize = 0_SI
    this%ImmedSize    = 0_SI
    this%StackSize    = 0_SI
    this%StackPtr     = 0_SI

    call this%CompileSubstr( 1_SI, INT(LEN_trim(this%funcString), KIND(1_SI)) )

    allocate ( this%ByteCode(this%ByteCodeSize), &
             this%Immed(this%ImmedSize), &
             this%Stack(this%StackSize), &
             STAT = istat                      )
    if (istat /= 0) then
      write(*,*) '*** Parser error: Memmory allocation for byte code failed'
      stop
    else
      this%ByteCodeSize = 0_SI
      this%ImmedSize    = 0_SI
      this%StackSize    = 0_SI
      this%StackPtr     = 0_SI
      call this%CompileSubstr( 1_SI, INT(LEN_trim(this%funcString), KIND(1_SI)) )
    end if

  end subroutine Compile

  !*****************************************************************************************
  subroutine AddCompiledByte( this, b )
    ! Add compiled byte to bytecode
    class(EquationParser) :: this
    integer(kind=SI), intent(in) :: b                             ! Value of byte to be added

    this%ByteCodeSize = this%ByteCodeSize + 1_SI

    if (ASSOCIATED(this%ByteCode)) then
      this%ByteCode(this%ByteCodeSize) = b
    end if

  end subroutine AddCompiledByte

  !*****************************************************************************************
  function MathItemIndex( this, b, e) result (n )
    ! Return math item index, if item is real number, enter it into Comp-structure
    class(EquationParser) :: this
    integer(kind=SI), intent(in) :: b, e      ! First and last pos. of substring
    integer(kind=SI) :: n         ! Byte value of math item

    n = 0_SI

    if (SCAN(this%funcString(b:b), '0123456789.') > 0_SI) then                 ! Check for begin of a number
      this%ImmedSize = this%ImmedSize + 1_SI
      if (ASSOCIATED(this%Immed)) this%Immed(this%ImmedSize) = RealNum(this%funcString(b:e))
      n = cImmed
    else                                                     ! Check for a variable
      n = VariableIndex(this%funcString(b:e), this%variableNames)
      if (n > 0_SI) n = VarBegin+n-1_SI
    end if

  end function MathItemIndex

  !*****************************************************************************************
  function CompletelyEnclosed( F, b, e) result (res )
    ! Check if function substring F(b:e) is completely enclosed by a pair of parenthesis
    character(len=*), intent(in) :: F                       ! Function substring
    integer(kind=SI), intent(in) :: b, e                     ! First and last pos. of substring

    logical :: res
    integer(kind=SI) :: j
    integer(kind=SI) :: k

    res=.false.

    if (F(b:b) == '(' .and. F(e:e) == ')') then
      k = 0_SI
      do j = b + 1_SI, e - 1_SI
        if (F(j:j) == '(') then
          k = k + 1_SI
        elseif (F(j:j) == ')') then
          k = k - 1_SI
        end if
        if (k < 0_SI) exit
      end do
      if (k == 0_SI) res=.true.                                ! All opened parenthesis closed
    end if

  end function CompletelyEnclosed

  !*****************************************************************************************
  recursive subroutine CompileSubstr(this, b, e)
    ! Compile i-th function string funcString into bytecode
    class(EquationParser) :: this
    integer(kind=SI), intent(in) :: b, e      ! Begin and end position substring

    integer(kind=SI) :: n
    integer(kind=SI) :: b2, j, k
    integer(kind=SI) :: io
    character(len=*), parameter :: calpha = 'abcdefghijklmnopqrstuvwxyz'// &
                                        'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ! Check for special cases of substring

    if (this%funcString(b:b) == '+') then                              ! Case 1: funcString(b:e) = '+...'
      !      write(*,*)'1. funcString(b:e) = "+..."'
      call this%CompileSubstr( b+1_SI, e )
      return
    elseif (CompletelyEnclosed (this%funcString, b, e)) then               ! Case 2: funcString(b:e) = '(...)'
      !      write(*,*)'2. funcString(b:e) = "(...)"'
      call this%CompileSubstr( b+1_SI, e-1_SI )
      return
    elseif (SCAN(this%funcString(b:b), calpha) > 0) then
      n = MathFunctionIndex(this%funcString(b:e))
      if (n > 0_SI) then
        b2 = b+INDEX(this%funcString(b:e), '(', .false., KIND(1_SI))-1_SI
        if (CompletelyEnclosed(this%funcString, b2, e)) then             ! Case 3: funcString(b:e) = 'fcn(...)'
          !            write(*,*)'3. funcString(b:e) = "fcn(...)"'
          call this%CompileSubstr( b2+1_SI, e-1_SI )
          call this%AddCompiledByte( n )
          return
        end if
      end if

    elseif (this%funcString(b:b) == '-') then
      if (CompletelyEnclosed(this%funcString, b+1_SI, e)) then              ! Case 4: this%funcString(b:e) = '-(...)'
        !         write(*,*)'4. this%funcString(b:e) = "-(...)"'
        call this%CompileSubstr( b+2_SI, e-1_SI )
        call this%AddCompiledByte( cNeg )
        return
      elseif (SCAN(this%funcString(b+1_SI:b+1_SI), calpha) > 0) then
        n = MathFunctionIndex(this%funcString(b+1_SI:e))
        if (n > 0_SI) then
          b2 = b+INDEX(this%funcString(b+1_SI:e), '(', .false., KIND(1_SI))
          if (CompletelyEnclosed(this%funcString, b2, e)) then          ! Case 5: this%funcString(b:e) = '-fcn(...)'
            !               write(*,*)'5. this%funcString(b:e) = "-fcn(...)"'
            call this%CompileSubstr( b2+1_SI, e-1_SI );
            call this%AddCompiledByte( n )
            call this%AddCompiledByte( cNeg )
            return
          end if
        end if
      end if
    end if

    ! Check for operator in substring: check only base level (k=0), exclude expr. in ()

    do io=cAdd, cPow                                          ! Increasing priority +-*/^
      k = 0_SI
      do j=e, b, -1_SI
        if (this%funcString(j:j) == ')') then
          k = k+1_SI
        elseif (this%funcString(j:j) == '(') then
          k = k-1_SI
        end if
        if (k == 0_SI .and. this%funcString(j:j) == Ops(io) .and. IsBinaryOp (j, this%funcString)) then
          if (ANY(this%funcString(j:j) == Ops(cMul:cPow)) .and. this%funcString(b:b) == '-') then ! Case 6: this%funcString(b:e) = '-...Op...' with Op > -
            !               write(*,*)'6. this%funcString(b:e) = "-...Op..." with Op > -'
            call this%CompileSubstr( b+1_SI, e )
            call this%AddCompiledByte( cNeg )
            return
          else                                                        ! Case 7: this%funcString(b:e) = '...BinOp...'
            !               write(*,*)'7. Binary operator',this%funcString(j:j)
            call this%CompileSubstr( b, j-1_SI )
            call this%CompileSubstr( j+1_SI, e )
            call this%AddCompiledByte( OperatorIndex(Ops(io)) )
            this%StackPtr = this%StackPtr - 1_SI
            return
          end if
        end if
      end do
    end do

    ! Check for remaining items, i.e. variables or explicit numbers

    b2 = b

    if (this%funcString(b:b) == '-') b2 = b2+1_SI

    n = this%MathItemIndex(b2, e)

    !   write(*,*)'8. AddCompiledByte ',n
    call this%AddCompiledByte( n )

    this%StackPtr = this%StackPtr + 1_SI
    if (this%StackPtr > this%StackSize) this%StackSize = this%StackSize + 1_SI

    if (b2 > b) call this%AddCompiledByte(cNeg)

  end subroutine CompileSubstr

  !*****************************************************************************************
  function IsBinaryOp( j, F) result (res )
    ! Check if operator F(j:j) in string F is binary operator
    ! Special cases already covered elsewhere:              (that is corrected in v1.1)
    ! - operator character F(j:j) is first character of string (j=1)
    integer(kind=SI), intent(in) :: j                       ! Position of Operator
    character(len=*), intent(in) :: F                       ! String

    logical :: res                     ! Result
    integer(kind=SI) :: k
    logical :: Dflag, Pflag

    res=.true.

    if (F(j:j) == '+' .or. F(j:j) == '-') then               ! Plus or minus sign:
      if (j == 1_SI) then                                      ! - leading unary operator ?
        res = .false.
      elseif (SCAN(F(j-1_SI:j-1_SI), '+-*/^(') > 0) then           ! - other unary operator ?
        res = .false.
      elseif (SCAN(F(j+1_SI:j+1_SI), '0123456789') > 0 .and. &     ! - in exponent of real number ?
               SCAN(F(j-1_SI:j-1_SI), 'eEdD') > 0) then
        Dflag=.false.; Pflag=.false.
        k = j-1_SI
        do while (k > 1_SI) !   step to the left in mantissa
          k = k-1_SI
          if (SCAN(F(k:k), '0123456789') > 0) then
            Dflag=.true.
          elseif (F(k:k) == '.') then
            if (Pflag) then
              exit                                      !   * exit: 2nd appearance of '.'
            else
              Pflag=.true.                              !   * mark 1st appearance of '.'
            end if
          else
            exit                                         !   * all other characters
          end if
        end do
        if (Dflag .and. (k == 1_SI .or. SCAN(F(k:k), '+-*/^(') > 0)) res = .false.
      end if
    end if
  end function IsBinaryOp

  !*****************************************************************************************
  function RealNum( str, ibegin, inext, error) result (res )
    ! Get real number from string - Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
    character(len=*), intent(in) :: str                    ! String
    real(kind=DP) :: res                    ! Real number
    integer(kind=SI), optional, intent(out) :: ibegin, & ! Start position of real number
                                                inext        ! 1st character after real number
    logical, optional, intent(out) :: error                  ! Error flag

    integer(kind=SI) :: ib, in
    integer :: istat
    logical :: Bflag, & ! .T. at begin of number in str
                                      InMan, & ! .T. in mantissa of number
                                      Pflag, & ! .T. after 1st '.' encountered
                                      Eflag, & ! .T. at exponent identifier 'eEdD'
                                      InExp, & ! .T. in exponent of number
                                      DInMan, & ! .T. if at least 1 digit in mant.
                                      DInExp, & ! .T. if at least 1 digit in exp.
                                      err                    ! Local error flag
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    Bflag=.true.; InMan=.false.; Pflag=.false.; Eflag=.false.; InExp=.false.
    DInMan=.false.; DInExp=.false.
    ib   = 1_SI
    in   = 1_SI
    do while (in <= LEN_trim(str))
      select case (str(in:in))
        case (' ') ! only leading blanks permitted
          ib = ib+1_SI
          if (InMan .or. Eflag .or. InExp) exit
        case ('+', '-') ! Permitted only
          if (Bflag) then
            InMan=.true.; Bflag=.false.                     ! - at beginning of mantissa
          elseif (Eflag) then
            InExp=.true.; Eflag=.false.                     ! - at beginning of exponent
          else
            exit                                            ! - otherwise stop
          end if
        case ('0':'9') ! Mark
          if (Bflag) then
            InMan=.true.; Bflag=.false.                     ! - beginning of mantissa
          elseif (Eflag) then
            InExp=.true.; Eflag=.false.                     ! - beginning of exponent
          end if
          if (InMan) DInMan=.true.                           ! Mantissa contains digit
          if (InExp) DInExp=.true.                           ! Exponent contains digit
        case ('.')
          if (Bflag) then
            Pflag=.true.                                    ! - mark 1st appearance of '.'
            InMan=.true.; Bflag=.false.                     !   mark beginning of mantissa
          elseif (InMan .and..not.Pflag) then
            Pflag=.true.                                    ! - mark 1st appearance of '.'
          else
            exit                                            ! - otherwise stop
          end if
        case ('e', 'E', 'd', 'D') ! Permitted only
          if (InMan) then
            Eflag=.true.; InMan=.false.                     ! - following mantissa
          else
            exit                                            ! - otherwise stop
          end if
        case DEFAULT
          exit                                               ! stop at all other characters
      end select
      in = in+1_SI
    end do
    err = (ib > in-1_SI) .or. (.not.DInMan) .or. ((Eflag.or.InExp).and..not.DInExp)
    if (err) then
      res = 0.0_DP
    else
      read(str(ib:in-1_SI),*, iostat=istat) res
      err = istat /= 0
    end if
    if (PRESENT(ibegin)) ibegin = ib
    if (PRESENT(inext)) inext  = in
    if (PRESENT(error)) error  = err
  end function RealNum

  !*****************************************************************************************
  subroutine LowCase( str1, str2 )
    ! Transform upper case letters in str1 into lower case letters, result is str2
    implicit none
    character(len=*), intent(in) :: str1
    character(len=*), intent(out) :: str2
    integer :: j, k
    character(len=*), parameter :: lc = 'abcdefghijklmnopqrstuvwxyz'
    character(len=*), parameter :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    str2 = str1

    do j=1, LEN_trim(str1)
      k = INDEX(uc, str1(j:j))
      if (k > 0) str2(j:j) = lc(k:k)
    end do

  end subroutine LowCase

end module FortranParser

module parser_mod
  use types_mod
  use FortranParser, only : EquationParser
  implicit none
  save

  type(EquationParser), allocatable :: eqParserGeneric(:), eqParserReaction(:)

  integer, parameter :: nvar = 12
  character(len=*), dimension(nvar), parameter :: var  = (/ 'TEMP    ', &
                                                            'N2      ', &
                                                            'O2      ', &
                                                            'M       ', &
                                                            'RH      ', &
                                                            'H2O     ', &
                                                            'DEC     ', &
                                                            'BLHEIGHT', &
                                                            'DILUTE  ', &
                                                            'JFAC    ', &
                                                            'ROOFOPEN', &
                                                            'RO2     ' /)
  integer :: ierr

contains
  subroutine initialiseGenericParser()
    use input_functions_mod, only : count_lines_in_file
    implicit none

    integer(kind=NPI) :: i, nparsers
    character(len=100), allocatable :: lines(:)

    nparsers = count_lines_in_file('src/gen/gen-complex.rates')
    allocate(lines(nparsers), eqParserGeneric(nparsers))

    if (nparsers > 0) then
      open (10, file='src/gen/gen-complex.rates', status='old')
      i = 1
      read (10, '(A100)', iostat=ierr) lines(i)

      do while ( ierr == 0 .and. i < nparsers)
        i = i + 1
        read (10, '(A100)', iostat=ierr) lines(i)
      end do
      close (10, status='keep')

      do i=1, nparsers
        eqParserGeneric(i) = EquationParser(trim(lines(i)), var) ! Initialize function parser for nfunc functions
      end do
    end if

    return
  end subroutine initialiseGenericParser

  subroutine initialiseReactionParser()
    use input_functions_mod, only : count_lines_in_file
    implicit none

    integer(kind=NPI) :: i, nparsers
    character(len=100), allocatable :: lines(:)

    nparsers = count_lines_in_file('src/gen/coeff.rates')
    allocate(lines(nparsers), eqParserReaction(nparsers))

    if (nparsers > 0) then
      open (10, file='src/gen/coeff.rates', status='old')
      i = 1
      read (10, '(A100)', iostat=ierr) lines(i)

      do while ( ierr == 0 .and. i < nparsers)
        i = i + 1
        read (10, '(A100)', iostat=ierr) lines(i)
      end do
      close (10, status='keep')

      do i=1, nparsers
        eqParserReaction(i) = EquationParser(trim(lines(i)), var) ! Initialize function parser for nfunc functions
      end do
    end if

    return
  end subroutine initialiseReactionParser

end module parser_mod
