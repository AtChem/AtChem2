module solverFunctions_mod
contains!     ---------------------------------------------------------------
  subroutine resid( nr, time, y, dy, lhs, rhs, coeff )
    ! calculate rhs of rate eqn dy()
    use types_mod
    use productionAndLossRates, only : ir, productionRates, lossRates
    implicit none

    integer(kind=NPI), intent(in) :: nr ! number of reactions
    real(kind=DP), intent(in) :: time, y(:) ! concentration array
    real(kind=DP), contiguous, intent(out) :: dy(:) ! array to hold value of rate equations
    integer(kind=NPI), intent(in) :: lhs(:,:), rhs(:,:)
    real(kind=DP), intent(in) :: coeff(:) ! coeff term of rhs

    real(kind=DP) :: r(nr) ! working array
    integer(kind=NPI) :: i

    if ( size( lhs, 1 ) /= 3 ) then
      stop 'size( lhs, 1 ) /= 3 in resid()'
    end if
    if ( size( rhs, 1 ) /= 2 ) then
      stop 'size( rhs, 1 ) /= 2 in resid()'
    end if
    if ( size( rhs, 2 ) /= size( coeff ) ) then
      stop 'size( rhs, 2 ) /= coeff in resid()'
    end if

    ! set rate eqn to zero
    dy(:) = 0
    productionRates(:) = 0
    lossRates(:) = 0

    ! get values of reactions rates
    call mechanism_rates( time, y, r )

    do i = 1, size( lhs, 2 )
      r(lhs(1, i)) = r(lhs(1, i)) * y(lhs(2, i)) ** lhs(3, i)
      ir(lhs(1, i)) = r(lhs(1, i))
    end do

    do i = 1, size( lhs, 2 )
      dy(lhs(2, i)) = dy(lhs(2, i)) - lhs(3, i) * r(lhs(1, i))
      lossRates(lhs(1, i)) = abs( dy(lhs(2, i)) )
    end do

    do i = 1, size( rhs, 2 )
      dy(rhs(2, i)) = dy(rhs(2, i)) + coeff(i) * r(rhs(1, i))
      productionRates(rhs(1, i)) = productionRates(rhs(1, i)) + coeff(i) * r(rhs(1, i))
    end do

    return
  end subroutine resid

  subroutine jfy( nr, y, t, fy )
    ! routine for calculating the Jacobian of the system
    ! nr = number of reactions
    ! for each species calculate the rhs of the rate equation
    ! for the reactants array
    ! lhs_size is the number of entries
    ! clhs(1,) = reaction number
    ! clhs(2,) = species number
    ! clhs(3,) = stoichiometric coefficient

    ! for the products array
    ! rhs_size is the number of entries
    ! crhs(1,) = reaction number
    ! crhs(2,) = species number
    ! ccoeff() = stoichiometric coefficient (double precision)

    ! y = concentration array - dimension ny
    ! fy = jacobian array - dimension ny x ny
    ! t = current time (s)
    ! p = reaction rates - dimension nr
    ! r = working array - dimension nr
    use types_mod
    use reactionStructure ! access is, crhs, nclhs, rhs_size
    implicit none

    integer(kind=NPI), intent(in) :: nr
    real(kind=DP), intent(in) :: y(:), t
    real(kind=DP), intent(out) :: fy(size( y ),*)
    integer(kind=NPI) :: j
    real(kind=DP) :: p(nr), r(nr)
    integer(kind=NPI) :: is

    ! set jacobian matrix to zero
    fy(1:size( y ), 1:size( y )) = 0.0

    ! call routine to get reaction rates in array p
    call mechanism_rates( t, y, p )

    do j = 1, size( y )
      r(1:nr) = 0.0
      do is = 1, lhs_size
        if ( clhs(2, is) == j ) then
          r(clhs(1, is)) = p(clhs(1, is))
        end if
      end do
      do is = 1, lhs_size
        if ( clhs(2, is) == j ) then
          r(clhs(1, is)) = r(clhs(1, is)) * clhs(3, is) * y(clhs(2, is)) ** ( clhs(3, is) - 1 )
        else
          r(clhs(1, is)) = r(clhs(1, is)) * y(clhs(2, is)) ** clhs(3, is)
        end if
      end do
      do is = 1, lhs_size
        fy(clhs(2, is), j) = fy(clhs(2, is), j) - clhs(3, is) * r(clhs(1, is))
      end do
      do is = 1, rhs_size
        fy(crhs(2, is), j) = fy(crhs(2, is), j) + ccoeff(is) * r(crhs(1, is))
      end do
    end do

    return
  end subroutine jfy

  subroutine mechanism_rates( t, y, p )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use storage, only : maxEnvVarNameLength
    use photolysisRates_mod
    use zenithData1, only : cosX, secX
    use envVars, only : ro2, envVarNames, currentEnvVarValues
    use interpolationFunctions_mod, only : getConstrainedQuantAtT
    use interpolationMethod, only : getConditionsInterpMethod
    use outputFunctions_mod, only : ro2sum
    use constraintFunctions_mod, only : getEnvVarsAtT, getEnvVarNum
    use utilityFunctions_mod, only : atmosphere
    implicit none

    ! calculates rate constants from arrhenius information
    real(kind=DP), intent(in) :: t
    real(kind=DP), intent(in) :: y(:)
    real(kind=DP), intent(out) :: p(:)

    real(kind=DP) :: temp, pressure, dummy, this_env_val, photoRateAtT
    integer(kind=NPI) :: i
    character(len=maxEnvVarNameLength) :: this_env_var_name

    include 'mechanism-rate-declarations.f90'

    call ro2sum( ro2, y )
    dummy = y(1)

    dec = -1e16

    call getEnvVarsAtT( t )

    do i = 1, size( envVarNames )
      this_env_var_name = envVarNames(i)
      this_env_val = currentEnvVarValues(getEnvVarNum(this_env_var_name))
      select case ( this_env_var_name )
        case ( 'TEMP' )
          temp = this_env_val
        case ( 'RH' )
          rh = this_env_val
        case ( 'H2O' )
          h2o = this_env_val
        case ( 'DEC' )
          dec = this_env_val
        case ( 'PRESS' )
          pressure = this_env_val
        case ( 'M' )
          m = this_env_val
        case ( 'BLHEIGHT' )
          blh = this_env_val
        case ( 'DILUTE' )
          dilute = this_env_val
        case ( 'JFAC' )
          jfac = this_env_val
        case ( 'ROOFOPEN' )
          roofOpen = this_env_val
        case default
          write(stderr,*) 'getEnvVarsAtT(): invalid environment name ' // trim( this_env_var_name )
          stop
      end select
    end do

    call atmosphere( m, o2, n2 )

    !O2 = 0.2095*m
    !N2 = 0.7809*m

    do i = 1, nrOfPhotoRates
      if ( usePhotolysisConstants .eqv. .false. ) then
        if ( cosX < 1.00d-10 ) then
          j(ck(i)) = 1.0d-30
        else
          j(ck(i)) = cl(i) * cosX ** cmm(i) * exp( -cnn(i) * secX ) * transmissionFactor(i) * roofOpen * jfac
        end if
      else
        j(ck(i)) = cl(i)
      end if
    end do

    do i = 1, numConPhotoRates
      call getConstrainedQuantAtT( t, photoX, photoY, photoY2, photoNumberOfPoints(i), &
                                   getConditionsInterpMethod(), i, photoRateAtT )
      j(constrainedPhotoRatesNumbers(i)) = photoRateAtT
    end do

    include 'mechanism-rate-coefficients.f90'
    return
  end subroutine mechanism_rates
end module solverFunctions_mod
