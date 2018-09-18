! -----------------------------------------------------------------------------
!
! Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
! Peter Jimack, Mike Pilling
!
! Copyright (c) 2017 Sam Cox, Roberto Sommariva
!
! This file is part of the AtChem2 software package.
!
! This file is covered by the MIT license which can be found in the file
! LICENSE.md at the top level of the AtChem2 distribution.
!
! -----------------------------------------------------------------------------

! ******************************************************************** !
! ATCHEM2 -- MODULE solverFunctions
!
! This module contains functions that are used in the main solver,
! and manipulate the rate information towards solving the system.
! ******************************************************************** !
module solver_functions_mod
contains

  ! ----------------------------------------------------------------- !
  ! Calculates the system residual
  subroutine resid( nr, time, y, dy, lhs, lcoeff, rhs, rcoeff )
    use types_mod
    use reaction_rates_mod, only : lossRates, productionRates, instantaneousRates
    implicit none

    integer(kind=NPI), intent(in) :: nr ! number of reactions
    real(kind=DP), intent(in) :: time, y(:) ! concentration array
    real(kind=DP), contiguous, intent(out) :: dy(:) ! array to hold value of rate equations
    integer(kind=NPI), intent(in) :: lhs(:,:), rhs(:,:)
    real(kind=DP), intent(in) :: lcoeff(:), rcoeff(:) ! coeff term of rhs
    real(kind=DP) :: r(nr) ! working array
    integer(kind=NPI) :: i

    ! calculate rhs of rate eqn dy()
    if ( size( lhs, 1 ) /= 2 ) then
      stop 'size( lhs, 1 ) /= 2 in resid()'
    end if
    if ( size( rhs, 1 ) /= 2 ) then
      stop 'size( rhs, 1 ) /= 2 in resid()'
    end if
    if ( size( lhs, 2 ) /= size( lcoeff ) ) then
      stop 'size( lhs, 2 ) /= lcoeff in resid()'
    end if
    if ( size( rhs, 2 ) /= size( rcoeff ) ) then
      stop 'size( rhs, 2 ) /= rcoeff in resid()'
    end if

    ! set rate eqn to zero
    dy(:) = 0
    productionRates(:) = 0
    lossRates(:) = 0

    ! get values of reactions rates
    call mechanism_rates( time, y, r )

    ! r is of length #reactions. This loop multiplies the reaction coefficient of
    ! the reaction by the concentration of each reactant, giving the
    ! reaction rate of the reaction.
    ! As an example, if we have reaction 1 as A+A+B -> C +C + D with rate k, then this
    ! loop will update r(1) to kAAB.
    do i = 1, size( lhs, 2 )
      r(lhs(1, i)) = r(lhs(1, i)) * y(lhs(2, i)) ** lcoeff(i)
      instantaneousRates(lhs(1, i)) = r(lhs(1, i))
    end do

    ! loop over each of the species in each reaction, updating the dy (rate of change of species y)
    ! with the reaction rate from each of the reactants in each reaction.
    ! continuing the example from above, this loop updates dy(A) by r(1) for the first reactant A.
    ! After that, it updates dy(A) again for the second reactant A.
    ! Now dy(A) = -2kAAB.
    ! Finally, it updates dy(B) by r(1) for the reactant B to set dy(B) = -kAAB.
    do i = 1, size( lhs, 2 )
      dy(lhs(2, i)) = dy(lhs(2, i)) - lcoeff(i) * r(lhs(1, i))
      lossRates(lhs(1, i)) = abs( dy(lhs(2, i)) )
    end do

    ! This does the same as the above but updates each of the product species by
    ! the values of the relevant elements of r.
    ! Continuing the example above, it will update dy(C) by kAAB, then again for
    ! the second product C, giving dy(C) = 2kAAB. Finally, update dy(D) once to
    ! give dy(D) = kAAB.
    do i = 1, size( rhs, 2 )
      dy(rhs(2, i)) = dy(rhs(2, i)) + rcoeff(i) * r(rhs(1, i))
      productionRates(rhs(1, i)) = productionRates(rhs(1, i)) + rcoeff(i) * r(rhs(1, i))
    end do

    return
  end subroutine resid

  ! ----------------------------------------------------------------- !
  ! subroutine to calculate the Jacobian matrix of the system
  subroutine jfy( nr, y, t )
    use types_mod
    use reaction_structure_mod ! access crhs, clhs, clcoeff, crcoeff
    implicit none

    integer(kind=NPI), intent(in) :: nr
    real(kind=DP), intent(in) :: y(:), t
    real(kind=DP) :: fy(size( y ), size( y ))
    integer(kind=NPI) :: i, j
    real(kind=DP) :: p(nr), r(nr)

    ! nr = number of reactions
    ! for each species calculate the rhs of the rate equation
    ! for the reactants array
    ! clhs(1,) = reaction number
    ! clhs(2,) = species number
    ! clcoeff(:) = stoichiometric coefficient

    ! for the products array
    ! rhs_size is the number of entries
    ! crhs(1,) = reaction number
    ! crhs(2,) = species number
    ! crcoeff(:) = stoichiometric coefficient

    ! y = concentration array - dimension ny
    ! fy = jacobian array - dimension ny x ny
    ! t = current time (s)
    ! p = reaction rates - dimension nr
    ! r = working array - dimension nr

    ! set jacobian matrix to zero
    fy(:,:) = 0.0_DP

    ! call routine to get reaction rates in array p. Each element of p
    ! corresponds to a single reaction
    call mechanism_rates( t, y, p )

    do j = 1, size( y )
      r(1:nr) = 0.0_DP
      do i = 1, size( clhs, 2 )
        if ( clhs(2, i) == j ) then
          r(clhs(1, i)) = p(clhs(1, i))
        end if
      end do
      do i = 1, size( clhs, 2)
        if ( clhs(2, i) == j ) then
          r(clhs(1, i)) = r(clhs(1, i)) * clcoeff(i) * y(clhs(2, i)) ** ( clcoeff(i) - 1 )
        else
          r(clhs(1, i)) = r(clhs(1, i)) * y(clhs(2, i)) ** clcoeff(i)
        end if
      end do
      fy(clhs(2,:), j) = fy(clhs(2,:), j) - clcoeff(:) * r(clhs(1,:))
      fy(crhs(2,:), j) = fy(crhs(2,:), j) + crcoeff(:) * r(crhs(1,:))
    end do

    ! Loop over all elements of fy, and print to jacobian.output,
    ! prefixed by t
    do i = 1, size( fy, 1 )
      write (55, '(100 (1P e15.7)) ') t, (fy(i, j), j = 1, size( fy, 1 ))
    end do
    write (55,*) '---------------'

    return
  end subroutine jfy

  ! -----------------------------------------------------------------
  ! calculates rate constants from arrhenius information output p(:)
  ! contains the rate of each reaction
  subroutine mechanism_rates( t, y, p )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use storage_mod, only : maxEnvVarNameLength
    use photolysis_rates_mod, only : numConstantPhotoRates, constantPhotoNumbers, constantPhotoValues, &
                                     numUnconstrainedPhotoRates, numConstrainedPhotoRates, j, ck, &
                                     constrainedPhotoNumbers, usePhotolysisConstants
    use zenith_data_mod, only : cosx_below_threshold
    use env_vars_mod, only : ro2, envVarNames, currentEnvVarValues
    use interpolation_functions_mod, only : getConstrainedPhotoRatesAtT
    use interpolation_method_mod, only : getConditionsInterpMethod
    use output_functions_mod, only : ro2sum
    use constraint_functions_mod, only : calcPhotolysis, getEnvVarsAtT, getEnvVarNum
    use atmosphere_functions_mod, only : calcAtmosphere
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP), intent(in) :: y(:)
    real(kind=DP), intent(out) :: p(:)

    real(kind=DP) :: temp, pressure, dummy, this_env_val, photoRateAtT
    integer(kind=NPI) :: i
    character(len=maxEnvVarNameLength) :: this_env_var_name

    real(kind=DP) :: N2, O2, M, RH, H2O, DEC, BLH, DILUTE, JFAC, ROOFOPEN
    include './gen/mechanism-rate-declarations.f90'

    ro2 = ro2sum( y )
    dummy = y(1)

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

    call calcAtmosphere( m, o2, n2 )

    if ( usePhotolysisConstants .eqv. .true. ) then
      do i = 1, numConstantPhotoRates
        j(constantPhotoNumbers(i)) = constantPhotoValues(i)
      end do
    else
      do i = 1, numUnconstrainedPhotoRates
        if ( cosx_below_threshold .eqv. .true. ) then
          ! Apply zero if below the cosx threshold, to reduce potential numerical issues
          j(ck(i)) = 0.0_DP
        else
          j(ck(i)) = calcPhotolysis( i ) * roofOpen * jfac
        end if
      end do

      do i = 1, numConstrainedPhotoRates
        call getConstrainedPhotoRatesAtT( t, i, photoRateAtT )
        j(constrainedPhotoNumbers(i)) = photoRateAtT
      end do
    end if
    !TODO: is this necessary a second time?
    ro2 = ro2sum( y )

    include './gen/mechanism-rate-coefficients.f90'

    return
  end subroutine mechanism_rates

end module solver_functions_mod
