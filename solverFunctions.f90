module solverFunctions_mod
contains!     ---------------------------------------------------------------
  subroutine resid( nr, clocktime, y, dy, lhs, rhs, coeff )
    ! calculate rhs of rate eqn dy()
    use types_mod
    use productionAndLossRates, only : ir, productionRates, lossRates
    use mechanismRates_mod
    implicit none

    integer(kind=NPI) :: i
    integer(kind=NPI) :: nr ! number of reactions
    integer(kind=NPI) :: lhs(:,:), rhs(:,:)
    real(kind=DP) :: coeff(:) ! coeff term of rhs
    real(kind=DP) :: y(:) ! concentration array
    real(kind=DP) :: p(nr) ! array to hold rates
    real(kind=DP) :: r(nr) ! working array
    real(kind=DP) :: dy(:) ! array to hold value of rate equations
    real(kind=DP) :: clocktime

    if ( size( lhs, 1 ) /= 3 ) then
      stop 'size( lhs, 1 ) /= 3 in resid()'
    end if
    if ( size( rhs, 1 ) /= 2 ) then
      stop 'size( rhs, 1 ) /= 2 in resid()'
    end if

    ! set rate eqn to zero
    dy(:) = 0

    productionRates(:) = 0
    lossRates(:) = 0

    ! get values of reactions rates
    call mechanism_rates( clocktime, y, p )

    ! calculation of rhs of rate equations
    do i = 1, nr
      r(i) = p(i)
    end do

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
    use mechanismRates_mod
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
end module solverFunctions_mod
