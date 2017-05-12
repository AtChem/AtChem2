module solverFunctions_mod
contains!     ---------------------------------------------------------------
  subroutine resid( nsp, nr, clocktime, y, dy, lhs, rhs, coeff, size1, size2 )
    ! calculate rhs of rate eqn dy()
    use types_mod
    use productionAndLossRates
    use mechanismRates_mod

    implicit none
    integer(kind=NPI) :: i
    integer(kind=NPI) :: nsp ! number of species involved
    integer(kind=NPI) :: nr ! number of reactions
    integer(kind=NPI) :: size1, size2 !number of entries in each equation array
    integer(kind=NPI) :: lhs(3, size1), rhs(2, size2)
    real(kind=DP) :: coeff(*) ! coeff term of rhs
    real(kind=DP) :: y(*) ! concentration array
    real(kind=DP) :: p(nr) ! array to hold rates
    real(kind=DP) :: r(nr) ! working array
    real(kind=DP) :: dy(*) ! array to hold value of rate equations
    real(kind=DP) :: clocktime

    ! set rate eqn to zero
    do i = 1, nsp
      dy(i) = 0

    end do

    do i = 1, nr
      productionRates(i) = 0
      lossRates(i) = 0
    end do

    ! get values of reactions rates
    call mechanism_rates( p, clocktime, y, nsp )

    ! calculation of rhs of rate equations
    do i = 1, nr
      r(i) = p(i)
    end do


    do i = 1, size1
      r(lhs(1, i)) = r(lhs(1, i))*y(lhs(2, i))**lhs(3, i)
      ir(lhs(1, i)) = r(lhs(1, i))
    end do

    do i = 1, size1
      dy(lhs(2, i)) = dy(lhs(2, i))-lhs(3, i)*r(lhs(1, i))
      lossRates(lhs(1, i)) = abs( dy(lhs(2, i)) )
    end do

    do i = 1, size2
      dy(rhs(2, i))=dy(rhs(2, i))+coeff(i)*r(rhs(1, i))
      productionRates(rhs(1, i)) = productionRates(rhs(1, i)) + coeff(i)*r(rhs(1, i))
    end do

    return
  end subroutine resid

  subroutine jfy( ny, nr, y, fy, t )
    ! routine for calculating the Jacobian of the system
    ! ny = number of species
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

    integer(kind=NPI), intent(in) :: ny, nr
    integer(kind=NPI) :: j
    real(kind=DP) :: p(nr), r(nr), t
    real(kind=DP) :: y(*)
    real(kind=DP), intent(out) :: fy(ny,*)
    integer :: is

    ! set jacobian matrix to zero
    fy(1:ny, 1:ny) = 0.0

    ! call routine to get reaction rates in array p
    call mechanism_rates( p, t, y, ny )

    do j = 1, ny
      r(1:nr) = 0.0
      do is = 1, lhs_size
        if (clhs(2, is)==j) then
          r(clhs(1, is)) = p(clhs(1, is))
        end if
      end do
      do is = 1, lhs_size
        if (clhs(2, is)==j) then
          r(clhs(1, is)) = r(clhs(1, is))*clhs(3, is)*y(clhs(2, is))**(clhs(3, is)-1)
        else
          r(clhs(1, is)) = r(clhs(1, is))*y(clhs(2, is))**clhs(3, is)
        end if
      end do
      do is = 1, lhs_size
        fy(clhs(2, is), j)=fy(clhs(2, is), j)-clhs(3, is)*r(clhs(1, is))
      end do
      do is = 1, rhs_size
        fy(crhs(2, is), j)=fy(crhs(2, is), j) + ccoeff(is) * r(crhs(1, is))
      end do
    end do

    return
  end subroutine jfy
end module solverFunctions_mod
