MODULE solverFunctions_mod
CONTAINS!     ---------------------------------------------------------------
SUBROUTINE resid (nsp, nr, clocktime, y, dy, lhs, rhs, coeff, size1, size2)
  ! calculate rhs of rate eqn dy()
  USE types_mod
  USE productionAndLossRates
  USE mechanismRates_mod

  IMPLICIT NONE
  INTEGER(kind=NPI) :: i
  INTEGER(kind=NPI) :: nsp ! number of species involved
  INTEGER(kind=NPI) :: nr ! number of reactions
  INTEGER(kind=NPI) :: size1, size2 !number of entries in each equation array
  INTEGER(kind=NPI) :: lhs(3, size1), rhs(2, size2)
  real(kind=DP) :: coeff(*) ! coeff term of rhs
  real(kind=DP) :: y(*) ! concentration array
  real(kind=DP) :: p(nr) ! array to hold rates
  real(kind=DP) :: r(nr) ! working array
  real(kind=DP) :: dy(*) ! array to hold value of rate equations
  real(kind=DP) :: clocktime

  ! set rate eqn to zero
  DO i = 1, nsp
     dy(i) = 0

  ENDDO

  DO i = 1, nr
     productionRates(i) = 0
     lossRates(i) = 0
  ENDDO

  ! get values of reactions rates
  CALL mechanism_rates (p, clocktime, y, nsp)

  ! calculation of rhs of rate equations
  DO i = 1, nr
     r(i) = p(i)
  ENDDO


  DO i = 1, size1
     r(lhs(1, i)) = r(lhs(1, i))*y(lhs(2, i))**lhs(3, i)
     ir(lhs(1, i)) = r(lhs(1, i))
  ENDDO

  DO i = 1, size1
     dy(lhs(2, i)) = dy(lhs(2, i))-lhs(3, i)*r(lhs(1, i))
     lossRates(lhs(1, i)) = ABS (dy(lhs(2, i)))
  ENDDO

  DO i = 1, size2
     dy(rhs(2, i))=dy(rhs(2, i))+coeff(i)*r(rhs(1, i))
     productionRates(rhs(1, i)) = productionRates(rhs(1, i)) + coeff(i)*r(rhs(1, i))
  ENDDO

  RETURN
END SUBROUTINE resid

SUBROUTINE jfy (ny, nr, y, fy, t)
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
  USE types_mod
  USE mechanismRates_mod
  USE reactionStructure ! access is, crhs, nclhs, rhs_size
  IMPLICIT NONE

  INTEGER(kind=NPI), intent(in) :: ny, nr
  INTEGER(kind=NPI) :: j
  real(kind=DP) :: p(nr), r(nr), t
  real(kind=DP) :: y(*)
  real(kind=DP), intent(out) :: fy(ny,*)
  INTEGER :: is

  ! set jacobian matrix to zero
  fy(1:ny, 1:ny) = 0.0

  ! call routine to get reaction rates in array p
  CALL mechanism_rates (p, t, y, ny)

  DO j = 1, ny
     r(1:nr) = 0.0
     DO is = 1, lhs_size
        IF (clhs(2, is)==j) THEN
           r(clhs(1, is)) = p(clhs(1, is))
        ENDIF
     ENDDO
     DO is = 1, lhs_size
        IF (clhs(2, is)==j) THEN
           r(clhs(1, is)) = r(clhs(1, is))*clhs(3, is)*y(clhs(2, is))**(clhs(3, is)-1)
        ELSE
           r(clhs(1, is)) = r(clhs(1, is))*y(clhs(2, is))**clhs(3, is)
        ENDIF
     ENDDO
     DO is = 1, lhs_size
        fy(clhs(2, is), j)=fy(clhs(2, is), j)-clhs(3, is)*r(clhs(1, is))
     ENDDO
     DO is = 1, rhs_size
        fy(crhs(2, is), j)=fy(crhs(2, is), j) + ccoeff(is) * r(crhs(1, is))
     ENDDO
  ENDDO

  RETURN
END SUBROUTINE jfy
END MODULE solverFunctions_mod
