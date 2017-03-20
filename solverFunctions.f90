!     ---------------------------------------------------------------
SUBROUTINE FCVJTIMES (v, fjv, t, y, fy, h, ipar, rpar, work, ier)

  USE species

  INTEGER, PARAMETER :: LongInt_Kind = SELECTED_INT_KIND (10)
  INTEGER (KIND=LongInt_Kind) ipar (*), ier, neq, i
  INTEGER j, np
  DOUBLE PRECISION t, h, rpar(*), y(*), v(*), fjv(*), fy(*), work(*), delta, deltaV, dummy
  DOUBLE PRECISION, ALLOCATABLE :: yPlusV (:), yPlusVi(:)
  CALL getNumberOfSpecies (np)
  ALLOCATE (yPlusV (np), yPlusVi(np))

  neq = ipar(1)
  delta = 1.00d-03
  ! fake using variables h and work, to avoid a warning (they are required by CVODE code)
  h = h
  dummy = work(1)

  ! calculate y + delta v
  j = 0
  DO i = 1, neq
     deltaV = delta * v(i)
     yPlusV (i) = y(i) + deltaV
  ENDDO

  ! get f(y + delta v)
  CALL FCVFUN (t, yPlusV, yPlusVi, ipar, rpar, ier)

  ! JVminus1 + deltaJV
  DO i = 1, neq
     fjv(i) = (yPlusVi(i) - fy(i)) / delta
  ENDDO
  DEALLOCATE (yPlusV, yPlusVi)

  RETURN
END SUBROUTINE FCVJTIMES

!     ---------------------------------------------------------------
SUBROUTINE FCVFUN (t, y, ydot, ipar, rpar, ier)
  USE species
  USE constraints
  USE reactionStructure
  USE chemcialConstraints

  ! Fortran routine for right-hand side function.
  IMPLICIT NONE
  !
  INTEGER, PARAMETER :: LongInt_Kind = SELECTED_INT_KIND (10)
  INTEGER (KIND=LongInt_Kind) ipar(*), ier, nConSpec, np, numReactions
  DOUBLE PRECISION t, y(*), ydot(*), RPAR (*), concAtT, dummy
  DOUBLE PRECISION, ALLOCATABLE :: dy(:), z(:)
  INTEGER i

  np = ipar(1) + numberOfConstrainedSpecies
  numReactions = ipar(2)
  dummy = rpar(1)

  nConSpec = numberOfConstrainedSpecies
  ALLOCATE (dy(np), z(np))

  DO i = 1, numberOfConstrainedSpecies
     IF (i.LE.numberOfVariableConstrainedSpecies) THEN
        CALL getConstrainedQuantAtT2D (t, datax, datay, datay2, speciesNumberOfPoints(i), concAtT, &
             1, i, maxNumberOfDataPoints, numberOfVariableConstrainedSpecies)
     ELSE
        concAtT = dataFixedY (i-numberOfVariableConstrainedSpecies)
     ENDIF
     constrainedConcs(i) = concAtT
     CALL setConstrainedConc (i, concAtT)

  ENDDO

  CALL addConstrainedSpeciesToProbSpec (y, z, numberOfConstrainedSpecies, constrainedSpecies, ipar(1), constrainedConcs)

  CALL resid (np, numReactions, t, z, dy, clhs, crhs, ccoeff, csize1, csize2)

  CALL removeConstrainedSpeciesFromProbSpec (dy, ydot, numberOfConstrainedSpecies, constrainedSpecies, np)

  DEALLOCATE (dy, z)
  IER = 0

  RETURN
END SUBROUTINE FCVFUN

!     ----------------------------------------------------------------
!-----------------------------------------------------------------
!     routine for reading in the reaction
SUBROUTINE DATA (lhs, rhs, coeff, size1, size2)
  INTEGER :: k, l, size1, size2
  INTEGER :: lhs(3, size1), rhs(2, size2)
  DOUBLE PRECISION :: coeff(size2)

  WRITE (*,*) 'Reading reactants (lhs) from mechanism.rec...'
  OPEN (4, file='modelConfiguration/mechanism.reac', status='old') ! input file for lhs of equations
  OPEN (14, file='modelConfiguration/mechanism.prod', status='old') ! input file for rhs of equations

  ! read data for lhs of equations
  size1 = 0
  READ (4,*)
  DO
     READ (4,*) k, l

     IF (k.EQ.0) EXIT
     size1 = size1+1
     lhs(1, size1) = k
     lhs(2, size1) = l
     lhs(3, size1) = 1

  ENDDO

  WRITE (*,*) 'Reading products (rhs) from mechanism.prod...'
  ! read data for rhs of equations
  size2 = 0
  DO
     READ (14,*) k, l
     IF (k.EQ.0) EXIT
     size2 = size2+1
     rhs(1, size2) = k
     rhs(2, size2) = l
     coeff(size2) = 1

  ENDDO

  CLOSE (4, status='keep')
  CLOSE (14, status='keep')

  WRITE (*,*) 'Finished reading lhs and rhs data.'
  RETURN
END SUBROUTINE DATA


SUBROUTINE resid (nsp, nr, clocktime, y, dy, lhs, rhs, coeff, size1, size2)
  ! calculate rhs of rate eqn dy()
  USE productionAndLossRates

  IMPLICIT NONE
  INTEGER, PARAMETER :: LongInt_Kind = SELECTED_INT_KIND (10)
  INTEGER (KIND=LongInt_Kind) :: i
  INTEGER (KIND=LongInt_Kind) :: nsp ! number of species involved
  INTEGER (KIND=LongInt_Kind) :: nr ! number of reactions
  INTEGER :: size1, size2 !number of entries in each equation array
  INTEGER :: lhs(3, size1), rhs(2, size2)
  DOUBLE PRECISION :: coeff(*) ! coeff term of rhs
  DOUBLE PRECISION :: y(*) ! concentration array
  DOUBLE PRECISION :: p(nr) ! array to hold rates
  DOUBLE PRECISION :: r(nr) ! working array
  DOUBLE PRECISION :: dy(*) ! array to hold value of rate equations
  DOUBLE PRECISION :: clocktime

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
  ! csize1 is the number of entries
  ! clhs(1,) = reaction number
  ! clhs(2,) = species number
  ! clhs(3,) = stochiometric coefficient

  ! for the products array
  ! csize2 is the number of entries
  ! crhs(1,) = reaction number
  ! crhs(2,) = species number
  ! ccoeff() = stochiometric coefficient (double precision)

  ! y = concentration array - dimension ny
  ! fy = jacobian array - dimension ny x ny
  ! t = current time (s)
  ! p = reaction rates - dimension nr
  ! r = working array - dimension nr

  USE reactionStructure ! access is, crhs, nclhs, csize2
  IMPLICIT NONE

  INTEGER ny, nr
  DOUBLE PRECISION p(nr), fy(ny,*), y(*), r(nr), t
  INTEGER j, is

  ! set jacobian matrix to zero
  fy(1:ny, 1:ny) = 0.0

  ! call routine to get reaction rates in array p
  CALL mechanism_rates (p, t, y, ny)

  DO j = 1, ny
     r(1:nr) = 0.0
     DO is = 1, csize1
        IF (clhs(2, is).EQ.j) THEN
           r(clhs(1, is)) = p(clhs(1, is))
        ENDIF
     ENDDO
     DO is = 1, csize1
        IF (clhs(2, is).EQ.j) THEN
           r(clhs(1, is)) = r(clhs(1, is))*clhs(3, is)*y(clhs(2, is))**(clhs(3, is)-1)
        ELSE
           r(clhs(1, is)) = r(clhs(1, is))*y(clhs(2, is))**clhs(3, is)
        ENDIF
     ENDDO
     DO is = 1, csize1
        fy(clhs(2, is), j)=fy(clhs(2, is), j)-clhs(3, is)*r(clhs(1, is))
     ENDDO
     DO is = 1, csize2
        fy(crhs(2, is), j)=fy(crhs(2, is), j) + ccoeff(is) * r(crhs(1, is))
     ENDDO
  ENDDO

  RETURN
END SUBROUTINE jfy
