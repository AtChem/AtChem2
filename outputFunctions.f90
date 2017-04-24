SUBROUTINE ro2Sum (ro2, y)
  DOUBLE PRECISION :: ro2
  DOUBLE PRECISION, intent (in) :: y(*)
  ro2 = 0.00e+00
END SUBROUTINE ro2Sum

SUBROUTINE outputEnvVar (t)
  USE envVars

  INTEGER :: i
  DOUBLE PRECISION :: t
  IF (ro2<0) ro2 = 0.0
  WRITE (52,*) t, (currentEnvVarValues(i), i = 1, numEnvVars), ro2

  RETURN
END SUBROUTINE outputEnvVar

!--------------------------------------------------------------------
SUBROUTINE outputjfy (fy, nsp, t)
  INTEGER, intent(in) :: nsp
  INTEGER i, j
  DOUBLE PRECISION, intent(in) :: fy(nsp, nsp), t

  ! Loop over all elements of fy, and print to jacobian.output, prefixed by t
  DO i = 1, nsp
     WRITE (55, '(100 (1x, e12.5)) ') t, (fy(i, j), j = 1, nsp)
  ENDDO
  WRITE (55,*) '---------------'
END SUBROUTINE outputjfy

!     ---------------------------------------------------------------
SUBROUTINE outputPhotolysisRates (j, t)
  USE photolysisRates, ONLY: nrOfPhotoRates, ck
  DOUBLE PRECISION :: j(*), t
  INTEGER :: i

  WRITE (58, '(100 (1x, e12.5)) ') t, (j(ck(i)), i = 1, nrOfPhotoRates)
  RETURN
END SUBROUTINE outputPhotolysisRates

!     ---------------------------------------------------------------
SUBROUTINE getConcForSpecInt (y, yInt, specInt, specIntSize, neq)
  ! This subroutine outputs yInt, the concentration of each species of interest,
  ! in the same order as the species are in specInt
  DOUBLE PRECISION, intent(IN)  :: y(*)
  DOUBLE PRECISION, intent(OUT) :: yInt(*)
  INTEGER, intent(IN) :: specIntSize, neq, specInt(*)
  INTEGER i, j
  ! set yInt(j) to the
  DO i = 1, neq
     DO j = 1, specIntSize
        IF (specInt(j)==i) THEN
           yInt(j) = y(i)
        ENDIF
     ENDDO
  ENDDO
  RETURN
END SUBROUTINE getConcForSpecInt

!     ---------------------------------------------------------------
SUBROUTINE getReaction (speciesNames, reactionNumber, reaction)
  ! Given a list speciesNames, and an integer reactionNumber, return reaction,
  ! a string containing
  USE reactionStructure
  IMPLICIT NONE
  CHARACTER (LEN=10) :: reactants(10), products(10)
  CHARACTER (LEN=10), intent(in) :: speciesNames(*)
  INTEGER :: i, numReactants, numProducts
  INTEGER, intent(in) :: reactionNumber
  CHARACTER (LEN=1000) :: reactantStr, productStr
  CHARACTER (LEN=1000), intent(out) :: reaction

  ! Loop over reactants, and copy the reactant name for any reactant used in
  ! reaction reactionNumber. use numReactants as a counter of the number of reactants.
  ! String these together with '+', and append a '='
  numReactants = 0
  DO i = 1, csize1
     IF (clhs(1, i)==reactionNumber) THEN
        numReactants = numReactants + 1
        reactants(numReactants) = speciesNames(clhs(2, i))
     ENDIF
  ENDDO

  reactantStr = ' '
  DO i = 1, numReactants
     reactantStr = trim(adjustl(trim(reactantStr) // trim(reactants(i))))
     IF (i<numReactants) THEN
        reactantStr = trim(reactantStr) // '+'
     ENDIF
  ENDDO
  reactantStr = trim(reactantStr) // '='


  ! Loop over products, and copy the product name for any product created in
  ! reaction reactionNumber. use numProducts as a counter of the number of products.
  ! String these together with '+', and append this to reactantStr. Save the
  ! result in reaction, which is returned
  numProducts = 0
  DO i = 1, csize2
     IF (crhs(1, i)==reactionNumber) THEN
        numProducts = numProducts + 1
        products(numProducts) = speciesNames(crhs(2, i))
     ENDIF
  ENDDO

  productStr = ' '
  DO i = 1, numProducts
     productStr = trim(adjustl(trim(productStr) // trim(products(i))))
     IF (i<numProducts) THEN
        productStr = trim(productStr) // '+'
     ENDIF
  ENDDO

  reaction = trim(reactantStr) // trim(productStr)

  RETURN
END SUBROUTINE getReaction

SUBROUTINE outputRates (r, t, p, flag, numberOfSpecies, csize, arrayLen, &
     speciesNames)

  USE reactionStructure
  USE, INTRINSIC :: iso_fortran_env, ONLY : stderr=>error_unit
  INTEGER numberOfSpecies, csize, arrayLen(*)
  INTEGER i, j, r(numberOfSpecies, csize), flag
  DOUBLE PRECISION t, p(*)
  CHARACTER (LEN=10) speciesNames(*)
  CHARACTER (LEN=1000) :: reaction

  DO i = 1, numberOfSpecies
     IF (arrayLen(i)>csize) THEN
        WRITE (stderr,*) "arrayLen(i)>csize in outputRates, which is illegal. i =", i
     ENDIF
     DO j = 2, arrayLen(i)
        IF (r(i, j)/=-1) THEN

           CALL getReaction (speciesNames, r(i, j), reaction)
           ! Flag = 0 for reaction, 1 for loss
           IF (flag==0) THEN
              WRITE (56,*) t, ' ', r(i, 1), ' ', speciesNames(r(i, 1)), ' ', r(i, j), ' ', p(r(i, j)), ' ', trim(reaction)
           ELSE
              IF (flag/=1) THEN
                 STOP "Unexpected flag value to outputRates()"
              END IF
              WRITE (60,*) t, ' ', r(i, 1), ' ', speciesNames(r(i, 1)), ' ', r(i, j), ' ', p(r(i, j)), ' ', trim(reaction)
           END IF
        ENDIF
     ENDDO
  ENDDO
  RETURN
END SUBROUTINE outputRates

SUBROUTINE outputInstantaneousRates (time, numReac)

  USE reactionStructure
  USE directories, ONLY : instantaneousRates_dir
  USE productionAndLossRates, ONLY : ir
  USE, INTRINSIC :: iso_fortran_env, ONLY : stderr=>error_unit

  INTEGER, intent(in) :: time, numReac
  INTEGER i
  CHARACTER (LEN=57) :: irfileLocation
  CHARACTER (LEN=30) :: strTime

  WRITE (strTime,*) time

  irfileLocation = trim(instantaneousRates_dir) // '/' // ADJUSTL (strTime)

  OPEN (10, file=irfileLocation)
  DO i = 1, numReac
     WRITE (10,*) ir(i)
  ENDDO
  CLOSE (10, status='keep')

  RETURN
END SUBROUTINE outputInstantaneousRates

!     ----------------------------------------------------------------
SUBROUTINE outputSpeciesOutputRequiredNames (names, namesSize)
  CHARACTER (LEN=10) names(*)
  INTEGER i, namesSize
  WRITE (50, '(100 (1x, a)) ') 't         ', (names(i), i = 1, namesSize)
  RETURN
END SUBROUTINE outputSpeciesOutputRequiredNames

SUBROUTINE outputSpeciesOutputRequired (t, yInt, yIntSize)
  DOUBLE PRECISION t, yInt(*)
  INTEGER yIntSize, i
  DO i = 1, yIntSize
     IF (yInt(i)<0) THEN
        yInt(i) = 0d0
     ENDIF
  END DO
  WRITE (50, '(100 (1x, e15.5e3)) ') t, (yInt(i), i = 1, yIntSize)
  RETURN
END SUBROUTINE outputSpeciesOutputRequired
