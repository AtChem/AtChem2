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
  INTEGER :: nsp, i, j
  DOUBLE PRECISION :: fy(nsp, nsp), t

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
  DOUBLE PRECISION y(*), yInt(*)
  INTEGER specIntSize, neq, i, j, specInt(*)

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
  USE reactionStructure
  IMPLICIT NONE
  CHARACTER (LEN=10) :: reactants(10), products(10)
  CHARACTER (LEN=10) :: speciesNames(*)
  INTEGER :: reactionNumber, i, numReactants, numProducts
  CHARACTER (LEN=1000) :: str1
  CHARACTER (LEN=1000) :: reaction, reactantStr, productStr
  numReactants = 0
  numProducts = 0

  ! LOOP OVER REACTANTS
  DO i = 1, csize1
     IF (clhs(1, i)==reactionNumber) THEN
        numReactants = numReactants +1
        reactants(numReactants) = speciesNames(clhs(2, i))
     ENDIF
  ENDDO

  str1 = ' '
  reactantStr = ' '
  DO i = 1, numReactants
     str1 = reactantStr

     reactantStr = TRIM (str1) // TRIM (reactants(i))
     reactantStr = ADJUSTL (reactantStr)
     reactantStr = TRIM (reactantStr)
     IF (i<numReactants) THEN
        reactantStr = TRIM (reactantStr)// '+'
     ENDIF

  ENDDO
  reactantStr = ADJUSTL (reactantStr)
  reactantStr = TRIM (reactantStr)// '='


  !     LOOP OVER PRODUCTS
  numProducts = 0
  DO i = 1, csize2
     IF (crhs(1, i)==reactionNumber) THEN
        numProducts = numProducts +1
        products(numProducts) = speciesNames(crhs(2, i))
     ENDIF
  ENDDO

  str1 = ' '
  productStr = ' '

  DO i = 1, numProducts
     str1 = productStr

     productStr = TRIM (str1) // TRIM (products(i))
     productStr = ADJUSTL (productStr)
     productStr = TRIM (productStr)
     IF (i<numProducts) THEN
        productStr = TRIM (productStr)// '+'
     ENDIF

  ENDDO

  productStr = ADJUSTL (productStr)


  reaction = TRIM (reactantStr) // TRIM (productStr)

  RETURN
END SUBROUTINE getReaction

SUBROUTINE outputRates (r, t, p, flag, nsp, rateOfProdNS, prodLossArrayLen, rateOfLossNS, prodArrayLen, &
     lossArrayLen, speciesNames)

  USE reactionStructure
  INTEGER nsp, rateOfProdNS, prodLossArrayLen, rateOfLossNS, prodArrayLen(*), lossArrayLen(*)
  INTEGER i, j, r(nsp, prodLossArrayLen), flag, x
  DOUBLE PRECISION t, p(*)
  CHARACTER (LEN=10) speciesNames(*)
  CHARACTER (LEN=1000) :: reaction


  ! Flag = 1 for production
  IF (flag==1) THEN

     DO i = 1, rateOfProdNS

        x = 2+prodArrayLen(i)
        DO j = 2, prodArrayLen(i)

           IF (r(i, j)/=0) THEN
              CALL getReaction (speciesNames, r(i, j), reaction)
              WRITE (60,*) t, ' ', r(i, 1), ' ', speciesNames(r(i, 1)), ' ', r(i, j), ' ', p(r(i, j)), ' ', TRIM (reaction)
           ENDIF
        ENDDO
     ENDDO
  ENDIF

  ! Flag = 0 for loss
  IF (flag==0) THEN

     DO i = 1, rateOfLossNS

        DO j = 2, lossArrayLen(i)

           IF (r(i, j)/=0) THEN
              CALL getReaction (speciesNames, r(i, j), reaction)
              WRITE (56,*) t, ' ', r(i, 1), ' ', speciesNames(r(i, 1)), ' ', r(i, j), ' ', p(r(i, j)), ' ', TRIM (reaction)
           ENDIF
        ENDDO
     ENDDO
  ENDIF
  RETURN
END SUBROUTINE outputRates

!     ----------------------------------------------------------------
SUBROUTINE outputInterestingNames (names, namesSize)
  CHARACTER (LEN=10) names(*)
  INTEGER i, namesSize
  WRITE (50, '(100 (1x, a)) ') 't         ', (names(i), i = 1, namesSize)
  RETURN
END SUBROUTINE outputInterestingNames

SUBROUTINE outputInteresting (t, yInt, yIntSize)
  DOUBLE PRECISION t, yInt(*)
  INTEGER yIntSize, i
  DO i = 1, yIntSize
     IF (yInt(i)<0) THEN
        yInt(i) = 0d0
     ENDIF
  END DO
  WRITE (50, '(100 (1x, e15.5e3)) ') t, (yInt(i), i = 1, yIntSize)
  RETURN
END SUBROUTINE outputInteresting
