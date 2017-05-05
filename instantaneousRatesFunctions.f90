MODULE instRates_mod
  USE types_mod
CONTAINS
SUBROUTINE findReactionsWithProductOrReactant (r, chs, chssize1, csize, rateOfNS, arrayLen, numSpecies)
  ! For each interesting species,
  INTEGER rCounter, i, j
  INTEGER, intent(in) :: chssize1
  INTEGER(kind=DI), intent(in) :: rateOfNS, csize, chs(chssize1, csize), numSpecies
  INTEGER, intent(out) :: arrayLen(*)
  INTEGER(kind=DI), intent(inout) :: r(:, :)

  ! chssize1 will be 3 for reactants, 2 for products. Reactants have an extra column
  ! due to subroutine readReactions() placing 1 in this column. This is the
  ! stoichiometric coefficient used in resid() and jfy().
  ! TODO: lhs(3,*) This is currently always set to 1 - should this be able to vary?
  IF (size( r, 1 )/=rateOfNS) THEN
    STOP "size( r, 2 )/=rateOfNS in readReactions()."
  END IF
  IF (size( chs, 2 )/=csize) THEN
    STOP "size( rhs, 1 )/=2 in readReactions()."
  END IF
  ! initialise counter for r array
  rCounter = 2
  ! loop over interesting species (i.e. over 1st index of r)
  DO i = 1, rateOfNS
    ! loop over elements of 2nd index of chs
     DO j = 1, csize
        ! Is the second element of this row in chs equal to the first element of this column in r?
        ! If so, then set th
        IF (chs(2, j)==r(i, 1)) THEN
           ! Match found
           r(i, rCounter) = chs(1, j)
           rCounter = rCounter + 1
        ENDIF
     ENDDO
     arrayLen(i) = rCounter -1
     rCounter = 2
  ENDDO

  RETURN
END SUBROUTINE findReactionsWithProductOrReactant
END MODULE instRates_mod
