MODULE instantaneousRatesFunctions_mod
CONTAINS
  SUBROUTINE findReactionsWithProductOrReactant (r, chs, chssize1, csize, rateOfNS, arrayLen)
  USE types_mod
  ! For each interesting species,
  INTEGER(kind=NPI), intent(in) :: chssize1, csize, chs(chssize1, csize)
  INTEGER(kind=NPI), intent(in) :: rateOfNS
  INTEGER(kind=NPI), intent(out) :: arrayLen(*)
  INTEGER(kind=NPI), intent(inout) :: r(rateOfNS, *)
  INTEGER(kind=NPI) :: rCounter, i, j

  ! chssize1 will be 3 for reactants, 2 for products. Reactants have an extra column
  ! due to subroutine readReactions() placing 1 in this column. This is the
  ! stoichiometric coefficient used in resid() and jfy().
  ! TODO: lhs(3,*) This is currently always set to 1 - should this be able to vary?

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
END MODULE instantaneousRatesFunctions_mod
