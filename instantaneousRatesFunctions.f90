!    -------------------------------------------------------------
SUBROUTINE findReactionsWithProductOrReactant (r, chs, chssize1, csize, rateOfNS, arrayLen, plArrayLen, numSpecies)
  INTEGER rCounter, i, j
  INTEGER, intent(in) :: chssize1, csize, rateOfNS, plArrayLen, numSpecies, chs(chssize1, csize)
  INTEGER, intent(out) :: arrayLen(*)
  INTEGER, intent(inout) :: r(numSpecies, plArrayLen)


  ! initialise counter for r array
  rCounter = 2
  ! loop over interesting species (i.e. over 1st index of r)
  DO i = 1, rateOfNS
    ! loop over elements of 2nd index of crhs
     DO j = 1, csize
        ! Is the second element of this row in crhs equal to the first element of this column in r?
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
