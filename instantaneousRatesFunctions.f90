!    -------------------------------------------------------------
SUBROUTINE findReactionsWithProductX (r, crhs, csize2, rateOfProdNS, prodArrayLen, prodLossArrayLen, nsp)
  INTEGER rCounter, i, j, csize2, rateOfProdNS, prodArrayLen(*), prodLossArrayLen, nsp
  INTEGER :: crhs(2, csize2), r(nsp, prodLossArrayLen)


  ! initialise counter for r array
  rCounter = 2
  ! loop over interesting species
  DO i = 1, rateOfProdNS
     DO j = 1, csize2
        IF (crhs(2, j)==r(i, 1)) THEN
           r(i, rCounter) = crhs(1, j)
           rCounter = rCounter + 1
        ENDIF
     ENDDO
     prodArrayLen(i) = rCounter -1
     rCounter = 2
  ENDDO

  RETURN
END SUBROUTINE findReactionsWithProductX

!    -------------------------------------------------------------
SUBROUTINE findReactionsWithReactant (r, clhs, csize1, rateOfLossNS, lossArrayLen, prodLossArrayLen, nsp)
  INTEGER rCounter, i, j, csize1, rateOfLossNS, prodLossArrayLen, lossArrayLen(*), nsp
  INTEGER :: clhs(3, csize1), r(nsp, prodLossArrayLen)

  ! initialise counter for r array
  rCounter = 2

  ! loop over interesting species
  DO i = 1, rateOfLossNS
     DO j = 1, csize1
        IF (clhs(2, j)==r(i, 1)) THEN
           r(i, rCounter) = clhs(1, j)
           rCounter = rCounter + 1
        ENDIF
     ENDDO
     lossArrayLen(i) = rCounter - 1
     rCounter = 2
  ENDDO

  RETURN
END SUBROUTINE findReactionsWithReactant
