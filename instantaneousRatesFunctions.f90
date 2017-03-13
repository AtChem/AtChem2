!    -------------------------------------------------------------
subroutine findReactionsWithProductX(r,crhs,csize2,rateOfProdNS,prodArrayLen,prodLossArrayLen,nsp)
    integer rCounter,i,j,csize2,rateOfProdNS,prodArrayLen(*),prodLossArrayLen,nsp
    integer:: crhs(2,csize2),r(nsp,prodLossArrayLen)


    ! initialise counter for r array
    rCounter = 2
    ! loop over interesting species
    do i=1,rateOfProdNS
        do j=1,csize2
            if(crhs(2,j).eq.r(i,1)) then
                r(i,rCounter) = crhs(1,j)
                rCounter = rCounter + 1
            endif
        enddo
        prodArrayLen(i) = rCounter    -1
        rCounter = 2
    enddo

    return
end

!    -------------------------------------------------------------
subroutine findReactionsWithReactant(r,clhs,csize1,rateOfLossNS,lossArrayLen,prodLossArrayLen,nsp)
    integer rCounter,i,j,csize1,rateOfLossNS,prodLossArrayLen,lossArrayLen(*),nsp
    integer:: clhs(3,csize1),r(nsp,prodLossArrayLen)

    ! initialise counter for r array
    rCounter = 2

    ! loop over interesting species
    do i=1,rateOfLossNS
        do j=1,csize1
            if(clhs(2,j).eq.r(i,1)) then
                r(i,rCounter) = clhs(1,j)
                rCounter = rCounter + 1
            endif
        enddo
        lossArrayLen(i) = rCounter - 1
        rCounter = 2
    enddo

    return
end
