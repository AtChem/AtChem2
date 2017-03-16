subroutine outputEnvVar(t)
    use envVars

    integer:: i
    double precision:: t
  if(ro2<0) ro2 = 0.0
    write(95,*) t, (currentEnvVarValues(i), i=1,numEnvVars), ro2

    return
end

!--------------------------------------------------------------------
subroutine outputjfy(fy,nsp,t)
    integer:: nsp,i,j
    double precision :: fy(nsp,nsp), t

    do i=1,nsp
    write (93,'(100(1x,e12.5))') t, (fy(i,j), j=1,nsp)
    enddo
    write(93,*)'---------------'
end

!     ---------------------------------------------------------------
subroutine outputPhotolysisRates(j,t)
    use photolysisRates, only: nrOfPhotoRates, ck
    double precision:: j(*),t
    integer:: i

    write(86,'(100(1x,e12.5))')t, (j(ck(i)), i=1,nrOfPhotoRates)
    return
end

!     ---------------------------------------------------------------
subroutine getConcForSpecInt(y,yInt,specInt,specIntSize,neq)
    double precision y(*),yInt(*)
    integer specIntSize,neq,i,j,specInt(*)

    do i=1,neq
        do j=1,specIntSize
            if (specInt(j) == i) then
                yInt(j) = y(i)
            endif
        enddo
    enddo
    return
end

!     ---------------------------------------------------------------
subroutine getReaction(speciesNames, reactionNumber,reaction)
    use reactionStructure
    implicit none
    character(LEN=10):: reactants(10), products(10)
    character(LEN=10):: speciesNames(*)
    integer:: reactionNumber,i, numReactants, numProducts
    character(LEN=1000):: str1
    character(LEN=1000)::reaction, reactantStr, productStr
    numReactants = 0
    numProducts = 0

    ! LOOP OVER REACTANTS
    do i=1, csize1
        if(clhs(1,i).eq.reactionNumber) then
            numReactants = numReactants +1
            reactants(numReactants) = speciesNames(clhs(2,i))
        endif
    enddo

    str1 =' '
    reactantStr=' '
    do i=1,numReactants
        str1 = reactantStr

        reactantStr = trim(str1) // trim(reactants(i))
        reactantStr = adjustl(reactantStr)
        reactantStr = trim(reactantStr)
        if(i<numReactants) then
            reactantStr = trim(reactantStr)// '+'
        endif

    enddo
    reactantStr = adjustl(reactantStr)
    reactantStr = trim(reactantStr)// '='


!     LOOP OVER PRODUCTS
    numProducts = 0
    do i=1, csize2
        if(crhs(1,i).eq.reactionNumber) then
            numProducts = numProducts +1
            products(numProducts) = speciesNames(crhs(2,i))
        endif
    enddo

    str1 =' '
    productStr = ' '

    do i=1,numProducts
        str1 = productStr

        productStr = trim(str1) // trim(products(i))
        productStr = adjustl(productStr)
        productStr = trim(productStr)
        if(i<numProducts) then
            productStr = trim(productStr)// '+'
        endif

    enddo

    productStr = adjustl(productStr)


    reaction = trim(reactantStr) // trim(productStr)

    return
end

subroutine outputRates(r,t,p,flag,nsp,rateOfProdNS,prodLossArrayLen,rateOfLossNS, prodArrayLen, &
    lossArrayLen, speciesNames)

    use reactionStructure
    integer nsp,rateOfProdNS,prodLossArrayLen,rateOfLossNS,prodArrayLen(*),lossArrayLen(*)
    integer i,j,r(nsp,prodLossArrayLen),flag,x
    double precision t,p(*)
    character(LEN=10) speciesNames(*)
    character(LEN=1000)::reaction


    ! Flag = 1 for production
    if(flag==1) then

        do i=1,rateOfProdNS

            x = 2+prodArrayLen(i)
            do j=2,prodArrayLen(i)

                if (r(i,j)/=0) then
                    call getReaction(speciesNames,r(i,j),reaction)
                    write(89,*)t,' ',r(i,1),' ',speciesNames(r(i,1)),' ',r(i,j),' ',p(r(i,j)),' ',trim(reaction)
                endif
            enddo
        enddo
    endif

    ! Flag = 0 for loss
    if(flag==0) then

        do i=1,rateOfLossNS

            do j=2,lossArrayLen(i)

                if (r(i,j)/=0) then
                    call getReaction(speciesNames,r(i,j),reaction)
                    write(90,*)t,' ',r(i,1),' ',speciesNames(r(i,1)),' ',r(i,j),' ',p(r(i,j)),' ',trim(reaction)
                endif
            enddo
        enddo
    endif
    return
end

!     ----------------------------------------------------------------
subroutine outputInterestingNames(names,namesSize)
    character(LEN=10) names(*)
    integer i,namesSize
    write(91,'(100(1x,a))')'t         ', (names(i), i=1,namesSize)
    return
end

subroutine outputInteresting(t,yInt,yIntSize)
    double precision t,yInt(*)
    integer yIntSize,i
  do i=1, yIntSize
    if(yInt(i) < 0) then
      yInt(i) = 0d0
    endif
  end do
    write(91,'(100(1x,e15.5e3))')t, (yInt(i), i=1,yIntSize)
    return
end
