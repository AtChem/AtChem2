subroutine calcDateParameters()
    use date
    integer:: i

    monthList = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
    totalDays = 0
    do i=1, month-1
        totalDays = totalDays + monthList(i)
    enddo

    totalDays = totalDays + day -1
	fractionYear = totalDays
    fractionYear = fractionYear / 365
    secYear = 3.6525d+02*2.40d+01*3.60d+03
    return
end

subroutine writeFileHeaders(photoRateNamesForHeader)
    use envVars
    use photolysisRates, only: nrOfPhotoRates, ck
    character (len = 30):: photoRateNamesForHeader(*)
    integer :: i

    ! WRITE FILE OUTPUT HEADERS AND OUTPUT AT t=0
    ! OUTPUT FOR CVODE MAIN SOLVER
    write(23,*),'t LNST LNFE LNETF LNGE'

    ! OUTPUT FOR SPARSE SOLVER
    write(21,*),'t NFELS NJTV NPE NPS'
    write(89,*)'time speciesNumber speciesName reactionNumber rate'
    write(90,*)'time speciesNumber speciesName reactionNumber rate'

    ! OTHER OUPUT
    ! write(85,*),'t temp m h2o'
    write(86,*)'t ', (trim(photoRateNamesForHeader(ck(i)) )// '    ', i=1,nrOfPhotoRates)
    write(22,*)'t currentStepSize previousStepSize'
    write(49,*)'t secx cosx lat longt lha sinld cosld'
    write(95,*)'time ', (envVarNames(i), i=1,numEnvVars), 'RO2'

    return
end


subroutine matchNameToNumber(speciesName,speciesList,listSize,neq,returnArray,returnArraySize)
    character*10 speciesList(*),speciesName(*),k,m
    integer i,j,match, matched_j, neq,returnArray(*),returnArraySize, listSize
    returnArraySize = 1

    do i=1,listSize
        k = speciesList(i)
        match = 0
        do j=1,neq
            m = speciesName(j)
            if (m.eq.k) then
                match = 1
                matched_j = j
                returnArray(returnArraySize) = j
                returnArraySize = returnArraySize + 1
            endif
        enddo
        ! substitute empty strings for invalid species 
        if (match.eq.0) then
            speciesList(i)=''
        endif	
    enddo
    return
end

subroutine matchOneNameToNumber(speciesName,oneSpecies,neq,id)
    character*10 oneSpecies,speciesName(*),m
    integer j, neq

	id=0
	do j=1,neq
		m = speciesName(j)
		if (m.eq.oneSpecies) then
			id=j
			return
		endif
	enddo
end


subroutine setConcentrations(y,speciesName,concSpeciesName,concentration,concCounter,neq)
    character*10 concSpeciesName(*),speciesName(*),k, m
    double precision concentration(*),y(*)
    integer concCounter,neq,i,j, match

    do i=1,concCounter
        k = concSpeciesName(i)
        ! flag for matching of string names
        match = 0
        do j=1,neq
            m = speciesName(j)
            if (m.eq.k) then
                ! Set concentration in y()
                y(j) = concentration(i)
                match = 1
                write(99,*)'match, m = k = ',m,' concentration = ',concentration(i)
            else
                write(99,*)'no match, m',m,' != k! = ',k,' concentration = ',concentration(i)
            endif
        enddo
        if(match.eq.0) then
            write(94,*)"Error in  setConcentrations"
            write(94,*)"Can't find species: ",k," in species list"
        end if
    enddo
    return
end
