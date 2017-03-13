subroutine readJFacSpecies()
    use photolysisRates
    implicit none
    integer :: i

    jfacSpeciesLine = 0
    write(*,*) 'Reading JFacSpecies...'
    open (63,file='modelConfiguration/JFacSpecies.config', status='old')
    read(63,*)jfacBase
    write(*,*)'JFacSpecies = ', jfacBase 
    close(63,status = 'keep')
    write(*,*) 'Finished reading JFacSpecies.'
    ! get line number for the JFac base species:
    do i=1,nrOfPhotoRates
        if((trim(photoRateNames(i))).eq.(trim(jfacBase))) then
        jfacSpeciesLine = i
        endif
    enddo
    return
end

subroutine readPhotoloysisRates(ck,cl,cmm,cnn,str,tf)
    use photolysisRates, only: useConstantValues, maxNrOfPhotoRates, nrOfPhotoRates
    implicit none
    integer:: i,ck(*), ierr
    double precision:: cl(*),cmm(*),cnn(*),tf(*)
    character (len = 30):: str(*)

    write(*,*) 'Reading photolysis rates from file...'
    open(13,file='modelConfiguration/photolysisRates.config', status='old')
    read(13,*)
    do i = 1, maxNrOfPhotoRates
        read(13,*,iostat=ierr) ck(i), cl(i), cmm(i), cnn(i),str(i),tf(i)
        if(ierr .ne. 0) then
            exit
        endif
    enddo
    nrOfPhotoRates = i-1
    close(13,status = 'keep')
    i = 1
    write(*,*) ck(i), cl(i), cmm(i), cnn(i),str(i),tf(i)
    i = nrOfPhotoRates
    write(*,*) ck(i), cl(i), cmm(i), cnn(i),str(i),tf(i)
    write(*,*)'Finished reading photolysis rates.'
    write(*,*) 'Number of photolysis rates:', nrOfPhotoRates
    return
end

subroutine readPhotoloysisConstants(ck,cl,cmm,cnn,str,tf)
    use photolysisRates, only: useConstantValues, maxNrOfPhotoRates, nrOfPhotoRates
    implicit none
    integer:: i,ck(*), ierr
    double precision::  cl(*),cmm(*),cnn(*),tf(*)
    character (len = 30):: str(*)

    write(*,*) 'Looking for photolysis constants file...'
    open(13,file='modelConfiguration/photolysisConstants.config', status='old',  iostat=ierr)
    if(ierr .ne. 0) then
        useConstantValues = 0	
	    write(*,*) 'Photolysis constants file not found, trying photolysis rates file...'
	    call readPhotoloysisRates(ck,cl,cmm,cnn,str,tf)
		return
	endif
    useConstantValues = 1
	write(*,*) 'Reading photolysis constants from file...'
    read(13,*)
    do i = 1, maxNrOfPhotoRates
        read(13,*,iostat=ierr) ck(i), cl(i),str(i)
        if(ierr .ne. 0) then
            exit
        endif
    enddo
    nrOfPhotoRates = i-1
    close(13,status = 'keep')
    i = 1
    write(*,*) ck(i), cl(i),str(i)
    i = nrOfPhotoRates
    write(*,*) ck(i), cl(i), str(i)
    write(*,*)'Finished reading photolysis constants.'
    write(*,*) 'Number of photolysis rates:', nrOfPhotoRates
    return
end


subroutine getReactionListSizes(csize1,csize2)
    integer:: csize1,csize2,k,l

    open (13,file='modelConfiguration/mechanism.reac',status = 'old') ! input file for lhs of equations
    open (14,file = 'modelConfiguration/mechanism.prod',status = 'old') ! input file for rhs of equations

    csize1=0
    read(13,*)
    do
        read(13,*) k,l
        if (k.eq.0) exit
        csize1=csize1+1
    enddo

    csize2=0
    do
        read(14,*) k,l
        if (k.eq.0) exit
        csize2=csize2+1
    enddo

    close(13,status = 'keep')
    close(14,status = 'keep')

    return
end

subroutine getParametersFromFile(str, parameterArray)
    character:: str*(*)
    double precision:: parameterArray(*)
    integer :: i

    ! READ IN SOLVER PARAMETERS
    open(5, file=str,status = 'old') ! input file
    i =1
    read(5,*),parameterArray(i)
    do while (parameterArray(i) .ne.-9999)
        i = i + 1
        read(5,*),parameterArray(i)
    end do
    close(5,status = 'keep')

    return
end

subroutine readPhotoRates(maxNumberOfDataPoints)

    use photolysisRates
    implicit none

    INTEGER:: counter,i,k
    INTEGER:: maxNumberOfDataPoints
    CHARACTER(LEN=30)::  string
    CHARACTER(LEN=27)::  fileLocationPrefix
    CHARACTER(LEN=57):: fileLocation

    ! GET NAMES OF PHOTO RATES
    call readPhotoloysisConstants(ck,cl,cmm,cnn,photoRateNames,transmissionFactor)
    write(*,*)
    ! GET NAMES OF CONSTRAINED PHOTO RATES
    write(*,*) 'Reading names of constrained photolysis rates from file...'
    open(5, file='modelConfiguration/constrainedPhotoRates.config',status = 'old') ! input file

    counter = 0
    do
        counter = counter + 1
        read(5,*)constrainedPhotoRates(counter)
        if(constrainedPhotoRates(counter).eq.'end') exit
    enddo
    numConPhotoRates = counter -1
    close(5,status = 'keep')
    write(*,*) 'Finished reading names of constrained photolysis rates.'
    write(*,*) 'Number of constrained photorates:', numConPhotoRates
    if(numConPhotoRates .gt. 0)  write(*,*) 1, constrainedPhotoRates(1)
    if(numConPhotoRates .gt. 2)	 write(*,*) '...'
    if(numConPhotoRates .gt. 1) write(*,*) numConPhotoRates, constrainedPhotoRates(numConPhotoRates)

    ! GET NUMBERS OF CONSTRAINED PHOTO RATES
    do i=1, numConPhotoRates
        do k =1,nrOfPhotoRates
            if(constrainedPhotoRates(i).eq.photoRateNames(k)) then
                constrainedPhotoRatesNumbers(i) = ck(k)
            endif
        enddo
    enddo
    ! ALLOCATE ARRAY SIZE FOR STOREAGE OF PHOTOLYSIS CONSTRAINT DATA
    ALLOCATE (photoX(numConPhotoRates,maxNumberOfDataPoints))
    ALLOCATE (photoY(numConPhotoRates,maxNumberOfDataPoints))
    ALLOCATE (photoY2(numConPhotoRates,maxNumberOfDataPoints))
    ALLOCATE(photoNumberOfPoints(numConPhotoRates))

    fileLocationPrefix = './environmentalConstraints/'

    ! READ IN PHOTOLYSIS DATA
    if(numConPhotoRates .gt. 0) then
        write(*,*)'Reading in constraint data for photolysis rates...'
        do i =1,numConPhotoRates
            string = constrainedPhotoRates(i)
            write(*,*) string, '...'
            fileLocation = fileLocationPrefix // string
            open(13,file=fileLocation, status='old')
            read(13,*),photoNumberOfPoints(i)
            do k = 1, photoNumberOfPoints(i)
                read(13,*)photoX(i,k),photoY(i,k) !,photoY2(i,k)
            enddo
            close(13,status = 'keep')
        enddo
        write(*,*)'Finished reading constraint data for photolysis rates.'
    endif
    return
end

subroutine readSpeciesOutputRequired(r,i, nsp)

    character*10 c,r(*)
    integer i, nsp
    write(*,*)'Reading concentration output from file...'
    open (4,file='modelConfiguration/concentrationOutput.config',status = 'old')

    i=1
    c='abc'

    read(4,*),c
    do while (c.ne.'end' .and. i.le.nsp )
        r(i) = c
        i = i + 1
        read(4,*),c
    enddo

    close(4,status = 'keep')
    write(*,*)'Finished reading concentration output from file.'
    i = i - 1

    ! ERROR HANDLING
    if(i>nsp) then
        write(94,*)'Error: Number of (number of species output is required for) > (number of species)'
        write(94,*)"(number of species output is required for) = ",i
        write(94,*)"(number of  species) = ",nsp
        stop 2
    endif

    return
end

subroutine readSpecies(y,neq,speciesName, speciesNumber)
    double precision y(*)
    integer neq,speciesNumber(*), j
    character*10 speciesName(*)

    ! READ IN INITIAL CONCENTRATIONS
    open(5, file='modelConfiguration/mechanism.species') ! input file
    do j=1,neq
        read(5,*),speciesNumber(j),speciesName(j)
        y(j) = 0
    enddo
    close(5,status = 'keep')

    return
end

SUBROUTINE readConcentrations(concSpeciesName, concentration, concCounter,nsp)

    character*10 concSpeciesName(*), k
    double precision concentration(*),l
    integer i, concCounter,nsp

    write(*,*) 'Reading initial concentrations...'
    open (4,file='modelConfiguration/initialConcentrations.config',status = 'old') ! input file for lhs of equations

    i = 1

    do
        read(4,*) k,l
        if (l.eq.-1) exit
        concentration(i) =  l
        concspeciesName(i) = k

        i = i + 1
    enddo

    close(4,status = 'keep')

    write(*,*)1,' ',concspeciesName(1),' ',concentration(1)
    write(*,*) '...'
    write(*,*)i-1,' ',concspeciesName(i-1),' ',concentration(i-1)

    write(*,*) 'Finished reading initial concentrations.'
		    
    concCounter = i - 1

    if(concCounter>nsp) then
        write(94,*)"Error:(number of species initial concentrations are set for) > (number of species)"
        write(94,*)"(number of species initial concentrations are set for) = ",concCounter
        write(94,*)"(number of  species) = ",nsp
    endif

    return
end

subroutine readProductsOfInterest(r,i)

    character*10 c,r(*)
    integer i
    
    write(*,*) 'Reading products of interest...'
    open (4,file='modelConfiguration/productionRatesOutput.config',status = 'old')

    i=0
    c='abc'
    read(4,*),c
    do while (c.ne.'end')
        i = i + 1
        r(i) = c
        read(4,*),c
    enddo
    if(i.gt.0) then
        write(*,*) 1, r(1)
    endif
    if(i.gt.2) then
        write(*,*) '...'
    endif
    if(i.gt.1) then
        write(*,*) i, r(i)
    endif
    close(4,status = 'keep')
    write(*,*) 'Finished reading products of interest.'
    return
end

subroutine readReactantsOfInterest(r,i)

    character*10 c,r(*)
    integer i
    write(*,*) 'Reading reactants of interest...'
    open (4,file='modelConfiguration/lossRatesOutput.config',status = 'old')

    i=0
    c='abc'
    read(4,*),c
    do while (c.ne.'end')
        i = i + 1
        r(i) = c
        read(4,*),c
    enddo
    if(i.gt.0) then
        write(*,*) 1, r(1)
    endif
    if(i.gt.2) then
        write(*,*) '...'
    endif
    if(i.gt.1) then
        write(*,*) i, r(i)
    endif
    close(4,status = 'keep')
    write(*,*) 'Finished reading reactants of interest.'
    return
    close(4,status = 'keep')

    return
    end

    subroutine readSpeciesConstraints (speciesName,neq,y,t)

    USE species
    USE constraints
    USE chemcialConstraints

    implicit none

    integer:: i,j, k, dataNumberOfPoints,neq, id
	integer :: countOfVarConSpecNames, countOfFixConSpecNames, countOfConNames
    character*13 string
    character*10:: speciesName(*), name
    CHARACTER(LEN=21)::  fileLocationPrefix
    CHARACTER(LEN=57):: fileLocation
	DOUBLE PRECISION :: concAtT, t, value
	DOUBLE PRECISION :: Y(*)
	
    ! READ IN SPECIES TO BE CONSTRAINED
    write(*,*) 'Counting the species to be constrained (in file constrainedSpecies.config)...'
    open(5, file='modelConfiguration/constrainedSpecies.config',status = 'old') ! input file

    i =0
    read(5,*)string
    do while (string.ne.'end')
        i = i + 1
        read(5,*)string
    end do
    close(5,status = 'keep')
	countOfVarConSpecNames = i 
	
    write(*,*) 'Finished counting the names of the species to be constrained.'
	write(*,*) 'Number of names for variable constrained species:', countOfVarConSpecNames
	
	! read in numberOfFixedConstrainedSpecies
	
	write(*,*) 'Counting the fixed-concentration species to be constrained (in file constrainedFixedSpecies.config)...'
    open(9, file='modelConfiguration/constrainedFixedSpecies.config',status = 'old') ! input file
	i =0
    read(9,*)string
    do while (string.ne.'end')
        i = i + 1
        read(9,*)string
    end do
    close(9,status = 'keep')
	write(*,*) 'Finished counting the names of fixed-concentration species'
    countOfFixConSpecNames = i 
	write(*,*) 'Number names of fixed constrained species:', countOfFixConSpecNames
	
	countOfConNames = countOfVarConSpecNames + countOfFixConSpecNames
    ALLOCATE (constrainedSpecies(countOfConNames),constrainedName(countOfConNames))



   write(*,*) 'Reading in the names of variable constrained species...'		    
    open(5, file='modelConfiguration/constrainedSpecies.config',status = 'old') ! input file
    i =0
    read(5,*),name
    do while (name .ne.'end')
		call matchOneNameToNumber(speciesName,name,neq,id)
		if(id.ne.0) then
			i = i + 1
			constrainedName(i)=name
			constrainedSpecies(i)=id
        endif
        read(5,*),name
    end do
    close(5,status = 'keep')
	numberOfVariableConstrainedSpecies = i
    write(*,*) 'Finished reading the names of variable constrained species'
	write(*,*) 'Number of constrained variable species:', numberOfVariableConstrainedSpecies

    write(*,*)'maxNumberOfDataPoints:',maxNumberOfDataPoints
    write(*,*) 'Allocating storage for variable constrained species...'
    ALLOCATE (dataX(numberOfVariableConstrainedSpecies,maxNumberOfDataPoints))
    ALLOCATE (dataY(numberOfVariableConstrainedSpecies,maxNumberOfDataPoints))
    ALLOCATE (dataY2(numberOfVariableConstrainedSpecies,maxNumberOfDataPoints))
    write(*,*) 'Finished allocating storage for variable constrained species.'


    if(numberOfVariableConstrainedSpecies.gt.0) write(*,*) 1, constrainedName(1)
    if(numberOfVariableConstrainedSpecies.gt.2) write(*,*) '...'
    if(numberOfVariableConstrainedSpecies.gt.1) write(*,*) numberOfVariableConstrainedSpecies, &
        constrainedName(numberOfVariableConstrainedSpecies)

    fileLocationPrefix = './speciesConstraints/'

    ! READ CONCENTRATION DATA FOR VARIABLE CONSTRAINED SPECIES
    write(*,*) 'Reading concentration data for constrained species...'
	ALLOCATE(speciesNumberOfPoints(numberOfVariableConstrainedSpecies+countOfFixConSpecNames))
    do i =1,numberOfVariableConstrainedSpecies
        if(i.lt.3 .or. i.eq.numberOfVariableConstrainedSpecies) then
            write(*,*) constrainedName(i), '...'
        else
			if (i.eq.2) write(*,*) '...'
        endif
		
        fileLocation = fileLocationPrefix // trim(constrainedName(i))
        open(13,file=fileLocation, status='old')

        read(13,*),dataNumberOfPoints
        if(dataNumberOfPoints.gt.maxNumberOfDataPoints) then
            dataNumberOfPoints = maxNumberOfDataPoints
            write(*,*) 'Warning! Truncated constraint data to', dataNumberOfPoints, '.'
        endif

        speciesNumberOfPoints(i) = dataNumberOfPoints
        do k = 1, dataNumberOfPoints
            read(13,*)dataX(i,k),dataY(i,k) !,dataY2(i,k)
        enddo
        close(13,status = 'keep')
		
    enddo

	
	! READ IN NAMES AND CONCENTRATION DATA FOR FIXED CONSTRAINED SPECIES
	ALLOCATE (dataFixedY(countOfFixConSpecNames))
	write(*,*) 'Reading in the names and concentration of the fixed constrained species (in file constrainedFixedSpecies.config)...'
    open(9, file='modelConfiguration/constrainedFixedSpecies.config',status = 'old') ! input file
	id=0
	j = 0
	DO i = 1, countOfFixConSpecNames
		read(9,*) name, value
		call matchOneNameToNumber(speciesName,name,neq,id)
		if(id.ne.0) then
			j = j+1	
			constrainedName(j+numberOfVariableConstrainedSpecies)=name
			dataFixedY(j)=value
			constrainedSpecies(j+numberOfVariableConstrainedSpecies)=id
		endif
    end do
    close(9,status = 'keep')
	numberOfFixedConstrainedSpecies = j
	write(94,*) 'Number of fixed constrained species:',numberOfFixedConstrainedSpecies

	if(numberOfFixedConstrainedSpecies.gt.0) THEN
		write(*,*) 1, constrainedName(1+numberOfVariableConstrainedSpecies), dataFixedY(1)
	ENDIF
    if(numberOfFixedConstrainedSpecies.gt.2) write(*,*) '...'
    if(numberOfFixedConstrainedSpecies.gt.1) THEN
		write(*,*) numberOfFixedConstrainedSpecies, &
		constrainedName(numberOfFixedConstrainedSpecies+numberOfVariableConstrainedSpecies), &
		dataFixedY(numberOfFixedConstrainedSpecies)
	ENDIF
	write(*,*) 'Finished reading in the names and concentration of fixed-concentration species.'

	numberOfConstrainedSpecies = numberOfVariableConstrainedSpecies + numberOfFixedConstrainedSpecies
	write(94,*) "Number of constrained species:",numberOfConstrainedSpecies

    ! ERROR HANDLING
    if(numberOfConstrainedSpecies.ge.neq) then
        write(94,*)"Error: Number of (number of constrained species) => (number of species)"
        write(94,*)"(number of constrained species) = ",numberOfConstrainedSpecies
        write(94,*)"(number of  species) = ",neq
        stop 2
    endif

    ALLOCATE (constrainedConcs(numberOfConstrainedSpecies))

    call setNumberOfConstrainedSpecies(numberOfConstrainedSpecies)

    write(*,*)'Finished reading constrained species.'

	! initialise concentrations of constrained species
	write(*,*) 'Initialising concentrations of constrained species...'
   do i=1, numberOfConstrainedSpecies
		if (i<=numberOfVariableConstrainedSpecies) THEN
			call    getConstrainedQuantAtT2D(t,datax,datay,datay2,speciesNumberOfPoints(i),concAtT,1,i, &
				maxNumberOfDataPoints,numberOfVariableConstrainedSpecies)
		ELSE
			concAtT = dataFixedY(i-numberOfVariableConstrainedSpecies)
		ENDIF
        constrainedConcs(i) = concAtT
		call setConstrainedConc(i,concAtT)
		y(constrainedSpecies(i))=concAtT
    enddo
	write(*,*) 'Finished initialising concentrations of constrained species.'
	
    return
end

subroutine readEnvVar(maxNumberOfDataPoints)

    use envVars

    implicit none

    integer:: i, counter,numConEnvVar,k,maxNumberOfDataPoints
    character(len=30) dummy
    CHARACTER(LEN=27)::  fileLocationPrefix
    CHARACTER(LEN=57):: fileLocation
    double precision, allocatable:: testArray(:)

    write(*,*) 'Reading environment variables...'
    open(5, file='modelConfiguration/environmentVariables.config',status = 'old') ! input file
    maxNumberOfDataPoints = 10000

    ! FIND NUMBER OF ENVIRONMENTAL VARIABLES
    counter = 0
    do
        counter = counter + 1
        read(5,*)dummy
        if(dummy.eq.'end') exit
    enddo

    numEnvVars = counter -1

    !ALLOCATE STOREAGE FOR CURRENT VALUES OF ENV VARS USED FOR OUTPUT
    ALLOCATE (currentEnvVarValues(numEnvVars))

    write(*,*) 'Number of environment variables: ', numEnvVars
    ALLOCATE(testArray(3))
    ALLOCATE(envVarTypesNum(numEnvVars),envVarNames(numEnvVars), envVarTypes(numEnvVars))
    ALLOCATE(envVarFixedValues(numEnvVars))
    rewind(5)
    ! READ IN ENV VARIABLES
    numConEnvVar = 0
    do i=1,numEnvVars
        read(5,*)dummy,envVarNames(i), envVarTypes(i)
        write(*,*)dummy,envVarNames(i), envVarTypes(i)

        if (trim(envVarTypes(i)).eq.'CALC') then
            envVarTypesNum(i) = 1
        else if (trim(envVarTypes(i)).eq.'CONSTRAINED') then
            envVarTypesNum(i) = 2
            numConEnvVar = numConEnvVar + 1
        else if (trim(envVarTypes(i)).eq.'NOTUSED') then
            envVarTypesNum(i) = 4
        ! OTHERWISE ASSUME  A FIXED VALUE
        else
            envVarTypesNum(i) = 3
            read(envVarTypes(i),*) envVarFixedValues(i)
        endif
    enddo
        close(5,status = 'keep')

    write(*,*) 'Finished reading environment variables.'
    write(*,*) 
		    
    ALLOCATE (envVarX(numEnvVars,maxNumberOfDataPoints))
    ALLOCATE (envVarY(numEnvVars,maxNumberOfDataPoints))
    ALLOCATE (envVarY2(numEnvVars,maxNumberOfDataPoints))
    ALLOCATE(envVarNumberOfPoints(numEnvVars))

    fileLocationPrefix = './environmentalConstraints/'
    ! READ IN CONSTRAINT DATA FOR CONSTRAINED ENV VARIABLES
    write(*,*) 'Checking for constrained environmental variables...'
    do i =1,numEnvVars
        if(envVarTypes(i).eq.'CONSTRAINED') then

            write(*,*) 'Reading constraint data for', envVarNames(i)

            fileLocation = fileLocationPrefix // trim(envVarNames(i))

            open(13,file=fileLocation, status='old')

            read(13,*),envVarNumberOfPoints(i)
            do k = 1, envVarNumberOfPoints(i)
                read(13,*)envVarX(i,k),envVarY(i,k) ! envVarY2(i,k)

            enddo
            close(13,status = 'keep')
            write(*,*) 'Finished reading constraint data.'
        endif
    enddo
    ! deallocate data
    deallocate(testArray)
    write(*,*) 'Finished checking for constrained environmental variables.'

    return
end
   
  