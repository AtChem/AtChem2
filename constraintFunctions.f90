subroutine calcJFac(jfac,t)

    use zenithData1
    use photolysisRates
    use constraints

    implicit none
    double precision:: jfac, JSpeciesAtT,t
    integer:: basePhotoRateNum,i
  integer:: firstTime=1
  if(firstTime .eq. 1) then
        write(*,*)"basePhotoRate: ",jfacBase
        firstTime=0
  endif

    !GET INDEX OF basePhotoRate SPECIES IN PHOTO CONSTRAINT ARRAY
    basePhotoRateNum=0
    do i=1,numConPhotoRates

        if((trim(constrainedPhotoRates(i))).eq.(trim(jfacBase))) then
        basePhotoRateNum = i
        endif

    enddo

    if(basePhotoRateNum .eq. 0) then
        write(*,*) 'Error! Missing constrained photo rate data for the JFAC species provided: ',  trim(jfacBase)
        stop 2
    endif

    !GET CURRENT VALUE OF basePhotoRate

    call getConstrainedQuantAtT2D(t,photoX,photoY,photoY2,photoNumberOfPoints(basePhotoRateNum),JSpeciesAtT, 2, &
    basePhotoRateNum, maxNumberOfDataPoints,numConPhotoRates)


    if (JSpeciesAtT.eq.0) then
        JFAC = 0
    else
        if (useConstantValues .eq. 0) then
            JFAC = JspeciesAtT/(transmissionFactor(jfacSpeciesLine)* cl(jfacSpeciesLine)* &
                (COSX**(cmm(jfacSpeciesLine)))* EXP(-cnn(jfacSpeciesLine)*SECX))
        else
            write (*,*) 'Error! JFAC should not be used, as constant photolysis rates have been provided.'
            stop 2
        endif
    endif
    return
end

subroutine calcM(PRESSURE, TEMP, M)
    implicit none
    double precision PRESSURE, TEMP, M
    M= 9.6576d18*(PRESSURE/TEMP)

    return
end

subroutine calcDec(dec, t)
    use date
    implicit none
    double precision dec, t, PI

    PI = 4.0*ATAN(1.0)
    currentFYear = fractionYear + (t / secYear)
    DEC    = -4.1420D-01*COS(2.00D+00*PI*currentFYear)

return
end

subroutine addConstrainedSpeciesToProbSpec(z,x,numberOfConstrainedSpecies,constrainedSpecies,neq,constrainedConcs)
    double precision z(*),x(*),constrainedConcs(*)
    integer numberOfConstrainedSpecies, constrainedSpecies(*), zCounter, speciesConstrained,i,neq, j

    zCounter = 1
    do i=1,numberOfConstrainedSpecies + neq
        speciesConstrained = 0
        do j=1,numberOfConstrainedSpecies
            if (i.eq.constrainedSpecies(j)) then
                speciesConstrained =j
            endif
        enddo
        if (speciesConstrained.gt.0) then
            x(i) = constrainedConcs(speciesConstrained)
        else if  (speciesConstrained.eq.0) then
            x(i) = z(zCounter)
            zCounter = zCounter + 1
        else
            write(*,*)'Error adding constrained values to measured values'
        endif
    enddo
    return
end

!     ---------------------------------------------------------------
subroutine removeConstrainedSpeciesFromProbSpec(y,z,numberOfConstrainedSpecies,constrainedSpecies,neq)
    double precision z(*),y(*)
    integer numberOfConstrainedSpecies, constrainedSpecies(*), zCounter, speciesConstrained,i,k,neq

    zCounter = 1
    ! loop through y()
    do i=1,neq
        speciesConstrained = 0
        ! loop through constrained species
        do k=1,numberOfConstrainedSpecies
            if(i.eq.constrainedSpecies(k)) then
                speciesConstrained =1
            endif
        enddo
        if (speciesConstrained.eq.1) then
        ! do nothing
        else if (speciesConstrained.eq.0) then
            z(zCounter) = y(i)
            zCounter = zCounter + 1
        else
            write(*,*) 'error removing constrained species from y()'
        endif
    enddo
    return
end

subroutine getEnvVarsAtT(t,temp,rh,h2o,dec,pressure,m,blh,dilute,jfac,roofOpen)
    USE envVars
    USE constraints
    use zenithData1
    implicit none
    double precision:: t, envVarAtT, theta
    double precision:: temp,rh,h2o,dec,pressure,m,blh,dilute,jfac,roofOpen
    integer:: envVarNum, envVarNumH2O

    ! ********************************************************************************************************************
    ! GET PRESSURE AT T
    ! ********************************************************************************************************************
    call getEnvVarNum('PRESSURE',envVarNum,envVarNames,numEnvVars)
    ! IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
    ! IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum), &
            envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        pressure = envVarAtT
    ! IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        pressure = envVarFixedValues(envVarNum)
    endif
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = pressure
    ! ********************************************************************************************************************
    ! GET TEMP AT T
    ! ********************************************************************************************************************
    call getEnvVarNum('TEMP',envVarNum,envVarNames,numEnvVars)
    ! IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
    ! IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum), &
            envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        temp = envVarAtT
    ! IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        temp = envVarFixedValues(envVarNum)
    endif
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = temp
    ! ********************************************************************************************************************
    ! GET H2O AT T
    ! ********************************************************************************************************************
    call getEnvVarNum('H2O',envVarNum,envVarNames,numEnvVars)
    ! IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
    ! IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum), &
            envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        H2O = envVarAtT
    ! IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        H2O = envVarFixedValues(envVarNum)
    endif
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  envVarNumH2O = envVarNum
    currentEnvVarValues(envVarNum) = H2O
    ! ********************************************************************************************************************
    ! GET M AT T
    ! ********************************************************************************************************************
    call getEnvVarNum('M',envVarNum,envVarNames,numEnvVars)
    ! IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
        call calcM(PRESSURE, TEMP, M)
    ! IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum), &
            envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        m = envVarAtT
    ! IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        m = envVarFixedValues(envVarNum)
    endif
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = m
    ! ********************************************************************************************************************
    ! GET DEC AT T
    ! ********************************************************************************************************************
    call getEnvVarNum('DEC',envVarNum,envVarNames,numEnvVars)
    ! IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
        CALL calcDec(dec, t)
    ! IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum), &
            envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        dec = envVarAtT
    ! IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        dec = envVarFixedValues(envVarNum)
    ! IF NOT USED
    else
        write(*,*) 'Error! DEC variable must be provided.' // &
         'Please set it to the declination angle of the sun ' // &
         '(or to CALC and then set a correct date).'
        stop 2
    endif
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = dec
    ! ********************************************************************************************************************
    ! GET BOUNDARY LAYER HEIGHT AT T
    ! ********************************************************************************************************************
    call getEnvVarNum('BOUNDARYLAYERHEIGHT',envVarNum,envVarNames,numEnvVars)
    ! IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
    ! IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum), &
            envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        blh = envVarAtT
    ! IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        blh = envVarFixedValues(envVarNum)
    ! IF NOT USED
    else

    endif
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = blh
    ! ********************************************************************************************************************
    ! GET RELATIVE HUMIDITY AT T
    ! ********************************************************************************************************************
    call getEnvVarNum('RH',envVarNum,envVarNames,numEnvVars)
    ! IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
    ! IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum), &
            envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        RH = envVarAtT
        call convertRHtoConcH2O(H2o,temp,RH)
    ! IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        RH = envVarFixedValues(envVarNum)
        call convertRHtoConcH2O(H2o,temp,RH)
    ! IF NOT USED
    else

    endif
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = rh
    currentEnvVarValues(envVarNumH2O) = H2O

    !*******************************************************************************************************
    !GET DILUTE AT T
    !***********************************************************************************************************
    call getEnvVarNum('DILUTE',envVarNum,envVarNames,numEnvVars)
    !IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
    !IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum) &
        ,envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        dilute = envVarAtT
    !IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        dilute = envVarFixedValues(envVarNum)

    !IF NOT USED
    else
    dilute = 0

    endif

    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = dilute

	!**************************************************************************************************
    !COMPUTE PARAMETERS FOR PHOTOLYSIS RATES
    !*************************************************************************************************
    call zenith(theta, secx, cosx, t,dec)

    !**************************************************************************************************
    !GET JFAC AT T
    !*************************************************************************************************
    call getEnvVarNum('JFAC',envVarNum,envVarNames,numEnvVars)
    !IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
        call calcJFac(jfac,t)
    !IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum) &
        ,envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        JFAC = envVarAtT
    !IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        JFAC = envVarFixedValues(envVarNum)

    !IF NOT USED
    else
        !set jfac = , so no effect on photolysis calculations
        jfac = 1
    endif

    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = jfac

    !**************************************************************************************************
    !GET ROOFOPEN AT T
    !************************************************************************************************************
    call getEnvVarNum('ROOFOPEN',envVarNum,envVarNames,numEnvVars)
    !IF CALCULATED
    if(envVarTypesNum(envVarNum).eq.1) then
        write(*,*)"No calculation available for ROOFOPEN Variable"
    !IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
        call getConstrainedQuantAtT2D(t,envVarX,envVarY,envVarY2,envVarNumberOfPoints(envVarNum) &
        ,envVarAtT, 2,envVarNum,maxNumberOfDataPoints,numEnvVars)
        roofOpen = envVarAtT
    !IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
        roofOpen = envVarFixedValues(envVarNum)

    !IF NOT USED
    else
    !set roofopen = , so no effect on photolysis calculations
    roofOpen = 1
    endif
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = roofOpen

    return
end

subroutine getEnvVarNum(name, envVarNum, envVarNames, numEnvVars)
    character name*(*)
    CHARACTER(LEN=30) envVarNames(*)
    integer:: envVarNum, i,numEnvVars

    do i=1,numEnvVars

        if(name.eq.trim(envVarNames(i))) then
            envVarNum = i
        endif
    enddo
    return
end

subroutine test_jfac()
! check jfac data consistency
    use photolysisRates
    use envVars
    implicit none
    integer :: envVarNum
    ! If JFAC species is provided (e.g. JNO2) and constraint file is not provided, then the program should complain.
    envVarNum = 0
   call getEnvVarNum('JFAC',envVarNum,envVarNames,numEnvVars)
    !IF CALCULATED
   ! If JFAC is CALC and there's no JFAC species, the program should complain
    if(envVarTypesNum(envVarNum).eq.1) then
        if ( '' .eq.(trim(jfacBase)) .or. trim(jfacBase) .eq. 'end' ) then
            write(*,*) 'Error! JFAC was set to CALC, but JFac species was not provided!'
            stop 2
        endif
        ! If jfacSpeciesLine=0 (no line in photolysis rates matches the JFac species), program should complain
        if (jfacSpeciesLine .eq. 0 ) then
            write(*,*) 'Error! No match found in photolysis rates file for provided JFAC species ', jfacBase
        endif
    !IF CONSTRAINED
    else if(envVarTypesNum(envVarNum).eq.2) then
    !IF FIXED
    else if(envVarTypesNum(envVarNum).eq.3) then
    !IF NOT USED
    ! if JFAC is NOTUSED: and JFAC species has anything in, the program should complain.
    else
         if ( '' .ne.(trim(jfacBase)) .and. trim(jfacBase) .ne. 'end' ) then
            write(*,*) 'Error! JFAC was set to NOTUSED, but at the same time JFac species was provided!'
            write(*,*) 'JFac species: ', jfacBase
            stop 2
        endif
    endif
end
