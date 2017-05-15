module constraintFunctions_mod
  use types_mod
contains

  ! ----------------------------------------------------------------- !

  subroutine calcJFac( jfac, t )
    use zenithData1
    use photolysisRates
    use constraints
    use interpolationFunctions_mod, only : getConstrainedQuantAtT2D

    implicit none
    real(kind=DP) :: jfac, JSpeciesAtT, t
    integer(kind=NPI) :: basePhotoRateNum, i
    integer :: firstTime = 1
    if ( firstTime == 1 ) then
      write (*,*) "basePhotoRate: ", jFacSpecies
      firstTime = 0
    end if

    !GET INDEX OF basePhotoRate SPECIES IN PHOTO CONSTRAINT ARRAY
    basePhotoRateNum = 0
    do i = 1, numConPhotoRates
      if ( trim( constrainedPhotoRates(i) ) == trim( jFacSpecies ) ) then
        basePhotoRateNum = i
      end if
    end do

    if ( basePhotoRateNum == 0 ) then
      write (*,*) 'Error! Missing constrained photo rate data for the JFAC species provided: ', trim( jFacSpecies )!
      stop 2
    end if

    !GET CURRENT VALUE OF basePhotoRate

    call getConstrainedQuantAtT2D( t, photoX, photoY, photoY2, photoNumberOfPoints (basePhotoRateNum), JSpeciesAtT, 2, &
                                   basePhotoRateNum, maxNumberOfDataPoints, numConPhotoRates )

    if ( JSpeciesAtT == 0 ) then
      jfac = 0
    else
      if ( usePhotolysisConstants .eqv. .false. ) then
        jfac = JspeciesAtT / ( transmissionFactor(jfacSpeciesLine) * cl(jfacSpeciesLine) * &
               ( cosx ** cmm(jfacSpeciesLine) ) * exp( -cnn(jfacSpeciesLine) * secx ) )
      else
        write (*,*) 'Error! JFAC should not be used, as constant photolysis rates have been provided.'!
        stop 2
      end if
    end if
    return
  end subroutine calcJFac

  ! ----------------------------------------------------------------- !

  subroutine addConstrainedSpeciesToProbSpec( z, x, numberOfConstrainedSpecies, constrainedSpecies, neq, constrainedConcs )
    real(kind=DP) :: z(*), x(*), constrainedConcs(*)
    integer(kind=NPI) :: constrainedSpecies(*), zCounter, neq, numberOfConstrainedSpecies, i, j, speciesConstrained

    zCounter = 1
    do i = 1, numberOfConstrainedSpecies + neq
      speciesConstrained = 0
      do j = 1, numberOfConstrainedSpecies
        if ( i == constrainedSpecies(j) ) then
          speciesConstrained = j
        end if
      end do
      if ( speciesConstrained > 0 ) then
        x(i) = constrainedConcs(speciesConstrained)
      else if ( speciesConstrained == 0 ) then
        x(i) = z(zCounter)
        zCounter = zCounter + 1
      else
        write (*,*) 'Error adding constrained values to measured values'
      end if
    end do
    return
  end subroutine addConstrainedSpeciesToProbSpec

  ! ----------------------------------------------------------------- !

  subroutine removeConstrainedSpeciesFromProbSpec( y, z, constrainedSpecies )
    implicit none
    real(kind=DP) :: y(:), z(:)
    integer(kind=NPI) :: constrainedSpecies(:), zCounter, speciesConstrained, i, k

    zCounter = 1
    ! loop through y()
    do i = 1, size( y )
      speciesConstrained = 0
      ! loop through constrained species
      do k = 1, size( constrainedSpecies )
        if ( i == constrainedSpecies(k) ) then
          speciesConstrained = 1
        end if
      end do
      if ( speciesConstrained == 1 ) then
        ! do nothing
      else if ( speciesConstrained == 0 ) then
        z(zCounter) = y(i)
        zCounter = zCounter + 1
      else
        write (*,*) 'error removing constrained species from y()'
      end if
    end do
    return
  end subroutine removeConstrainedSpeciesFromProbSpec

  ! ----------------------------------------------------------------- !

  subroutine getEnvVarsAtT( t, temp, rh, h2o, dec, pressure, m, blh, dilute, jfac, roofOpen )
    use envVars
    use constraints
    use zenithData1
    use interpolationFunctions_mod, only : getConstrainedQuantAtT2D
    use conversionFunctions_mod
    use utilityFunctions_mod, only : zenith
    implicit none

    real(kind=DP) :: t, envVarAtT, theta
    real(kind=DP) :: temp, rh, h2o, dec, pressure, m, blh, dilute, jfac, roofOpen
    integer(kind=NPI) :: envVarNum, envVarNumH2O

    ! ********************************************
    ! GET PRESSURE AT T
    ! ********************************************
    call getEnvVarNum( 'PRESS', envVarNum )
    ! IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      ! IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      pressure = envVarAtT
      ! IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      pressure = envVarFixedValues(envVarNum)
    else
      pressure = -1
    end if
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = pressure
    ! ********************************************
    ! GET TEMP AT T
    ! ********************************************
    call getEnvVarNum( 'TEMP', envVarNum )
    ! IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      ! IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      temp = envVarAtT
      ! IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      temp = envVarFixedValues(envVarNum)
    else
      temp = -1
    end if
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = temp
    ! ********************************************
    ! GET H2O AT T
    ! ********************************************
    call getEnvVarNum( 'H2O', envVarNum )
    ! IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      ! IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      h2o = envVarAtT
      ! IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      h2o = envVarFixedValues(envVarNum)
    else
      h2o = -1
    end if
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    envVarNumH2O = envVarNum
    currentEnvVarValues(envVarNum) = H2O
    ! ********************************************
    ! GET M AT T
    ! ********************************************
    call getEnvVarNum( 'M', envVarNum )
    ! IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      M = calcM(pressure, temp)
      ! IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      m = envVarAtT
      ! IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      m = envVarFixedValues(envVarNum)
    else
      m = -1
    end if
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = m
    ! ********************************************
    ! GET DEC AT T
    ! ********************************************
    call getEnvVarNum( 'DEC', envVarNum )
    ! IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      call calcDec( dec, t )
      ! IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      dec = envVarAtT
      ! IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      dec = envVarFixedValues(envVarNum)
      ! IF NOTUSED
    else
      write (*,*) 'Error! DEC variable must be provided.' // &
                  'Please set it to the declination angle of the sun ' // &
                  '(or to CALC and then set a correct date ).'
      stop 2
    end if
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = dec
    ! ********************************************
    ! GET BOUNDARY LAYER HEIGHT AT T
    ! ********************************************
    call getEnvVarNum( 'BLHEIGHT', envVarNum )
    ! IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      ! IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      blh = envVarAtT
      ! IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      blh = envVarFixedValues(envVarNum)
      ! IF NOTUSED
    else
      blh = -1
    end if
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = blh
    ! ********************************************
    ! GET RELATIVE HUMIDITY AT T
    ! ********************************************
    call getEnvVarNum( 'RH', envVarNum )
    ! IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      ! IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      rh = envVarAtT
      h2o =  convertRHtoH2O(rh, temp, pressure)
      ! IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      rh = envVarFixedValues(envVarNum)
      h2o = convertRHtoH2O( rh, temp, pressure )
      ! IF NOTUSED
    else
      rh = -1
    end if
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = rh
    currentEnvVarValues(envVarNumH2O) = h2o

    !*********************************************
    !GET DILUTE AT T
    !*********************************************
    call getEnvVarNum( 'DILUTE', envVarNum )
    !IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      !IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      dilute = envVarAtT
      !IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      dilute = envVarFixedValues(envVarNum)
      !IF NOTUSED
    else
      dilute = 0
    end if

    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = dilute

    !*********************************************
    !COMPUTE PARAMETERS FOR PHOTOLYSIS RATES
    !*********************************************
    call zenith( theta, secx, cosx, t, dec )

    !*********************************************
    !GET JFAC AT T
    !*********************************************
    call getEnvVarNum( 'JFAC', envVarNum )
    !IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      call calcJFac( jfac, t )
      !IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      jfac = envVarAtT
      !IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      jfac = envVarFixedValues(envVarNum)
      !IF NOTUSED
    else
      !set jfac = , so no effect on photolysis calculations
      jfac = 1
    end if

    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = jfac

    !*********************************************
    !GET ROOFOPEN AT T
    !*********************************************
    call getEnvVarNum( 'ROOFOPEN', envVarNum )
    !IF CALC
    if ( envVarTypesNum(envVarNum) == 1 ) then
      write (*,*) "No calculation available for ROOFOPEN Variable"
      !IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      call getConstrainedQuantAtT2D( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum), &
                                     envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars )
      roofOpen = envVarAtT
      !IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      roofOpen = envVarFixedValues(envVarNum)
      !IF NOTUSED
    else
      !set roofopen = , so no effect on photolysis calculations
      roofOpen = 1
    end if
    ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
    currentEnvVarValues(envVarNum) = roofOpen

    return
  end subroutine getEnvVarsAtT

  ! ----------------------------------------------------------------- !

  subroutine getEnvVarNum( name, envVarNum )
    ! Set envVarNum to the index of name within enVarNames
    use envVars, only : envVarNames, numEnvVars
    implicit none

    character, intent(in) :: name*(*)
    integer(kind=NPI), intent(out) :: envVarNum
    integer(kind=NPI) :: i

    do i = 1, numEnvVars
      if ( name == trim( envVarNames(i) ) ) then
        envVarNum = i
      end if
    end do
    return
  end subroutine getEnvVarNum

  ! ----------------------------------------------------------------- !

  subroutine test_jfac()
    ! check jfac data consistency
    use photolysisRates
    use envVars
    implicit none
    integer(kind=NPI) :: envVarNum
    ! If JFAC species is provided (e.g. JNO2) and constraint file is not provided, then the program should complain.
    envVarNum = 0
    call getEnvVarNum( 'JFAC', envVarNum )
    !IF CALC
    ! If JFAC is CALC and there's no JFAC species, the program should complain
    if ( envVarTypesNum(envVarNum) == 1 ) then
      if ( '' == trim( jFacSpecies ) ) then
        write (*,*) 'Error! JFAC was set to CALC, but JFac species was not provided!'!
        stop 2
      end if
      ! If jfacSpeciesLine = 0 (no line in photolysis rates matches the JFac species), program should complain
      if ( jfacSpeciesLine == 0 ) then
        write (*,*) 'Error! No match found in photolysis rates file for provided JFAC species ', jFacSpecies
      end if
      !IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      !IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      !IF NOTUSED
      ! if JFAC is NOTUSED: and JFAC species has anything in, the program should complain.
    else
      if ( '' /= trim( jFacSpecies ) ) then
        write (*,*) 'Error! JFAC was set to NOTUSED, but at the same time JFac species was provided!'!
        write (*,*) 'JFac species: ', jFacSpecies
        stop 2
      end if
    end if
  end subroutine test_jfac

  ! ----------------------------------------------------------------- !

end module constraintFunctions_mod
