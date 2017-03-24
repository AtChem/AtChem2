SUBROUTINE calcJFac(jfac, t)

  USE zenithData1
  USE photolysisRates
  USE constraints

  IMPLICIT NONE
  DOUBLE PRECISION :: jfac, JSpeciesAtT, t
  INTEGER :: basePhotoRateNum, i
  INTEGER :: firstTime = 1
  IF (firstTime==1) THEN
     WRITE (*,*) "basePhotoRate: ", jfacBase
     firstTime = 0
  ENDIF

  !GET INDEX OF basePhotoRate SPECIES IN PHOTO CONSTRAINT ARRAY
  basePhotoRateNum = 0
  DO i = 1, numConPhotoRates

     IF ((TRIM (constrainedPhotoRates(i)))==(TRIM (jfacBase))) THEN
        basePhotoRateNum = i
     ENDIF

  ENDDO

  IF (basePhotoRateNum==0) THEN
     WRITE (*,*) 'Error! Missing constrained photo rate data for the JFAC species provided: ', TRIM (jfacBase)
     STOP 2
  ENDIF

  !GET CURRENT VALUE OF basePhotoRate

  CALL getConstrainedQuantAtT2D (t, photoX, photoY, photoY2, photoNumberOfPoints (basePhotoRateNum), JSpeciesAtT, 2, &
       basePhotoRateNum, maxNumberOfDataPoints, numConPhotoRates)


  IF (JSpeciesAtT==0) THEN
     JFAC = 0
  ELSE
     IF (useConstantValues==0) THEN
        JFAC = JspeciesAtT/(transmissionFactor(jfacSpeciesLine)* cl(jfacSpeciesLine)* &
             (COSX**(cmm(jfacSpeciesLine)))* EXP (-cnn(jfacSpeciesLine)*SECX))
     ELSE
        WRITE (*,*) 'Error! JFAC should not be used, as constant photolysis rates have been provided.'
        STOP 2
     ENDIF
  ENDIF
  RETURN
END SUBROUTINE calcJFac

SUBROUTINE calcM (PRESSURE, TEMP, M)
  IMPLICIT NONE
  DOUBLE PRECISION PRESSURE, TEMP, M
  M = 9.6576d18*(PRESSURE/TEMP)

  RETURN
END SUBROUTINE calcM

SUBROUTINE calcDec(dec, t)
  USE date
  IMPLICIT NONE
  DOUBLE PRECISION dec, t, PI

  PI = 4.0*ATAN (1.0)
  currentFYear = fractionYear + (t / secYear)
  DEC = -4.1420D-01*COS (2.00D+00*PI*currentFYear)

  RETURN
END SUBROUTINE calcDec

SUBROUTINE addConstrainedSpeciesToProbSpec(z, x, numberOfConstrainedSpecies, constrainedSpecies, neq, constrainedConcs)
  DOUBLE PRECISION z(*), x(*), constrainedConcs(*)
  INTEGER numberOfConstrainedSpecies, constrainedSpecies(*), zCounter, speciesConstrained, i, neq, j

  zCounter = 1
  DO i = 1, numberOfConstrainedSpecies + neq
     speciesConstrained = 0
     DO j = 1, numberOfConstrainedSpecies
        IF (i==constrainedSpecies(j)) THEN
           speciesConstrained = j
        ENDIF
     ENDDO
     IF (speciesConstrained>0) THEN
        x(i) = constrainedConcs(speciesConstrained)
     ELSE IF (speciesConstrained==0) THEN
        x(i) = z(zCounter)
        zCounter = zCounter + 1
     ELSE
        WRITE (*,*) 'Error adding constrained values to measured values'
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE addConstrainedSpeciesToProbSpec

!     ---------------------------------------------------------------
SUBROUTINE removeConstrainedSpeciesFromProbSpec(y, z, numberOfConstrainedSpecies, constrainedSpecies, neq)
  DOUBLE PRECISION z(*), y(*)
  INTEGER numberOfConstrainedSpecies, constrainedSpecies(*), zCounter, speciesConstrained, i, k, neq

  zCounter = 1
  ! loop through y()
  DO i = 1, neq
     speciesConstrained = 0
     ! loop through constrained species
     DO k = 1, numberOfConstrainedSpecies
        IF (i==constrainedSpecies(k)) THEN
           speciesConstrained = 1
        ENDIF
     ENDDO
     IF (speciesConstrained==1) THEN
        ! do nothing
     ELSE IF (speciesConstrained==0) THEN
        z(zCounter) = y(i)
        zCounter = zCounter + 1
     ELSE
        WRITE (*,*) 'error removing constrained species from y() '
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE removeConstrainedSpeciesFromProbSpec

SUBROUTINE getEnvVarsAtT (t, temp, rh, h2o, dec, pressure, m, blh, dilute, jfac, roofOpen)
  USE envVars
  USE constraints
  USE zenithData1
  IMPLICIT NONE
  DOUBLE PRECISION :: t, envVarAtT, theta
  DOUBLE PRECISION :: temp, rh, h2o, dec, pressure, m, blh, dilute, jfac, roofOpen
  INTEGER :: envVarNum, envVarNumH2O

  ! ********************************************************************************************************************
  ! GET PRESSURE AT T
  ! ********************************************************************************************************************
  CALL getEnvVarNum ('PRESSURE', envVarNum, envVarNames, numEnvVars)
  ! IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     ! IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum), &
          envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     pressure = envVarAtT
     ! IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     pressure = envVarFixedValues(envVarNum)
  ELSE
    pressure = -1
  ENDIF
  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = pressure
  ! ********************************************************************************************************************
  ! GET TEMP AT T
  ! ********************************************************************************************************************
  CALL getEnvVarNum ('TEMP', envVarNum, envVarNames, numEnvVars)
  ! IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     ! IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum), &
          envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     temp = envVarAtT
     ! IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     temp = envVarFixedValues(envVarNum)
  ELSE
     temp = -1
  ENDIF
  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = temp
  ! ********************************************************************************************************************
  ! GET H2O AT T
  ! ********************************************************************************************************************
  CALL getEnvVarNum ('H2O', envVarNum, envVarNames, numEnvVars)
  ! IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     ! IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum), &
          envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     H2O = envVarAtT
     ! IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     H2O = envVarFixedValues(envVarNum)
  ELSE
     H2O = -1
  ENDIF
  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  envVarNumH2O = envVarNum
  currentEnvVarValues(envVarNum) = H2O
  ! ********************************************************************************************************************
  ! GET M AT T
  ! ********************************************************************************************************************
  CALL getEnvVarNum ('M', envVarNum, envVarNames, numEnvVars)
  ! IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     CALL calcM (PRESSURE, TEMP, M)
     ! IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum), &
          envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     m = envVarAtT
     ! IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     m = envVarFixedValues(envVarNum)
  ELSE
     m = -1
  ENDIF
  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = m
  ! ********************************************************************************************************************
  ! GET DEC AT T
  ! ********************************************************************************************************************
  CALL getEnvVarNum ('DEC', envVarNum, envVarNames, numEnvVars)
  ! IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     CALL calcDec (dec, t)
     ! IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum), &
          envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     dec = envVarAtT
     ! IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     dec = envVarFixedValues(envVarNum)
     ! IF NOT USED
  ELSE
     WRITE (*,*) 'Error! DEC variable must be provided.' // &
          'Please set it to the declination angle of the sun ' // &
          '(or to CALC and then set a correct date).'
     STOP 2
  ENDIF
  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = dec
  ! ********************************************************************************************************************
  ! GET BOUNDARY LAYER HEIGHT AT T
  ! ********************************************************************************************************************
  CALL getEnvVarNum ('BOUNDARYLAYERHEIGHT', envVarNum, envVarNames, numEnvVars)
  ! IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     ! IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum), &
          envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     blh = envVarAtT
     ! IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     blh = envVarFixedValues(envVarNum)
     ! IF NOT USED
  ELSE
     blh = -1
  ENDIF
  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = blh
  ! ********************************************************************************************************************
  ! GET RELATIVE HUMIDITY AT T
  ! ********************************************************************************************************************
  CALL getEnvVarNum ('RH', envVarNum, envVarNames, numEnvVars)
  ! IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     ! IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum), &
          envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     RH = envVarAtT
     CALL convertRHtoConcH2O (H2o, temp, RH)
     ! IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     RH = envVarFixedValues(envVarNum)
     CALL convertRHtoConcH2O (H2o, temp, RH)
     ! IF NOT USED
  ELSE
     RH = -1
  ENDIF
  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = rh
  currentEnvVarValues(envVarNumH2O) = H2O

  !*******************************************************************************************************
  !GET DILUTE AT T
  !***********************************************************************************************************
  CALL getEnvVarNum ('DILUTE', envVarNum, envVarNames, numEnvVars)
  !IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     !IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum) &
          , envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     dilute = envVarAtT
     !IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     dilute = envVarFixedValues(envVarNum)

     !IF NOT USED
  ELSE
     dilute = 0

  ENDIF

  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = dilute

  !**************************************************************************************************
  !COMPUTE PARAMETERS FOR PHOTOLYSIS RATES
  !*************************************************************************************************
  CALL zenith (theta, secx, cosx, t, dec)

  !**************************************************************************************************
  !GET JFAC AT T
  !*************************************************************************************************
  CALL getEnvVarNum ('JFAC', envVarNum, envVarNames, numEnvVars)
  !IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     CALL calcJFac (jfac, t)
     !IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum) &
          , envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     JFAC = envVarAtT
     !IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     JFAC = envVarFixedValues(envVarNum)

     !IF NOT USED
  ELSE
     !set jfac = , so no effect on photolysis calculations
     jfac = 1
  ENDIF

  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = jfac

  !**************************************************************************************************
  !GET ROOFOPEN AT T
  !************************************************************************************************************
  CALL getEnvVarNum ('ROOFOPEN', envVarNum, envVarNames, numEnvVars)
  !IF CALCULATED
  IF (envVarTypesNum(envVarNum)==1) THEN
     WRITE (*,*) "No calculation available for ROOFOPEN Variable"
     !IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     CALL getConstrainedQuantAtT2D (t, envVarX, envVarY, envVarY2, envVarNumberOfPoints (envVarNum) &
          , envVarAtT, 2, envVarNum, maxNumberOfDataPoints, numEnvVars)
     roofOpen = envVarAtT
     !IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     roofOpen = envVarFixedValues(envVarNum)

     !IF NOT USED
  ELSE
     !set roofopen = , so no effect on photolysis calculations
     roofOpen = 1
  ENDIF
  ! CAPTURE CURRENT ENVVAR VALUES FOR OUTPUT
  currentEnvVarValues(envVarNum) = roofOpen

  RETURN
END SUBROUTINE getEnvVarsAtT

SUBROUTINE getEnvVarNum(name, envVarNum, envVarNames, numEnvVars)
  CHARACTER name*(*)
  CHARACTER (LEN=30) envVarNames(*)
  INTEGER :: envVarNum, i, numEnvVars

  DO i = 1, numEnvVars

     IF (name==TRIM (envVarNames(i))) THEN
        envVarNum = i
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE getEnvVarNum

SUBROUTINE test_jfac()
  ! check jfac data consistency
  USE photolysisRates
  USE envVars
  IMPLICIT NONE
  INTEGER :: envVarNum
  ! If JFAC species is provided (e.g. JNO2) and constraint file is not provided, then the program should complain.
  envVarNum = 0
  CALL getEnvVarNum ('JFAC', envVarNum, envVarNames, numEnvVars)
  !IF CALCULATED
  ! If JFAC is CALC and there's no JFAC species, the program should complain
  IF (envVarTypesNum(envVarNum)==1) THEN
     IF ( ''==(TRIM (jfacBase)) .OR. TRIM (jfacBase)=='end' ) THEN
        WRITE (*,*) 'Error! JFAC was set to CALC, but JFac species was not provided!'
        STOP 2
     ENDIF
     ! If jfacSpeciesLine = 0 (no line in photolysis rates matches the JFac species), program should complain
     IF (jfacSpeciesLine==0 ) THEN
        WRITE (*,*) 'Error! No match found in photolysis rates file for provided JFAC species ', jfacBase
     ENDIF
     !IF CONSTRAINED
  ELSE IF (envVarTypesNum(envVarNum)==2) THEN
     !IF FIXED
  ELSE IF (envVarTypesNum(envVarNum)==3) THEN
     !IF NOT USED
     ! if JFAC is NOTUSED: and JFAC species has anything in, the program should complain.
  ELSE
     IF ( ''/=(TRIM (jfacBase)) .AND. TRIM (jfacBase)/='end' ) THEN
        WRITE (*,*) 'Error! JFAC was set to NOTUSED, but at the same time JFac species was provided!'
        WRITE (*,*) 'JFac species: ', jfacBase
        STOP 2
     ENDIF
  ENDIF
END SUBROUTINE test_jfac
