!     ----------------------------------------------------------------
!     MAIN PROGRAM FOR THE ATMOSPHERE CHEMISTRY PROJECT
!     ----------------------------------------------------------------

PROGRAM ATCHEM

  USE species
  USE constraints
  USE interpolationMethod
  USE reactionStructure
  USE photolysisRates
  USE chemcialConstraints
  USE zenithData
  USE zenithData1
  USE productionAndLossRates
  USE envVars
  USE SZACalcVars
  USE date

  IMPLICIT NONE

  !    ********************************************************************************************************
  !    DECLARATIONS
  !    ********************************************************************************************************

  !   DECLARATIONS FOR SOLVER PARAMETERS
  INTEGER ier, i
  INTEGER lnst, lnfe, lnsetup, lnni, lncf, lnetf, lnje
  INTEGER nfels, njtv, npe, nps
  INTEGER meth, itmeth, itol, itask, jout, nout
  INTEGER, PARAMETER :: LongInt_Kind = SELECTED_INT_KIND (11)
  INTEGER (KIND=LongInt_Kind) :: iout (21), ipar (10)
  INTEGER :: neq
  DOUBLE PRECISION rtol, t, t0, tout
  DOUBLE PRECISION atol, rout (6)
  DOUBLE PRECISION :: rpar (1)
  DATA lnst/3/, lnfe/4/, lnetf/5/, lncf/6/, lnni/7/, lnsetup/8/, &
       lnje/17/, nfels/16/, njtv/17/ , npe/18/, nps/19/
  DOUBLE PRECISION, ALLOCATABLE :: y(:)

  !   DECLARATIONS FOR CONFIGURABLE SOLVER PARAMETERS
  DOUBLE PRECISION :: deltaJv, deltaMain, max_step
  INTEGER :: JvApprox, lookBack
  INTEGER :: SpeciesIntMethod, conditionsIntMethod, decIntMethod
  INTEGER :: preconBandUpper, preconBandLower, solverType
  DOUBLE PRECISION :: d

  !   DECLARATIONS FOR TIME PARAMETERS
  INTEGER runStart, runEnd, runTime, rate, previousSeconds
  INTEGER np, numReactions, numSteps
  DOUBLE PRECISION tminus1, outputStepSize

  !   DECLARATIONS FOR SPECIES PARAMETERS
  INTEGER concCounter
  DOUBLE PRECISION, ALLOCATABLE :: concentration(:)
  CHARACTER (LEN=10), ALLOCATABLE :: speciesName(:), concSpeciesName(:)
  INTEGER, ALLOCATABLE :: speciesNumber(:)

  !   DECLARATIONS FOR RATES OF PRODUCTION AND LOSS
  INTEGER, ALLOCATABLE :: prodIntSpecies(:,:), returnArray(:), SORNumber(:), reacIntSpecies(:,:)
  INTEGER speciesOutputRequiredSize, SORNumberSize, prodIntNameSize, returnArraySize, reacIntNameSize
  DOUBLE PRECISION, ALLOCATABLE :: yInt(:)
  CHARACTER (LEN=10), ALLOCATABLE :: prodIntName(:), reacIntName(:), speciesOutputRequired(:)
  INTEGER rateOfProdNS, prodLossArrayLen, rateOfLossNS, ratesOutputStepSize, time, elapsed
  INTEGER, ALLOCATABLE :: prodArrayLen(:), lossArrayLen(:)

  !    DECLARATIONS FOR CULUMATIVE RATES OF PRODUCTION AND LOSS
  INTEGER, ALLOCATABLE :: culmSpeciesProduced(:), culmReactionNumber(:)
  DOUBLE PRECISION, ALLOCATABLE :: culmRates(:)
  INTEGER, ALLOCATABLE :: culmSpeciesLoss(:), culmReactionNumberLoss(:)
  DOUBLE PRECISION, ALLOCATABLE :: culmRatesLoss(:)

  !   DECLARATIONS FOR CHEMICAL SPECIES CONSTRAINTS
  DOUBLE PRECISION, ALLOCATABLE :: z(:)
  DOUBLE PRECISION solverParameters(100), modelParameters(100)

  DOUBLE PRECISION :: modelStartTime

  !   DECLARATIONS FOR JACOBIAN PRODUCTION
  DOUBLE PRECISION, ALLOCATABLE :: fy(:,:)
  INTEGER :: jacobianOutputStepSize

  CHARACTER (LEN=30) :: photoRateNamesForHeader(200)
  CHARACTER (LEN=400) :: fmt

  !   DECLARATIONS FOR IR OUTPUT
  CHARACTER (LEN=21) :: irfileLocationPrefix
  CHARACTER (LEN=57) :: irfileLocation
  INTEGER :: irOutStepSize
  CHARACTER (LEN=30) :: strTime

  !    MISC
  CHARACTER (LEN=40) :: solverTypeName(3)
  CHARACTER (LEN=40) :: InterpolationMethodName(4)
  solverTypeName(1) = 'SPGMR'
  solverTypeName(2) = 'SPGMR + Banded Preconditioner'
  solverTypeName(3) = 'Dense'
  InterpolationMethodName(1) = 'cubic spline'
  InterpolationMethodName(2) = 'cubic spline ln'
  InterpolationMethodName(3) = 'piecewise constant'
  InterpolationMethodName(4) = 'piecewise linear'

  !    ********************************************************************************************************
  !    MODEL SETUP AND CONFIGURATION
  !    ********************************************************************************************************

  CALL SYSTEM_CLOCK (runStart)
  previousSeconds = 0

  !   OPEN FILES FOR OUTPUT
  OPEN (unit=91, file="modelOutput/concentration.output")
  OPEN (unit=86, file="modelOutput/photolysisRates.output")
  OPEN (unit=49, file="modelOutput/photoRateCalcParameters.output")
  OPEN (unit=23, file="modelOutput/mainSolverParameters.output")
  OPEN (unit=21, file="modelOutput/sparseSolverParameters.output")
  OPEN (unit=22, file="modelOutput/stepSize.output")
  OPEN (unit=99, file="modelOutput/initialConditionsSetting.output")
  OPEN (unit=89, file="modelOutput/productionRates.output")
  OPEN (unit=90, file="modelOutput/lossRates.output")
  OPEN (unit=92, file="modelOutput/finalModelState.output")
  OPEN (unit=93, file="modelOutput/jacobian.output")
  OPEN (unit=94, file="modelOutput/errors.output")
  OPEN (unit=95, file="modelOutput/envVar.output")
  flush(6)

  !    OPEN FILES FOR INPUT
  OPEN (5, file='modelConfiguration/mechanism.reac', status='old') ! input file

  !   READ IN MECHANISM PARAMETERS
  READ (5,*) np, numReactions
  WRITE (*,*)
  WRITE (*,*) 'Number of Species', np
  WRITE (*,*) 'Number of Reactions = ', numReactions
  WRITE (*,*)
  CLOSE (5, status='keep')

  prodLossArrayLen = numReactions*2

  !    SET ARRAY SIZES = NO. OF SPECIES
  ALLOCATE (y(np), speciesName(np), concSpeciesName(np*2), speciesNumber(np), z(np), concentration(np*2))
  ALLOCATE (prodIntSpecies(np, prodLossArrayLen), returnArray(np), reacIntSpecies(np, prodLossArrayLen))
  ALLOCATE (SORNumber(np), yInt(np), prodIntName(np), reacIntName(np), speciesOutputRequired(np))
  ALLOCATE (fy(np, np))
  !    SET ARRAY SIZES = NO. OF REACTIONS
  ALLOCATE (culmSpeciesProduced(numReactions), culmReactionNumber(numReactions), culmRates(numReactions))
  ALLOCATE (culmSpeciesLoss(numReactions), culmReactionNumberLoss(numReactions), culmRatesLoss(numReactions))
  ALLOCATE (lossRates(numReactions), productionRates(numReactions), ir(numReactions))

  !   GET SIZES OF REACTANTS AND PRODUCT INPUT FILES
  CALL getReactionListSizes (csize1, csize2)

  ALLOCATE (clhs(3, csize1), crhs(2, csize2), ccoeff(csize2))

  !   READ IN CHEMICAL REACTIONS
  CALL data (clhs, crhs, ccoeff, csize1, csize2)
  neq = np

  WRITE (*,*) 'Size of lhs =', csize1, 'size of rhs2 = ', csize2, '.'

  !   READ SPECIES NAMES AND NUMBERS
  WRITE (*,*)
  WRITE (*,*) 'Reading species names from mechanism.species...'
  CALL readSpecies (y, neq, speciesName, speciesNumber)
  WRITE (*,*) 'Finished reading species names.'
  WRITE (*,*)

  !   SET PARAMETERS FOR SPECIES OBJECT
  CALL setNumberOfSpecies (neq)
  CALL setSpeciesList (speciesName)

  !   SET INITIAL SPECIES CONCENTRATIONS
  CALL readConcentrations (concSpeciesName, concentration, concCounter, np)
  CALL setConcentrations (y, speciesName, concSpeciesName, concentration, concCounter, neq)
  WRITE (*,*)

  !   READ IN PHOTOLYSIS RATE INFORMATION
  CALL readPhotoloysisConstants (ck, cl, cmm, cnn, photoRateNames, transmissionFactor)
  WRITE (*,*)
  !   Set default value for photonames array
  DO i = 1, 200
     photoRateNamesForHeader(i) = 'na'
  ENDDO

  DO i = 1, nrOfPhotoRates
     photoRateNamesForHeader(ck(i)) = photoRateNames(i)
  ENDDO

  !   READ IN JFAC SPECIES
  CALL readJFacSpecies ()
  WRITE (*,*)

  !   CONFIGURE FOR OUTPUT OF PRODUCTION RATES
  CALL readProductsOfInterest (prodIntName, prodIntNameSize)

  CALL matchNameToNumber (speciesName, prodIntName, prodIntNameSize, neq, returnArray, returnArraySize)

  rateOfProdNS = returnArraySize - 1
  ALLOCATE (prodArrayLen(rateOfProdNS))

  DO i = 1, rateOfProdNS
     prodIntSpecies(i, 1) = returnArray(i)
  ENDDO

  CALL findReactionsWithProductX (prodIntSpecies, crhs, csize2, rateOfProdNS, prodArrayLen, prodLossArrayLen, np)
  WRITE (*,*) 'rateOfProdNS (number of species found):', rateOfProdNS
  WRITE (*,*)

  !   CONFIGURE FOR OUTPUT OF LOSS RATES
  CALL readReactantsOfInterest (reacIntName, reacIntNameSize)
  CALL matchNameToNumber (speciesName, reacIntName, reacIntNameSize, neq, returnArray, returnArraySize)

  rateOfLossNS = returnArraySize - 1
  WRITE (*,*) 'lossOfProdNS (number of species found):', rateOfLossNS
  WRITE (*,*)

  ALLOCATE (lossArrayLen(rateOfLossNS))

  DO i = 1, rateOfLossNS
     reacIntSpecies(i, 1) = returnArray(i)
  ENDDO

  CALL findReactionsWithReactant (reacIntSpecies, clhs, csize1, rateOfLossNS, lossArrayLen, prodLossArrayLen, np)

  DO i = 1, numReactions
     culmReactionNumberLoss(i) = reacIntSpecies(1, i)
     culmRates(i) = 0
  ENDDO


  !    READ IN SOLVER PARAMETERS
  WRITE (*,*) 'Reading solver parameters from file...'
  CALL getParametersFromFile ("modelConfiguration/solver.parameters", solverParameters)
  WRITE (*,*) 'Finished reading solver parameters from file.'

  !   READ IN MODEL PARAMETERS
  WRITE (*,*) 'Reading model parameters from file...'
  CALL getParametersFromFile ("modelConfiguration/model.parameters", modelParameters)
  WRITE (*,*) 'Finished eading model parameters from file.'
  WRITE (*,*)

  !   SET SOLVER PARAMETERS
  atol = solverParameters(1)
  rtol = solverParameters(2)
  JvApprox = solverParameters(3)
  deltaJv = solverParameters(4)
  deltaMain = solverParameters(5)
  lookBack = solverParameters(6)
  max_step = solverParameters(7)
  solverType = solverParameters(8)
  preconBandUpper = solverParameters(9)
  preconBandLower = solverParameters(10)

  WRITE (*,*) 'Solver parameters:'
  WRITE (*,*) 'atol: ', atol, 'rtol:', rtol, 'JacVApprox:', JvApprox, 'deltaJv:', deltaJv
  WRITE (*,*) 'deltaMain:', deltaMain, 'lookBack:', lookBack, 'max_step:', max_step, &
       'precondBandUpper:', preconBandUpper, 'preconBandLower', preconBandLower
  WRITE (*,*) 'solverType:', solverTypeName(solverType)
  WRITE (*,*)

  !   SET MODEL PARAMETERS
  nout = modelParameters(1)
  tout = modelParameters(2)
  SpeciesIntMethod = modelParameters(3)
  CALL setSpeciesIntMethod (SpeciesIntMethod)
  conditionsIntMethod = modelParameters(4)
  CALL setConditionIntMethod (conditionsIntMethod)
  decIntMethod = modelParameters(5)
  CALL setDecIntMethod (decIntMethod)
  maxNumberOfDataPoints = modelParameters(6)
  numberOfConstrainedSpecies = modelParameters(7)
  ratesOutputStepSize = modelParameters(8)
  modelStartTime = modelParameters(9)
  jacobianOutputStepSize = modelParameters(10)
  latitude = modelParameters(11)
  longitude = modelParameters(12)
  day = modelParameters(13)
  month = modelParameters(14)
  year = modelParameters(15)
  irOutStepSize = modelParameters(16)

  WRITE (*,*) 'Model parameters:'
  WRITE (*,*) 'number of steps: ', nout, 'step size(seconds):', tout
  WRITE (*,*) 'species interpolation method: ', InterpolationMethodName(SpeciesIntMethod)
  WRITE (*,*) 'conditions interpolation method: ', InterpolationMethodName(conditionsIntMethod)
  WRITE (*,*) 'dec interpolation method: ', InterpolationMethodName(decIntMethod)
  WRITE (*,*) 'maximum number of data points in constraint file:', maxNumberOfDataPoints
  WRITE (*,*) 'maximum number of constrained species:', numberOfConstrainedSpecies
  WRITE (*,*) 'ratesOutputStepSize', ratesOutputStepSize, &
       'instantaneous rates output step size:', irOutStepSize, &
       'modelStartTime:', modelStartTime, 'jacobianOutputStepSize:', jacobianOutputStepSize
  WRITE (*,*) 'latitude:', latitude, 'longitude:', longitude, &
       'day/month/year:', day, month, year
  WRITE (*,*)

  CALL calcDateParameters ()

  !   HARD CODED SOLVER PARAMETERS
  t0 = modelStartTime
  tout = tout + t0
  outputStepSize = tout - t0
  t = t0
  meth = 2
  itmeth = 2
  itol = 1
  itask = 1
  jout = 0

  !   READ IN ENVIRONEMENTAL VARIABLES (FIXED VALUES, CONSTRAIEND, CALCUALTED OR NOT USED, SEE ENVVAR.CONFIG)
  CALL readEnvVar (maxNumberOfDataPoints)
  WRITE (*,*)

  !   WRITE FILE OUTPUT HEADERS AND OUTPUT AT t=0
  CALL writeFileHeaders (photoRateNamesForHeader)

  !   CONCENTRATION OUTPUT
  CALL readSpeciesOutputRequired (speciesOutputRequired, speciesOutputRequiredSize, np)
  CALL matchNameToNumber (speciesName, speciesOutputRequired, speciesOutputRequiredSize, neq, SORNumber, SORNumberSize)
  CALL getConcForSpecInt (y, yInt, SORNumber, SORNumberSize, neq)
  CALL outputInterestingNames (speciesOutputRequired, speciesOutputRequiredSize)
  SORNumberSize = SORNumberSize -1

  WRITE (*,*) 'Output required for concentration of', speciesOutputRequiredSize, 'species:'
  IF (speciesOutputRequiredSize>2) THEN
     WRITE (*,*) 1, speciesOutputRequired(1)
     WRITE (*,*) '...'
     WRITE (*,*) speciesOutputRequiredSize, speciesOutputRequired(speciesOutputRequiredSize)
  ELSE
     DO i = 1, speciesOutputRequiredSize
        WRITE (*,*) i, speciesOutputRequired(i)
     ENDDO
  ENDIF

  flush(6)
  !    ********************************************************************************************************
  !    CONSTRAINTS
  !    ********************************************************************************************************

  WRITE (*,*)
  CALL readPhotoRates (maxNumberOfDataPoints)
  WRITE (*,*)

  CALL readSpeciesConstraints (speciesName, neq, y, t)

  WRITE (*,*)
  !test
  CALL getConcForSpecInt (y, yInt, SORNumber, SORNumberSize, neq+numberOfConstrainedSpecies)
  CALL outputInteresting (t, yInt, SORNumberSize)

  CALL removeConstrainedSpeciesFromProbSpec (y, z, numberOfConstrainedSpecies, constrainedSpecies, neq)

  !   ADJUST PROBLEM SPECIFICATION TO GIVE NUMBER OF SPECIES TO BE SOLVED FOR (N - C = M)
  neq = neq - numberOfConstrainedSpecies
  WRITE (*,*) 'neq = ', neq, ' numberOfConstrainedSpecies = ', numberOfConstrainedSpecies

  flush(6)
  !    ********************************************************************************************************
  !    CONFIGURE SOLVER
  !    ********************************************************************************************************

  WRITE (6,*) 'Dense example problem:'
  WRITE (6,*) ' Robertson kinetics, neq = ', neq

  ipar(1) = neq
  ipar(2) = numReactions

  CALL fnvinits (1, neq, ier)
  IF (ier/=0) THEN
     WRITE (6, 20) ier
20   FORMAT (///' SUNDIALS_ERROR: FNVINITS returned ier = ', I5)
     STOP
  ENDIF

  WRITE (*,*) 't0 = ', t0
  CALL fcvmalloc (t0, z, meth, itmeth, itol, rtol, atol, &
       iout, rout, ipar, rpar, ier)
  IF (ier/=0) THEN
     WRITE (6, 30) ier
30   FORMAT (///' SUNDIALS_ERROR: FCVMALLOC returned ier = ', I5)
     STOP
  ENDIF

  numsteps = 100000
  CALL fcvsetiin ('MAX_NSTEPS', numsteps, ier)
  WRITE (*,*) 'setting maxsteps ier = ', ier

  CALL fcvsetrin ('MAX_STEP', max_step, ier)

  !   SELECT SOLVER TYPE ACCORDING TO FILE INPUT
  !   SPGMR SOLVER
  IF (solverType==1) THEN
     CALL fcvspgmr (0, 1, lookBack, deltaMain, ier)
     ! SPGMR SOLVER WITH BANDED PRECONDITIONER
  ELSE IF (solverType==2) THEN
     CALL fcvspgmr (1, 1, lookBack, deltaMain, ier)
     CALL fcvbpinit (neq, preconBandUpper, preconBandLower, ier)
     IF (ier/=0 ) THEN
        WRITE (6,*) 'SUNDIALS_ERROR: preconditioner returned ier = ', ier ;
        CALL fcvfree
        STOP
     END IF
     ! DENSE SOLVER
  ELSE IF (solverType==3) THEN
     ! make sure no Jacobian approximation is required
     IF (JVapprox==1) THEN
        WRITE (6,*) 'Solver parameter conflict! Jv approximation cannot be used for dense solver.'
        WRITE (6,*) 'Fix parameters in "modelConfiguration/solver.parameters" file.'
        STOP
     END IF
     CALL fcvdense (neq, ier)
     ! UNEXPECTED SOLVER TYPE
  ELSE
     WRITE (*,*) 'error with solverType input, error = ', solverType
  ENDIF
  ! ERROR HANDLING
  IF (ier/=0) THEN
     WRITE (6,*) ' SUNDIALS_ERROR: SOLVER returned ier = ', ier
     CALL fcvfree
     STOP
  ENDIF

  !    USE JACOBIAN APPROXIMATION IF REQUIRED
  IF (JVapprox==1) THEN
     CALL fcvspilssetjac (1, ier)
  ENDIF

  IF (ier/=0) THEN
     WRITE (6, 40) ier
40   FORMAT (///' SUNDIALS_ERROR: FCVDENSE returned ier = ', I5)
     CALL fcvfree
     STOP
  ENDIF

  ! check JFac data consistency:
  CALL test_jfac ()

  !    ********************************************************************************************************
  !    RUN MODEL
  !    ********************************************************************************************************

  DO WHILE (jout<nout)
     ! GET CONCENTRATIONS FOR SOLVED SPECIES
     WRITE (49,*) t, secx, cosx, lat, longt, lha, sinld, cosld
     CALL fcvode (tout, t, z, itask, ier)
     IF (ier/=0) THEN
        WRITE (*,*) 'ier POST FCVODE = ', ier
     ENDIF
     flush(6)
     ! GET CONCENTRATIONS FOR CONSTRAINED SPECIES AND ADD TO ARRAY FOR OUTPUT
     DO i = 1, numberOfConstrainedSpecies
        CALL getConstrainedConc (i, d)
        constrainedConcs(i) = d
     ENDDO

     CALL addConstrainedSpeciesToProbSpec (z, y, numberOfConstrainedSpecies, constrainedSpecies, neq, constrainedConcs)

     ! OUTPUT ON SCREEN
     fmt = "('At t = ', E12.4, '   y = ', 3E14.6) "
     ! printing concentration of two first species - seems unnecessary at the moment
     ! WRITE (6, fmt) T, Y (1), Y (2)

     ! OUTPUT RATES OF PRODIUCTION ON LOSS (OUTPUT FREQUENCY SET IN MODEL.PARAMETERS)
     time = INT (t)
     elapsed = INT (t-modelStartTime)
     IF (MOD (elapsed, ratesOutputStepSize)==0) THEN
        CALL outputRates (prodIntSpecies, t, productionRates, 1, np, rateOfProdNS, prodLossArrayLen, rateOfLossNS, prodArrayLen, &
             lossArrayLen , speciesName)
        CALL outputRates (reacIntSpecies, t, lossRates, 0, np, rateOfProdNS, prodLossArrayLen, rateOfProdNS, prodArrayLen, &
             lossArrayLen, speciesName)
     ENDIF

     ! OUTPUT JACOBIAN MATRIX (OUTPUT FREQUENCY SET IN MODEL PARAMETERS)
     WRITE (*,*) 'time = ', time
     IF (MOD (elapsed, jacobianOutputStepSize)==0) THEN
        CALL jfy (np, numReactions, y, fy, t)
        CALL outputjfy (fy, np, t)
     ENDIF

     CALL getConcForSpecInt (y, yInt, SORNumber, SORNumberSize, neq+numberOfConstrainedSpecies)
     CALL outputInteresting (t, yInt, SORNumberSize)
     CALL outputPhotolysisRates (j, t)


     !OUTPUT INSTANTANEOUS RATES
     IF (MOD (elapsed, irOutStepSize)==0) THEN
        WRITE (strTime,*) time

        irfileLocationPrefix = './instantaneousRates/'
        irfileLocation = irfileLocationPrefix // ADJUSTL (strTime)

        OPEN (27, file=irfileLocation)
        DO i = 1, numReactions
           WRITE (27,*) ir(i)
        ENDDO
        CLOSE (27, status='keep')
     ENDIF

     ! OUTPUT FOR CVODE MAIN SOLVER
     WRITE (23,*) t, ' ', iout (lnst), ' ', iout (lnfe), ' ', iout (lnetf)
     ! OUTPUT FOR SPARSE SOLVER
     WRITE (21,*) t, ' ', iout (nfels), ' ', iout (njtv), ' ', iout (npe), ' ', iout (nps)
     ! OUTPUT STEP SIZE
     WRITE (22,*) t, ' ', rout (3), ' ', rout (2)

     !OUTPUT ENVVAR VALUES
     CALL ro2sum (ro2, y)
     CALL outputEnvVar (t)

     ! CALCULATE AND OUTPUT RUNTIME
     ! not using timing at the moment
     ! call system_clock(current, rate)
     ! currentSeconds =(current - runStart) / rate
     ! stepTime = currentSeconds - previousSeconds
     ! write(*,*) 'Current time = ', currentSeconds, '        step time = ', stepTime
     ! previousSeconds = currentSeconds

     ! ERROR HANDLING
     IF (ier<0) THEN
        fmt = "(///' SUNDIALS_ERROR: FCVODE returned ier = ', I5, /, '                 Linear Solver returned ier = ', I5) "
        WRITE (6, fmt) ier, iout (15)
        ! free memory
        CALL fcvfree
        STOP
     ENDIF

     IF (ier==0) THEN
        tminus1 = t
        tout = tout + outputStepSize
        jout = jout + 1
     ENDIF

  ENDDO

  CALL fcvdky (t, 1, z, ier)
  IF (ier/=0) THEN
     fmt = "(///' SUNDIALS_ERROR: FCVDKY returned ier = ', I4) "
     WRITE (6, fmt) ier
     CALL fcvfree
     STOP
  ENDIF

  !   OUPUT FINAL MODEL CONCENTRATIONS FOR MODEL RESTART
  DO i = 1, np
     WRITE (92,*) speciesName(i), y(i)
  ENDDO

  !   printing of final statistics desactivated - nobody finds it useful
  !   FINAL ON SCREEN OUTPUT
  fmt = "(//'Final statistics:'//" // &
       "' No. steps = ', I4, '   No. f-s = ', I4," // &
       "'   No. J-s = ', I4, '   No. LU-s = ', I4/" // &
       "' No. nonlinear iterations = ', I4/" // &
       "' No. nonlinear convergence failures = ', I4/" // &
       "' No. error test failures = ', I4/) "

  WRITE (6, fmt) iout (lnst), iout (LNFE), iout (lnje), iout (lnsetup), &
       iout (lnni), iout (lncf), iout (lnetf)

  CALL SYSTEM_CLOCK (runEnd, rate)
  runTime =(runEnd - runStart) / rate
  WRITE (*,*) 'Runtime = ', runTime

  !   deallocate all
  WRITE (*,*) 'Deallocating memory.'
  !   deallocate CVODE internal data

  CALL fcvfree
  DEALLOCATE (y, speciesName, concSpeciesName, speciesNumber, z, concentration)
  DEALLOCATE (prodIntSpecies, returnArray, reacIntSpecies)
  DEALLOCATE (SORNumber, yInt, prodIntName, reacIntName, speciesOutputRequired)
  DEALLOCATE (fy)
  DEALLOCATE (culmSpeciesProduced, culmReactionNumber, culmRates, ir)
  DEALLOCATE (culmSpeciesLoss, culmReactionNumberLoss, culmRatesLoss)
  DEALLOCATE (lossRates, productionRates)
  DEALLOCATE (clhs, crhs, ccoeff)
  DEALLOCATE (prodArrayLen)
  DEALLOCATE (lossArrayLen)
  !   deallocate data allocated before in input functions(inputFunctions.f)
  !   deallocate arrays from module constraints
  CALL deallocateConstrainedSpecies ()
  !   deallocate arrays from module species
  CALL deallocateSpeciesList
  !   deallocate arrays from module chemcialConstraints
  DEALLOCATE (dataX, dataY, dataY2, dataFixedY, constrainedConcs, constrainedName)
  DEALLOCATE (speciesNumberOfPoints, constrainedSpecies)
  !   deallocate arrays from module envVars
  DEALLOCATE (envVarTypesNum, envVarNames, envVarTypes, envVarFixedValues)
  DEALLOCATE (envVarX, envVarY, envVarY2, envVarNumberOfPoints)
  !   deallocate arrays from module photolysisRates
  DEALLOCATE (photoX, photoY, photoY2, photoNumberOfPoints)

  STOP
END PROGRAM ATCHEM
