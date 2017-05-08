!     ----------------------------------------------------------------
!     MAIN PROGRAM FOR THE ATMOSPHERE CHEMISTRY PROJECT
!     ----------------------------------------------------------------

PROGRAM ATCHEM

  USE, INTRINSIC :: iso_fortran_env, ONLY : stderr=>error_unit
  USE types_mod
  USE species
  USE constraints
  USE interpolationMethod
  USE reactionStructure
  USE photolysisRates
  USE chemicalConstraints
  USE zenithData
  USE zenithData1
  USE productionAndLossRates
  USE envVars
  USE SZACalcVars
  USE date
  USE directories, ONLY : output_dir, instantaneousRates_dir, param_dir, spec_constraints_dir, env_constraints_dir
  USE storage, ONLY : maxSpecLength, maxPhotoRateNameLength
  USE inputFunctions_mod
  USE configFunctions_mod
  USE instantaneousRatesFunctions_mod
  USE outputFunctions_mod
  USE constraintFunctions_mod
  IMPLICIT NONE

  !    ********************************************************************************************************
  !    DECLARATIONS
  !    ********************************************************************************************************

  !   DECLARATIONS FOR SOLVER PARAMETERS
  INTEGER(kind=QI) :: ier
  INTEGER :: i
  INTEGER(kind=NPI) :: species_counter
  INTEGER lnst, lnfe, lnsetup, lnni, lncf, lnetf, lnje
  INTEGER nfels, njtv, npe, nps
  INTEGER meth, itmeth, iatol, itask, currentNumTimestep, maxNumTimesteps
  INTEGER(kind=NPI) :: iout (21), ipar (10)
  INTEGER(kind=NPI) :: neq
  DOUBLE PRECISION rtol, t, t0, tout
  DOUBLE PRECISION atol, rout (6)
  DOUBLE PRECISION :: rpar (1)
  DATA lnst/3/, lnfe/4/, lnetf/5/, lncf/6/, lnni/7/, lnsetup/8/, &
       lnje/17/, nfels/16/, njtv/17/ , npe/18/, nps/19/
  DOUBLE PRECISION, ALLOCATABLE :: speciesConcs(:)

  !   DECLARATIONS FOR CONFIGURABLE SOLVER PARAMETERS
  DOUBLE PRECISION :: deltaJv, deltaMain, maxStep
  INTEGER :: JvApprox, lookBack
  INTEGER(kind=SI) :: speciesInterpolationMethod, conditionsInterpolationMethod, decInterpolationMethod
  INTEGER :: preconBandUpper, preconBandLower, solverType
  DOUBLE PRECISION :: d

  !   DECLARATIONS FOR TIME PARAMETERS
  INTEGER(kind=QI) :: runStart, runEnd, runTime, rate, previousSeconds
  INTEGER :: numSteps
  INTEGER(kind=NPI) :: numSpec, numReactions
  DOUBLE PRECISION tminus1, timestepSize

  !   DECLARATIONS FOR SPECIES PARAMETERS
  DOUBLE PRECISION, ALLOCATABLE :: initialConcentrations(:)
  CHARACTER(LEN=maxSpecLength), ALLOCATABLE :: speciesName(:), concSpeciesName(:)
  INTEGER(kind=NPI), ALLOCATABLE :: speciesNumber(:)

  !   DECLARATIONS FOR RATES OF PRODUCTION AND LOSS
  INTEGER(kind=NPI), ALLOCATABLE :: returnArray(:), tempSORNumber(:), SORNumber(:)
  INTEGER(kind=NPI), ALLOCATABLE :: prodIntSpecies(:,:), reacIntSpecies(:,:), prodArrayLen(:), lossArrayLen(:)
  INTEGER(kind=NPI) :: speciesOutputRequiredSize, SORNumberSize, prodIntNameSize, reacIntNameSize
  DOUBLE PRECISION, ALLOCATABLE :: concsOfSpeciesOfInterest(:)
  CHARACTER(LEN=maxSpecLength), ALLOCATABLE :: prodIntName(:), reacIntName(:)
  CHARACTER(LEN=maxSpecLength), ALLOCATABLE :: speciesOutputRequired(:)
  INTEGER(kind=NPI) :: rateOfProdNS, rateOfLossNS
  INTEGER :: ratesOutputStepSize, time, elapsed

  !   DECLARATIONS FOR CHEMICAL SPECIES CONSTRAINTS
  DOUBLE PRECISION, ALLOCATABLE :: z(:)
  DOUBLE PRECISION, ALLOCATABLE :: tempForSolverParameters(:), tempForModelParameters(:)
  DOUBLE PRECISION, ALLOCATABLE :: solverParameters(:), modelParameters(:)
  INTEGER(kind=DI) :: modelParameterSize, solverParameterSize

  DOUBLE PRECISION :: modelStartTime

  !   DECLARATIONS FOR JACOBIAN PRODUCTION
  DOUBLE PRECISION, ALLOCATABLE :: fy(:,:)
  INTEGER :: jacobianOutputStepSize

  CHARACTER(LEN=maxPhotoRateNameLength) :: photoRateNamesForHeader(200)
  CHARACTER(LEN=400) :: fmt

  !   DECLARATIONS FOR IR OUTPUT
  INTEGER :: irOutStepSize

  INTEGER(kind=QI) :: cmd_arg_count
  !    MISC
  CHARACTER(LEN=30) :: solverTypeName(3)
  CHARACTER(LEN=20) :: interpolationMethodName(4)
  solverTypeName(1) = 'SPGMR'
  solverTypeName(2) = 'SPGMR + Banded Preconditioner'
  solverTypeName(3) = 'Dense'
  interpolationMethodName(1) = 'cubic spline'
  interpolationMethodName(2) = 'cubic spline ln'
  interpolationMethodName(3) = 'piecewise constant'
  interpolationMethodName(4) = 'piecewise linear'

  !    ********************************************************************************************************
  !    MODEL SETUP AND CONFIGURATION
  !    ********************************************************************************************************

  CALL SYSTEM_CLOCK (runStart)
  previousSeconds = 0

  ! Read in command line argument to direct output files to a given directory
  cmd_arg_count = command_argument_count()
  IF (cmd_arg_count>0) THEN
     CALL get_command_argument(1, output_dir)
  ELSE
     output_dir = "modelOutput"
  ENDIF
  IF (cmd_arg_count>1) THEN
     CALL get_command_argument(2, instantaneousRates_dir)
  ELSE
     instantaneousRates_dir = "instantaneousRates"
  ENDIF
  IF (cmd_arg_count>2) THEN
     CALL get_command_argument(3, param_dir)
  ELSE
     param_dir = "modelConfiguration"
  ENDIF
  IF (cmd_arg_count>3) THEN
     CALL get_command_argument(4, spec_constraints_dir)
  ELSE
     spec_constraints_dir = "speciesConstraints"
  ENDIF
  IF (cmd_arg_count>4) THEN
     CALL get_command_argument(5, env_constraints_dir)
  ELSE
     env_constraints_dir = "environmentConstraints"
  ENDIF

  WRITE (*,*) 'Output dir is ', output_dir
  WRITE (*,*) 'Instantaneous rates dir is ', instantaneousRates_dir
  WRITE (*,*) 'Parameter dir is ', param_dir
  !   OPEN FILES FOR OUTPUT
  OPEN (unit=50, file=trim(output_dir) // "/concentration.output")
  OPEN (unit=51, file=trim(output_dir) // "/errors.output")
  OPEN (unit=52, file=trim(output_dir) // "/envVar.output")
  OPEN (unit=53, file=trim(output_dir) // "/finalModelState.output")
  OPEN (unit=54, file=trim(output_dir) // "/initialConditionsSetting.output")
  OPEN (unit=55, file=trim(output_dir) // "/jacobian.output")
  OPEN (unit=56, file=trim(output_dir) // "/lossRates.output")
  OPEN (unit=57, file=trim(output_dir) // "/mainSolverParameters.output")
  OPEN (unit=58, file=trim(output_dir) // "/photolysisRates.output")
  OPEN (unit=59, file=trim(output_dir) // "/photoRateCalcParameters.output")
  OPEN (unit=60, file=trim(output_dir) // "/productionRates.output")
  OPEN (unit=61, file=trim(output_dir) // "/sparseSolverParameters.output")
  OPEN (unit=62, file=trim(output_dir) // "/stepSize.output")
  flush(6)

  !    READ IN MECHANISM PARAMETERS
  OPEN (10, file='modelConfiguration/mechanism.reac', status='old') ! input file
  READ (10,*) numSpec, numReactions
  CLOSE (10, status='keep')

  WRITE (*,*)
  WRITE (*,*) 'Number of Species = ', numSpec
  WRITE (*,*) 'Number of Reactions = ', numReactions
  WRITE (*,*)

  !    SET ARRAY SIZES = NO. OF SPECIES
  ALLOCATE (speciesConcs(numSpec), speciesName(numSpec))
  ALLOCATE (speciesNumber(numSpec), z(numSpec), initialConcentrations(numSpec*2))
  ALLOCATE (returnArray(numSpec))
  ALLOCATE (tempSORNumber(numSpec))
  ALLOCATE (fy(numSpec, numSpec))
  !    SET ARRAY SIZES = NO. OF REACTIONS
  ALLOCATE (lossRates(numReactions), productionRates(numReactions), ir(numReactions))

  !   GET SIZES OF REACTANTS AND PRODUCT INPUT FILES
  CALL getReactionListSizes (csize1, csize2)

  ALLOCATE (clhs(3, csize1), crhs(2, csize2), ccoeff(csize2))

  !   READ IN CHEMICAL REACTIONS
  CALL readReactions (clhs, crhs, ccoeff)
  neq = numSpec

  WRITE (*,*) 'Size of lhs =', csize1, 'size of rhs2 = ', csize2, '.'

  !   READ SPECIES NAMES AND NUMBERS
  WRITE (*,*)
  WRITE (*,*) 'Reading species names from mechanism.species...'
  CALL readSpecies (speciesConcs, numSpec, speciesName, speciesNumber)
  WRITE (*,*) 'Finished reading species names.'
  WRITE (*,*)

  !   SET PARAMETERS FOR SPECIES OBJECT
  CALL setNumberOfSpecies (numSpec)
  CALL setSpeciesList (speciesName)

  !   SET INITIAL SPECIES CONCENTRATIONS
  CALL readInitialConcentrations (concSpeciesName, initialConcentrations)
  CALL setConcentrations (speciesName, concSpeciesName, initialConcentrations, speciesConcs)
  WRITE (*,*)

  !   READ IN PHOTOLYSIS RATE INFORMATION
  CALL readPhotolysisConstants (ck, cl, cmm, cnn, photoRateNames, transmissionFactor)
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
  WRITE (*,*) 'Reading products of interest...'
  CALL readProductsOReactantsOfInterest( trim( param_dir ) // '/productionRatesOutput.config', prodIntName, prodIntNameSize )
  WRITE (*,*) 'Finished reading products of interest.'
  CALL matchNameToNumber (speciesName, prodIntName, returnArray, rateOfProdNS)
  ! prodArrayLen will hold the length of each line of prodIntSpecies
  ALLOCATE (prodArrayLen(rateOfProdNS))
  ALLOCATE (prodIntSpecies(rateOfProdNS, csize2))
  DO species_counter = 1, rateOfProdNS
     prodIntSpecies(species_counter, 1) = returnArray(species_counter)
  ENDDO
  CALL findReactionsWithProductOrReactant (prodIntSpecies, crhs, 2, csize2, rateOfProdNS, prodArrayLen)
  WRITE (*,*) 'rateOfProdNS (number of species found):', rateOfProdNS
  WRITE (*,*)

  !   CONFIGURE FOR OUTPUT OF LOSS RATES
  WRITE (*,*) 'Reading reactants of interest...'
  CALL readProductsOReactantsOfInterest( trim( param_dir ) // '/lossRatesOutput.config', reacIntName, reacIntNameSize )
  WRITE (*,*) 'Finished reading reactants of interest.'

  CALL matchNameToNumber (speciesName, reacIntName, returnArray, rateOfLossNS)
  ! lossArrayLen will hold the length of each line of reacIntSpecies
  ALLOCATE (lossArrayLen(rateOfLossNS))
  ALLOCATE (reacIntSpecies(rateOfLossNS, csize1))
  DO species_counter = 1, rateOfLossNS
     reacIntSpecies(species_counter, 1) = returnArray(species_counter)
  ENDDO
  CALL findReactionsWithProductOrReactant (reacIntSpecies, clhs, 3, csize1, rateOfLossNS, lossArrayLen)
  WRITE (*,*) 'rateOfLossNS (number of species found):', rateOfLossNS
  WRITE (*,*)


  !    READ IN SOLVER PARAMETERS
  ALLOCATE(tempForSolverParameters(100))
  WRITE (*,*) 'Reading solver parameters from file...'
  CALL getParametersFromFile (trim(param_dir) // "/solver.parameters", tempForSolverParameters, solverParameterSize)
  WRITE (*,*) 'Finished reading solver parameters from file.'
  ALLOCATE (solverParameters(solverParameterSize))
  DO i = 1, solverParameterSize
     solverParameters(i) = tempForSolverParameters(i)
  ENDDO
  DEALLOCATE(tempForSolverParameters)
  !   READ IN MODEL PARAMETERS
  ALLOCATE(tempForModelParameters(100))
  WRITE (*,*) 'Reading model parameters from file...'
  CALL getParametersFromFile (trim(param_dir) //  "/model.parameters", tempForModelParameters, modelParameterSize)
  WRITE (*,*) 'Finished reading model parameters from file.'
  ALLOCATE (modelParameters(modelParameterSize))
  DO i = 1, modelParameterSize
     modelParameters(i) = tempForModelParameters(i)
  ENDDO
  DEALLOCATE(tempForModelParameters)
  WRITE (*,*)

  !   SET SOLVER PARAMETERS
  ! Used in FCVMALLOC: ATOL is the absolute tolerance (scalar or array).
  atol = solverParameters(1)
  ! Used in FCVMALLOC: RTOL is the relative tolerance (scalar).
  rtol = solverParameters(2)
  ! TODO: convert this to boolean?
  ! If JvApprox==1 and solverType={1,2}, call fcvspilssetjac() below, with non-zero flag.
  ! This means FCVJTIMES() in solverFunctions.f90 should be used to approximate the Jacobian.
  JvApprox = solverParameters(3)
  ! This is never used, but is referenced in a comment in FCVJTIMES.
  ! TODO: delete?
  deltaJv = solverParameters(4)
  ! From CVODE docs: DELT is the linear convergence tolerance factor of the SPGMR. Used in FCVSPGMR.
  deltaMain = solverParameters(5)
  ! From CVODE docs: MAXL is the maximum Krylov subspace dimension. Used in FCVSPGMR.
  ! TODO: Rename to MAXL?
  lookBack = solverParameters(6)
  ! From CVODE docs: Maximum absolute step size. Passed via FCVSETRIN.
  maxStep = solverParameters(7)
  ! USed to choose which solver to use:
  ! 1: SPGMR
  ! 2: SPGMR + Banded preconditioner
  ! 3: Dense solver
  ! otherwise: error
  solverType = solverParameters(8)
  ! From CVODE docs: MU (preconBandUpper) and ML (preconBandLower) are the upper
  ! and lower half- bandwidths of the band matrix that is retained as an
  ! approximation of the Jacobian.
  preconBandUpper = solverParameters(9)
  preconBandLower = solverParameters(10)

  ! float format
100 FORMAT (A17, E11.3)
  ! integer format
200 FORMAT (A17, I11)
  WRITE (*,*) 'Solver parameters:'
  WRITE (*,*) '------------------'
  WRITE (*,100) 'atol: ', atol
  WRITE (*,100) 'rtol: ', rtol
  WRITE (*,200) 'JacVApprox: ', JvApprox
  WRITE (*,100) 'deltaJv: ', deltaJv
  WRITE (*,100) 'deltaMain: ', deltaMain
  WRITE (*,200) 'lookBack: ', lookBack
  WRITE (*,100) 'maxStep: ', maxStep
  WRITE (*,200) 'preconBandUpper: ', preconBandUpper
  WRITE (*,200) 'preconBandLower: ', preconBandLower
  WRITE (*,'(A17, A29)') 'solverType: ', adjustl(solverTypeName(solverType))
  WRITE (*,*) '------------------'
  WRITE (*,*)

  !   SET MODEL PARAMETERS
  ! maxNumTimesteps sets the maximum number of timesteps to calculate.
  ! Calculation will terminate when currentNumTimestep>=maxNumTimesteps.
  maxNumTimesteps = modelParameters(1)
  ! Size of timestep: tout is incremented by this amount on each iteration of the main while loop.
  timestepSize = modelParameters(2)
  ! Use the local variable speciesInterpolationMethod to set the value speciesInterpMethod,
  ! the private member of MODULE interpolationMethod.
  ! getSpeciesInterpMethod() is called by getConstrainedQuantAtT2D.
  ! Values:
  ! 1: Cubic spline interpolation
  ! 2: Cubic spline interpolation (log) TODO: this just outputs the exponential of the value. Is this right?
  ! 3: Piecewise constant
  ! 4: Piecewise linear
  ! otherwise: error
  speciesInterpolationMethod = modelParameters(3)
  CALL setSpeciesInterpMethod (speciesInterpolationMethod)
  conditionsInterpolationMethod = modelParameters(4)
  CALL setConditionsInterpMethod (conditionsInterpolationMethod)
  decInterpolationMethod = modelParameters(5)
  CALL setDecInterpMethod (decInterpolationMethod)
  ! Member variable of MODULE constraints. Used in getConstrainedQuantAtT2D and readEnvVar
  maxNumberOfDataPoints = modelParameters(6)
  ! Member variable of chemicalConstraints.
  numberOfConstrainedSpecies = modelParameters(7)
  ! Frequency at which outputRates is called below.
  ratesOutputStepSize = modelParameters(8)
  ! Start time of model. Used to set t0, and to calculate the elapsed time.
  modelStartTime = modelParameters(9)
  ! Frequency at which outputjfy is called below.
  jacobianOutputStepSize = modelParameters(10)
  ! Member variables of module SZACalcVars
  latitude = modelParameters(11)
  longitude = modelParameters(12)
  ! Member variables of module date
  day = modelParameters(13)
  month = modelParameters(14)
  year = modelParameters(15)
  ! Frequency at which to output instantaneous rates
  irOutStepSize = modelParameters(16)

  ! float format
300 FORMAT (A52, E11.3)
  ! integer format
400 FORMAT (A52, I11)
  ! string format
500 FORMAT (A52, A17)
  WRITE (*,*) 'Model parameters:'
  WRITE (*,*) '-----------------'
  WRITE (*,400) 'number of steps: ', maxNumTimesteps
  WRITE (*,300) 'step size (seconds): ', timestepSize
  WRITE (*,500) 'species interpolation method: ', adjustl(interpolationMethodName(speciesInterpolationMethod))
  WRITE (*,500) 'conditions interpolation method: ', adjustl(interpolationMethodName(conditionsInterpolationMethod))
  WRITE (*,500) 'dec interpolation method: ', adjustl(interpolationMethodName(decInterpolationMethod))
  WRITE (*,400) 'maximum number of data points in constraint file: ', maxNumberOfDataPoints
  WRITE (*,400) 'maximum number of constrained species: ', numberOfConstrainedSpecies
  WRITE (*,400) 'ratesOutputStepSize: ', ratesOutputStepSize
  WRITE (*,400) 'instantaneous rates output step size: ', irOutStepSize
  WRITE (*,300) 'modelStartTime: ', modelStartTime
  WRITE (*,400) 'jacobianOutputStepSize: ', jacobianOutputStepSize
  WRITE (*,300) 'latitude: ', latitude
  WRITE (*,300) 'longitude: ', longitude
  WRITE (*,'(A52, I3, A, I2, A, I4)') 'day/month/year: ', day, '/', month, '/', year
  WRITE (*,*) '-----------------'
  WRITE (*,*)

  ! Set the members dayOfYear, dayAsFractionOfYear, secondsInYear of MODULE date to their value based on day, month, year
  CALL calcDateParameters ()

  !   HARD CODED SOLVER PARAMETERS
  t0 = modelStartTime
  tout = timestepSize
  tout = tout + t0
  t = t0
  ! Parameters for fcvmalloc. (Comments from cvode guide)
  ! meth specifies the basic integration: 1 for Adams (nonstiff) or 2 for BDF stiff)
  meth = 2
  ! itmeth specifies the nonlinear iteration method: 1 for functional iteration or 2 for Newton iteration.
  itmeth = 2
  ! IATOL specifies the type for absolute tolerance ATOL: 1 for scalar or 2 for array.
  ! If IATOL= 3, the arguments RTOL and ATOL are ignored and the user is
  ! expected to subsequently call FCVEWTSET and provide the function FCVEWT.
  iatol = 1

  ! Parameter for FCVODE. Comment from cvode guide: ITASK is a task indicator and should be
  ! set to 1 for normal mode (overshoot TOUT and interpolate),
  ! or to 2 for one-step mode (return after each internal step taken)
  itask = 1

  ! currentNumTimestep counts the number of iterative steps. Set to zero. Calculation will terminate when currentNumTimestep>=maxNumTimesteps.
  currentNumTimestep = 0

  ! Read in environment variables (FIXED, CONSTRAINED, CALC or NOTUSED, see environmentVariables.config)
  CALL readEnvVar ()
  WRITE (*,*)

  ! fill speciesOutputRequired with the names of species to output to concentration.output
  CALL readSpeciesOutputRequired (speciesOutputRequired, speciesOutputRequiredSize)

  ! fill SORNumber with the global numbering of the species found in speciesOutputRequired
  CALL matchNameToNumber (speciesName, speciesOutputRequired, &
                          tempSORNumber, SORNumberSize)
  ! Allocate SORNumber and fill from temporary array
  ALLOCATE (SORNumber(SORNumberSize), concsOfSpeciesOfInterest(SORNumberSize))
  DO species_counter = 1, SORNumberSize
     SORNumber(species_counter) = tempSORNumber(species_counter)
  ENDDO
  DEALLOCATE(tempSORNumber)
  ! fill concsOfSpeciesOfInterest with the concentrations of the species to be output
  CALL getConcForSpecInt (speciesConcs, SORNumber, concsOfSpeciesOfInterest)

  !   Write file output headers
  CALL writeFileHeaders (photoRateNamesForHeader, speciesOutputRequired, speciesOutputRequiredSize)

  flush(stderr)
  !    ********************************************************************************************************
  !    CONSTRAINTS
  !    ********************************************************************************************************

  WRITE (*,*)
  CALL readPhotoRates (maxNumberOfDataPoints)
  WRITE (*,*)

  CALL readSpeciesConstraints (speciesName, numSpec, speciesConcs, t)

  WRITE (*,*)
  !test
  ! TODO: Why does this not use neq, but neq+numberOfConstrainedSpecies?
  CALL getConcForSpecInt (speciesConcs, SORNumber, concsOfSpeciesOfInterest)
  CALL outputSpeciesOutputRequired (t, concsOfSpeciesOfInterest, SORNumberSize)

  ! This outputs z, which is y with all the constrained species removed.
  CALL removeConstrainedSpeciesFromProbSpec (speciesConcs, z, numberOfConstrainedSpecies, constrainedSpecies, numSpec)

  !   ADJUST PROBLEM SPECIFICATION TO GIVE NUMBER OF SPECIES TO BE SOLVED FOR (N - C = M)
  neq = numSpec - numberOfConstrainedSpecies
  WRITE (*,'(A30, I6)') 'neq = ', neq
  WRITE (*,'(A30, I6)') 'numberOfConstrainedSpecies = ', numberOfConstrainedSpecies

  flush(stderr)

  !    ********************************************************************************************************
  !    CONFIGURE SOLVER
  !    ********************************************************************************************************

  ipar(1) = neq
  ipar(2) = numReactions

  CALL fnvinits (1, neq, ier)
  IF (ier/=0) THEN
     WRITE (stderr, 20) ier
20   FORMAT (///' SUNDIALS_ERROR: FNVINITS returned ier = ', I5)
     STOP
  ENDIF

  WRITE (*,'(A30, E15.3)') 't0 = ', t0
  CALL fcvmalloc (t0, z, meth, itmeth, iatol, rtol, atol, &
       iout, rout, ipar, rpar, ier)
  IF (ier/=0) THEN
     WRITE (stderr, 30) ier
30   FORMAT (///' SUNDIALS_ERROR: FCVMALLOC returned ier = ', I5)
     STOP
  ENDIF

  numsteps = 100000
  CALL fcvsetiin ('MAX_NSTEPS', numsteps, ier)
  WRITE (*,*) 'setting maxsteps ier = ', ier

  CALL fcvsetrin ('MAX_STEP', maxStep, ier)

  !   SELECT SOLVER TYPE ACCORDING TO FILE INPUT
  !   SPGMR SOLVER
  IF (solverType==1) THEN
     CALL fcvspgmr (0, 1, lookBack, deltaMain, ier)
     ! SPGMR SOLVER WITH BANDED PRECONDITIONER
  ELSE IF (solverType==2) THEN
     CALL fcvspgmr (1, 1, lookBack, deltaMain, ier)
     CALL fcvbpinit (neq, preconBandUpper, preconBandLower, ier)
     IF (ier/=0 ) THEN
        WRITE (stderr,*) 'SUNDIALS_ERROR: preconditioner returned ier = ', ier ;
        CALL fcvfree
        STOP
     END IF
     ! DENSE SOLVER
  ELSE IF (solverType==3) THEN
     ! make sure no Jacobian approximation is required
     IF (JVapprox==1) THEN
        WRITE (stderr,*) 'Solver parameter conflict! Jv approximation cannot be used for dense solver.'
        WRITE (stderr,*) 'Fix parameters in "modelConfiguration/solver.parameters" file.'
        STOP
     END IF
     CALL fcvdense (neq, ier)
     ! UNEXPECTED SOLVER TYPE
  ELSE
     WRITE (stderr,*) 'Error with solverType input, input = ', solverType
     WRITE (stderr,*) 'Available options are 1, 2, 3.'
     STOP
  ENDIF
  ! ERROR HANDLING
  IF (ier/=0) THEN
     WRITE (stderr,*) ' SUNDIALS_ERROR: SOLVER returned ier = ', ier
     CALL fcvfree
     STOP
  ENDIF

  ! Use Jacobian approximation if required. Calling fcvspilssetjac with non-zero flag
  ! specifies that spgmr, spbcg, or sptfqmr should use the supplied FCVJTIMES (in solverfunctions.f90).
  ! In our case, solverType={1,2} calls SPGMR above, while solverType=3 errors out if JvApprox=1
  IF (JVapprox==1) THEN
     CALL fcvspilssetjac (1, ier)
  ENDIF

  IF (ier/=0) THEN
     WRITE (stderr, 40) ier
40   FORMAT (///' SUNDIALS_ERROR: FCVDENSE returned ier = ', I5)
     CALL fcvfree
     STOP
  ENDIF

  ! check JFac data consistency:
  CALL test_jfac ()

  !    ********************************************************************************************************
  !    RUN MODEL
  !    ********************************************************************************************************

  DO WHILE (currentNumTimestep<maxNumTimesteps)

     ! GET CONCENTRATIONS FOR SOLVED SPECIES
     WRITE (59,*) t, secx, cosx, lat, longt, lha, sinld, cosld
     CALL fcvode (tout, t, z, itask, ier)
     IF (ier/=0) THEN
        WRITE (*,*) 'ier POST FCVODE = ', ier
     ENDIF
     flush(6)

     ! GET CONCENTRATIONS FOR CONSTRAINED SPECIES AND ADD TO ARRAY FOR OUTPUT
     DO species_counter = 1, numberOfConstrainedSpecies
        CALL getConstrainedConc (species_counter, d)
        constrainedConcs(species_counter) = d
     ENDDO

     CALL addConstrainedSpeciesToProbSpec (z, speciesConcs, numberOfConstrainedSpecies, constrainedSpecies, neq, constrainedConcs)

     ! OUTPUT ON SCREEN
     fmt = "('At t = ', E12.4, '   y = ', 3E14.6) "
     ! printing concentration of two first species - seems unnecessary at the moment
     ! WRITE (stderr, fmt) t, y (1), y (2)

     ! OUTPUT RATES OF PRODUCTION ON LOSS (OUTPUT FREQUENCY SET IN MODEL.PARAMETERS)
     time = INT (t)

     elapsed = INT (t-modelStartTime)
     IF (MOD (elapsed, ratesOutputStepSize)==0) THEN
        CALL outputRates (prodIntSpecies, t, productionRates, 1, &
                          rateOfProdNS, csize2, &
                          prodArrayLen, speciesName)
        CALL outputRates (reacIntSpecies, t, lossRates, 0, &
                          rateOfLossNS, csize1, &
                          lossArrayLen, speciesName)
     ENDIF

     ! OUTPUT JACOBIAN MATRIX (OUTPUT FREQUENCY SET IN MODEL PARAMETERS)
     WRITE (*,*) 'time = ', time
     IF (MOD (elapsed, jacobianOutputStepSize)==0) THEN
        CALL jfy (numSpec, numReactions, speciesConcs, fy, t)
        CALL outputjfy (fy, numSpec, t)
     ENDIF

     CALL getConcForSpecInt (speciesConcs, SORNumber, concsOfSpeciesOfInterest)
     CALL outputSpeciesOutputRequired (t, concsOfSpeciesOfInterest, SORNumberSize)
     CALL outputPhotolysisRates (j, t)

     !OUTPUT INSTANTANEOUS RATES
     IF (MOD (elapsed, irOutStepSize)==0) THEN
        CALL outputInstantaneousRates(time, numReactions)
     ENDIF

     ! OUTPUT FOR CVODE MAIN SOLVER
     WRITE (57,*) t, ' ', iout (lnst), ' ', iout (lnfe), ' ', iout (lnetf)
     ! OUTPUT FOR SPARSE SOLVER
     WRITE (61,*) t, ' ', iout (nfels), ' ', iout (njtv), ' ', iout (npe), ' ', iout (nps)
     ! OUTPUT STEP SIZE
     WRITE (62,*) t, ' ', rout (3), ' ', rout (2)

     !OUTPUT ENVVAR VALUES
     CALL ro2sum (ro2, speciesConcs)
     CALL outputEnvVar (t)

     ! CALCULATE AND OUTPUT RUNTIME
     ! not using timing at the moment
     ! CALL system_clock(current, rate)
     ! currentSeconds =(current - runStart) / rate
     ! stepTime = currentSeconds - previousSeconds
     ! WRITE (*,*) 'Current time = ', currentSeconds, 'step time = ', stepTime
     ! previousSeconds = currentSeconds

     ! ERROR HANDLING
     IF (ier<0) THEN
        fmt = "(///' SUNDIALS_ERROR: FCVODE returned ier = ', I5, /, 'Linear Solver returned ier = ', I5) "
        WRITE (stderr, fmt) ier, iout (15)
        ! free memory
        CALL fcvfree
        STOP
     ENDIF

     IF (ier==0) THEN
        tminus1 = t
        tout = tout + timestepSize
        currentNumTimestep = currentNumTimestep + 1
     ENDIF

  ENDDO

  CALL fcvdky (t, 1, z, ier)
  IF (ier/=0) THEN
     fmt = "(///' SUNDIALS_ERROR: FCVDKY returned ier = ', I4) "
     WRITE (stderr, fmt) ier
     CALL fcvfree
     STOP
  ENDIF

  !   OUPUT FINAL MODEL CONCENTRATIONS FOR MODEL RESTART
  DO species_counter = 1, numSpec
     WRITE (53,*) speciesName(species_counter), speciesConcs(species_counter)
  ENDDO

  !   printing of final statistics desactivated - nobody finds it useful
  !   FINAL ON SCREEN OUTPUT
  fmt = "(//'Final statistics:'//" // &
       "' No. steps = ', I4, '   No. f-s = ', I4," // &
       "'   No. J-s = ', I4, '   No. LU-s = ', I4/" // &
       "' No. nonlinear iterations = ', I4/" // &
       "' No. nonlinear convergence failures = ', I4/" // &
       "' No. error test failures = ', I4/) "

  WRITE (*, fmt) iout (lnst), iout (LNFE), iout (lnje), iout (lnsetup), &
       iout (lnni), iout (lncf), iout (lnetf)

  CALL SYSTEM_CLOCK (runEnd, rate)
  runTime =(runEnd - runStart) / rate
  WRITE (*,*) 'Runtime = ', runTime

  !   deallocate all
  WRITE (*,*) 'Deallocating memory.'
  !   deallocate CVODE internal data

  CALL fcvfree
  DEALLOCATE (speciesConcs, speciesName, concSpeciesName, speciesNumber, z, initialConcentrations)
  DEALLOCATE (prodIntSpecies, returnArray, reacIntSpecies)
  DEALLOCATE (SORNumber, concsOfSpeciesOfInterest, prodIntName, reacIntName, speciesOutputRequired)
  DEALLOCATE (fy, ir)
  DEALLOCATE (lossRates, productionRates)
  DEALLOCATE (clhs, crhs, ccoeff)
  DEALLOCATE (prodArrayLen)
  DEALLOCATE (lossArrayLen)
  !   deallocate data allocated before in input functions(inputFunctions.f)
  !   deallocate arrays from module constraints
  CALL deallocateConstrainedSpecies ()
  !   deallocate arrays from module species
  CALL deallocateSpeciesList
  !   deallocate arrays from module chemicalConstraints
  DEALLOCATE (dataX, dataY, dataY2, dataFixedY, constrainedConcs, constrainedName)
  DEALLOCATE (speciesNumberOfPoints, constrainedSpecies)
  !   deallocate arrays from module envVars
  DEALLOCATE (envVarTypesNum, envVarNames, envVarTypes, envVarFixedValues)
  DEALLOCATE (envVarX, envVarY, envVarY2, envVarNumberOfPoints)
  !   deallocate arrays from module photolysisRates
  DEALLOCATE (photoX, photoY, photoY2, photoNumberOfPoints)

  CLOSE (50)
  CLOSE (51)
  CLOSE (52)
  CLOSE (53)
  CLOSE (54)
  CLOSE (55)
  CLOSE (56)
  CLOSE (57)
  CLOSE (58)
  CLOSE (59)
  CLOSE (60)
  CLOSE (61)
  CLOSE (62)

  STOP
END PROGRAM ATCHEM
