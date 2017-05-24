!     ----------------------------------------------------------------
!     MAIN PROGRAM FOR THE ATMOSPHERE CHEMISTRY PROJECT
!     ----------------------------------------------------------------

PROGRAM ATCHEM

  use, intrinsic :: iso_fortran_env, only : stderr => error_unit
  use types_mod
  use species
  use constraints
  use interpolationMethod
  use reactionStructure
  use photolysisRates_mod
  use chemicalConstraints
  use zenithData
  use productionAndLossRates
  use envVars
  use SZACalcVars
  use date
  use directories, only : output_dir, instantaneousRates_dir, param_dir, spec_constraints_dir, env_constraints_dir
  use storage, only : maxSpecLength, maxPhotoRateNameLength
  use inputFunctions_mod
  use configFunctions_mod
  use outputFunctions_mod
  use constraintFunctions_mod
  use solverFunctions_mod
  implicit none

  !    ********************************************************************************************************
  !    DECLARATIONS
  !    ********************************************************************************************************

  !   DECLARATIONS FOR SOLVER PARAMETERS
  integer(kind=QI) :: ier
  integer :: i
  integer :: meth, itmeth, iatol, itask, currentNumTimestep, maxNumTimesteps
  integer(kind=NPI) :: iout(21), ipar(10)
  integer(kind=NPI) :: neq
  real(kind=DP) :: rtol, t, tout
  real(kind=DP) :: atol, rout(6)
  real(kind=DP) :: rpar(1)
  real(kind=DP), allocatable :: speciesConcs(:)

  !   DECLARATIONS FOR CONFIGURABLE SOLVER PARAMETERS
  real(kind=DP) :: deltaJv, deltaMain, maxStep
  integer :: JvApprox, lookBack
  integer(kind=SI) :: speciesInterpolationMethod, conditionsInterpolationMethod, decInterpolationMethod
  integer :: preconBandUpper, preconBandLower, solverType

  !   DECLARATIONS FOR TIME PARAMETERS
  integer(kind=QI) :: runStart, runEnd, runTime, rate, previousSeconds
  integer :: maxNumSteps
  integer(kind=NPI) :: numSpec, numReac
  real(kind=DP) :: timestepSize

  !   DECLARATIONS FOR SPECIES PARAMETERS
  real(kind=DP), allocatable :: initialConcentrations(:)
  character(len=maxSpecLength), allocatable :: speciesNames(:), initConcSpeciesNames(:)

  !   DECLARATIONS FOR RATES OF PRODUCTION AND LOSS
  integer(kind=NPI), allocatable :: SORNumber(:)
  integer(kind=NPI), allocatable :: prodIntSpecies(:,:), reacIntSpecies(:,:), prodIntSpeciesLengths(:), reacIntSpeciesLengths(:)
  integer(kind=NPI) :: numProdIntSpecies, numReacIntSpecies
  real(kind=DP), allocatable :: concsOfSpeciesOfInterest(:)
  character(len=maxSpecLength), allocatable :: prodIntName(:), reacIntName(:)
  character(len=maxSpecLength), allocatable :: speciesOutputRequired(:)
  integer :: ratesOutputStepSize, time, elapsed

  !   DECLARATIONS FOR CHEMICAL SPECIES CONSTRAINTS
  real(kind=DP), allocatable :: z(:)
  real(kind=DP), allocatable :: tempForSolverParameters(:), tempForModelParameters(:)
  real(kind=DP), allocatable :: solverParameters(:), modelParameters(:)
  integer(kind=DI) :: modelParameterSize, solverParameterSize

  real(kind=DP) :: modelStartTime

  !   DECLARATIONS FOR JACOBIAN PRODUCTION
  real(kind=DP), allocatable :: fy(:,:)
  integer :: jacobianOutputStepSize

  character(len=maxPhotoRateNameLength) :: photoRateNamesForHeader(200)
  character(len=400) :: fmt

  !   DECLARATIONS FOR IR OUTPUT
  integer :: irOutStepSize

  integer(kind=QI) :: cmd_arg_count
  !    MISC
  character(len=30) :: solverTypeName(3)
  character(len=20) :: interpolationMethodName(4)


  interface
    subroutine FCVJTIMES( v, fjv, t, y, fy, h, ipar, rpar, work, ier )
      use types_mod
      use species
      implicit none

      real(kind=DP), intent(in) :: v(*)
      real(kind=DP), intent(out) :: fjv(*)
      real(kind=DP), intent(in) :: t, y(*), fy(*)
      real(kind=DP), intent(out) :: h
      integer(kind=NPI), intent(in) :: ipar (*)
      real(kind=DP), intent(in) :: rpar(*), work(*)
      integer(kind=NPI), intent(out) :: ier

      integer(kind=NPI) :: neq, i, np
      real(kind=DP) :: delta, dummy
      real(kind=DP), allocatable :: yPlusV(:), yPlusVi(:)
    end subroutine FCVJTIMES

    subroutine FCVFUN( t, y, ydot, ipar, rpar, ier )
      use types_mod
      use species
      use constraints
      use reactionStructure
      use chemicalConstraints
      use interpolationFunctions_mod, only : getConstrainedQuantAtT
      use constraintFunctions_mod

      ! Fortran routine for right-hand side function.
      implicit none
      !
      real(kind=DP), intent(in) :: t, y(*)
      real(kind=DP), intent(out) :: ydot(*)
      integer(kind=NPI), intent(in) :: ipar(*)
      real(kind=DP), intent(in) :: rpar(*)
      integer(kind=NPI), intent(out) :: ier

      integer(kind=NPI) :: nConSpec, np, numReac
      real(kind=DP) :: concAtT, dummy
      real(kind=DP), allocatable :: dy(:), z(:)
      integer(kind=NPI) :: i
    end subroutine FCVFUN
  end interface

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

  call SYSTEM_CLOCK( runStart )
  previousSeconds = 0

  ! Read in command line argument to direct output files to a given directory
  cmd_arg_count = command_argument_count()
  if ( cmd_arg_count > 0 ) then
    call get_command_argument( 1, output_dir )
  else
    output_dir = "modelOutput"
  end if
  if ( cmd_arg_count > 1 ) then
    call get_command_argument( 2, instantaneousRates_dir )
  else
    instantaneousRates_dir = "instantaneousRates"
  end if
  if ( cmd_arg_count > 2 ) then
    call get_command_argument( 3, param_dir )
  else
    param_dir = "modelConfiguration"
  end if
  if ( cmd_arg_count > 3 ) then
    call get_command_argument( 4, spec_constraints_dir )
  else
    spec_constraints_dir = "speciesConstraints"
  end if
  if ( cmd_arg_count > 4 ) then
    call get_command_argument( 5, env_constraints_dir )
  else
    env_constraints_dir = "environmentConstraints"
  end if

  write (*,*) 'Output dir is ', trim( output_dir )
  write (*,*) 'Instantaneous rates dir is ', trim( instantaneousRates_dir )
  write (*,*) 'Parameter dir is ', trim( param_dir )
  !   OPEN FILES FOR OUTPUT
  open (unit=50, file=trim( output_dir ) // "/concentration.output")
  open (unit=51, file=trim( output_dir ) // "/errors.output")
  open (unit=52, file=trim( output_dir ) // "/envVar.output")
  open (unit=53, file=trim( output_dir ) // "/finalModelState.output")
  open (unit=54, file=trim( output_dir ) // "/initialConditionsSetting.output")
  open (unit=55, file=trim( output_dir ) // "/jacobian.output")
  open (unit=56, file=trim( output_dir ) // "/lossRates.output")
  open (unit=57, file=trim( output_dir ) // "/mainSolverParameters.output")
  open (unit=58, file=trim( output_dir ) // "/photolysisRates.output")
  open (unit=59, file=trim( output_dir ) // "/photoRateCalcParameters.output")
  open (unit=60, file=trim( output_dir ) // "/productionRates.output")
  open (unit=61, file=trim( output_dir ) // "/sparseSolverParameters.output")
  open (unit=62, file=trim( output_dir ) // "/stepSize.output")
  flush(6)

  call readNumberOfSpeciesAndReactions()
  numSpec = getNumberOfSpecies()
  numReac = getNumberOfReactions()

  !    SET ARRAY SIZES = NO. OF SPECIES
  allocate (speciesConcs(numSpec), speciesNames(numSpec))
  speciesConcs(:) = 0
  allocate (z(numSpec), initialConcentrations(numSpec))
  allocate (fy(numSpec, numSpec))
  !    SET ARRAY SIZES = NO. OF REACTIONS
  allocate (lossRates(numReac), productionRates(numReac), ir(numReac))

  !   GET SIZES OF REACTANTS AND PRODUCT INPUT FILES
  call getAndAllocateReactantAndProductListSizes()


  !   READ IN CHEMICAL REACTIONS
  call readReactions( clhs, clcoeff, crhs, crcoeff )
  neq = numSpec

  !   READ SPECIES NAMES AND NUMBERS
  write (*,*)
  write (*,*) 'Reading species names from mechanism.species...'
  call readSpecies( speciesNames )
  write (*,*) 'Finished reading species names.'
  write (*,*)

  !   SET PARAMETERS FOR SPECIES OBJECT
  call setSpeciesList( speciesNames )

  !   SET INITIAL SPECIES CONCENTRATIONS
  call readInitialConcentrations( initConcSpeciesNames, initialConcentrations )
  call setConcentrations( speciesNames, initConcSpeciesNames, initialConcentrations, speciesConcs )
  deallocate (initConcSpeciesNames, initialConcentrations)
  write (*,*)

  !   READ IN PHOTOLYSIS RATE INFORMATION
  call readPhotolysisConstants()
  write (*,*)
  !   Set default value for photonames array
  photoRateNamesForHeader(:) = 'na'

  do i = 1, nrOfPhotoRates
    photoRateNamesForHeader(ck(i)) = photoRateNames(i)
  end do

  !   READ IN JFAC SPECIES
  call readJFacSpecies()
  write (*,*)

  ! Read in product species of interest, and set up variables to hold these
  write (*,*) 'Reading products of interest...'
  call readProductsOrReactantsOfInterest( trim( param_dir ) // '/productionRatesOutput.config', prodIntName, numProdIntSpecies )
  write (*,*) 'Finished reading products of interest.'

  allocate (prodIntSpecies(numProdIntSpecies, size( crhs, 2 )))
  allocate (prodIntSpeciesLengths(numProdIntSpecies))
  ! Fill prodIntSpecies(:,1) with a list of the numbers of the interesting product species, with numbers from their ordering in speciesNames
  call matchNameToNumber( speciesNames, prodIntName, prodIntSpecies(:, 1) )
  ! prodIntSpecies will eventually hold one row per interesting product species, with the first element being the number
  ! of that species, and the remaining elements being the numbers of the reactions in which that species is a product

  ! Fill the remaining elements of each row of prodIntSpecies with the numbers of the reactions in which that species is a product
  call findReactionsWithProductOrReactant( prodIntSpecies, crhs, prodIntSpeciesLengths )
  write (*, '(A, I0)') ' products of interest (number of species found): ', numProdIntSpecies
  write (*,*)

  ! Read in reactant species of interest, and set up variables to hold these
  write (*,*) 'Reading reactants of interest...'
  call readProductsOrReactantsOfInterest( trim( param_dir ) // '/lossRatesOutput.config', reacIntName, numReacIntSpecies )
  write (*,*) 'Finished reading reactants of interest.'

  allocate (reacIntSpecies(numReacIntSpecies, size( clhs, 2 )))
  allocate (reacIntSpeciesLengths(numReacIntSpecies))
  ! Fill reacIntSpecies(:,1) with a list of the numbers of the interesting reaction species, with numbers from their ordering in speciesNames
  call matchNameToNumber( speciesNames, reacIntName, reacIntSpecies(:, 1) )
  ! reacIntSpecies will eventually hold one row per interesting reactant species, with the first element being the number
  ! of that species, and the remaining elements being the numbers of the reactions in which that species is a reactant

  ! Fill the remaining elements of each row of reacIntSpecies with the numbers of the reactions in which that species is a reactant
  call findReactionsWithProductOrReactant( reacIntSpecies, clhs, reacIntSpeciesLengths )
  write (*, '(A, I0)') ' reactants of interest (number of species found): ', numReacIntSpecies
  write (*,*)


  !    READ IN SOLVER PARAMETERS
  allocate(tempForSolverParameters(100))
  write (*,*) 'Reading solver parameters from file...'
  call getParametersFromFile( trim( param_dir ) // "/solver.parameters", tempForSolverParameters, solverParameterSize )
  write (*,*) 'Finished reading solver parameters from file.'
  allocate (solverParameters(solverParameterSize))
  solverParameters(:) = tempForSolverParameters(1:solverParameterSize)
  deallocate(tempForSolverParameters)
  !   READ IN MODEL PARAMETERS
  allocate(tempForModelParameters(100))
  write (*,*) 'Reading model parameters from file...'
  call getParametersFromFile( trim( param_dir ) //  "/model.parameters", tempForModelParameters, modelParameterSize )
  write (*,*) 'Finished reading model parameters from file.'
  allocate (modelParameters(modelParameterSize))
  modelParameters(:) = tempForModelParameters(1:modelParameterSize)
  deallocate (tempForModelParameters)
  write (*,*)

  !   SET SOLVER PARAMETERS
  ! Used in FCVMALLOC(): ATOL is the absolute tolerance (scalar or array).
  atol = solverParameters(1)
  ! Used in FCVMALLOC(): RTOL is the relative tolerance (scalar).
  rtol = solverParameters(2)
  ! TODO: convert this to boolean?
  ! If JvApprox==1 and solverType={1,2}, call FCVSPILSSETJAC() below, with non-zero flag.
  ! This means FCVJTIMES() in solverFunctions.f90 should be used to approximate the Jacobian.
  JvApprox = solverParameters(3)
  ! This is never used, but is referenced in a comment in FCVJTIMES().
  ! TODO: delete?
  deltaJv = solverParameters(4)
  ! From CVODE docs: DELT is the linear convergence tolerance factor of the SPGMR. Used in FCVSPGMR().
  deltaMain = solverParameters(5)
  ! From CVODE docs: MAXL is the maximum Krylov subspace dimension. Used in FCVSPGMR().
  ! TODO: Rename to MAXL?
  lookBack = solverParameters(6)
  ! From CVODE docs: Maximum absolute step size. Passed via FCVSETRIN().
  maxStep = solverParameters(7)
  ! From CVODE docs: Maximum no. of internal steps before tout. Passed via FCVSETIIN().
  maxNumsteps = solverParameters(8)
  ! USed to choose which solver to use:
  ! 1: SPGMR
  ! 2: SPGMR + Banded preconditioner
  ! 3: Dense solver
  ! otherwise: error
  solverType = solverParameters(9)
  ! From CVODE docs: MU (preconBandUpper) and ML (preconBandLower) are the upper
  ! and lower half- bandwidths of the band matrix that is retained as an
  ! approximation of the Jacobian.
  preconBandUpper = solverParameters(10)
  preconBandLower = solverParameters(11)

  ! float format
  100 format (A18, 1P E11.3)
  ! integer format
  200 format (A18, I11)
  write (*,*) 'Solver parameters:'
  write (*,*) '------------------'
  write (*, 100) 'atol: ', atol
  write (*, 100) 'rtol: ', rtol
  write (*, 200) 'JacVApprox: ', JvApprox
  write (*, 100) 'deltaJv: ', deltaJv
  write (*, 100) 'deltaMain: ', deltaMain
  write (*, 200) 'lookBack: ', lookBack
  write (*, 100) 'maxStep: ', maxStep
  write (*, 200) 'preconBandUpper: ', preconBandUpper
  write (*, 200) 'preconBandLower: ', preconBandLower
  write (*, '(A18, A)') 'solverType: ', adjustl( solverTypeName(solverType) )
  write (*,*) '------------------'
  write (*,*)

  !   SET MODEL PARAMETERS
  ! maxNumTimesteps sets the maximum number of timesteps to calculate.
  ! Calculation will terminate when currentNumTimestep>=maxNumTimesteps.
  maxNumTimesteps = modelParameters(1)
  ! Size of timestep: tout is incremented by this amount on each iteration of the main while loop.
  timestepSize = modelParameters(2)
  ! Use the local variable speciesInterpolationMethod to set the value speciesInterpMethod,
  ! the private member of MODULE interpolationMethod.
  ! getSpeciesInterpMethod() is called by getConstrainedQuantAtT.
  ! Values:
  ! 1: Cubic spline interpolation
  ! 2: Cubic spline interpolation (log) TODO: this just outputs the exponential of the value. Is this right?
  ! 3: Piecewise constant
  ! 4: Piecewise linear
  ! otherwise: error
  speciesInterpolationMethod = modelParameters(3)
  call setSpeciesInterpMethod( speciesInterpolationMethod )
  conditionsInterpolationMethod = modelParameters(4)
  call setConditionsInterpMethod( conditionsInterpolationMethod )
  decInterpolationMethod = modelParameters(5)
  call setDecInterpMethod( decInterpolationMethod )
  ! Member variable of MODULE constraints. Used in getConstrainedQuantAtT and readEnvVar
  maxNumberOfDataPoints = modelParameters(6)
  ! Member variable of chemicalConstraints.
  numberOfConstrainedSpecies = modelParameters(7)
  ! Frequency at which outputRates is called below.
  ratesOutputStepSize = modelParameters(8)
  ! Start time of model. Used to set t initially, and to calculate the elapsed time.
  modelStartTime = modelParameters(9)
  ! Frequency at which output_jfy is called below.
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
  300 format (A52, E11.3)
  ! integer format
  400 format (A52, I11)
  ! string format
  500 format (A52, A17)
  write (*,*) 'Model parameters:'
  write (*,*) '-----------------'
  write (*, 400) 'number of steps: ', maxNumTimesteps
  write (*, 300) 'step size (seconds): ', timestepSize
  write (*, 500) 'species interpolation method: ', adjustl( interpolationMethodName(speciesInterpolationMethod) )
  write (*, 500) 'conditions interpolation method: ', adjustl( interpolationMethodName(conditionsInterpolationMethod) )
  write (*, 500) 'dec interpolation method: ', adjustl( interpolationMethodName(decInterpolationMethod) )
  write (*, 400) 'maximum number of data points in constraint file: ', maxNumberOfDataPoints
  write (*, 400) 'maximum number of constrained species: ', numberOfConstrainedSpecies
  write (*, 400) 'ratesOutputStepSize: ', ratesOutputStepSize
  write (*, 400) 'instantaneous rates output step size: ', irOutStepSize
  write (*, 300) 'modelStartTime: ', modelStartTime
  write (*, 400) 'jacobianOutputStepSize: ', jacobianOutputStepSize
  write (*, 300) 'latitude: ', latitude
  write (*, 300) 'longitude: ', longitude
  write (*, '(A52, I3, A, I2, A, I4) ') 'day/month/year: ', day, '/', month, '/', year
  write (*,*) '-----------------'
  write (*,*)

  ! Set the members dayOfYear, dayAsFractionOfYear, secondsInYear of MODULE date to their value based on day, month, year
  call calcDateParameters()

  !   HARD CODED SOLVER PARAMETERS
  t = modelStartTime
  tout = timestepSize + t
  ! Parameters for FCVMALLOC(). (Comments from cvode guide)
  ! meth specifies the basic integration: 1 for Adams (nonstiff) or 2 for BDF stiff)
  meth = 2
  ! itmeth specifies the nonlinear iteration method: 1 for functional iteration or 2 for Newton iteration.
  itmeth = 2
  ! IATOL specifies the type for absolute tolerance ATOL: 1 for scalar or 2 for array.
  ! If IATOL= 3, the arguments RTOL and ATOL are ignored and the user is
  ! expected to subsequently call FCVEWTSET() and provide the function FCVEWT().
  iatol = 1

  ! Parameter for FCVODE(). Comment from cvode guide: ITASK is a task indicator and should be
  ! set to 1 for normal mode (overshoot TOUT and interpolate),
  ! or to 2 for one-step mode (return after each internal step taken)
  itask = 1

  ! currentNumTimestep counts the number of iterative steps. Set to zero. Calculation will terminate when currentNumTimestep>=maxNumTimesteps.
  currentNumTimestep = 0

  ! Read in environment variables (FIXED, CONSTRAINED, CALC or NOTUSED, see environmentVariables.config)
  call readEnvVar()
  write (*,*)

  ! fill speciesOutputRequired with the names of species to output to concentration.output
  call readSpeciesOutputRequired( speciesOutputRequired )

  ! Allocate SORNumber and fill with the global numbering of the species found in speciesOutputRequired
  allocate (SORNumber(size( speciesOutputRequired )), concsOfSpeciesOfInterest(size( speciesOutputRequired )))
  call matchNameToNumber( speciesNames, speciesOutputRequired, SORNumber )

  flush(stderr)
  !    ********************************************************************************************************
  !    CONSTRAINTS
  !    ********************************************************************************************************

  write (*,*)
  call readPhotoRates()
  write (*,*)

  call readSpeciesConstraints( t, speciesConcs )

  write (*,*)
  !test
  ! TODO: Why does this not use neq, but neq+numberOfConstrainedSpecies?
  call getConcForSpecInt( speciesConcs, SORNumber, concsOfSpeciesOfInterest )
  call outputSpeciesOutputRequired( t, speciesOutputRequired, concsOfSpeciesOfInterest )

  ! This outputs z, which is y with all the constrained species removed.
  call removeConstrainedSpeciesFromProbSpec( speciesConcs, constrainedSpecies, z )

  !   ADJUST PROBLEM SPECIFICATION TO GIVE NUMBER OF SPECIES TO BE SOLVED FOR (N - C = M)
  neq = numSpec - numberOfConstrainedSpecies
  write (*, '(A30, I0) ') ' neq = ', neq
  write (*, '(A30, I0) ') ' numberOfConstrainedSpecies = ', numberOfConstrainedSpecies

  flush(stderr)

  !    ********************************************************************************************************
  !    CONFIGURE SOLVER
  !    ********************************************************************************************************

  ipar(1) = neq
  ipar(2) = numReac

  call FNVINITS( 1, neq, ier )
  if ( ier /= 0 ) then
    write (stderr, 20) ier
    20   format (///' SUNDIALS_ERROR: FNVINITS() returned ier = ', I5)
    stop
  end if

  write (*, '(A30, 1P e15.3) ') ' t0 = ', t
  call FCVMALLOC( t, z, meth, itmeth, iatol, rtol, atol, &
                  iout, rout, ipar, rpar, ier )
  if ( ier /= 0 ) then
    write (stderr, 30) ier
    30   format (///' SUNDIALS_ERROR: FCVMALLOC() returned ier = ', I5)
    stop
  end if

  call FCVSETIIN( 'MAX_NSTEPS', maxNumSteps, ier )
  write (*, '(A, I0)') ' setting maxnumsteps ier = ', ier

  call FCVSETRIN( 'MAX_STEP', maxStep, ier )
  write (*, '(A, I0)') ' setting maxstep ier = ', ier

  !   SELECT SOLVER TYPE ACCORDING TO FILE INPUT
  !   SPGMR SOLVER
  if ( solverType == 1 ) then
    call FCVSPGMR( 0, 1, lookBack, deltaMain, ier )
    ! SPGMR SOLVER WITH BANDED PRECONDITIONER
  else if ( solverType == 2 ) then
    call FCVSPGMR( 1, 1, lookBack, deltaMain, ier )
    call FCVBPINIT( neq, preconBandUpper, preconBandLower, ier )
    if ( ier /= 0 ) then
      write (stderr,*) 'SUNDIALS_ERROR: preconditioner returned ier = ', ier ;
      call FCVFREE()
      stop
    end if
    ! DENSE SOLVER
  else if ( solverType == 3 ) then
    ! make sure no Jacobian approximation is required
    if ( JVapprox == 1 ) then
      write (stderr,*) 'Solver parameter conflict! Jv approximation cannot be used for dense solver.'!
      write (stderr,*) 'Fix parameters in "modelConfiguration/solver.parameters" file.'
      stop
    end if
    call FCVDENSE( neq, ier )
    ! UNEXPECTED SOLVER TYPE
  else
    write (stderr,*) 'Error with solverType input, input = ', solverType
    write (stderr,*) 'Available options are 1, 2, 3.'
    stop
  end if
  ! ERROR HANDLING
  if ( ier /= 0 ) then
    write (stderr,*) ' SUNDIALS_ERROR: SOLVER returned ier = ', ier
    call FCVFREE()
    stop
  end if

  ! Use Jacobian approximation if required. Calling FCVSPILSSETJAC() with non-zero flag
  ! specifies that spgmr, spbcg, or sptfqmr should use the supplied FCVJTIMES() (in solverfunctions.f90).
  ! In our case, solverType={1,2} calls SPGMR above, while solverType=3 errors out if JvApprox=1
  if ( JVapprox == 1 ) then
    call FCVSPILSSETJAC( 1, ier )
  end if

  if ( ier /= 0 ) then
    write (stderr, 40) ier
    40   format (///' SUNDIALS_ERROR: FCVDENSE() returned ier = ', I5)
    call FCVFREE()
    stop
  end if

  ! check JFac data consistency:
  call test_jfac()

  !    ********************************************************************************************************
  !    RUN MODEL
  !    ********************************************************************************************************

  do while ( currentNumTimestep < maxNumTimesteps )

    call outputPhotoRateCalcParameters( t )

    ! GET CONCENTRATIONS FOR SOLVED SPECIES
    call FCVODE( tout, t, z, itask, ier )
    if ( ier /= 0 ) then
      write (*,*) 'ier POST FCVODE()= ', ier
    end if
    flush(6)

    time = int( t )
    write (*, '(A, I0)') ' time = ', time

    ! GET CONCENTRATIONS FOR CONSTRAINED SPECIES AND ADD TO ARRAY FOR OUTPUT
    constrainedConcs = getConstrainedConcs()

    call addConstrainedSpeciesToProbSpec( z, constrainedConcs, constrainedSpecies, speciesConcs )

    ! OUTPUT RATES OF PRODUCTION OR LOSS (OUTPUT FREQUENCY SET IN MODEL.PARAMETERS)
    elapsed = int( t-modelStartTime )
    if ( mod( elapsed, ratesOutputStepSize ) == 0 ) then
      call outputRates( prodIntSpecies, prodIntSpeciesLengths, t, productionRates, 1 )
      call outputRates( reacIntSpecies, reacIntSpeciesLengths, t, lossRates, 0 )
    end if

    ! OUTPUT JACOBIAN MATRIX (OUTPUT FREQUENCY SET IN MODEL PARAMETERS)
    if ( mod( elapsed, jacobianOutputStepSize ) == 0 ) then
      call jfy( numReac, speciesConcs, t, fy )
      call output_jfy( t, fy )
    end if

    call getConcForSpecInt( speciesConcs, SORNumber, concsOfSpeciesOfInterest )
    call outputSpeciesOutputRequired( t, speciesOutputRequired, concsOfSpeciesOfInterest )
    call outputPhotolysisRates( t, photoRateNamesForHeader )

    !OUTPUT INSTANTANEOUS RATES
    if ( mod( elapsed, irOutStepSize ) == 0 ) then
      call outputInstantaneousRates( time )
    end if

    ! OUTPUT FOR CVODE MAIN SOLVER
    call outputSolverParameters( t, iout, solverType )

    ! OUTPUT STEP SIZE
    call outputStepSize( t, rout (3), rout (2) )

    !OUTPUT ENVVAR VALUES
    ro2 = ro2sum( speciesConcs )
    call outputEnvVar( t )

    ! CALCULATE AND OUTPUT RUNTIME
    ! not using timing at the moment
    ! CALL system_clock(current, rate)
    ! currentSeconds =(current - runStart) / rate
    ! stepTime = currentSeconds - previousSeconds
    ! WRITE (*,*) 'Current time = ', currentSeconds, 'step time = ', stepTime
    ! previousSeconds = currentSeconds

    ! ERROR HANDLING
    if ( ier < 0 ) then
      fmt = "(///' SUNDIALS_ERROR: FCVODE() returned ier = ', I5, /, 'Linear Solver returned ier = ', I5) "
      write (stderr, fmt) ier, iout (15)
      ! free memory
      call FCVFREE()
      stop
    end if

    ! increment time
    tout = tout + timestepSize
    currentNumTimestep = currentNumTimestep + 1

  end do

  call FCVDKY( t, 1, z, ier )
  if ( ier /= 0 ) then
    fmt = "(///' SUNDIALS_ERROR: FCVDKY() returned ier = ', I4) "
    write (stderr, fmt) ier
    call FCVFREE()
    stop
  end if

  !   OUTPUT FINAL MODEL CONCENTRATIONS FOR MODEL RESTART
  call outputFinalModelState( speciesNames, speciesConcs )

  !   printing of final statistics desactivated - nobody finds it useful
  !   FINAL ON SCREEN OUTPUT
  fmt = "(//' Final statistics:'//" // &
        "' No. steps = ', I0, '   No. f-s = ', I0, " // &
        "'   No. J-s = ', I0, '   No. LU-s = ', I0/" // &
        "' No. nonlinear iterations = ', I0/" // &
        "' No. nonlinear convergence failures = ', I0/" // &
        "' No. error test failures = ', I0/) "

  write (*, fmt) iout (3), iout (4), iout (17), iout (8), &
                 iout (7), iout (6), iout (5)

  call SYSTEM_CLOCK( runEnd, rate )
  runTime = ( runEnd - runStart ) / rate
  write (*, '(A, I0)') ' Runtime = ', runTime

  !   deallocate all
  write (*,*) 'Deallocating memory.'
  !   deallocate CVODE internal data

  call FCVFREE()
  deallocate (speciesConcs, speciesNames, z)
  deallocate (prodIntSpecies, reacIntSpecies)
  deallocate (SORNumber, concsOfSpeciesOfInterest, prodIntName, reacIntName, speciesOutputRequired)
  deallocate (fy, ir)
  deallocate (lossRates, productionRates)
  deallocate (clhs, clcoeff, crhs, crcoeff)
  deallocate (prodIntSpeciesLengths)
  deallocate (reacIntSpeciesLengths)
  !   deallocate data allocated before in input functions(inputFunctions.f)
  !   deallocate arrays from module constraints
  call deallocateConstrainedSpecies()
  !   deallocate arrays from module species
  call deallocateSpeciesList
  !   deallocate arrays from module chemicalConstraints
  deallocate (dataX, dataY, dataY2, dataFixedY, constrainedConcs, constrainedNames)
  deallocate (speciesNumberOfPoints, constrainedSpecies)
  !   deallocate arrays from module envVars
  deallocate (envVarTypesNum, envVarNames, envVarTypes, envVarFixedValues)
  deallocate (envVarX, envVarY, envVarY2, envVarNumberOfPoints)
  !   deallocate arrays from module photolysisRates
  deallocate (photoX, photoY, photoY2, photoNumberOfPoints)

  close (50)
  close (51)
  close (52)
  close (53)
  close (54)
  close (55)
  close (56)
  close (57)
  close (58)
  close (59)
  close (60)
  close (61)
  close (62)

  stop
END PROGRAM ATCHEM

subroutine FCVJTIMES( v, fjv, t, y, fy, h, ipar, rpar, work, ier )
  use types_mod
  use species
  implicit none

  real(kind=DP), intent(in) :: v(*)
  real(kind=DP), intent(out) :: fjv(*)
  real(kind=DP), intent(in) :: t, y(*), fy(*)
  real(kind=DP), intent(out) :: h
  integer(kind=NPI), intent(in) :: ipar (*)
  real(kind=DP), intent(in) :: rpar(*), work(*)
  integer(kind=NPI), intent(out) :: ier

  integer(kind=NPI) :: neq, np
  real(kind=DP) :: delta, dummy
  real(kind=DP), allocatable :: yPlusV(:), fyPlusV(:)

  np = getNumberOfSpecies()
  allocate (yPlusV(np), fyPlusV(np))

  neq = ipar(1)
  delta = 1.00d-03
  ! fake using variables h and work, to avoid a warning (they are required by CVODE code)
  h = h
  dummy = work(1)

  ! calculate y + delta v
  yPlusV(1:neq) = y(1:neq) + delta * v(1:neq)

  ! get f(y + delta v) into fyPlusVi
  call FCVFUN( t, yPlusV, fyPlusV, ipar, rpar, ier )

  ! JVminus1 + deltaJV
  fjv(1:neq) = ( fyPlusV(1:neq) - fy(1:neq) ) / delta

  deallocate (yPlusV, fyPlusV)

  return
end subroutine FCVJTIMES

!     ---------------------------------------------------------------
subroutine FCVFUN( t, y, ydot, ipar, rpar, ier )
  use types_mod
  use species
  use constraints
  use reactionStructure
  use chemicalConstraints
  use interpolationMethod, only : getSpeciesInterpMethod
  use interpolationFunctions_mod, only : getConstrainedQuantAtT
  use constraintFunctions_mod
  use solverFunctions_mod, only : resid

  ! Fortran routine for right-hand side function.
  implicit none
  !
  real(kind=DP), intent(in) :: t, y(*)
  real(kind=DP), intent(out) :: ydot(*)
  integer(kind=NPI), intent(in) :: ipar(*)
  real(kind=DP), intent(in) :: rpar(*)
  integer(kind=NPI), intent(out) :: ier

  integer(kind=NPI) :: nConSpec, np, numReac
  real(kind=DP) :: concAtT, dummy
  real(kind=DP), allocatable :: dy(:), z(:)
  integer(kind=NPI) :: i

  np = ipar(1) + numberOfConstrainedSpecies
  numReac = ipar(2)
  dummy = rpar(1)

  nConSpec = numberOfConstrainedSpecies
  allocate (dy(np), z(np))

  ! for each constrained species...
  do i = 1, numberOfConstrainedSpecies
    ! if it's a variable-concentration constrained species,
    if ( i <= numberOfVariableConstrainedSpecies ) then
      call getConstrainedQuantAtT( t, datax, datay, datay2, speciesNumberOfPoints(i), getSpeciesInterpMethod(), i, concAtT )
    else
      concAtT = dataFixedY(i - numberOfVariableConstrainedSpecies)
    end if
    constrainedConcs(i) = concAtT
    call setConstrainedConc( i, concAtT )

  end do

  call addConstrainedSpeciesToProbSpec( y, constrainedConcs, constrainedSpecies, z )

  call resid( numReac, t, z, dy, clhs, clcoeff, crhs, crcoeff )

  call removeConstrainedSpeciesFromProbSpec( dy, constrainedSpecies, ydot )

  deallocate (dy, z)
  ier = 0

  return
end subroutine FCVFUN
