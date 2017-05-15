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
  use photolysisRates
  use chemicalConstraints
  use zenithData
  use zenithData1
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
  integer(kind=NPI) :: species_counter
  integer :: lnst, lnfe, lnsetup, lnni, lncf, lnetf, lnje
  integer :: nfels, njtv, npe, nps
  integer :: meth, itmeth, iatol, itask, currentNumTimestep, maxNumTimesteps
  integer(kind=NPI) :: iout(21), ipar(10)
  integer(kind=NPI) :: neq
  real(kind=DP) :: rtol, t, t0, tout
  real(kind=DP) :: atol, rout(6)
  real(kind=DP) :: rpar(1)
  data lnst/3/, lnfe/4/, lnetf/5/, lncf/6/, lnni/7/, lnsetup/8/, &
       lnje/17/, nfels/16/, njtv/17/ , npe/18/, nps/19/
  real(kind=DP), allocatable :: speciesConcs(:)

  !   DECLARATIONS FOR CONFIGURABLE SOLVER PARAMETERS
  real(kind=DP) :: deltaJv, deltaMain, maxStep
  integer :: JvApprox, lookBack
  integer(kind=SI) :: speciesInterpolationMethod, conditionsInterpolationMethod, decInterpolationMethod
  integer :: preconBandUpper, preconBandLower, solverType
  real(kind=DP) :: d

  !   DECLARATIONS FOR TIME PARAMETERS
  integer(kind=QI) :: runStart, runEnd, runTime, rate, previousSeconds
  integer :: numSteps
  integer(kind=NPI) :: numSpec, numReac
  real(kind=DP) :: tminus1, timestepSize

  !   DECLARATIONS FOR SPECIES PARAMETERS
  real(kind=DP), allocatable :: initialConcentrations(:)
  character(len=maxSpecLength), allocatable :: speciesNames(:), concSpeciesNames(:)
  integer(kind=NPI), allocatable :: speciesNumber(:)

  !   DECLARATIONS FOR RATES OF PRODUCTION AND LOSS
  integer(kind=NPI), allocatable :: returnArray(:), tempSORNumber(:), SORNumber(:)
  integer(kind=NPI), allocatable :: prodIntSpecies(:,:), reacIntSpecies(:,:), prodArrayLen(:), lossArrayLen(:)
  integer(kind=NPI) :: speciesOutputRequiredSize, SORNumberSize, prodIntNameSize, reacIntNameSize
  real(kind=DP), allocatable :: concsOfSpeciesOfInterest(:)
  character(len=maxSpecLength), allocatable :: prodIntName(:), reacIntName(:)
  character(len=maxSpecLength), allocatable :: speciesOutputRequired(:)
  integer(kind=NPI) :: rateOfProdNS, rateOfLossNS
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

      integer(kind=NPI) :: ipar (*), ier, neq, i, np
      integer :: j
      real(kind=DP) :: t, h, rpar(*), y(*), v(*), fjv(*), fy(*), work(*), delta, deltaV, dummy
      real(kind=DP), allocatable :: yPlusV(:), yPlusVi(:)
    end subroutine FCVJTIMES

    subroutine FCVFUN( t, y, ydot, ipar, rpar, ier )
      use types_mod
      use species
      use constraints
      use reactionStructure
      use chemicalConstraints
      use interpolationFunctions_mod, only : getConstrainedQuantAtT2D
      use constraintFunctions_mod

      ! Fortran routine for right-hand side function.
      implicit none
      !
      integer(kind=NPI) :: ipar(*), ier, nConSpec, np, numReac
      real(kind=DP) :: t, y(*), ydot(*), rpar (*), concAtT, dummy
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

  write (*,*) 'Output dir is ', output_dir
  write (*,*) 'Instantaneous rates dir is ', instantaneousRates_dir
  write (*,*) 'Parameter dir is ', param_dir
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
  allocate (speciesNumber(numSpec), z(numSpec), initialConcentrations(numSpec))
  allocate (returnArray(numSpec))
  allocate (tempSORNumber(numSpec))
  allocate (fy(numSpec, numSpec))
  !    SET ARRAY SIZES = NO. OF REACTIONS
  allocate (lossRates(numReac), productionRates(numReac), ir(numReac))

  !   GET SIZES OF REACTANTS AND PRODUCT INPUT FILES
  call getReactantAndProductListSizes()

  allocate (clhs(3, lhs_size), crhs(2, rhs_size), ccoeff(rhs_size))

  !   READ IN CHEMICAL REACTIONS
  call readReactions( clhs, crhs, ccoeff )
  neq = numSpec

  write (*,*) 'Size of lhs =', lhs_size, 'size of rhs2 = ', rhs_size, '.'

  !   READ SPECIES NAMES AND NUMBERS
  write (*,*)
  write (*,*) 'Reading species names from mechanism.species...'
  call readSpecies( numSpec, speciesNames, speciesNumber )
  write (*,*) 'Finished reading species names.'
  write (*,*)

  !   SET PARAMETERS FOR SPECIES OBJECT
  call setSpeciesList( speciesNames )

  !   SET INITIAL SPECIES CONCENTRATIONS
  call readInitialConcentrations( concSpeciesNames, initialConcentrations )
  call setConcentrations( speciesNames, concSpeciesNames, initialConcentrations, speciesConcs )
  deallocate (concSpeciesNames, initialConcentrations)
  write (*,*)

  !   READ IN PHOTOLYSIS RATE INFORMATION
  call readPhotolysisConstants( ck, cl, cmm, cnn, photoRateNames, transmissionFactor )
  write (*,*)
  !   Set default value for photonames array
  do i = 1, 200
    photoRateNamesForHeader(i) = 'na'
  end do

  do i = 1, nrOfPhotoRates
    photoRateNamesForHeader(ck(i)) = photoRateNames(i)
  end do

  !   READ IN JFAC SPECIES
  call readJFacSpecies()
  write (*,*)

  ! Read in product species of interest, and set up variables to hold these
  write (*,*) 'Reading products of interest...'
  call readProductsOrReactantsOfInterest( trim( param_dir ) // '/productionRatesOutput.config', prodIntName, prodIntNameSize )
  write (*,*) 'Finished reading products of interest.'
  ! Fill returnArray with a list of the numbers of the interesting product species, with numbers from their ordering in speciesNames
  call matchNameToNumber( speciesNames, prodIntName, returnArray, rateOfProdNS )
  ! prodIntSpecies will eventually hold one row per interesting product species, with the first element being the number
  ! of that species, and the remaining elements being the numbers of the reactions in which that species is a product
  allocate (prodArrayLen(rateOfProdNS))
  allocate (prodIntSpecies(rateOfProdNS, rhs_size))
  ! Write product species number to first element of each row of prodIntSpecies
  do species_counter = 1, rateOfProdNS
    prodIntSpecies(species_counter, 1) = returnArray(species_counter)
  end do
  ! Fill the remaining elements of each row of prodIntSpecies with the numbers of the reactions in which that species is a product
  call findReactionsWithProductOrReactant( prodIntSpecies, crhs, prodArrayLen )
  write (*,*) 'rateOfProdNS (number of species found):', rateOfProdNS
  write (*,*)

  ! Read in reactant species of interest, and set up variables to hold these
  write (*,*) 'Reading reactants of interest...'
  call readProductsOrReactantsOfInterest( trim( param_dir ) // '/lossRatesOutput.config', reacIntName, reacIntNameSize )
  write (*,*) 'Finished reading reactants of interest.'
  ! Fill returnArray with a list of the numbers of the interesting reaction species, with numbers from their ordering in speciesNames
  call matchNameToNumber( speciesNames, reacIntName, returnArray, rateOfLossNS )
  ! reacIntSpecies will eventually hold one row per interesting reactant species, with the first element being the number
  ! of that species, and the remaining elements being the numbers of the reactions in which that species is a reactant
  allocate (lossArrayLen(rateOfLossNS))
  allocate (reacIntSpecies(rateOfLossNS, lhs_size))
  ! Write reactant species number to first element of each row of reacIntSpecies
  do species_counter = 1, rateOfLossNS
    reacIntSpecies(species_counter, 1) = returnArray(species_counter)
  end do
  ! Fill the remaining elements of each row of reacIntSpecies with the numbers of the reactions in which that species is a reactant
  call findReactionsWithProductOrReactant( reacIntSpecies, clhs, lossArrayLen )
  write (*,*) 'rateOfLossNS (number of species found):', rateOfLossNS
  write (*,*)


  !    READ IN SOLVER PARAMETERS
  allocate(tempForSolverParameters(100))
  write (*,*) 'Reading solver parameters from file...'
  call getParametersFromFile( trim( param_dir ) // "/solver.parameters", tempForSolverParameters, solverParameterSize )
  write (*,*) 'Finished reading solver parameters from file.'
  allocate (solverParameters(solverParameterSize))
  do i = 1, solverParameterSize
    solverParameters(i) = tempForSolverParameters(i)
  end do
  deallocate(tempForSolverParameters)
  !   READ IN MODEL PARAMETERS
  allocate(tempForModelParameters(100))
  write (*,*) 'Reading model parameters from file...'
  call getParametersFromFile( trim( param_dir ) //  "/model.parameters", tempForModelParameters, modelParameterSize )
  write (*,*) 'Finished reading model parameters from file.'
  allocate (modelParameters(modelParameterSize))
  do i = 1, modelParameterSize
    modelParameters(i) = tempForModelParameters(i)
  end do
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
  100 format (A17, E11.3)
  ! integer format
  200 format (A17, I11)
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
  write (*, '(A17, A29) ') 'solverType: ', adjustl( solverTypeName(solverType) )
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
  ! getSpeciesInterpMethod() is called by getConstrainedQuantAtT2D.
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
  ! Member variable of MODULE constraints. Used in getConstrainedQuantAtT2D and readEnvVar
  maxNumberOfDataPoints = modelParameters(6)
  ! Member variable of chemicalConstraints.
  numberOfConstrainedSpecies = modelParameters(7)
  ! Frequency at which outputRates is called below.
  ratesOutputStepSize = modelParameters(8)
  ! Start time of model. Used to set t0, and to calculate the elapsed time.
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
  t0 = modelStartTime
  tout = timestepSize
  tout = tout + t0
  t = t0
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
  call readSpeciesOutputRequired( speciesOutputRequired, speciesOutputRequiredSize )

  ! fill SORNumber with the global numbering of the species found in speciesOutputRequired
  call matchNameToNumber( speciesNames, speciesOutputRequired, &
                          tempSORNumber, SORNumberSize )
  ! Allocate SORNumber and fill from temporary array
  allocate (SORNumber(SORNumberSize), concsOfSpeciesOfInterest(SORNumberSize))
  do species_counter = 1, SORNumberSize
    SORNumber(species_counter) = tempSORNumber(species_counter)
  end do
  deallocate (tempSORNumber)
  ! fill concsOfSpeciesOfInterest with the concentrations of the species to be output
  call getConcForSpecInt( speciesConcs, SORNumber, concsOfSpeciesOfInterest )

  !   Write file output headers
  call writeFileHeaders( photoRateNamesForHeader, speciesOutputRequired )

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
  call outputSpeciesOutputRequired( t, concsOfSpeciesOfInterest )

  ! This outputs z, which is y with all the constrained species removed.
  call removeConstrainedSpeciesFromProbSpec( speciesConcs, z, constrainedSpecies )

  !   ADJUST PROBLEM SPECIFICATION TO GIVE NUMBER OF SPECIES TO BE SOLVED FOR (N - C = M)
  neq = numSpec - numberOfConstrainedSpecies
  write (*, '(A30, I6) ') 'neq = ', neq
  write (*, '(A30, I6) ') 'numberOfConstrainedSpecies = ', numberOfConstrainedSpecies

  flush(stderr)

  !    ********************************************************************************************************
  !    CONFIGURE SOLVER
  !    ********************************************************************************************************

  ipar(1) = neq
  ipar(2) = numReac

  call FNVINITS( 1, neq, ier )
  if ( ier /= 0 ) then
    write (stderr, 20) ier
    20   format (///' SUNDIALS_ERROR: FNVINITS()returned ier = ', I5)
    stop
  end if

  write (*, '(A30, E15.3) ') 't0 = ', t0
  call FCVMALLOC( t0, z, meth, itmeth, iatol, rtol, atol, &
                  iout, rout, ipar, rpar, ier )
  if ( ier /= 0 ) then
    write (stderr, 30) ier
    30   format (///' SUNDIALS_ERROR: FCVMALLOC()returned ier = ', I5)
    stop
  end if

  numsteps = 100000
  call FCVSETIIN( 'MAX_NSTEPS', numsteps, ier )
  write (*,*) 'setting maxsteps ier = ', ier

  call FCVSETRIN( 'MAX_STEP', maxStep, ier )

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
    40   format (///' SUNDIALS_ERROR: FCVDENSE()returned ier = ', I5)
    call FCVFREE()
    stop
  end if

  ! check JFac data consistency:
  call test_jfac()

  !    ********************************************************************************************************
  !    RUN MODEL
  !    ********************************************************************************************************

  do while ( currentNumTimestep < maxNumTimesteps )

    ! GET CONCENTRATIONS FOR SOLVED SPECIES
    write (59,*) t, secx, cosx, lat, longt, lha, sinld, cosld
    call FCVODE( tout, t, z, itask, ier )
    if ( ier /= 0 ) then
      write (*,*) 'ier POST FCVODE()= ', ier
    end if
    flush(6)

    ! GET CONCENTRATIONS FOR CONSTRAINED SPECIES AND ADD TO ARRAY FOR OUTPUT
    do species_counter = 1, numberOfConstrainedSpecies
      call getConstrainedConc( species_counter, d )
      constrainedConcs(species_counter) = d
    end do

    call addConstrainedSpeciesToProbSpec( z, speciesConcs, numberOfConstrainedSpecies, constrainedSpecies, neq, constrainedConcs )

    ! OUTPUT ON SCREEN
    fmt = "('At t = ', E12.4, '   y = ', 3E14.6) "
    ! printing concentration of two first species - seems unnecessary at the moment
    ! write (stderr, fmt) t, y (1), y (2)

    ! OUTPUT RATES OF PRODUCTION ON LOSS (OUTPUT FREQUENCY SET IN MODEL.PARAMETERS)
    time = int( t )

    elapsed = int( t-modelStartTime )
    if ( mod( elapsed, ratesOutputStepSize ) == 0 ) then
      call outputRates( prodIntSpecies, prodArrayLen, t, productionRates, 1 )
      call outputRates( reacIntSpecies, lossArrayLen, t, lossRates, 0 )
    end if

    ! OUTPUT JACOBIAN MATRIX (OUTPUT FREQUENCY SET IN MODEL PARAMETERS)
    write (*,*) 'time = ', time
    if ( mod( elapsed, jacobianOutputStepSize ) == 0 ) then
      call jfy( numSpec, numReac, speciesConcs, fy, t )
      call output_jfy( fy, t )
    end if

    call getConcForSpecInt( speciesConcs, SORNumber, concsOfSpeciesOfInterest )
    call outputSpeciesOutputRequired( t, concsOfSpeciesOfInterest )
    call outputPhotolysisRates( t )

    !OUTPUT INSTANTANEOUS RATES
    if ( mod( elapsed, irOutStepSize ) == 0 ) then
      call outputInstantaneousRates( time, numReac )
    end if

    ! OUTPUT FOR CVODE MAIN SOLVER
    write (57,*) t, ' ', iout (lnst), ' ', iout (lnfe), ' ', iout (lnetf)
    ! OUTPUT FOR SPARSE SOLVER
    write (61,*) t, ' ', iout (nfels), ' ', iout (njtv), ' ', iout (npe), ' ', iout (nps)
    ! OUTPUT STEP SIZE
    write (62,*) t, ' ', rout (3), ' ', rout (2)

    !OUTPUT ENVVAR VALUES
    call ro2sum( ro2, speciesConcs )
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
      fmt = "(///' SUNDIALS_ERROR: FCVODE()returned ier = ', I5, /, 'Linear Solver returned ier = ', I5) "
      write (stderr, fmt) ier, iout (15)
      ! free memory
      call FCVFREE()
      stop
    end if

    if ( ier == 0 ) then
      tminus1 = t
      tout = tout + timestepSize
      currentNumTimestep = currentNumTimestep + 1
    end if

  end do

  call FCVDKY( t, 1, z, ier )
  if ( ier /= 0 ) then
    fmt = "(///' SUNDIALS_ERROR: FCVDKY()returned ier = ', I4) "
    write (stderr, fmt) ier
    call FCVFREE()
    stop
  end if

  !   OUPUT FINAL MODEL CONCENTRATIONS FOR MODEL RESTART
  do species_counter = 1, numSpec
    write (53,*) speciesNames(species_counter), speciesConcs(species_counter)
  end do

  !   printing of final statistics desactivated - nobody finds it useful
  !   FINAL ON SCREEN OUTPUT
  fmt = "(//'Final statistics:'//" // &
        "' No. steps = ', I4, '   No. f-s = ', I4, " // &
        "'   No. J-s = ', I4, '   No. LU-s = ', I4/" // &
        "' No. nonlinear iterations = ', I4/" // &
        "' No. nonlinear convergence failures = ', I4/" // &
        "' No. error test failures = ', I4/) "

  write (*, fmt) iout (lnst), iout (LNFE), iout (lnje), iout (lnsetup), &
                 iout (lnni), iout (lncf), iout (lnetf )

  call SYSTEM_CLOCK( runEnd, rate )
  runTime = ( runEnd - runStart ) / rate
  write (*,*) 'Runtime = ', runTime

  !   deallocate all
  write (*,*) 'Deallocating memory.'
  !   deallocate CVODE internal data

  call FCVFREE()
  deallocate (speciesConcs, speciesNames, speciesNumber, z)
  deallocate (prodIntSpecies, returnArray, reacIntSpecies)
  deallocate (SORNumber, concsOfSpeciesOfInterest, prodIntName, reacIntName, speciesOutputRequired)
  deallocate (fy, ir)
  deallocate (lossRates, productionRates)
  deallocate (clhs, crhs, ccoeff)
  deallocate (prodArrayLen)
  deallocate (lossArrayLen)
  !   deallocate data allocated before in input functions(inputFunctions.f)
  !   deallocate arrays from module constraints
  call deallocateConstrainedSpecies()
  !   deallocate arrays from module species
  call deallocateSpeciesList
  !   deallocate arrays from module chemicalConstraints
  deallocate (dataX, dataY, dataY2, dataFixedY, constrainedConcs, constrainedName)
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

  integer(kind=NPI) :: ipar (*), ier, neq, i, np
  integer :: j
  real(kind=DP) :: t, h, rpar(*), y(*), v(*), fjv(*), fy(*), work(*), delta, deltaV, dummy
  real(kind=DP), allocatable :: yPlusV(:), yPlusVi(:)
  np = getNumberOfSpecies()
  allocate (yPlusV(np), yPlusVi(np))

  neq = ipar(1)
  delta = 1.00d-03
  ! fake using variables h and work, to avoid a warning (they are required by CVODE code)
  h = h
  dummy = work(1)

  ! calculate y + delta v
  j = 0
  do i = 1, neq
    deltaV = delta * v(i)
    yPlusV (i) = y(i) + deltaV
  end do

  ! get f(y + delta v)
  call FCVFUN( t, yPlusV, yPlusVi, ipar, rpar, ier )

  ! JVminus1 + deltaJV
  do i = 1, neq
    fjv(i) = ( yPlusVi(i) - fy(i) ) / delta
  end do
  deallocate (yPlusV, yPlusVi)

  return
end subroutine FCVJTIMES

!     ---------------------------------------------------------------
subroutine FCVFUN( t, y, ydot, ipar, rpar, ier )
  use types_mod
  use species
  use constraints
  use reactionStructure
  use chemicalConstraints
  use interpolationFunctions_mod, only : getConstrainedQuantAtT2D
  use constraintFunctions_mod
  use solverFunctions_mod, only : resid

  ! Fortran routine for right-hand side function.
  implicit none
  !
  integer(kind=NPI) :: ipar(*), ier, nConSpec, np, numReac
  real(kind=DP) :: t, y(*), ydot(*), rpar (*), concAtT, dummy
  real(kind=DP), allocatable :: dy(:), z(:)
  integer(kind=NPI) :: i

  np = ipar(1) + numberOfConstrainedSpecies
  numReac = ipar(2)
  dummy = rpar(1)

  nConSpec = numberOfConstrainedSpecies
  allocate (dy(np), z(np))

  do i = 1, numberOfConstrainedSpecies
    if ( i <= numberOfVariableConstrainedSpecies ) then
      call getConstrainedQuantAtT2D( t, datax, datay, datay2, speciesNumberOfPoints(i), concAtT, &
                                     1, i, maxNumberOfDataPoints, numberOfVariableConstrainedSpecies )
    else
      concAtT = dataFixedY(i - numberOfVariableConstrainedSpecies)
    end if
    constrainedConcs(i) = concAtT
    call setConstrainedConc( i, concAtT )

  end do

  call addConstrainedSpeciesToProbSpec( y, z, numberOfConstrainedSpecies, constrainedSpecies, ipar(1), constrainedConcs )

  call resid( np, numReac, t, z, dy, clhs, crhs, ccoeff, lhs_size, rhs_size )

  call removeConstrainedSpeciesFromProbSpec( dy, ydot, constrainedSpecies )

  deallocate (dy, z)
  ier = 0

  return
end subroutine FCVFUN
