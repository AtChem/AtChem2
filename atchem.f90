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
  use zenithData
  use productionAndLossRates
  use envVars
  use SZACalcVars
  use date
  use directories, only : output_dir, param_dir
  use storage, only : maxSpecLength, maxPhotoRateNameLength
  use solver_params_mod
  use model_params_mod
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
  integer :: meth, itmeth, iatol, itask, currentNumTimestep
  integer(kind=NPI) :: iout(21), ipar(10)
  integer(kind=NPI) :: neq
  real(kind=DP) :: t, tout
  real(kind=DP) :: rout(6)
  real(kind=DP) :: rpar(1)

  ! walltime variables
  integer(kind=QI) :: runStart, runEnd, runTime, clockRate
  ! number of species and reactions
  integer(kind=NPI) :: numSpec, numReac

  !   DECLARATIONS FOR RATES OF PRODUCTION AND LOSS
  integer(kind=NPI), allocatable :: prodIntSpecies(:,:), reacIntSpecies(:,:), prodIntSpeciesLengths(:), reacIntSpeciesLengths(:)
  real(kind=DP), allocatable :: concsOfSpeciesOfInterest(:)
  character(len=maxSpecLength), allocatable :: prodIntName(:), reacIntName(:)
  character(len=maxSpecLength), allocatable :: speciesOfInterest(:)
  ! simulation output time variables
  integer(kind=QI) :: time, elapsed
  ! species concentrations with and without constrained species
  real(kind=DP), allocatable :: speciesConcs(:)
  real(kind=DP), allocatable :: z(:)

  character(len=400) :: fmt

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

  !    ********************************************************************************************************
  !    MODEL SETUP AND CONFIGURATION
  !    ********************************************************************************************************

  call SYSTEM_CLOCK( runStart )

  call get_and_set_directories_from_command_arguments()

  ! Open files for output
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
  flush(6)

  call readNumberOfSpeciesAndReactions()
  numSpec = getNumberOfSpecies()
  numReac = getNumberOfReactions()

  ! Set array sizes = number of species
  allocate (speciesConcs(numSpec), z(numSpec))
  ! Set array sizes = number of reactions
  allocate (lossRates(numReac), productionRates(numReac), instantaneousRates(numReac))

  ! Read in reactions
  call readReactions()
  write (*,*)
  neq = numSpec

  ! Read species names and numbers

  call setSpeciesList( readSpecies() )
  write (*,*)

  ! Set initial concentrations for all species for which this info is provided, 0.0 for any unspecified.
  call readAndSetInitialConcentrations( speciesConcs )
  write (*,*)

  ! Read in photolysis rate information
  call readPhotolysisConstants()
  write (*,*)

  ! Read in JFac species, and set jfacSpeciesLine
  call readJFacSpecies()
  write (*,*)

  ! Read in product species of interest, and set up variables to hold these
  write (*, '(A)') ' Reading products of interest...'
  call readProductsOrReactantsOfInterest( trim( param_dir ) // '/productionRatesOutput.config', prodIntName )
  write (*, '(A)') ' Finished reading products of interest.'

  allocate (prodIntSpecies(size( prodIntName ), size( crhs, 2 )))
  allocate (prodIntSpeciesLengths(size( prodIntName )))
  ! Fill prodIntSpecies(:,1) with a list of the numbers of the interesting product species, with numbers from their ordering in speciesList
  call matchNameToNumber( getSpeciesList(), prodIntName, prodIntSpecies(:, 1) )
  ! prodIntSpecies will eventually hold one row per interesting product species, with the first element being the number
  ! of that species, and the remaining elements being the numbers of the reactions in which that species is a product

  ! Fill the remaining elements of each row of prodIntSpecies with the numbers of the reactions in which that species is a product
  call findReactionsWithProductOrReactant( prodIntSpecies, crhs, prodIntSpeciesLengths )
  write (*, '(A, I0)') ' products of interest (number of species found): ', size( prodIntName )
  write (*,*)

  ! Read in reactant species of interest, and set up variables to hold these
  write (*, '(A)') ' Reading reactants of interest...'
  call readProductsOrReactantsOfInterest( trim( param_dir ) // '/lossRatesOutput.config', reacIntName )
  write (*, '(A)') ' Finished reading reactants of interest.'

  allocate (reacIntSpecies(size( reacIntName ), size( clhs, 2 )))
  allocate (reacIntSpeciesLengths(size( reacIntName )))
  ! Fill reacIntSpecies(:,1) with a list of the numbers of the interesting reaction species, with numbers from their ordering in speciesList
  call matchNameToNumber( getSpeciesList(), reacIntName, reacIntSpecies(:, 1) )
  ! reacIntSpecies will eventually hold one row per interesting reactant species, with the first element being the number
  ! of that species, and the remaining elements being the numbers of the reactions in which that species is a reactant

  ! Fill the remaining elements of each row of reacIntSpecies with the numbers of the reactions in which that species is a reactant
  call findReactionsWithProductOrReactant( reacIntSpecies, clhs, reacIntSpeciesLengths )
  write (*, '(A, I0)') ' reactants of interest (number of species found): ', size( reacIntName )
  write (*,*)


  ! Read in and set solver parameters
  call set_solver_parameters( getParametersFromFile( trim( param_dir ) // "/solver.parameters" ) )
  write (*,*)

  ! Read in and set model parameters
  call set_model_parameters( getParametersFromFile( trim( param_dir ) //  "/model.parameters" ) )
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

  ! fill speciesOfInterest with the names of species to output to concentration.output
  speciesOfInterest = readSpeciesOfInterest()

  ! Allocate concsOfSpeciesOfInterest
  allocate ( concsOfSpeciesOfInterest(size( speciesOfInterest )))

  flush(stderr)
  !    ********************************************************************************************************
  !    CONSTRAINTS
  !    ********************************************************************************************************

  write (*,*)
  call readPhotoRates()
  write (*,*)

  call readSpeciesConstraints( t, speciesConcs )

  write (*,*)
  concsOfSpeciesOfInterest = getConcForSpeciesOfInterest( speciesConcs, speciesOfInterest )
  call outputSpeciesOfInterest( t, speciesOfInterest, concsOfSpeciesOfInterest )

  ! This outputs z, which is speciesConcs with all the constrained species removed.
  call removeConstrainedSpeciesFromProbSpec( speciesConcs, getConstrainedSpecies(), z )

  !   ADJUST PROBLEM SPECIFICATION TO GIVE NUMBER OF SPECIES TO BE SOLVED FOR (N - C = M)
  neq = numSpec - getNumberOfConstrainedSpecies()
  write (*, '(A30, I0) ') ' neq = ', neq
  write (*, '(A30, I0) ') ' numberOfConstrainedSpecies = ', getNumberOfConstrainedSpecies()

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
  write (*,*)
  call FCVMALLOC( t, z, meth, itmeth, iatol, rtol, atol, &
                  iout, rout, ipar, rpar, ier )
  if ( ier /= 0 ) then
    write (stderr, 30) ier
    30   format (///' SUNDIALS_ERROR: FCVMALLOC() returned ier = ', I5)
    stop
  end if

  call FCVSETIIN( 'MAX_NSTEPS', maxNumInternalSteps, ier )
  write (*, '(A, I0)') ' setting maxnumsteps ier = ', ier

  call FCVSETRIN( 'MAX_STEP', maxStep, ier )
  write (*, '(A, I0)') ' setting maxstep ier = ', ier
  write (*,*)

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
      write (stderr,*) 'Solver parameter conflict! Jv approximation cannot be used for dense solver.'
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

    ! Get concentrations for unconstrained species
    call FCVODE( tout, t, z, itask, ier )
    if ( ier /= 0 ) then
      write (*, '(A, I0)') ' ier POST FCVODE()= ', ier
    end if
    flush(6)

    time = int( t )
    write (*, '(A, I0)') ' time = ', time

    ! Get concentrations for constrained species and add to array for output
    call addConstrainedSpeciesToProbSpec( z, getConstrainedConcs(), getConstrainedSpecies(), speciesConcs )

    ! Output rates of production and loss (output frequency set in model.parameters)
    elapsed = int( t-modelStartTime )
    if ( mod( elapsed, ratesOutputStepSize ) == 0 ) then
      call outputRates( prodIntSpecies, prodIntSpeciesLengths, t, productionRates, 1_SI )
      call outputRates( reacIntSpecies, reacIntSpeciesLengths, t, lossRates, 0_SI )
    end if

    ! Output Jacobian matrix (output frequency set in model.parameters)
    if ( mod( elapsed, jacobianOutputStepSize ) == 0 ) then
      call jfy( numReac, speciesConcs, t )
    end if

    concsOfSpeciesOfInterest = getConcForSpeciesOfInterest( speciesConcs, speciesOfInterest )
    call outputSpeciesOfInterest( t, speciesOfInterest, concsOfSpeciesOfInterest )
    call outputPhotolysisRates( t )

    ! Output instantaneous rates
    if ( mod( elapsed, irOutStepSize ) == 0 ) then
      call outputInstantaneousRates( time )
    end if

    ! Output CVODE solver parameters and timestep sizes
    call outputSolverParameters( t, rout(3), rout(2), iout, solverType )

    ! Output envVar values
    ro2 = ro2sum( speciesConcs )
    call outputEnvVar( t )

    ! Error handling
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

  ! Output final model concentrations, in a usable format for model restart
  call outputFinalModelState( getSpeciesList(), speciesConcs )

  ! Final on-screen output
  fmt = "(//' Final statistics:'//" // &
        "' No. steps = ', I0, '   No. f-s = ', I0, " // &
        "'   No. J-s = ', I0, '   No. LU-s = ', I0/" // &
        "' No. nonlinear iterations = ', I0/" // &
        "' No. nonlinear convergence failures = ', I0/" // &
        "' No. error test failures = ', I0/) "

  write (*, fmt) iout (3), iout (4), iout (17), iout (8), &
                 iout (7), iout (6), iout (5)

  call SYSTEM_CLOCK( runEnd, clockRate )
  runTime = ( runEnd - runStart ) / clockRate
  write (*, '(A, I0)') ' Runtime = ', runTime

  write (*, '(A)') ' Deallocating memory.'
  ! deallocate CVODE internal data
  call FCVFREE()
  deallocate (speciesConcs, z)
  deallocate (prodIntSpecies, reacIntSpecies)
  deallocate (concsOfSpeciesOfInterest, prodIntName, reacIntName, speciesOfInterest)
  deallocate (instantaneousRates)
  deallocate (lossRates, productionRates)
  deallocate (clhs, clcoeff, crhs, crcoeff)
  deallocate (prodIntSpeciesLengths)
  deallocate (reacIntSpeciesLengths)
  ! deallocate data allocated before in input functions (inputFunctions.f90)
  ! deallocate arrays from module constraints
  call deallocateConstrainedConcs()
  call deallocateConstrainedSpecies()
  deallocate (dataX, dataY, dataY2, dataFixedY)
  deallocate (speciesNumberOfPoints)
  ! deallocate arrays from module species
  call deallocateSpeciesList()
  ! deallocate arrays from module envVars
  deallocate (envVarTypesNum, envVarNames, envVarTypes, envVarFixedValues)
  deallocate (envVarX, envVarY, envVarY2, envVarNumberOfPoints)
  ! deallocate arrays from module photolysisRates
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

  integer(kind=NPI) :: numConSpec, np, numReac
  real(kind=DP) :: dummy
  real(kind=DP), allocatable :: dy(:), z(:), constrainedConcs(:)
  integer(kind=NPI) :: i

  numConSpec = getNumberOfConstrainedSpecies()
  np = ipar(1) + numConSpec
  numReac = ipar(2)
  dummy = rpar(1)

  allocate (dy(np), z(np), constrainedConcs(numConSpec))

  ! for each constrained species...
  do i = 1, numConSpec
    ! if it's a variable-concentration constrained species,
    if ( i <= numberOfVariableConstrainedSpecies ) then
      call getConstrainedQuantAtT( t, datax, datay, datay2, speciesNumberOfPoints(i), &
                                   getSpeciesInterpMethod(), i, constrainedConcs(i) )
    else
      constrainedConcs(i) = dataFixedY(i - numberOfVariableConstrainedSpecies)
    end if
  end do

  call setConstrainedConcs( constrainedConcs )

  call addConstrainedSpeciesToProbSpec( y, constrainedConcs, getConstrainedSpecies(), z )

  call resid( numReac, t, z, dy, clhs, clcoeff, crhs, crcoeff )

  call removeConstrainedSpeciesFromProbSpec( dy, getConstrainedSpecies(), ydot )

  deallocate (dy, z)
  ier = 0

  return
end subroutine FCVFUN
