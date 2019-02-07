! -----------------------------------------------------------------------------
!
! Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
! Peter Jimack, Mike Pilling
!
! Copyright (c) 2017 - 2018 Sam Cox, Roberto Sommariva
!
! This file is part of the AtChem2 software package.
!
! This file is covered by the MIT license which can be found in the file
! LICENSE.md at the top level of the AtChem2 distribution.
!
! -----------------------------------------------------------------------------

! ******************************************************************** !
!
!                        ATCHEM2 -- MAIN PROGRAM                        !
!
! ******************************************************************** !

PROGRAM ATCHEM2

  use, intrinsic :: iso_fortran_env, only : stderr => error_unit
  use iso_c_binding
  use types_mod
  use species_mod
  use constraints_mod
  use interpolation_method_mod
  use reaction_structure_mod
  use photolysis_rates_mod, only : photoX, photoY, photoNumberOfPoints, PR_type
  use reaction_rates_mod
  use env_vars_mod
  use date_mod, only : calcInitialDateParameters, calcCurrentDateParameters
  use directories_mod, only : output_dir, configuration_dir
  use storage_mod, only : maxSpecLength, maxPhotoRateNameLength, maxFilepathLength
  use solver_params_mod
  use model_params_mod
  use argparse_mod
  use input_functions_mod
  use config_functions_mod
  use output_functions_mod
  use constraint_functions_mod, only : addConstrainedSpeciesToProbSpec, removeConstrainedSpeciesFromProbSpec
  use solver_functions_mod, only : jfy, proc
  implicit none

  !
  ! interface to linux API
  interface
    function dlopen( filename, mode ) bind ( c, name="dlopen" )
      ! void *dlopen(const char *filename, int mode);
      use iso_c_binding
      implicit none
      type(c_ptr) :: dlopen
      character(c_char), intent(in) :: filename(*)
      integer(c_int), value :: mode
    end function

    function dlsym( handle, name ) bind ( c, name="dlsym" )
      ! void *dlsym(void *handle, const char *name);
      use iso_c_binding
      implicit none
      type(c_funptr) :: dlsym
      type(c_ptr), value :: handle
      character(c_char), intent(in) :: name(*)
    end function

    function dlclose( handle ) bind ( c, name="dlclose" )
      ! int dlclose(void *handle);
      use iso_c_binding
      implicit none
      integer(c_int) :: dlclose
      type(c_ptr), value :: handle
    end function
  end interface

  ! *****************************************************************
  ! DECLARATIONS
  ! *****************************************************************

  ! Declarations for solver parameters
  integer(kind=QI) :: ier
  integer :: meth, itmeth, iatol, itask, currentNumTimestep
  integer(kind=NPI) :: iout(21), ipar(10)
  integer(kind=NPI) :: neq
  real(kind=DP) :: t, tout
  real(kind=DP) :: rout(6), rpar(1)

  ! Walltime variables
  integer(kind=QI) :: runStart, runEnd, runTime, clockRate
  ! Number of species and reactions
  integer(kind=NPI) :: numSpec, numReac

  ! Declarations for detailed rates output
  type(reaction_frequency_pair) :: invalid_reaction_frequency_pair
  type(reaction_frequency_pair), allocatable :: reacDetailedRatesSpecies(:,:), prodDetailedRatesSpecies(:,:)
  integer(kind=NPI), allocatable :: reacDetailedRatesSpeciesLengths(:), prodDetailedRatesSpeciesLengths(:)
  integer(kind=NPI), allocatable :: detailedRatesSpecies(:)
  character(len=maxSpecLength), allocatable :: detailedRatesSpeciesName(:)
  ! Declarations for concentration outputs
  character(len=maxSpecLength), allocatable :: speciesOfInterest(:)
  ! simulation output time variables
  integer(kind=QI) :: time, elapsed
  ! species concentrations with and without constrained species
  real(kind=DP), allocatable :: speciesConcs(:)
  real(kind=DP), allocatable :: z(:)

  character(len=400) :: fmt

  type(c_ptr) :: handle
  character(len=maxFilepathLength) :: library
  type(c_funptr) :: proc_addr
  integer :: closure
  integer(c_int), parameter :: rtld_lazy=1 ! value extracted from the C header file
  integer(c_int), parameter :: rtld_now=2 ! value extracted from the C header file


  ! *****************************************************************
  ! Explicit declaration of FCVFUN() interface, which is a
  ! user-supplied function to CVODE.
  interface

    subroutine FCVFUN( t, y, ydot, ipar, rpar, ier )
      use types_mod
      use species_mod
      use constraints_mod
      use reaction_structure_mod
      use interpolation_functions_mod, only : getVariableConstrainedSpeciesConcentrationAtT
      use constraint_functions_mod
      implicit none

      ! Fortran routine for right-hand side function.
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

  ! *****************************************************************
  ! MODEL SETUP AND CONFIGURATION
  ! *****************************************************************

  call SYSTEM_CLOCK( runStart )

  ! Initialise some variables used by CVODE functions to invalid values
  iout(:) = -1_NPI
  ipar(:) = -1_NPI
  rout(:) = -1.0_DP
  rpar(:) = -1.0_DP

  write (*, '(A)') 'AtChem2 v1.2-dev'
  write (*,*)
  write (*, '(A)') '-------------'
  write (*, '(A)') ' Directories'
  write (*, '(A)') '-------------'
  call get_and_set_directories_from_command_arguments()
  write (*,*)

  ! Open files for output
  open (unit=50, file=trim( output_dir ) // "/speciesConcentrations.output")
  open (unit=51, file=trim( output_dir ) // "/errors.output")
  open (unit=52, file=trim( output_dir ) // "/environmentVariables.output")
  open (unit=53, file=trim( output_dir ) // "/finalModelState.output")
  open (unit=55, file=trim( output_dir ) // "/jacobian.output")
  open (unit=56, file=trim( output_dir ) // "/lossRates.output")
  open (unit=57, file=trim( output_dir ) // "/mainSolverParameters.output")
  open (unit=58, file=trim( output_dir ) // "/photolysisRates.output")
  open (unit=59, file=trim( output_dir ) // "/photolysisRatesParameters.output")
  open (unit=60, file=trim( output_dir ) // "/productionRates.output")
  flush(6)

  library = trim( shared_library )
  handle = dlopen(trim( library )//c_null_char, RTLD_LAZY)
  if (.not. c_associated(handle)) then
    write(*, '(2A)') 'Unable to load DLL ', trim( library )
    stop
  end if

  proc_addr=dlsym( handle, "update_p"//c_null_char )
  if ( .not. c_associated(proc_addr) ) then
    write(*,*) 'Unable to load the procedure update_p'
    stop
  end if
  call c_f_procpointer( proc_addr, proc )

  write (*, '(A)') '-----------------------'
  write (*, '(A)') ' Species and reactions'
  write (*, '(A)') '-----------------------'
  call readNumberOfSpeciesAndReactions()
  write (*,*)
  numSpec = getNumberOfSpecies()
  numReac = getNumberOfReactions()

  ! Set array sizes = number of species
  allocate (speciesConcs(numSpec), z(numSpec))
  ! Set array size = number of reactions
  allocate (reactionRates(numReac))

  ! Read in reactions
  call readReactions()
  write (*,*)
  neq = numSpec

  ! Read species names and numbers
  call setSpeciesList( readSpecies() )
  write (*,*)

  ! Set initial concentrations for all species for which this info is
  ! provided, 0.0 for any unspecified.
  call readAndSetInitialConcentrations( speciesConcs )
  write (*,*)

  write (*, '(A)') '----------------------------------------'
  write (*, '(A)') ' Species requiring detailed rate output'
  write (*, '(A)') '----------------------------------------'

  ! Read in species requiring detailed reats output, and set up variables to
  ! hold these
  write (*, '(A)') ' Reading which species require detailed rate output...'
  call readDetailedRatesSpeciesNames( trim( configuration_dir ) // '/outputRates.config', detailedRatesSpeciesName )
  write (*, '(A)') ' Finished reading which species require detailed rate output.'

  allocate (detailedRatesSpecies(size( detailedRatesSpeciesName )))
  allocate (reacDetailedRatesSpecies(size( detailedRatesSpeciesName ), size( clhs, 2 )))
  allocate (prodDetailedRatesSpecies(size( detailedRatesSpeciesName ), size( crhs, 2 )))
  invalid_reaction_frequency_pair = reaction_frequency_pair(-1_NPI, 0_NPI)
  reacDetailedRatesSpecies(:,:) = invalid_reaction_frequency_pair
  prodDetailedRatesSpecies(:,:) = invalid_reaction_frequency_pair
  allocate (reacDetailedRatesSpeciesLengths(size( detailedRatesSpeciesName )))
  allocate (prodDetailedRatesSpeciesLengths(size( detailedRatesSpeciesName )))

  ! Fill detailedRatesSpecies with a list of the numbers of the
  ! species requiring detailed rates output, with numbers from their ordering in
  ! speciesList
  call matchNameToNumber( getSpeciesList(), detailedRatesSpeciesName, detailedRatesSpecies )

  ! reac/prodDetailedRatesSpecies will each eventually hold one row per species
  ! requiring detailed rate output, with the first element being the number of that
  ! species, and the remaining elements being the numbers of the
  ! reactions in which that species appears as a reactant/product respectively
  !
  ! Fill the remaining elements of each row of reac/prodDetailedRatesSpecies with the
  ! numbers of the reactions in which that species appears as a reactant/product respectively
  call findReactionsWithProductOrReactant( detailedRatesSpecies, clhs, reacDetailedRatesSpecies, reacDetailedRatesSpeciesLengths )
  call findReactionsWithProductOrReactant( detailedRatesSpecies, crhs, prodDetailedRatesSpecies, prodDetailedRatesSpeciesLengths )
  write (*, '(A, I0)') ' Species requiring detailed rate output (number of species found): ', size( detailedRatesSpeciesName )
  write (*,*)

  ! Read in the numbers of RO2 species from mechanism.ro2
  call readRO2species()

  ! Read in and set solver parameters
  call set_solver_parameters( getParametersFromFile( trim( configuration_dir ) // "/solver.parameters" ) )
  write (*,*)

  ! Read in and set model parameters
  call set_model_parameters( getParametersFromFile( trim( configuration_dir ) //  "/model.parameters" ) )
  write (*,*)

  ! Set the parameters of MODULE date_mod to their value based on
  ! startDay, startMonth, startYear
  call calcInitialDateParameters()

  ! Hard coded solver parameters
  t = modelStartTime
  call calcCurrentDateParameters( t )
  tout = timestepSize + t
  ! Parameters for FCVMALLOC(). (Comments from cvode guide) meth
  ! specifies the basic integration: 1 for Adams (nonstiff) or 2 for
  ! BDF stiff)
  meth = 2
  ! itmeth specifies the nonlinear iteration method: 1 for functional
  ! iteration or 2 for Newton iteration.
  itmeth = 2
  ! IATOL specifies the type for absolute tolerance ATOL: 1 for scalar
  ! or 2 for array.  If IATOL= 3, the arguments RTOL and ATOL are
  ! ignored and the user is expected to subsequently call FCVEWTSET()
  ! and provide the function FCVEWT().
  iatol = 1

  ! Parameter for FCVODE(). Comment from cvode guide: ITASK is a task
  ! indicator and should be set to 1 for normal mode (overshoot TOUT
  ! and interpolate), or to 2 for one-step mode (return after each
  ! internal step taken)
  itask = 1

  ! currentNumTimestep counts the number of iterative steps. Set to
  ! zero. Calculation will terminate when
  ! currentNumTimestep>=maxNumTimesteps.
  currentNumTimestep = 0

  ! fill speciesOfInterest with the names of species to output to
  ! concentration.output
  write (*, '(A)') '---------------------'
  write (*, '(A)') ' Species of Interest'
  write (*, '(A)') '---------------------'
  speciesOfInterest = readSpeciesOfInterest()
  write (*,*)

  flush(stderr)

  ! *****************************************************************
  ! SET PHOTOLYSIS RATES
  ! *****************************************************************

  write (*, '(A)') '------------'
  write (*, '(A)') ' Photolysis'
  write (*, '(A)') '------------'
  call readPhotoRates()
  write (*,*)

  ! Read in environment variables (FIXED, CONSTRAINED, CALC or
  ! NOTUSED, see environmentVariables.config)
  write (*, '(A)') '-----------------------'
  write (*, '(A)') ' Environment variables'
  write (*, '(A)') '-----------------------'
  call readEnvVar()
  write (*,*)

  ! *****************************************************************
  ! SET CONSTRAINTS
  ! *****************************************************************

  write (*, '(A)') '-------------'
  write (*, '(A)') ' Constraints'
  write (*, '(A)') '-------------'
  call readSpeciesConstraints( t, speciesConcs )
  write (*,*)

  call outputSpeciesOfInterest( t, speciesOfInterest, speciesConcs )

  ! This outputs z, which is speciesConcs with all the constrained
  ! species removed.
  call removeConstrainedSpeciesFromProbSpec( speciesConcs, getConstrainedSpecies(), z )

  ! ADJUST PROBLEM SPECIFICATION TO GIVE NUMBER OF SPECIES TO BE
  ! SOLVED FOR (N - C = M)
  neq = numSpec - getNumberOfConstrainedSpecies()
  write (*, '(A)') '---------------'
  write (*, '(A)') ' Problem stats'
  write (*, '(A)') '---------------'
  write (*, '(A30, I0) ') ' neq = ', neq
  write (*, '(A30, I0) ') ' numberOfConstrainedSpecies = ', getNumberOfConstrainedSpecies()

  flush(stderr)

  ! *****************************************************************
  ! CONFIGURE SOLVER
  ! *****************************************************************

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

  ! SELECT SOLVER TYPE ACCORDING TO FILE INPUT
  ! SPGMR SOLVER
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

  if ( ier /= 0 ) then
    write (stderr, 40) ier
    40   format (///' SUNDIALS_ERROR: FCVDENSE() returned ier = ', I5)
    call FCVFREE()
    stop
  end if

  ! *****************************************************************
  ! RUN MODEL
  ! *****************************************************************

  write (*, '(A)') '-----------'
  write (*, '(A)') ' Model run'
  write (*, '(A)') '-----------'

  elapsed = int( t - modelStartTime )

  do while ( currentNumTimestep < maxNumTimesteps )

    call calcCurrentDateParameters( t )

    call outputPhotoRateCalcParameters( t )

    ! Output Jacobian matrix (output frequency set in
    ! model.parameters)
    if ( outputJacobian .eqv. .true. ) then
      if ( mod( elapsed, jacobianOutputStepSize ) == 0 ) then
        call jfy( numReac, speciesConcs, t )
      end if
    end if

    ! Get concentrations for unconstrained species
    call FCVODE( tout, t, z, itask, ier )
    if ( ier /= 0 ) then
      write (*, '(A, I0)') ' ier POST FCVODE()= ', ier
    end if
    flush(6)

    time = nint( t )
    elapsed = time - modelStartTime

    write (*, '(A, I0)') ' time = ', time

    ! Get concentrations for constrained species and add to array for
    ! output
    call addConstrainedSpeciesToProbSpec( z, getConstrainedConcs(), getConstrainedSpecies(), speciesConcs )

    ! Output rates of production and loss (output frequency set in
    ! model.parameters)
    if ( mod( elapsed, ratesOutputStepSize ) == 0 ) then
      call outputRates( detailedRatesSpecies, prodDetailedRatesSpecies, prodDetailedRatesSpeciesLengths, t, reactionRates, 1_SI )
      call outputRates( detailedRatesSpecies, reacDetailedRatesSpecies, reacDetailedRatesSpeciesLengths, t, reactionRates, 0_SI )
    end if

    call outputSpeciesOfInterest( t, speciesOfInterest, speciesConcs )
    call outputPhotolysisRates( t )

    ! Output reaction rates
    if ( mod( elapsed, irOutStepSize ) == 0 ) then
      call outputreactionRates( time )
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

  ! Output final model concentrations, in a usable format for model
  ! restart
  call outputFinalModelState( getSpeciesList(), speciesConcs )
  write (*,*)

  write (*, '(A)') '------------------'
  write (*, '(A)') ' Final statistics'
  write (*, '(A)') '------------------'

  ! Final on-screen output
  fmt = "(' No. steps = ', I0, '   No. f-s = ', I0, " // &
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

  ! *****************************************************************
  ! STOP MODEL
  ! *****************************************************************

  ! deallocate CVODE internal data
  call FCVFREE()
  deallocate (speciesConcs, z)
  deallocate (reacDetailedRatesSpecies, prodDetailedRatesSpecies)
  deallocate (detailedRatesSpeciesName, speciesOfInterest)
  deallocate (reactionRates)
  deallocate (clhs, clcoeff, crhs, crcoeff)
  deallocate (reacDetailedRatesSpeciesLengths, prodDetailedRatesSpeciesLengths)

  ! deallocate data allocated in inputFunctions.f90
  ! deallocate arrays from module constraints_mod
  call deallocateConstrainedConcs()
  call deallocateConstrainedSpecies()
  deallocate (dataX, dataY, dataFixedY)
  deallocate (speciesNumberOfPoints)

  ! deallocate arrays from module species_mod
  call deallocateSpeciesList()

  ! deallocate arrays from module env_vars_mod
  deallocate (envVarTypesNum, envVarNames, envVarTypes, envVarFixedValues)
  deallocate (envVarX, envVarY, envVarNumberOfPoints)

  ! deallocate arrays from module photolysis_rates_mod
  if ( PR_type == 2 .or. PR_type == 3) then
    deallocate (photoX, photoY, photoNumberOfPoints)
  end if

  ! Close output files and end program
  close (50)
  close (51)
  close (52)
  close (53)
  close (55)
  close (56)
  close (57)
  close (58)
  close (59)
  close (60)
  closure=dlclose(handle)

  stop

END PROGRAM ATCHEM2


! ******************************************************************** !
! CVODE function implementations
! ******************************************************************** !

! -------------------------------------------------------- !
!  Fortran routine for right-hand side function.
subroutine FCVFUN( t, y, ydot, ipar, rpar, ier )
  use types_mod
  use constraints_mod, only : getNumberOfConstrainedSpecies, numberOfVariableConstrainedSpecies, dataFixedY, &
                              getConstrainedSpecies, setConstrainedConcs
  use reaction_structure_mod, only : clhs, clcoeff, crhs, crcoeff
  use interpolation_method_mod, only : getSpeciesInterpMethod
  use interpolation_functions_mod, only : getVariableConstrainedSpeciesConcentrationAtT, getConstrainedPhotoRatesAtT
  use constraint_functions_mod, only : addConstrainedSpeciesToProbSpec, removeConstrainedSpeciesFromProbSpec
  use solver_functions_mod, only : resid
  implicit none

  real(kind=DP), intent(in) :: t, y(*)
  real(kind=DP), intent(out) :: ydot(*)
  integer(kind=NPI), intent(in) :: ipar(*)
  real(kind=DP), intent(in) :: rpar(*)
  integer(kind=NPI), intent(out) :: ier
  integer(kind=NPI) :: numConSpec, np, numReac, i
  real(kind=DP) :: dummy
  real(kind=DP), allocatable :: dy(:), z(:), constrainedConcs(:)

  numConSpec = getNumberOfConstrainedSpecies()
  np = ipar(1) + numConSpec
  numReac = ipar(2)
  dummy = rpar(1)
  ! TODO: these should be the same size on every call? If so, then better to allocate once and re-use
  ! TODO: rather than re-allocating each time.
  allocate (dy(np), z(np), constrainedConcs(numConSpec))

  ! for each constrained species...
  do i = 1, numConSpec
    ! if it's a variable-concentration constrained species,
    if ( i <= numberOfVariableConstrainedSpecies ) then
      call getVariableConstrainedSpeciesConcentrationAtT( t, i, constrainedConcs(i) )
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

! ******************************************************************** !
