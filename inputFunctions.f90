! ******************************************************************** !
! AtChem -- module inputFunctions
!
!
! ******************************************************************** !
module inputFunctions_mod
contains

  ! -----------------------------------------------------------------
  ! Read in command line argument to direct output files to a given
  ! directory
  subroutine get_and_set_directories_from_command_arguments()
    use types_mod
    use directories
    implicit none

    integer(kind=QI) :: cmd_arg_count

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

    write (*, '(2A)') ' Output dir is ', trim( output_dir )
    write (*, '(2A)') ' Instantaneous rates dir is ', trim( instantaneousRates_dir )
    write (*, '(2A)') ' Parameter dir is ', trim( param_dir )

  end subroutine get_and_set_directories_from_command_arguments

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine readNumberOfSpeciesAndReactions()
    use types_mod
    use directories, only : param_dir
    use species, only : setNumberOfSpecies, setNumberOfReactions
    use storage, only : maxFilepathLength
    implicit none

    integer(kind=NPI) :: numSpec, numReac
    character(len=maxFilepathLength) :: fileLocation

    fileLocation = trim( param_dir ) // '/mechanism.reac'
    ! read in mechanism parameters
    call inquire_or_abort( fileLocation, 'readNumberOfSpeciesAndReactions()')

    open (10, file=fileLocation, status='old') ! input file
    read (10,*) numSpec, numReac
    close (10, status='keep')

    call setNumberOfSpecies( numSpec )
    call setNumberOfReactions( numReac )

    write (*, '(A, I0)') ' Number of Species   = ', numSpec
    write (*, '(A, I0)') ' Number of Reactions = ', numReac

  end subroutine readNumberOfSpeciesAndReactions

  ! -----------------------------------------------------------------
  ! outputs lhs_size and rhs_size, which hold the number of lines in
  ! modelConfiguration/mechanism.(reac/prod), excluding the first line
  ! and last line
  subroutine readReactions()
    use types_mod
    use directories, only : param_dir
    use reactionStructure
    implicit none

    integer(kind=NPI) :: lhs_size, rhs_size
    integer(kind=NPI) :: k, l, count
    integer(kind=IntErr) :: ierr

    call inquire_or_abort( trim( param_dir ) // '/mechanism.reac', 'getReactantAndProductListSizes()')
    lhs_size = count_lines_in_file( trim( param_dir ) // '/mechanism.reac', skip_first_line_in=.true. )
    call inquire_or_abort( trim( param_dir ) // '/mechanism.prod', 'getReactantAndProductListSizes()')
    rhs_size = count_lines_in_file( trim( param_dir ) // '/mechanism.prod', skip_first_line_in=.false. )

    allocate (clhs(2, lhs_size), crhs(2, rhs_size), clcoeff(lhs_size), crcoeff(rhs_size))

    write (*, '(A, I0)') ' Size of lhs = ', lhs_size
    write (*, '(A, I0)') ' Size of rhs = ', rhs_size
    write (*,*)
    write (*, '(A)') ' Reading reactants (lhs) from mechanism.reac...'
    call inquire_or_abort( trim( param_dir ) // '/mechanism.reac', 'readReactions()')
    open (10, file=trim( param_dir ) // '/mechanism.reac', status='old') ! input file for lhs of equations
    ! read data for lhs of equations
    ! lhs(1, i) contains the reaction number
    ! lhs(2, i) contains the species number of the reactant
    ! lhs(3, i) contains 1 as a constant factor (stoichiometric coefficient)
    count = 0
    read (10,*, iostat=ierr)
    read (10,*, iostat=ierr) k, l
    do while ( ierr == 0 )
      count = count + 1
      clhs(1, count) = k
      clhs(2, count) = l
      clcoeff(count) = 1.0
      read (10,*, iostat=ierr) k, l
    end do
    close (10, status='keep')

    write (*, '(A)') ' Reading products (rhs) from mechanism.prod...'
    call inquire_or_abort( trim( param_dir ) // '/mechanism.prod', 'readReactions()')
    open (11, file=trim( param_dir ) // '/mechanism.prod', status='old') ! input file for rhs of equations
    ! read data for rhs of equations
    ! rhs(1, i) contains the reaction number
    ! rhs(2, i) contains the species number of the product
    ! coeff(i) contains 1.0 as a constant factor (stoichiometric coefficient)
    count = 0
    ierr = 0
    read (11,*, iostat=ierr) k, l
    do while ( ierr == 0 )
      count = count + 1
      crhs(1, count) = k
      crhs(2, count) = l
      crcoeff(count) = 1.0
      read (11,*, iostat=ierr) k, l
    end do
    close (11, status='keep')

    write (*, '(A)') ' Finished reading lhs and rhs data.'

    return
  end subroutine readReactions

  ! ----------------------------------------------------------------- !
  ! ???
  function readSpecies() result ( speciesName )
    use types_mod
    use directories, only : param_dir
    use storage, only : maxSpecLength, maxFilepathLength
    use species, only : getNumberOfSpecies
    implicit none

    character(len=maxSpecLength), allocatable :: speciesName(:)
    integer(kind=NPI) :: j, dummy
    character(len=maxFilepathLength) :: fileLocation

    write (*, '(A)') ' Reading species names from mechanism.species...'
    fileLocation=trim( param_dir ) // '/mechanism.species'
    ! Read in species number and name from mC/mechanism.species to
    ! speciesName and sdummy (to be thrown).
    allocate(speciesName(getNumberOfSpecies()))
    call inquire_or_abort( fileLocation, 'readSpecies()')
    open (10, file=fileLocation) ! input file
    do j = 1, size ( speciesName )
      read (10,*) dummy, speciesName(j)
    end do
    close (10, status='keep')
    write (*, '(A)') ' Finished reading species names.'

    return
  end function readSpecies

  ! -----------------------------------------------------------------
  ! Reads in concentration per species from
  ! mC/initialConcentrations.config Checks that there aren't more
  ! inputs than species.  concSpeciesNames is filled with all species
  ! names of initial concentrations, concentration is filled with
  ! corresponding concentration VALUES
  subroutine readAndSetInitialConcentrations( speciesConcs )
    use types_mod
    use species, only : getNumberOfSpecies, getSpeciesList
    use directories, only : param_dir
    use storage, only : maxSpecLength, maxFilepathLength
    implicit none

    real(kind=DP), allocatable, intent(out) :: speciesConcs(:)
    real(kind=DP), allocatable :: concentration(:)
    character(len=maxSpecLength), allocatable :: concSpeciesNames(:)
    character(len=maxSpecLength) :: k
    character(len=maxFilepathLength) :: filename
    real(kind=DP) :: l
    integer(kind=NPI) :: numLines, i, nsp
    integer(kind=IntErr) :: ierr

    write (*, '(A)') ' Reading initial concentrations...'
    filename = trim( param_dir ) // '/initialConcentrations.config'
    ! Count lines in file, allocate appropriately
    numLines = count_lines_in_file( trim( filename ), .false. )
    nsp = getNumberOfSpecies()
    allocate (speciesConcs(nsp))
    if ( numLines > nsp ) then
      write (51, '(A)') 'Error:(number of species initial concentrations are set for) > (number of species) '
      write (51, '(A, I0)') '(number of species initial concentrations are set for) = ', numLines
      write (51, '(A, I0)') '(number of species) = ', nsp
    end if
    allocate (concSpeciesNames(numLines), concentration(numLines) )

    open (10, file=trim( filename ), status='old') ! input file for lhs of equations
    i = 0
    read (10,*, iostat=ierr) k, l
    do while ( ierr == 0 )
      i = i + 1
      concentration(i) = l
      concSpeciesNames(i) = k
      read (10,*, iostat=ierr) k, l
    end do
    close (10, status='keep')

    if ( numLines > 3 ) then
      write (*, '(I7, A, A, A, 1P e15.3)') 1, ' ', concSpeciesNames(1), ' ', concentration(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, A, A, A, 1P e15.3)') numLines, ' ', concSpeciesNames(numLines), ' ', concentration(numLines)
    else
      do i = 1, numLines
        write (*, '(I7, A, A, A, 1P e15.3)') i, ' ', concSpeciesNames(i), ' ', concentration(i)
      end do
    end if
    write (*, '(A)') ' Finished reading initial concentrations.'

    call setConcentrations( concSpeciesNames, concentration, getSpeciesList(), speciesConcs )

    return
  end subroutine readAndSetInitialConcentrations

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine setConcentrations( concSpeciesNames, inputConcentrations, refSpeciesNames, outputConcentrations )
    use types_mod
    use storage, only : maxSpecLength
    implicit none

    character(len=maxSpecLength), intent(in) :: concSpeciesNames(:), refSpeciesNames(:)
    real(kind=DP), intent(in) :: inputConcentrations(:)
    real(kind=DP), intent(out) :: outputConcentrations(:)
    character(len=maxSpecLength) :: k, m
    integer(kind=NPI) :: j, i
    logical :: match

    ! For each input species in concSpeciesNames (size concCounter),
    ! and matching value in inputConcentrations (size
    ! inputConcentrationsSize), look through refSpeciesNames (size
    ! numSpecies) for the number of this species in that list, then
    ! transer the value from inputConcentrations to
    ! outputConcentrations. If no match is found, output this to
    ! errors.output, but don't stop, just ignore the input value.
    ! Print outcome of each search into
    ! initialConditionsSetting.output.
    if ( size( concSpeciesNames ) /= size( inputConcentrations ) ) then
      stop "size(concSpeciesNames) /= size(inputConcentrations) in setConcentrations()."
    end if
    if ( size( refSpeciesNames ) /= size( outputConcentrations ) ) then
      stop "size(refSpeciesNames) /= size(outputConcentrations) in setConcentrations()."
    end if
    outputConcentrations(:) = 0.0
    do i = 1, size( concSpeciesNames )
      match = .false.
      k = concSpeciesNames(i)
      do j = 1, size( refSpeciesNames )
        m = refSpeciesNames(j)
        if ( m == k ) then
          match = .true.
          ! Set concentration in outputConcentrations
          outputConcentrations(j) = inputConcentrations(i)
          write (54, '(A, A, A, 1P e15.3)') 'match, m = k = ', m, ' concentration = ', inputConcentrations(i)
          exit
        else
          write (54, '(A, A, A, A, A, 1P e15.3)') 'no match, m = ', m, ' != k = ', k, ' concentration = ', inputConcentrations(i)!
        end if
      end do
      if ( match .eqv. .false. ) then
        ! If we reach this point, we've failed to find this species
        write (51,*) "Error in setConcentrations"
        write (51, '(A, A, A)') "Can't find species: ", k, " in species list"
      end if
    end do

    return
  end subroutine setConcentrations

  ! -----------------------------------------------------------------
  ! If modelConfiguration/photolysisConstants.config exists, then read
  ! in 3 values to fill ck, cl and str. Otherwise, call
  ! ReadPhotolysisRates to fill ck, cl, cmm, cnn, str and tf.
  subroutine readPhotolysisConstants()
    use types_mod
    use photolysisRates_mod, only : usePhotolysisConstants, nrOfPhotoRates, ck, cl, photoRateNames, &
                                    allocate_photolysis_rates_variables, size_of_j, allocate_photolysis_j
    use directories, only : param_dir
    use storage, only : maxFilepathLength
    implicit none

    integer(kind=NPI) :: i
    integer(kind=IntErr) :: ierr
    character(len=maxFilepathLength) :: filename
    logical :: file_exists
    logical :: allocated = .false.
    logical :: allocated_j = .false.

    ! Check whether file exists correctly in readPhotolysisConstants.
    filename = trim( param_dir ) // '/photolysisConstants.config'
    write (*, '(A)') ' Looking for photolysis constants file...'
    inquire(file=filename, exist=file_exists)
    if ( file_exists .eqv. .false. ) then
      usePhotolysisConstants = .false.
      write (*, '(A)') ' Photolysis constants file not found, trying photolysis rates file...'
      call readPhotolysisRates()
      return
    end if
    usePhotolysisConstants = .true.

    write (*, '(A)') ' Reading photolysis constants from file...'
    nrOfPhotoRates = count_lines_in_file( filename, .true. )
    if ( allocated .eqv. .false. ) then
      call allocate_photolysis_rates_variables()
      allocated = .true.
    end if
    open (10, file=filename, status='old', iostat=ierr)
    read (10,*) ! Ignore first line
    do i = 1, nrOfPhotoRates
      read (10,*, iostat=ierr) ck(i), cl(i), photoRateNames(i)
      if ( ierr /= 0 ) then
        stop 'readPhotolysisConstants(): error reading file'
      end if
    end do
    if ( allocated_j .eqv. .false. ) then
      size_of_j = maxval(ck)
      call allocate_photolysis_j()
      allocated_j = .true.
    end if
    close (10, status='keep')
    if ( nrOfPhotoRates > 3 ) then
      write (*, '(I7, 1P e15.3, 1P e15.3)') ck(1), cl(1), photoRateNames(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, 1P e15.3, 1P e15.3)') ck(nrOfPhotoRates), cl(nrOfPhotoRates), photoRateNames(nrOfPhotoRates)
    else
      do i = 1, nrOfPhotoRates
        write (*, '(I7, 1P e15.3, 1P e15.3)') ck(i), cl(i), photoRateNames(i)
      end do
    end if
    write (*, '(A)') ' Finished reading photolysis constants.'
    write (*, '(A, I0)') ' Number of photolysis rates: ', nrOfPhotoRates

    return
  end subroutine readPhotolysisConstants

  ! -----------------------------------------------------------------
  ! This is called from readPhotolysisConstants if
  ! modelConfiguration/photolysisConstants.config doesn't exist. It
  ! reads ck, cl, cmm, cnn, str, and tf from
  ! modelConfiguration/photolysisRates.config.
  subroutine readPhotolysisRates()
    use types_mod
    use photolysisRates_mod, only : nrOfPhotoRates, ck, cl, cmm, cnn, photoRateNames, transmissionFactor, &
                                    allocate_photolysis_rates_variables, size_of_j, allocate_photolysis_j
    use directories, only : param_dir
    use storage, only : maxFilepathLength
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    implicit none

    integer(kind=NPI) :: i
    integer(kind=IntErr) :: ierr
    character(len=maxFilepathLength) :: filename
    logical :: allocated = .false.
    logical :: allocated_j = .false.

    filename = trim( param_dir ) // '/photolysisRates.config'
    write (*, '(A)') ' Reading photolysis rates from file...'
    call inquire_or_abort( filename, 'readPhotolysisRates()')
    nrOfPhotoRates = count_lines_in_file( filename, .true. )
    if ( allocated .eqv. .false. ) then
      call allocate_photolysis_rates_variables()
      allocated = .true.
    end if
    open (10, file=filename, status='old')
    read (10,*) ! Ignore first line
    do i = 1, nrOfPhotoRates
      read (10,*, iostat=ierr) ck(i), cl(i), cmm(i), cnn(i), photoRateNames(i), transmissionFactor(i)
      if ( ierr /= 0 ) then
        stop 'readPhotolysisRates(): error reading file'
      end if
    end do
    close (10, status='keep')
    if ( allocated_j .eqv. .false. ) then
      size_of_j = maxval(ck)
      call allocate_photolysis_j()
      allocated_j = .true.
    end if
    if ( nrOfPhotoRates > 3 ) then
      write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') &
                                ck(1), cl(1), cmm(1), cnn(1), adjustr( photoRateNames(1) ), transmissionFactor(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') ck(nrOfPhotoRates), cl(nrOfPhotoRates), cmm(nrOfPhotoRates), &
                  cnn(nrOfPhotoRates), adjustr( photoRateNames(nrOfPhotoRates) ), transmissionFactor(nrOfPhotoRates)
    else
      do i = 1, nrOfPhotoRates
        write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') &
                                ck(i), cl(i), cmm(i), cnn(i), adjustr( photoRateNames(i) ), transmissionFactor(i)
      end do
    end if
    write (*, '(A)') ' Finished reading photolysis rates.'
    write (*, '(A, I0)') ' Number of photolysis rates: ', nrOfPhotoRates

    return
  end subroutine readPhotolysisRates

  ! -----------------------------------------------------------------
  ! Read modelConfiguration/JFacSpecies.config, and store this in
  ! jFacSpecies.  Test this against known species, and if it is known
  ! then set jfacSpeciesLine to that line number in photoRateNames
  subroutine readJFacSpecies()
    use types_mod
    use photolysisRates_mod
    use directories, only : param_dir
    implicit none

    integer(kind=NPI) :: i
    integer(kind=IntErr) :: ierr
    logical :: file_exists

    jfacSpeciesLine = 0
    write (*, '(A)') ' Reading JFacSpecies...'
    inquire(file=trim( param_dir ) // '/JFacSpecies.config', exist=file_exists)
    if ( file_exists .eqv. .false. ) then
      write (*, '(A)') " No JFacSpecies.config file exists, so setting jFacSpecies to ''"
      jFacSpecies = ''
    else
      open (10, file=trim( param_dir ) // '/JFacSpecies.config', status='old', iostat=ierr)
      read (10,*, iostat=ierr) jFacSpecies
      close (10, status='keep')
      ! Catch the case where the file is empty
      if ( ierr /= 0 ) then
        jFacSpecies = ''
      end if
    end if
    write (*, '(2A)') ' JFacSpecies = ', trim( jFacSpecies )
    write (*, '(A)') ' Finished reading JFacSpecies.'
    ! get line number for the JFac base species:
    do i = 1, nrOfPhotoRates
      if ( trim( photoRateNames(i) ) == trim( jFacSpecies ) ) then
        jfacSpeciesLine = i
      end if
    end do

    return
  end subroutine readJFacSpecies

  ! -----------------------------------------------------------------
  ! Read in contents of
  ! modelConfiguration/productionRatesOutput.config and
  ! modelConfiguration/lossRatesOutput.config, which contains a list
  ! of the species we want to have outputted to
  ! mC/production/lossRates.output Output the contents in r, with i as
  ! the length of r.
  subroutine readProductsOrReactantsOfInterest( filename, r )
    use types_mod
    use storage, only : maxSpecLength
    implicit none

    character(len=*), intent(in) :: filename
    character(len=maxSpecLength), allocatable, intent(out) :: r(:)
    integer(kind=NPI) :: length, j

    length = count_lines_in_file ( trim( filename ), .false. )
    allocate (r(length) )
    call read_in_single_column_string_file( trim( filename ), r, .false. )

    if ( length > 3 ) then
      write (*, '(I7, A, A)') 1, ' ', r(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, A, A)') length, ' ', r(length)
    else
      do j = 1, length
        write (*, '(I7, A, A)') j, ' ', r(j)
      end do
    end if

    return
  end subroutine readProductsOrReactantsOfInterest

  ! -----------------------------------------------------------------
  ! Read in parameters from file at input_file, and save the contents
  ! of each line to an element of the array
  function getParametersFromFile( input_file ) result ( parameterArray )
    use types_mod
    implicit none

    character(len=*), intent(in) :: input_file
    real(kind=DP), allocatable :: parameterArray(:)
    character(len=100) :: dummy
    integer(kind=DI) :: numValidEntries, counter

    call inquire_or_abort( input_file, 'getParametersFromFile()')

    open (10, file=input_file, status='old') ! input file
    numValidEntries = 0
    read (10,*) dummy
    do while ( dummy /= '-9999' )
      numValidEntries = numValidEntries + 1_DI
      read (10,*) dummy
    end do
    close (10, status='keep')
    allocate (parameterArray(numValidEntries))
    open (10, file=input_file, status='old') ! input file
    do counter = 1, numValidEntries
      read (10,*) parameterArray(counter)
    end do
    close (10, status='keep')

    return
  end function getParametersFromFile

  ! -----------------------------------------------------------------
  ! This function reads in data from environmentVariables.config, and
  ! sets envVarTypesNum for each one. In the case of a constrained
  ! variable, this also reads in the constraint data from
  ! environmentConstraints directory, the file named after the
  ! environmental variable.

  subroutine readEnvVar()
    use types_mod
    use envVars
    use directories, only : param_dir, env_constraints_dir
    use constraints, only : maxNumberOfDataPoints
    use storage, only : maxFilepathLength, maxEnvVarNameLength
    implicit none

    integer(kind=NPI) :: k
    integer(kind=SI) :: counter, i
    integer(kind=IntErr) :: ierr
    real(kind=DP) :: input1, input2
    character(len=10) :: dummy
    character(len=maxFilepathLength) :: fileLocationPrefix
    character(len=maxFilepathLength+maxEnvVarNameLength) :: fileLocation

    write (*, '(A)') ' Reading environment variables...'

    ! Count number of environment variables by reading in lines from
    ! file
    counter = int( count_lines_in_file( trim( param_dir ) // '/environmentVariables.config' ), SI )

    numEnvVars = counter

    ! Allocate storage for current values of env vars used for output
    allocate (currentEnvVarValues(numEnvVars) )

    write (*, '(A, I0)') ' Number of environment variables: ', numEnvVars
    allocate (envVarTypesNum(numEnvVars), envVarNames(numEnvVars), envVarTypes(numEnvVars) )
    allocate (envVarFixedValues(numEnvVars) )

    fileLocation = trim( param_dir ) // '/environmentVariables.config'
    call inquire_or_abort( fileLocation, 'readEnvVar()')
    open (10, file=fileLocation, status='old') ! input file
    ! Read in environment variables - if
    do i = 1_SI, numEnvVars
      read (10,*) dummy, envVarNames(i), envVarTypes(i)
      write (*, '(A, A4, A12, A20) ') ' ', dummy, envVarNames(i), adjustr( envVarTypes(i) )

      select case ( trim( envVarTypes(i) ) )
        case ('CALC')
          envVarTypesNum(i) = 1_SI
        case ('CONSTRAINED')
          envVarTypesNum(i) = 2_SI
        case ('NOTUSED')
          envVarTypesNum(i) = 4_SI
        case default
          envVarTypesNum(i) = 3_SI
          ! Copy 3rd column value to envVarFixedValues(i)
          read (envVarTypes(i),*) envVarFixedValues(i)
      end select
    end do
    close (10, status='keep')

    write (*, '(A)') ' Finished reading environment variables.'
    write (*,*)

    ! Allocate variables for next section
    allocate (envVarX(numEnvVars, maxNumberOfDataPoints))
    allocate (envVarY(numEnvVars, maxNumberOfDataPoints))
    allocate (envVarY2(numEnvVars, maxNumberOfDataPoints))
    allocate (envVarNumberOfPoints(numEnvVars))

    ! TODO: convert this to a command line input argument
    fileLocationPrefix = trim( env_constraints_dir ) // "/"
    ! If environment variable is constrained, read in constraint data
    write (*, '(A)') ' Checking for constrained environment variables...'
    do i = 1, numEnvVars
      if ( envVarTypes(i) == 'CONSTRAINED' ) then

        write (*, '(2A)') ' Reading constraint data for', trim( envVarNames(i) )

        fileLocation = trim( fileLocationPrefix ) // trim( envVarNames(i) )
        call inquire_or_abort( fileLocation, 'readEnvVar()')
        open (11, file=fileLocation, status='old')

        k = 0
        read (11,*, iostat=ierr) input1, input2
        do while ( ierr == 0 )
          k = k + 1
          envVarX(i, k) = input1
          envVarY(i, k) = input2
          read (11,*, iostat=ierr) input1, input2
        end do
        close (11, status='keep')
        envVarNumberOfPoints(i) = k
        write (*, '(A)') ' Finished reading constraint data.'
      end if
    end do
    write (*, '(A)') ' Finished checking for constrained environment variables.'
    write (*,*)

    return
  end subroutine readEnvVar

  ! ----------------------------------------------------------------- !
  ! ???
  function readSpeciesOfInterest() result ( r )
    use types_mod
    use species, only : getNumberOfSpecies
    use directories, only : param_dir
    use storage, only : maxSpecLength, maxFilepathLength
    implicit none

    character(len=maxSpecLength), allocatable :: r(:)
    character(len=maxFilepathLength) :: filename
    integer(kind=NPI) :: j, nsp, length

    filename = trim( param_dir ) // '/concentrationOutput.config'
    write (*, '(A)') ' Reading concentration output from file...'
    length = count_lines_in_file( trim( filename ) )
    allocate (r(length) )
    call read_in_single_column_string_file( trim( filename ), r )
    write (*, '(A)') ' Finished reading concentration output from file.'

    ! ERROR HANDLING
    nsp = getNumberOfSpecies()
    if ( length > nsp ) then
      write (51,*) 'Error: Number of (number of species output is required for) > (number of species) '
      write (51, '(A, I0)') '(number of species output is required for) = ', length
      write (51, '(A, I0)') '(number of species) = ', nsp
      stop 2
    end if

    write (*, '(A, I0, A)') ' Output required for concentration of ', length, ' species:'
    if ( length > 3 ) then
      write (*, '(I7, A, A)') 1, ' ', r(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, A, A)') length, ' ', r(length)
    else
      do j = 1, length
        write (*, '(I7, A, A)') j, ' ', r(j)
      end do
    end if

    return
  end function readSpeciesOfInterest

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine readPhotoRates()
    use types_mod
    use photolysisRates_mod, only : photoX, photoY, photoY2, numConPhotoRates, maxNrOfConPhotoRates, photoNumberOfPoints, &
                                constrainedPhotoRates, nrOfPhotoRates, photoRateNames, constrainedPhotoRatesNumbers, ck
    use directories, only : param_dir, env_constraints_dir
    use storage, only : maxPhotoRateNameLength, maxFilepathLength
    use constraints, only : maxNumberOfDataPoints
    implicit none

    integer(kind=NPI) :: i, k
    integer(kind=IntErr) :: ierr
    character(len=maxPhotoRateNameLength) :: string
    character(len=maxFilepathLength) :: fileLocationPrefix
    character(len=maxFilepathLength+maxPhotoRateNameLength) :: fileLocation

    ! Get names of photo rates
    call readPhotolysisConstants()
    write (*,*)

    numConPhotoRates = 0
    ! Get names of constrained photo rates
    write (*, '(A)') ' Reading names of constrained photolysis rates from file...'
    call inquire_or_abort( trim( param_dir ) // '/constrainedPhotoRates.config', 'readPhotoRates()')
    open (10, file=trim( param_dir ) // '/constrainedPhotoRates.config', status='old') ! input file
    do i = 1, maxNrOfConPhotoRates
      read (10,*, iostat=ierr) constrainedPhotoRates(i)
      if ( ierr /= 0 ) then
        exit
      end if
      numConPhotoRates = i
    end do
    close (10, status='keep')
    if ( numConPhotoRates > 3 ) then
      write (*,*) constrainedPhotoRates(1)
      write (*, '(A)') ' ...'
      write (*,*) constrainedPhotoRates(numConPhotoRates)
    else
      do i = 1, numConPhotoRates
        write (*,*) constrainedPhotoRates(i)
      end do
    end if
    write (*, '(A)') ' Finished reading names of constrained photolysis rates.'
    write (*, '(A, I0)') ' Number of constrained photorates: ', numConPhotoRates

    ! Get numbers of constrained photo rates
    do i = 1, numConPhotoRates
      do k = 1, nrOfPhotoRates
        if ( constrainedPhotoRates(i) == photoRateNames(k) ) then
          constrainedPhotoRatesNumbers(i) = ck(k)
        end if
      end do
    end do
    ! Allocate array size for storage of photolysis constraint data
    allocate (photoX (numConPhotoRates, maxNumberOfDataPoints) )
    allocate (photoY (numConPhotoRates, maxNumberOfDataPoints) )
    allocate (photoY2 (numConPhotoRates, maxNumberOfDataPoints) )
    allocate (photoNumberOfPoints(numConPhotoRates) )

    fileLocationPrefix = trim( env_constraints_dir ) // "/"

    ! Read in photolysis data
    if ( numConPhotoRates > 0 ) then
      write (*, '(A)') ' Reading in constraint data for photolysis rates...'
      do i = 1, numConPhotoRates
        string = constrainedPhotoRates(i)
        write (*,*) string, '...'
        fileLocation = trim( fileLocationPrefix ) // trim( string )
        call inquire_or_abort( fileLocation, 'readPhotoRates()')
        photoNumberOfPoints(i) = count_lines_in_file( fileLocation )
        if ( photoNumberOfPoints(i) > maxNumberOfDataPoints ) then
          photoNumberOfPoints(i) = maxNumberOfDataPoints
          write (*, '(A, I0, A)') ' Warning! Truncated constraint data to ', photoNumberOfPoints(i), ' points.'!
        end if
        open (11, file=fileLocation, status='old')
        do k = 1, photoNumberOfPoints(i)
          read (11,*) photoX(i, k), photoY(i, k) !, photoY2 (i, k)
        end do
        close (11, status='keep')
      end do
      write (*, '(A)') ' Finished reading constraint data for photolysis rates.'
    end if

    return
  end subroutine readPhotoRates

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine readSpeciesConstraints( t, y )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use species
    use constraints, only : maxNumberOfDataPoints, speciesNumberOfPoints, numberOfVariableConstrainedSpecies, &
                            numberOfFixedConstrainedSpecies, setNumberOfConstrainedSpecies, setConstrainedConcs, &
                            setConstrainedSpecies, getOneConstrainedSpecies, dataX, dataY, dataY2, dataFixedY
    use directories, only : param_dir, spec_constraints_dir
    use storage, only : maxSpecLength, maxFilepathLength
    use configFunctions_mod, only : getIndexWithinList
    use interpolationFunctions_mod, only : getConstrainedQuantAtT
    use interpolationMethod , only : getSpeciesInterpMethod
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP), intent(inout) :: y(:)
    real(kind=DP), allocatable :: concAtT(:)
    real(kind=DP) :: value
    integer(kind=NPI) :: i, j, id, numberOfSpecies, k, dataNumberOfPoints, numberOfConstrainedSpecies
    character(len=maxSpecLength), allocatable :: speciesNames(:), constrainedNames(:)
    character(len=maxSpecLength) :: name
    character(len=maxFilepathLength+maxSpecLength) :: fileLocation

    speciesNames = getSpeciesList()

    ! read in number of variable-concentration constrained species
    write (*, '(A)') ' Counting the variable-concentration species to be constrained (in file constrainedSpecies.config)...'
    numberOfVariableConstrainedSpecies = count_lines_in_file( trim( param_dir ) // '/constrainedSpecies.config' )
    write (*, '(A)') ' Finished counting the names of variable-concentration constrained species.'
    write (*, '(A, I0)') ' Number of names of variable-concentration constrained species: ', numberOfVariableConstrainedSpecies

    ! read in number of fixed-concentration constrained species
    write (*, '(A)') ' Counting the fixed-concentration species to be constrained (in file constrainedFixedSpecies.config)...'
    numberOfFixedConstrainedSpecies = count_lines_in_file( trim( param_dir ) // '/constrainedFixedSpecies.config' )
    write (*, '(A)') ' Finished counting the names of fixed-concentration constrained species.'
    write (*, '(A, I0)') ' Number of names of fixed-concentration constrained species: ', numberOfFixedConstrainedSpecies

    numberOfConstrainedSpecies = numberOfVariableConstrainedSpecies + numberOfFixedConstrainedSpecies
    allocate (constrainedNames(numberOfConstrainedSpecies) )
    call setNumberOfConstrainedSpecies( numberOfConstrainedSpecies )

    ! fill constrainedNames and constrainedSpecies with the names and
    ! numbers of variable-concentration constrained species
    if ( numberOfVariableConstrainedSpecies > 0 ) then
      write (*, '(A)') ' Reading in the names of variable-concentration constrained species...'
      call read_in_single_column_string_file( trim( param_dir ) // '/constrainedSpecies.config', constrainedNames )
      do i = 1, numberOfVariableConstrainedSpecies
        id = getIndexWithinList( speciesNames, constrainedNames(i) )
        if ( id /= 0 ) then
          call setConstrainedSpecies( i, id )
        else
          write (stderr, '(A, A, A)') 'Supplied constrained species ', &
                                      trim( constrainedNames(i) ), ' is not a species in the problem.'
          stop
        end if
      end do
      write (*, '(A)') ' Finished reading the names of variable-concentration constrained species'
    else
      write (*, '(A)') ' Skipped reading the names of variable-concentration constrained species'
    end if

    write (*, '(A, I0)') ' maxNumberOfDataPoints: ', maxNumberOfDataPoints
    write (*, '(A)') ' Allocating storage for variable-concentration constrained species...'
    allocate (dataX(numberOfVariableConstrainedSpecies, maxNumberOfDataPoints) )
    allocate (dataY(numberOfVariableConstrainedSpecies, maxNumberOfDataPoints) )
    allocate (dataY2(numberOfVariableConstrainedSpecies, maxNumberOfDataPoints) )
    write (*, '(A)') ' Finished allocating storage for variable-concentration constrained species.'

    if ( numberOfVariableConstrainedSpecies > 0 ) then
      if ( numberOfVariableConstrainedSpecies > 3 ) then
        write (*, '(I7, A, A)') 1, ' ', constrainedNames(1)
        write (*, '(A)') ' ...'
        write (*, '(I7, A, A)') numberOfVariableConstrainedSpecies, ' ', constrainedNames(numberOfVariableConstrainedSpecies)
      else
        do i = 1, numberOfVariableConstrainedSpecies
          write (*, '(I7, A, A)') i, ' ', constrainedNames(i)
        end do
      end if
    end if

    ! Read concentration data for variable constrained species
    write (*, '(A)') ' Reading concentration data for variable-concentration constrained species...'
    allocate (speciesNumberOfPoints(numberOfVariableConstrainedSpecies+numberOfFixedConstrainedSpecies) )
    if ( numberOfVariableConstrainedSpecies > 0 ) then
      do i = 1, numberOfVariableConstrainedSpecies
        if ( i < 3 .or. i == numberOfVariableConstrainedSpecies ) then
          write (*,*) constrainedNames(i), '...'
        else
          if ( i == 2 ) write (*, '(A)') ' ...'
        end if

        fileLocation = trim( spec_constraints_dir ) // '/' // trim( constrainedNames(i) )
        ! Count lines in file
        dataNumberOfPoints = count_lines_in_file( fileLocation )
        if ( dataNumberOfPoints > maxNumberOfDataPoints ) then
          dataNumberOfPoints = maxNumberOfDataPoints
          write (*, '(A, I0, A)') ' Warning! Truncated constraint data to ', dataNumberOfPoints, ' points.'!
        end if
        speciesNumberOfPoints(i) = dataNumberOfPoints
        ! Read contents of file
        call inquire_or_abort( fileLocation, 'readSpeciesConstraints()')
        open (13, file=fileLocation, status='old')
        do k = 1, dataNumberOfPoints
          read (13,*) dataX(i, k), dataY(i, k) !, dataY2(i, k)
        end do
        close (13, status='keep')
      end do
    end if

    ! Read in names and concentration data for fixed constrained species
    allocate (dataFixedY(numberOfFixedConstrainedSpecies))
    fileLocation = trim( param_dir ) // '/constrainedFixedSpecies.config'
    write (*, '(A)') ' Reading in the names and concentration of the fixed constrained species ' // &
                '(in file constrainedFixedSpecies.config)...'
    call inquire_or_abort( fileLocation, 'readSpeciesConstraints()')
    open (14, file=fileLocation, status='old') ! input file
    id = 0
    j = 0
    do i = 1, numberOfFixedConstrainedSpecies
      read (14,*) name, value
      id = getIndexWithinList( speciesNames, name )
      if (id/=0) then
        j = j+1
        constrainedNames(j + numberOfVariableConstrainedSpecies) = name
        dataFixedY(j) = value
        call setConstrainedSpecies( j + numberOfVariableConstrainedSpecies, id )
      end if
    end do
    close (14, status='keep')
    write (51, '(A, I0)') 'Number of fixed constrained species: ', numberOfFixedConstrainedSpecies

    if ( numberOfFixedConstrainedSpecies > 3 ) then
      write (*, '(I7, A, A, A, 1P e15.3)') 1, ' ', constrainedNames(1 + numberOfVariableConstrainedSpecies), ' ', dataFixedY(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, A, A, A, 1P e15.3)') numberOfFixedConstrainedSpecies, ' ', &
                  constrainedNames(numberOfFixedConstrainedSpecies + numberOfVariableConstrainedSpecies), ' ', &
                  dataFixedY(numberOfFixedConstrainedSpecies)
    else
      do i = 1, numberOfFixedConstrainedSpecies
        write (*, '(I7, A, A, A, 1P e15.3)') i, ' ', constrainedNames(i + numberOfVariableConstrainedSpecies), ' ', &
                    dataFixedY(i)
      end do
    end if
    write (*, '(A)') ' Finished reading in the names and concentration of fixed-concentration species.'

    write (51, '(A, I0)') "Total number of constrained species: ", numberOfConstrainedSpecies

    ! Error handling
    numberOfSpecies = getNumberOfSpecies()
    if ( numberOfConstrainedSpecies >= numberOfSpecies ) then
      write (51,*) "Error: number of constrained species >= number of species "
      write (51, '(A, I0)') "number of constrained species = ", numberOfConstrainedSpecies
      write (51, '(A, I0)') "number of species = ", numberOfSpecies
      stop 2
    end if

    write (*, '(A)') ' Finished reading constrained species.'

    ! Initialise concentrations of constrained species
    write (*, '(A)') ' Initialising concentrations of constrained species...'
    allocate (concAtT(numberOfConstrainedSpecies))
    do i = 1, numberOfConstrainedSpecies
      if ( i <= numberOfVariableConstrainedSpecies ) then
        call getConstrainedQuantAtT( t, datax, datay, datay2, speciesNumberOfPoints(i), getSpeciesInterpMethod(), i, concAtT(i) )
      else
        concAtT(i) = dataFixedY(i - numberOfVariableConstrainedSpecies)
      end if
      y(getOneConstrainedSpecies(i)) = concAtT(i)
    end do
    call setConstrainedConcs( concAtT )
    write (*, '(A)') ' Finished initialising concentrations of constrained species.'

    return
  end subroutine readSpeciesConstraints

  ! ----------------------------------------------------------------- !
  ! ???
  function count_lines_in_file( filename, skip_first_line_in ) result ( counter )
    use types_mod
    implicit none

    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: skip_first_line_in
    integer(kind=NPI) :: counter
    logical :: skip_first_line
    character(len=10) :: dummy
    integer(kind=IntErr) :: ierr

    ! Set default to not skip first line
    if (.not. present( skip_first_line_in ) ) then
      skip_first_line = .false.
    else
      skip_first_line = skip_first_line_in
    end if
    call inquire_or_abort( filename, 'count_lines_in_file()')
    counter = 0
    ierr = 0
    open (11, file=filename, status='old')
    if ( skip_first_line ) read (11,*, iostat=ierr) dummy
    do while (ierr==0)
      counter = counter + 1
      read (11,*, iostat=ierr) dummy
    end do
    close (11, status='keep')
    ! Remove 1 from counter, as the last wasn't used
    counter = counter - 1
    ! Handle the case where skip_first_line==.true. and there was no
    ! contents: return 0.
    if ( counter == -1 ) counter = 0

  end function count_lines_in_file

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine read_in_single_column_string_file( filename, output_vector, skip_first_line_in )
    use types_mod
    use storage, only : maxSpecLength
    implicit none

    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: output_vector(:)
    logical, intent(in), optional :: skip_first_line_in
    logical :: skip_first_line
    character(len=maxSpecLength) :: c
    integer(kind=NPI) :: i
    integer(kind=IntErr) :: ierr

    ! Set default to not skip first line
    if ( .not. present(skip_first_line_in)) then
      skip_first_line = .false.
    else
      skip_first_line = skip_first_line_in
    end if
    call inquire_or_abort( filename, 'read_in_single_column_string_file()')
    open (10, file=filename, status='old')
    ! Skip first line if necessary.
    if ( skip_first_line ) read (11,*, iostat=ierr) c
    ! Loop over all lines of the file, and add each entry to r(i) Then
    ! check we don't have more species of interest than total species
    i = 0
    read (10,*, iostat=ierr) c
    do while ( ierr == 0 )
      i = i + 1
      output_vector(i) = c
      read (10,*, iostat=ierr) c
    end do
    close (10, status='keep')

  end subroutine read_in_single_column_string_file

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine inquire_or_abort( filename, calling_subroutine )
    implicit none

    character(len=*) :: filename, calling_subroutine
    logical :: file_exists

    inquire(file=filename, exist=file_exists)
    if ( file_exists .eqv. .false. ) then
      write (*, '(A)') trim( calling_subroutine ) // ": No file '" // trim( filename ) // "' exists, so aborting."
      stop
    end if

  end subroutine inquire_or_abort

end module inputFunctions_mod
