! -----------------------------------------------------------------------------
!
! Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
! Peter Jimack, Mike Pilling
!
! Copyright (c) 2017 Sam Cox, Roberto Sommariva
!
! This file is part of the AtChem2 software package.
!
! This file is covered by the MIT license which can be found in the file
! LICENSE.md at the top level of the AtChem2 distribution.
!
! -----------------------------------------------------------------------------

! ******************************************************************** !
! ATCHEM2 -- MODULE inputFunctions
!
! This module contains functions controlling the reading of data from
! external files, apart from model.parameters and solver.parameters
! ******************************************************************** !
module input_functions_mod
contains

  ! -----------------------------------------------------------------
  ! Read in command line argument to direct output files to a given
  ! directory
  subroutine get_and_set_directories_from_command_arguments()
    use types_mod
    use directories_mod
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
    write (*, '(2A)') ' Species constraints dir is ', trim( spec_constraints_dir )
    write (*, '(2A)') ' Environment constraints dir is ', trim( env_constraints_dir )

  end subroutine get_and_set_directories_from_command_arguments

  ! ----------------------------------------------------------------- !
  ! Read the top line of mechanism.reac to get the number of species
  ! and number of reactions in the system
  subroutine readNumberOfSpeciesAndReactions()
    use types_mod
    use directories_mod, only : param_dir
    use species_mod, only : setNumberOfSpecies, setNumberOfReactions
    use storage_mod, only : maxFilepathLength
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
    use directories_mod, only : param_dir
    use reaction_structure_mod
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
      clcoeff(count) = 1.0_DP
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
      crcoeff(count) = 1.0_DP
      read (11,*, iostat=ierr) k, l
    end do
    close (11, status='keep')

    write (*, '(A)') ' Finished reading lhs and rhs data.'

    return
  end subroutine readReactions

  ! ----------------------------------------------------------------- !
  ! Read in all species names and numbers from mechanism.species
  function readSpecies() result ( speciesName )
    use types_mod
    use directories_mod, only : param_dir
    use storage_mod, only : maxSpecLength, maxFilepathLength
    use species_mod, only : getNumberOfSpecies
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
    use species_mod, only : getNumberOfSpecies, getSpeciesList
    use directories_mod, only : param_dir
    use storage_mod, only : maxSpecLength, maxFilepathLength
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
  ! Set the concentrations of all species - default to 0.0_DP.
  ! Called in readAndSetInitialConcentrations()
  subroutine setConcentrations( concSpeciesNames, inputConcentrations, refSpeciesNames, outputConcentrations )
    use types_mod
    use storage_mod, only : maxSpecLength
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
    outputConcentrations(:) = 0.0_DP
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
  ! This is called from readPhotoRates(). It reads photolysisNumbers from the first column of
  ! modelConfiguration/photolysisRates.config so that we know the numbers
  ! of all the photolysis rates and how many there are.
  subroutine readPhotolysisNumbers()
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use photolysis_rates_mod, only : totalNumPhotos, photoNumbers, photoRateNames, &
                                     allocate_photolysis_numbers_variables, &
                                     size_of_j, allocate_photolysis_j
    use directories_mod, only : param_dir
    use storage_mod, only : maxFilepathLength
    implicit none

    integer(kind=NPI) :: i
    integer(kind=IntErr) :: ierr
    character(len=maxFilepathLength) :: filename
    logical :: allocated = .false.
    logical :: allocated_j = .false.

    filename = trim( param_dir ) // '/photolysisRates.config'
    write (*, '(A)') ' Reading photolysis numbers from file...'
    call inquire_or_abort( filename, 'readPhotolysisNumbers()')
    totalNumPhotos = count_lines_in_file( filename, .true. )
    if ( allocated .eqv. .false. ) then
      call allocate_photolysis_numbers_variables()
      allocated = .true.
    end if
    open (10, file=filename, status='old')
    read (10,*) ! Ignore first line
    do i = 1, totalNumPhotos
      read (10,*, iostat=ierr) photoNumbers(i)
      if ( ierr /= 0 ) then
        stop 'readPhotolysisNumbers(): error reading file'
      end if
    end do
    if ( allocated_j .eqv. .false. ) then
      size_of_j = maxval(photoNumbers)
      call allocate_photolysis_j()
      allocated_j = .true.
    end if
    close (10, status='keep')
    if ( totalNumPhotos > 3 ) then
      write (*, '(I7)') photoNumbers(1)
      write (*, '(A)') ' ...'
      write (*, '(I7)') photoNumbers(totalNumPhotos)
    else
      do i = 1, totalNumPhotos
        write (*, '(I7)') photoNumbers(i)
      end do
    end if
    write (*, '(A)') ' Finished reading photolysis numbers.'
    write (*, '(A, I0)') ' Number of photolysis numbers: ', totalNumPhotos

    return
  end subroutine readPhotolysisNumbers

  ! -----------------------------------------------------------------
  ! Read in 3 values to fill ck, cl and str.
  subroutine readPhotolysisConstants()
    use types_mod
    use photolysis_rates_mod, only : photoRateNames, allocate_photolysis_constants_variables, size_of_j, allocate_photolysis_j, &
                                    constantPhotoJNumbers, constantPhotoValues, constantPhotoNames, numConstantPhotoRates
    use directories_mod, only : param_dir
    use storage_mod, only : maxFilepathLength
    implicit none

    integer(kind=NPI) :: i
    integer(kind=IntErr) :: ierr
    character(len=maxFilepathLength) :: filename
    logical :: allocated = .false.

    filename = trim( param_dir ) // '/photolysisConstants.config'
    write (*, '(A)') ' Reading photolysis constants from file...'
    !  Setting all the photolysis rates to their constant values from file
    !   Any photolysis rates not in the file will be set to zero.
    !   Only the variables for constants are required - the rest should be left uninitialised.
    !   So we should have a dedicated array to hold the constant (or zero) for each
    !   rate, with the index on the left being the photo number from photoNumbers.
    !   Future behaviour is dictated by the setting of usePhotolysisConstants = .true.
    if ( allocated .eqv. .false. ) then
      call allocate_photolysis_constants_variables()
      allocated = .true.
    end if
    open (10, file=filename, status='old', iostat=ierr)
    read (10,*) ! Ignore first line
    do i = 1, numConstantPhotoRates
      read (10,*, iostat=ierr) constantPhotoJNumbers(i), constantPhotoValues(i), constantPhotoNames(i)
      if ( ierr /= 0 ) then
        stop 'readPhotolysisConstants(): error reading file'
      end if
    end do
    close (10, status='keep')

    if ( numConstantPhotoRates > 3 ) then
      write (*, '(I7, 1P e15.3, 1P e15.3)') constantPhotoJNumbers(1), constantPhotoValues(1), constantPhotoNames(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, 1P e15.3, 1P e15.3)') constantPhotoJNumbers(numConstantPhotoRates), &
                                            constantPhotoValues(numConstantPhotoRates), constantPhotoNames(numConstantPhotoRates)
    else
      do i = 1, numConstantPhotoRates
        write (*, '(I7, 1P e15.3, 1P e15.3)') constantPhotoJNumbers(i), constantPhotoValues(i), constantPhotoNames(i)
      end do
    end if
    write (*, '(A)') ' Finished reading photolysis constants.'
    write (*, '(A, I0)') ' Number of photolysis rates: ', numConstantPhotoRates

    return
  end subroutine readPhotolysisConstants


  subroutine readPhotolysisConstraints()
    use types_mod
    use photolysis_rates_mod, only : photoRateNames, &
                                    constrainedPhotoNames, constrainedPhotoRatesNumbers, photoX, photoY, photoNumberOfPoints, &
                                    numConstrainedPhotoRates
    use directories_mod, only : param_dir, env_constraints_dir
    use storage_mod, only : maxFilepathLength, maxPhotoRateNameLength
    use constraints_mod, only : maxNumberOfPhotoDataPoints

    character(len=maxFilepathLength) :: fileLocationPrefix
    character(len=maxFilepathLength+maxPhotoRateNameLength) :: fileLocation
    integer(kind=NPI) :: i, k
    integer(kind=IntErr) :: ierr
    real(kind=DP) :: input1, input2
    character(len=maxPhotoRateNameLength) :: string

    ! Get names of constrained photo rates
    write (*, '(A)') ' Reading names of constrained photolysis rates from file...'
    call inquire_or_abort( trim( param_dir ) // '/constrainedPhotoRates.config', 'readPhotoConstraints()')
    allocate ( constrainedPhotoNames(numConstrainedPhotoRates), constrainedPhotoRatesNumbers(numConstrainedPhotoRates) )
    open (10, file=trim( param_dir ) // '/constrainedPhotoRates.config', status='old') ! input file
    do i = 1, numConstrainedPhotoRates
      read (10,*, iostat=ierr) constrainedPhotoNames(i)
      if ( ierr /= 0 ) then
        exit
      end if
    end do
    close (10, status='keep')

    if ( numConstrainedPhotoRates > 3 ) then
      write (*,*) constrainedPhotoNames(1)
      write (*, '(A)') ' ...'
      write (*,*) constrainedPhotoNames(numConstrainedPhotoRates)
    else
      do i = 1, numConstrainedPhotoRates
        write (*,*) constrainedPhotoNames(i)
      end do
    end if
    write (*, '(A)') ' Finished reading names of constrained photolysis rates.'
    write (*, '(A, I0)') ' Number of constrained photorates: ', numConstrainedPhotoRates

    ! TODO: REVIEW Get numbers of constrained photo rates
    ! Strip first character (which will be 'J'), then convert to integer
    do i = 1, numConstrainedPhotoRates
      read(constrainedPhotoNames(i)(2:maxPhotoRateNameLength),*,iostat=ierr ) constrainedPhotoRatesNumbers(i)
    end do

    fileLocationPrefix = trim( env_constraints_dir ) // "/"

    ! Read in photolysis data
    maxNumberOfPhotoDataPoints = 0_NPI
    if ( numConstrainedPhotoRates > 0 ) then
      write (*, '(A)') ' Reading in constraint data for photolysis rates...'
      do i = 1, numConstrainedPhotoRates
        string = constrainedPhotoNames(i)
        write (*,*) string, '...'
        fileLocation = trim( fileLocationPrefix ) // trim( string )
        maxNumberOfPhotoDataPoints = max( maxNumberOfPhotoDataPoints, count_lines_in_file( fileLocation ) )
      end do
      write (*, '(A, I0)') ' maximum number of photolysis constraint data points: ', maxNumberOfPhotoDataPoints
    end if
    ! Allocate array size for storage of photolysis constraint data
    allocate ( photoX(numConstrainedPhotoRates, maxNumberOfPhotoDataPoints) )
    allocate ( photoY(numConstrainedPhotoRates, maxNumberOfPhotoDataPoints) )
    allocate ( photoNumberOfPoints(numConstrainedPhotoRates) )
    if ( numConstrainedPhotoRates > 0 ) then
      do i = 1, numConstrainedPhotoRates
        string = constrainedPhotoNames(i)
        fileLocation = trim( fileLocationPrefix ) // trim( string )
        call inquire_or_abort( fileLocation, 'readPhotolysisConstraints()')
        photoNumberOfPoints(i) = count_lines_in_file( fileLocation )
        open (11, file=fileLocation, status='old')
        k = 0
        read (11,*, iostat=ierr) input1, input2
        do while ( ierr == 0 )
          k = k + 1
          photoX(i, k) = input1
          photoY(i, k) = input2
          read (11,*, iostat=ierr) input1, input2
        end do
        close (11, status='keep')
      end do
      write (*, '(A)') ' Finished reading constraint data for photolysis rates.'
    end if

  end subroutine readPhotolysisConstraints

  ! Returns an array of unconstrained photos rates, and a logical indicating whether there are none
  subroutine findUnconstrainedPhotos()
    use types_mod
    use photolysis_rates_mod, only : photoNumbers, constrainedPhotoRatesNumbers, numConstrainedPhotoRates, totalNumPhotos, &
                                     numUnconstrainedPhotoRates, unconstrainedPhotoNumbers, unconstrainedPhotosExist, photoNumbers
    implicit none

    integer(kind=NPI) :: i, j, counter
    logical :: this_number_unconstrained

    ! Find all elements in photoNumbers that are not in constrainedPhotoRatesNumbers
    unconstrainedPhotosExist = .false.
    numUnconstrainedPhotoRates = 0
    do j = 1, totalNumPhotos
      this_number_unconstrained = .true.
      do i = 1, numConstrainedPhotoRates
        if ( photoNumbers(j) == constrainedPhotoRatesNumbers(i) ) then
          this_number_unconstrained = .false.
          exit
        end if
      end do
      if ( this_number_unconstrained .eqv. .true. ) then
        ! This constrained number is unconstrained, so add to the list of unconstrained numbers
        numUnconstrainedPhotoRates = numUnconstrainedPhotoRates + 1
      end if
    end do
    allocate ( unconstrainedPhotoNumbers(numUnconstrainedPhotoRates))
    counter = 0_NPI
    write (*,*) 'maxval(photoNumbers):', maxval(photoNumbers)
    do j = 1, totalNumPhotos
      this_number_unconstrained = .true.
      do i = 1, numConstrainedPhotoRates
        if ( photoNumbers(j) == constrainedPhotoRatesNumbers(i) ) then
          this_number_unconstrained = .false.
          exit
        end if
      end do
      if ( this_number_unconstrained .eqv. .true. ) then
        ! This constrained number is unconstrained, so add to the list of unconstrained numbers
        counter = counter + 1
        unconstrainedPhotoNumbers(counter) = photoNumbers(j)
      end if
    end do
    if ( numUnconstrainedPhotoRates > 0 ) then
      unconstrainedPhotosExist = .true.
    end if
    write (*,*) 'There are ', numUnconstrainedPhotoRates, ' unconstrained photo rates'
    return
  end subroutine findUnconstrainedPhotos


  pure function isInNPIArray( a, array ) result ( isInArray )
    use types_mod
    implicit none

    integer(kind=NPI), intent(in) :: a, array(:)
    integer(kind=NPI) :: i
    logical :: isInArray

    isInArray = .false.

    do i = 1, size(array)
      if ( array(i) == a ) then
        isInArray = .true.
        exit
      end if
    end do

    return
  end function isInNPIArray


  subroutine readUnconstrainedPhotolysisRates()
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use photolysis_rates_mod, only : ck, cl, cmm, cnn, photoRateNames, transmissionFactor, &
                                     allocate_photolysis_j, numUnconstrainedPhotoRates, unconstrainedPhotoNumbers, &
                                     unconstrainedPhotoNames, allocate_unconstrained_photolysis_rates_variables, totalNumPhotos
    use directories_mod, only : param_dir
    use storage_mod, only : maxFilepathLength
    implicit none

    integer(kind=NPI) :: i, index, this_ck, this_ck_pos
    integer(kind=IntErr) :: ierr
    character(len=maxFilepathLength) :: filename, line
    logical :: allocated = .false.

    filename = trim( param_dir ) // '/photolysisRates.config'
    write (*, '(A)') ' Reading unconstrained photolysis rates from file...'
    call inquire_or_abort( filename, 'readUnconstrainedPhotolysisRates()')
    totalNumPhotos = count_lines_in_file( filename, .true. )
    if ( allocated .eqv. .false. ) then
      call allocate_unconstrained_photolysis_rates_variables()
      allocated = .true.
    end if
    ! For each line, check whether the first column is in the unconstrained list. If so, write
    ! the values to the appropriate variables.
    open (10, file=filename, status='old')
    read (10,*) ! Ignore first line
    index = 0_NPI
    do i = 1_NPI, totalNumPhotos
      read (10,'(A100)', iostat=ierr) line
      if ( ierr /= 0 ) then
        stop 'readUnconstrainedPhotolysisRates(): error reading file'
      end if
      this_ck_pos = scan(line, "0123456789")
      read(line(this_ck_pos:),'(I4)') this_ck
      ! If this line is associated to an unconstrained photo rate, then write the line to the appropriate variables
      if ( isInNPIArray( this_ck, unconstrainedPhotoNumbers ) .eqv. .true. ) then
        index = index + 1_NPI
        read (line,*, iostat=ierr) ck(index), cl(index), cmm(index), cnn(index), unconstrainedPhotoNames(index), &
                                   transmissionFactor(index)
      end if
      if ( ierr /= 0 ) then
        stop 'readUnconstrainedPhotolysisRates(): error reading line'
      end if
    end do
    close (10, status='keep')

    if ( numUnconstrainedPhotoRates > 3 ) then
      write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') &
                                ck(1), cl(1), cmm(1), cnn(1), adjustr( unconstrainedPhotoNames(1) ), transmissionFactor(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') ck(numUnconstrainedPhotoRates), cl(numUnconstrainedPhotoRates), &
                  cmm(numUnconstrainedPhotoRates), cnn(numUnconstrainedPhotoRates), &
                  adjustr( unconstrainedPhotoNames(numUnconstrainedPhotoRates) ), transmissionFactor(numUnconstrainedPhotoRates)
    else
      do i = 1, numUnconstrainedPhotoRates
        write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') &
                                ck(i), cl(i), cmm(i), cnn(i), adjustr( unconstrainedPhotoNames(i) ), transmissionFactor(i)
      end do
    end if
    write (*, '(A)') ' Finished reading unconstrained photolysis rates.'
    write (*, '(A, I0)') ' Number of unconstrained photolysis rates: ', numUnconstrainedPhotoRates

    return
  end subroutine readUnconstrainedPhotolysisRates

  ! -----------------------------------------------------------------
  ! This is called from readPhotoRates() if
  ! modelConfiguration/photolysisConstants.config doesn't exist. It
  ! reads ck, cl, cmm, cnn, str, and tf from
  ! modelConfiguration/photolysisRates.config.
  subroutine readAllPhotolysisRates()
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use photolysis_rates_mod, only : ck, cl, cmm, cnn, unconstrainedPhotoNames, &
                                     transmissionFactor, allocate_unconstrained_photolysis_rates_variables, &
                                     allocate_photolysis_j, numConstrainedPhotoRates, numUnconstrainedPhotoRates
    use directories_mod, only : param_dir
    use storage_mod, only : maxFilepathLength
    implicit none

    integer(kind=NPI) :: i
    integer(kind=IntErr) :: ierr
    character(len=maxFilepathLength) :: filename
    logical :: allocated = .false.

    filename = trim( param_dir ) // '/photolysisRates.config'
    write (*, '(A)') ' Reading all photolysis rates from file...'
    call inquire_or_abort( filename, 'readAllPhotolysisRates()')
    numUnconstrainedPhotoRates = count_lines_in_file( filename, .true. )
    if ( allocated .eqv. .false. ) then
      call allocate_unconstrained_photolysis_rates_variables()
      allocated = .true.
    end if
    ! For each
    open (10, file=filename, status='old')
    read (10,*) ! Ignore first line
    do i = 1, numUnconstrainedPhotoRates
      read (10,*, iostat=ierr) ck(i), cl(i), cmm(i), cnn(i), unconstrainedPhotoNames(i), transmissionFactor(i)
      if ( ierr /= 0 ) then
        stop 'readAllPhotolysisRates(): error reading file'
      end if
    end do
    close (10, status='keep')

    if ( numUnconstrainedPhotoRates > 3 ) then
      write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') &
                                ck(1), cl(1), cmm(1), cnn(1), adjustr( unconstrainedPhotoNames(1) ), transmissionFactor(1)
      write (*, '(A)') ' ...'
      write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') ck(numUnconstrainedPhotoRates), cl(numUnconstrainedPhotoRates), &
                  cmm(numUnconstrainedPhotoRates), cnn(numUnconstrainedPhotoRates), &
                  adjustr( unconstrainedPhotoNames(numUnconstrainedPhotoRates) ), transmissionFactor(numUnconstrainedPhotoRates)
    else
      do i = 1, numUnconstrainedPhotoRates
        write (*, '(I7, 1P e15.3, 1P e15.3, 1P e15.3, A, 1P e15.3)') &
                                ck(i), cl(i), cmm(i), cnn(i), adjustr( unconstrainedPhotoNames(i) ), transmissionFactor(i)
      end do
    end if
    write (*, '(A)') ' Finished reading all photolysis rates.'
    write (*, '(A, I0)') ' Number of all photolysis rates: ', numUnconstrainedPhotoRates

    return
  end subroutine readAllPhotolysisRates


  ! ----------------------------------------------------------------- !
  ! Read photolysis rates from file, either by (a) constant values,
  ! (b) non-constant fixed values, or (c) calculation from equation.
  subroutine readPhotoRates()
    use types_mod
    use photolysis_rates_mod
    use directories_mod, only : param_dir
    use storage_mod, only : maxPhotoRateNameLength, maxFilepathLength
    implicit none

    character(len=maxFilepathLength) :: filename
    logical :: file_exists

    ! Find out how many photolysis rates there are in the MCM data, and their numbers
    call readPhotolysisNumbers()
    ! Now that we know the photolysis rates' numbers, we can go about setting their values

    ! Check whether photolysisConstants.config file exists - if so, and it is non-empty,
    ! call readPhotolysisConstants() to set those to their given value, and set the rest to zero.
    filename = trim( param_dir ) // '/photolysisConstants.config'
    write (*, '(A)') ' Looking for photolysis constants file...'
    inquire(file=filename, exist=file_exists)
    usePhotolysisConstants = .false.
    if ( file_exists .eqv. .true. ) then
      write (*, '(A)') ' Checking that photolysis constants exist in file...'
      numConstantPhotoRates = count_lines_in_file( filename, .true. )
      ! Only use if the file exists and is not empty
      if ( numConstantPhotoRates > 0) then
        usePhotolysisConstants = .true.
        PR_type = 1
      else
        write (*, '(A)') ' Photolysis constants file is empty.'

      end if
    else
      write (*, '(A)') ' No photolysis constants file found.'

    end if
    if ( usePhotolysisConstants .eqv. .true. ) then
      call readPhotolysisConstants()
    else
      ! Check whether constrainedPhotoNames.config file exists - if so, and it is non-empty,
      ! call readPhotolysisConstraints() to read in their values.
      write (*, '(A)') ' No photolysis constants applied, so trying constrained photolysis rates file...'
      filename = trim( param_dir ) // '/constrainedPhotoRates.config'
      write (*, '(A)') ' Looking for photolysis constraints file...'
      inquire(file=filename, exist=file_exists)
      usePhotolysisConstraints = .false.
      if ( file_exists .eqv. .true. ) then
        write (*, '(A)') ' Checking that photolysis constraints exist in file...'
        numConstrainedPhotoRates = count_lines_in_file( filename, .false. )
        ! Only use constraints if the file exists and is not empty
        if ( numConstrainedPhotoRates > 0) then
          usePhotolysisConstraints = .true.
          call readPhotolysisConstraints()
          ! Test whether there are any unconstrained species left. If there are, read their calculation parameters in.
          ! Exact test is whether there are any species in pR.config that aren't already covered by constraints.
          call findUnconstrainedPhotos()
          if ( unconstrainedPhotosExist .eqv. .false. ) then
            write (*, '(2A)') ' Photolysis constraint file constrains all photolysis rates, ',  &
                             'so no photolysis rates will be calculated.'
            PR_type = 2
          else
            write (*, '(2A)') ' Photolysis constraint file does not constrain all photolysis rates, ', &
                             'so some photolysis rates will be calculated.'
            PR_type = 3
            call readUnconstrainedPhotolysisRates()
          end if
        else
          write (*, '(A)') ' Photolysis constraint file is empty, so all photolysis rates will be calculated.'
          PR_type = 4
          call readAllPhotolysisRates()
        end if
      else
        write (*, '(A)') ' No photolysis constraint file exists, so all photolysis rates will be calculated.'
        PR_type = 4
        call readAllPhotolysisRates()
      end if
    end if
    write (*, '(A, I0)') ' PR_type = ', PR_type
    write (*,*)

    return
  end subroutine readPhotoRates


  ! -----------------------------------------------------------------
  ! Read in contents of
  ! modelConfiguration/productionRatesOutput.config and
  ! modelConfiguration/lossRatesOutput.config, which contains a list
  ! of the species we want to have outputted to
  ! mC/production/lossRates.output Output the contents in r, with i as
  ! the length of r.
  subroutine readProductsOrReactantsOfInterest( filename, r )
    use types_mod
    use storage_mod, only : maxSpecLength
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
  ! of each line to an element of parameterArray
  function getParametersFromFile( input_file ) result ( parameterArray )
    use types_mod
    implicit none

    character(len=*), intent(in) :: input_file
    real(kind=DP), allocatable :: parameterArray(:)
    integer(kind=NPI) :: num_lines, counter

    call inquire_or_abort( input_file, 'getParametersFromFile()')
    num_lines = count_lines_in_file( input_file, skip_first_line_in=.false. )
    allocate (parameterArray(num_lines))

    open (10, file=input_file, status='old') ! input file
    do counter = 1_NPI, num_lines
      read (10,*) parameterArray(counter)
    end do
    close (10, status='keep')

    return
  end function getParametersFromFile

  ! -----------------------------------------------------------------
  ! This routine reads in the parameters required to calculate the reference
  ! rate of the JFac photolysis rate
  subroutine readJFacCalculationParameters()
    use types_mod
    use storage_mod, only: maxFilepathLength, maxPhotoRateNameLength
    use directories_mod, only : param_dir
    use photolysis_rates_mod, only : jFacSpecies, jFacSpeciesFound, &
                                     jFacL, jFacM, jFacN, jFacTransmissionFactor
    implicit none

    character(len=maxFilepathLength) :: filename
    character(len=maxPhotoRateNameLength) :: name
    integer(kind=NPI) :: i, totalLines, temp
    integer(kind=IntErr) :: ierr
    !logical :: jFacSpeciesFound

    ! Read the config file, counting the lines
    filename = trim( param_dir ) // '/photolysisRates.config'
    write (*, '(A)') ' Reading all photolysis rates from file...'
    call inquire_or_abort( filename, 'readJFacCalculationParameters()')
    totalLines = count_lines_in_file( filename, .true. )

    jFacSpeciesFound = .false.
    ! Loop over the lines - if it finds one where the name matches jFacSpecies,
    ! then read in all the elements in that line, and then calculate the jFacBaseRate
    open (11, file=filename, status='old') ! input file
    read (11,*, iostat=ierr)
    do i = 1, totalLines
      read (11,*, iostat=ierr) temp, jFacL, jFacM, jFacN, name, jFacTransmissionFactor
      if ( ierr /= 0 ) then
        stop 'readJFacCalculationParameters(): error reading file'
      end if
  !     ! If this line is associated to an unconstrained photo rate, then write the line to the appropriate variables
       if ( trim( name ) == trim( jFacSpecies ) ) then
         jFacSpeciesFound = .true.
         exit
      end if
    end do
    close (11, status='keep')
    if ( jFacSpeciesFound .eqv. .false. ) then
       stop 'jFac base data for species ' // trim( jFacSpecies ) // ' not found in ' // trim( filename )
    end if
    return
  end subroutine readJFacCalculationParameters


  ! -----------------------------------------------------------------
  ! This function reads in data from environmentVariables.config, and
  ! sets envVarTypesNum for each one. In the case of a constrained
  ! variable, this also reads in the constraint data from
  ! environmentConstraints directory, the file named after the
  ! environmental variable.
  subroutine readEnvVar()
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use env_vars_mod
    use directories_mod, only : param_dir, env_constraints_dir
    use constraints_mod, only : maxNumberOfEnvVarDataPoints
    use storage_mod, only : maxFilepathLength, maxEnvVarNameLength
    use photolysis_rates_mod, only : jFacSpecies, jFacSpeciesLine, photoRateNames, jFacSpeciesFound, &
                                     numUnconstrainedPhotoRates, unconstrainedPhotoNames
    implicit none

    integer(kind=NPI) :: k, j
    integer(kind=SI) :: i
    integer(kind=IntErr) :: ierr
    real(kind=DP) :: input1, input2
    character(len=10) :: dummy
    character(len=maxFilepathLength) :: fileLocationPrefix
    character(len=maxFilepathLength+maxEnvVarNameLength) :: fileLocation

    write (*, '(A)') ' Reading environment variables...'

    ! Count number of environment variables by reading in lines from
    ! file, and then adding 1 to account for M, which should be omitted
    ! from the config file since it is always calculated from temperature
    ! and pressure
    numEnvVars = int( count_lines_in_file( trim( param_dir ) // '/environmentVariables.config' ), SI ) + 1_SI

    ! Allocate storage for current values of env vars used for output
    allocate (currentEnvVarValues(numEnvVars) )

    write (*, '(A, I0)') ' Number of environment variables: ', numEnvVars
    allocate (envVarTypesNum(numEnvVars), envVarNames(numEnvVars), envVarTypes(numEnvVars) )
    allocate (envVarFixedValues(numEnvVars) )

    envVarNames(1) = 'M'
    envVarTypes(1) = 'CALC'
    envVarTypesNum(1) = 1_SI

    fileLocation = trim( param_dir ) // '/environmentVariables.config'
    call inquire_or_abort( fileLocation, 'readEnvVar()')
    open (10, file=fileLocation, status='old') ! input file
    ! Read in environment variables
    do i = 2_SI, numEnvVars
      read (10,*) dummy, envVarNames(i), envVarTypes(i)
      write (*, '(A, A4, A12, A20) ') ' ', dummy, envVarNames(i), adjustr( envVarTypes(i) )

      select case ( trim( envVarNames(i) ) )
        case ('JFAC')
          ! JFAC gets special treatment so that we can pass in the name
          ! of the JFAC species to environmentVariables.config if we're
          ! calculating JFAC on the fly.
          select case ( trim( envVarTypes(i) ) )
            case ('CONSTRAINED')
              ! We now expect a file JFAC in environmentConstraints directory
              envVarTypesNum(i) = 2_SI
            case ('NOTUSED')
              ! JFAC should be set to its default value of 1 everywhere
              envVarTypesNum(i) = 4_SI
            case default
              ! Firstly treat as a species: 'CALC' equivalent.
              ! Set to the value given, and then check that the
              ! relevant line exists in the photolysis rate file.
              envVarTypesNum(i) = 1_SI
              jFacSpecies = trim( envVarTypes(i) )
              ! Get line number for the JFac base species:
              call readJFacCalculationParameters()

              ! If it's not a valid photolysis rate then treat as a fixed number
              if ( jFacSpeciesFound .eqv. .false. ) then
                jFacSpecies = ''
                envVarTypesNum(i) = 3_SI
                read (envVarTypes(i),*) envVarFixedValues(i)
              end if
          end select
        case ('ROOFOPEN')
          if ( trim( envVarTypes(i) ) == 'ON' ) then
            envVarTypesNum(i) = 3_SI
            envVarFixedValues(i) = 1.0_DP
          else if ( trim( envVarTypes(i) ) == 'OFF' ) then
            envVarTypesNum(i) = 3_SI
            envVarFixedValues(i) = 0.0_DP
          else
            write (stderr,*) 'readEnvVar(): Invalid option given to ROOFOPEN in environmentVariables.config.'
            stop
          end if
        case ('TEMP', 'RH', 'H2O', 'PRESS', 'BLHEIGHT', 'DILUTE', 'DEC')
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
        case default
          write (stderr,*) 'readEnvVar(): Invalid environment variable ', trim( envVarNames(i) ), &
                           ' given in environmentVariables.config.'
          stop
      end select
    end do
    close (10, status='keep')

    write (*, '(A)') ' Finished reading environment variables.'
    write (*,*)

    ! TODO: convert this to a command line input argument
    fileLocationPrefix = trim( env_constraints_dir ) // "/"
    ! If environment variable is constrained, read in constraint data
    write (*, '(A)') ' Checking for constrained environment variables...'
    maxNumberOfEnvVarDataPoints = 0_NPI
    numConEnvVarRates = 0_SI
    do i = 1, numEnvVars
      if ( envVarTypes(i) == 'CONSTRAINED' ) then

        fileLocation = trim( fileLocationPrefix ) // trim( envVarNames(i) )
        call inquire_or_abort( fileLocation, 'readEnvVar()')
        maxNumberOfEnvVarDataPoints = max( maxNumberOfEnvVarDataPoints, count_lines_in_file( fileLocation ) )
        numConEnvVarRates = numConEnvVarRates + 1_SI
      end if
    end do

    if ( numConEnvVarRates > 0_SI ) then
      write (*, '(A, I0)') ' maximum number of environment variable constraint data points: ', maxNumberOfEnvVarDataPoints
    end if
    ! Allocate variables for next section
    allocate (envVarX(numEnvVars, maxNumberOfEnvVarDataPoints))
    allocate (envVarY(numEnvVars, maxNumberOfEnvVarDataPoints))
    allocate (envVarNumberOfPoints(numEnvVars))

    do i = 1, numEnvVars
      if ( envVarTypes(i) == 'CONSTRAINED' ) then

        write (*, '(2A)') ' Reading constraint data for ', trim( envVarNames(i) )

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
  ! Read concentrationOutput.config to get species of interest
  function readSpeciesOfInterest() result ( r )
    use types_mod
    use species_mod, only : getNumberOfSpecies
    use directories_mod, only : param_dir
    use storage_mod, only : maxSpecLength, maxFilepathLength
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
  ! Read constraint data for all constrained species from file, either
  ! (a) constant values, or (b) non-constant fixed values.
  subroutine readSpeciesConstraints( t, y )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use species_mod
    use constraints_mod, only : maxNumberOfConstraintDataPoints, speciesNumberOfPoints, numberOfVariableConstrainedSpecies, &
                                numberOfFixedConstrainedSpecies, setNumberOfConstrainedSpecies, setConstrainedConcs, &
                                setConstrainedSpecies, getOneConstrainedSpecies, dataX, dataY, dataFixedY
    use directories_mod, only : param_dir, spec_constraints_dir
    use storage_mod, only : maxSpecLength, maxFilepathLength
    use config_functions_mod, only : getIndexWithinList
    use interpolation_functions_mod, only : getConstrainedQuantAtT
    use interpolation_method_mod , only : getSpeciesInterpMethod
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
    allocate ( constrainedNames(numberOfConstrainedSpecies) )
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

    maxNumberOfConstraintDataPoints = 0_NPI
    if ( numberOfVariableConstrainedSpecies > 0 ) then
      do i = 1, numberOfVariableConstrainedSpecies
        fileLocation = trim( spec_constraints_dir ) // '/' // trim( constrainedNames(i) )
        ! Count lines in file
        maxNumberOfConstraintDataPoints = max( count_lines_in_file( fileLocation ), maxNumberOfConstraintDataPoints )
      end do
      write (*, '(A, I0)') ' maximum number of species constraint data points: ', maxNumberOfConstraintDataPoints
    end if

    allocate ( dataX(numberOfVariableConstrainedSpecies, maxNumberOfConstraintDataPoints) )
    allocate ( dataY(numberOfVariableConstrainedSpecies, maxNumberOfConstraintDataPoints) )

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
        if ( dataNumberOfPoints > maxNumberOfConstraintDataPoints ) then
          dataNumberOfPoints = maxNumberOfConstraintDataPoints
          write (*, '(A, I0, A)') ' Warning! Truncated constraint data to ', dataNumberOfPoints, ' points.'!
        end if
        speciesNumberOfPoints(i) = dataNumberOfPoints
        ! Read contents of file
        call inquire_or_abort( fileLocation, 'readSpeciesConstraints()')
        open (13, file=fileLocation, status='old')
        do k = 1, dataNumberOfPoints
          read (13,*) dataX(i, k), dataY(i, k)
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
        j = j + 1
        constrainedNames(j + numberOfVariableConstrainedSpecies) = name
        dataFixedY(j) = value
        call setConstrainedSpecies( j + numberOfVariableConstrainedSpecies, id )
      else
        write (stderr, '(A, A, A)') 'Supplied fixed constrained species ', &
                                    trim( constrainedNames(i) ), ' is not a species in the problem.'
        stop
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
        call getConstrainedQuantAtT( t, datax, datay, speciesNumberOfPoints(i), getSpeciesInterpMethod(), i, concAtT(i) )
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
  ! Given a filename, count the number of lines. Optional argument
  ! skip_first_line_in ignores the first line if it exists.
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
  ! REad in from filename and fille output_vector with the contents of
  ! the first column of the input. Optional argument skip_first_line_in
  ! ignores the first line if it exists.
  subroutine read_in_single_column_string_file( filename, output_vector, skip_first_line_in )
    use types_mod
    use storage_mod, only : maxSpecLength
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
  ! Inquire whether the file exists. If it doesn't abort, and print a
  ! message showing the calling subroutine's name.
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

end module input_functions_mod
