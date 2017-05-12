module inputFunctions_mod
  use types_mod
contains
  subroutine readReactions( lhs, rhs, coeff )
    implicit none

    ! Reads in the data from mC/mechanism.reac and mC/mechanism.prod
    integer(kind=NPI), intent(out) :: lhs(:,:), rhs(:,:)
    real(kind=DP), intent(out) :: coeff(:)
    integer(kind=NPI) :: k, l, count
    integer(kind=IntErr) :: ierr

    if (size( lhs, 1 )/=3) then
      stop "size( lhs, 1 )/=3 in readReactions()."
    end if
    if (size( rhs, 1 )/=2) then
      stop "size( rhs, 1 )/=2 in readReactions()."
    end if
    if (size(coeff)/=size(rhs, 2)) then
      stop "size(coeff)/=size(rhs, 2) in readReactions()."
    end if

    write (*,*) 'Reading reactants (lhs) from mechanism.reac...'
    open (10, file='modelConfiguration/mechanism.reac', status='old') ! input file for lhs of equations
    ! read data for lhs of equations
    count = 0
    read (10,*, iostat=ierr)
    read (10,*, iostat=ierr) k, l
    do while (ierr==0)
      count = count+1
      lhs(1, count) = k
      lhs(2, count) = l
      lhs(3, count) = 1
      read (10,*, iostat=ierr) k, l
    end do
    close (10, status='keep')

    write (*,*) 'Reading products (rhs) from mechanism.prod...'
    open (11, file='modelConfiguration/mechanism.prod', status='old') ! input file for rhs of equations
    ! read data for rhs of equations
    count = 0
    ierr = 0
    read (11,*, iostat=ierr) k, l
    do while (ierr==0)
      count = count+1
      rhs(1, count) = k
      rhs(2, count) = l
      coeff(count) = 1
      read (11,*, iostat=ierr) k, l
    end do
    close (11, status='keep')

    write (*,*) 'Finished reading lhs and rhs data.'
    return
  end subroutine readReactions


  subroutine readJFacSpecies()
    ! Read modelConfiguration/JFacSpecies.config, and store this in jFacSpecies.
    ! Test this against known species, and if it is known then set jfacSpeciesLine
    ! to that line number in photoRateNames
    use photolysisRates
    use directories, only : param_dir
    implicit none

    integer(kind=NPI) :: i
    integer(kind=IntErr) :: ierr
    logical :: file_exists

    jfacSpeciesLine = 0
    write (*,*) 'Reading JFacSpecies...'
    inquire(file=trim(param_dir) // '/JFacSpecies.config', exist=file_exists)
    if (file_exists.eqv..false.) then
      write (*,*) "No JFacSpecies.config file exists, so setting jFacSpecies to ''"
      jFacSpecies = ''
    else
      open (10, file=trim(param_dir) // '/JFacSpecies.config', status='old', iostat=ierr)
      read (10,*, iostat=ierr) jFacSpecies
      close (10, status='keep')
      ! Catch the case where the file is empty
      if (ierr/=0) then
        jFacSpecies = ''
      end if
    end if
    write (*,*) 'JFacSpecies = ', trim(jFacSpecies)
    write (*,*) 'Finished reading JFacSpecies.'
    ! get line number for the JFac base species:
    do i = 1, nrOfPhotoRates
      if ((trim(photoRateNames(i)))==(trim(jFacSpecies))) then
        jfacSpeciesLine = i
      end if
    end do
    return
  end subroutine readJFacSpecies


  subroutine readPhotolysisRates( ck, cl, cmm, cnn, str, tf )
    ! This is called from readPhotolysisConstants if modelConfiguration/photolysisConstants.config
    ! doesn't exist. It reads ck, cl, cmm, cnn, str, and tf from
    ! modelConfiguration/photolysisRates.config.
    use photolysisRates, only : maxNrOfPhotoRates, nrOfPhotoRates
    use directories, only : param_dir
    use storage, only : maxPhotoRateNameLength
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    implicit none

    integer(kind=NPI) :: i, ck(:)
    integer(kind=IntErr) :: ierr
    real(kind=DP) :: cl(:), cmm(:), cnn(:), tf(:)
    character(len=maxPhotoRateNameLength) :: str(:)

    write (*,*) 'Reading photolysis rates from file...'
    open (10, file=trim(param_dir) // '/photolysisRates.config', status='old')
    ! Ignore first line
    read (10,*)
    do i = 1, maxNrOfPhotoRates
      read (10,*, iostat=ierr) ck(i), cl(i), cmm(i), cnn(i), str(i), tf(i)
      if (ierr/=0) then
        ! We've reached the end of file, so exit this loop
        exit
      end if
      nrOfPhotoRates = i
    end do
    close (10, status='keep')

    if (nrOfPhotoRates>3) then
      write (*,*) ck(1), cl(1), cmm(1), cnn(1), str(1), tf(1)
      write (*,*) '...'
      write (*,*) ck(nrOfPhotoRates), cl(nrOfPhotoRates), cmm(nrOfPhotoRates), &
                  cnn(nrOfPhotoRates), str(nrOfPhotoRates), tf(nrOfPhotoRates )
    else
      do i = 1, nrOfPhotoRates
        write (*,*) ck(i), cl(i), cmm(i), cnn(i), str(i), tf(i)
      end do
    end if
    write (*,*) 'Finished reading photolysis rates.'
    write (*,*) 'Number of photolysis rates:', nrOfPhotoRates
    return
  end subroutine readPhotolysisRates


  subroutine readPhotolysisConstants( ck, cl, cmm, cnn, str, tf )
    ! If modelConfiguration/photolysisConstants.config exists, then read in
    ! 3 values to fill ck, cl and str.
    ! Otherwise, call ReadPhotolysisRates to fill ck, cl, cmm, cnn, str and tf.
    use photolysisRates, only : usePhotolysisConstants, maxNrOfPhotoRates, nrOfPhotoRates
    use directories, only : param_dir
    use storage, only : maxPhotoRateNameLength
    implicit none

    integer(kind=NPI) :: i, ck(:)
    integer(kind=IntErr) :: ierr
    real(kind=DP) :: cl(:), cmm(:), cnn(:), tf(:)
    character(len=maxPhotoRateNameLength) :: str(:)
    logical :: file_exists

    ! Check whether file exists correctly in readPhotolysisConstants,
    write (*,*) 'Looking for photolysis constants file...'
    inquire(file=trim(param_dir) // '/photolysisConstants.config', exist=file_exists)
    if (file_exists.eqv..false.) then
      usePhotolysisConstants = .false.
      write (*,*) 'Photolysis constants file not found, trying photolysis rates file...'
      call readPhotolysisRates( ck, cl, cmm, cnn, str, tf )
      return
    end if
    usePhotolysisConstants = .true.

    nrOfPhotoRates = 0
    write (*,*) 'Reading photolysis constants from file...'
    open (10, file=trim(param_dir) // '/photolysisConstants.config', status='old', iostat=ierr)
    read (10,*)
    do i = 1, maxNrOfPhotoRates
      read (10,*, iostat=ierr) ck(i), cl(i), str(i)
      if (ierr/=0) then
        exit
      end if
      nrOfPhotoRates = i
    end do
    close (10, status='keep')
    if (nrOfPhotoRates>3) then
      write (*,*) ck(1), cl(1), str(1)
      write (*,*) '...'
      write (*,*) ck(nrOfPhotoRates), cl(nrOfPhotoRates), str(nrOfPhotoRates)
    else
      do i = 1, nrOfPhotoRates
        write (*,*) ck(i), cl(i), str(i)
      end do
    end if
    write (*,*) 'Finished reading photolysis constants.'
    write (*,*) 'Number of photolysis rates:', nrOfPhotoRates
    return
  end subroutine readPhotolysisConstants


  subroutine getReactantAndProductListSizes()
    use reactionStructure, only : lhs_size, rhs_size
    implicit none
    ! outputs lhs_size and rhs_size, which hold the number of lines in
    ! modelConfiguration/mechanism.(reac/prod), excluding the first line and
    ! last line
    lhs_size = count_lines_in_file( 'modelConfiguration/mechanism.reac', skip_first_line_in=.true. )
    rhs_size = count_lines_in_file( 'modelConfiguration/mechanism.prod', skip_first_line_in=.false. )

    return
  end subroutine getReactantAndProductListSizes


  subroutine getParametersFromFile( input_file, parameterArray, numValidEntries )
    ! Read in parameters from file at input_file, and save the contents of each
    ! line to an element of the array
    character(len=*), intent(in) :: input_file
    real(kind=DP), intent(out) :: parameterArray(:)
    integer(kind=DI), intent(out) :: numValidEntries

    open (10, file=input_file, status='old') ! input file
    numValidEntries = 0
    read (10,*) parameterArray(1)
    do while (parameterArray(numValidEntries+1)/=-9999)
      numValidEntries = numValidEntries + 1
      read (10,*) parameterArray(numValidEntries+1)
    end do
    close (10, status='keep')

    return
  end subroutine getParametersFromFile


  subroutine readPhotoRates()

    use photolysisRates
    use directories, only : param_dir, env_constraints_dir
    use storage, only : maxPhotoRateNameLength, maxFilepathLength
    implicit none

    integer(kind=NPI) :: i, k
    integer(kind=IntErr) :: ierr
    integer :: maxNumberOfDataPoints
    character(len=maxPhotoRateNameLength) :: string
    character(len=maxFilepathLength) :: fileLocationPrefix
    character(len=maxFilepathLength+maxPhotoRateNameLength) :: fileLocation

    ! GET NAMES OF PHOTO RATES
    call readPhotolysisConstants( ck, cl, cmm, cnn, photoRateNames, transmissionFactor )
    write (*,*)

    numConPhotoRates = 0
    ! GET NAMES OF CONSTRAINED PHOTO RATES
    write (*,*) 'Reading names of constrained photolysis rates from file...'
    open (10, file=trim(param_dir) // '/constrainedPhotoRates.config', status='old') ! input file
    do i = 1, maxNrOfConPhotoRates
      read (10,*, iostat=ierr) constrainedPhotoRates(i)
      if (ierr/=0) then
        exit
      end if
      numConPhotoRates = i
    end do
    close (10, status='keep')
    if (numConPhotoRates>3) then
      write (*,*) constrainedPhotoRates(1)
      write (*,*) '...'
      write (*,*) constrainedPhotoRates(numConPhotoRates)
    else
      do i = 1, numConPhotoRates
        write (*,*) constrainedPhotoRates(i)
      end do
    end if
    write (*,*) 'Finished reading names of constrained photolysis rates.'
    write (*,*) 'Number of constrained photorates:', numConPhotoRates

    ! GET NUMBERS OF CONSTRAINED PHOTO RATES
    do i = 1, numConPhotoRates
      do k = 1, nrOfPhotoRates
        if (constrainedPhotoRates(i)==photoRateNames(k)) then
          constrainedPhotoRatesNumbers(i) = ck(k)
        end if
      end do
    end do
    ! ALLOCATE ARRAY SIZE FOR STOREAGE OF PHOTOLYSIS CONSTRAINT DATA
    allocate (photoX (numConPhotoRates, maxNumberOfDataPoints))
    allocate (photoY (numConPhotoRates, maxNumberOfDataPoints))
    allocate (photoY2 (numConPhotoRates, maxNumberOfDataPoints))
    allocate (photoNumberOfPoints(numConPhotoRates))

    fileLocationPrefix = trim(env_constraints_dir) // "/"

    ! READ IN PHOTOLYSIS DATA
    if (numConPhotoRates>0) then
      write (*,*) 'Reading in constraint data for photolysis rates...'
      do i = 1, numConPhotoRates
        string = constrainedPhotoRates(i)
        write (*,*) string, '...'
        fileLocation = fileLocationPrefix // string
        open (11, file=fileLocation, status='old')
        read (11,*) photoNumberOfPoints(i)
        do k = 1, photoNumberOfPoints(i)
          read (11,*) photoX (i, k), photoY (i, k) !, photoY2 (i, k)
        end do
        close (11, status='keep')
      end do
      write (*,*) 'Finished reading constraint data for photolysis rates.'
    end if
    return
  end subroutine readPhotoRates


  subroutine readSpeciesOutputRequired( r, i )
    use species, only : getNumberOfSpecies
    use directories, only : param_dir
    use storage, only : maxSpecLength, maxFilepathLength
    implicit none

    character(len=maxSpecLength), allocatable, intent(out) :: r(:)
    integer(kind=NPI), intent(out) :: i
    character(len=maxFilepathLength) :: filename
    integer(kind=NPI) :: j, nsp, length

    filename = trim(param_dir) // '/concentrationOutput.config'
    write (*,*) 'Reading concentration output from file...'
    length = count_lines_in_file( trim(filename) )
    allocate (r(length))
    call read_in_single_column_string_file( trim(filename), r, i )
    write (*,*) 'Finished reading concentration output from file.'

    ! ERROR HANDLING
    nsp = getNumberOfSpecies()
    if (i>nsp) then
      write (51,*) 'Error: Number of (number of species output is required for) > (number of species) '
      write (51,*) "(number of species output is required for) = ", i
      write (51,*) "(number of species) = ", nsp
      stop 2
    end if

    write (*,*) 'Output required for concentration of', i, 'species:'
    if (i>3) then
      write (*,*) 1, r(1)
      write (*,*) '...'
      write (*,*) i, r(i)
    else
      do j = 1, i
        write (*,*) j, r(j)
      end do
    end if

    return
  end subroutine readSpeciesOutputRequired


  subroutine readNumberOfSpeciesAndReactions()
    use types_mod
    use species, only : setNumberOfSpecies, setNumberOfReactions
    implicit none

    integer(kind=NPI) :: numSpec, numReac
    !    READ IN MECHANISM PARAMETERS
    open (10, file='modelConfiguration/mechanism.reac', status='old') ! input file
    read (10,*) numSpec, numReac
    close (10, status='keep')

    call setNumberOfSpecies( numSpec )
    call setNumberOfReactions( numReac )

    write (*,*)
    write (*,*) 'Number of Species = ', numSpec
    write (*,*) 'Number of Reactions = ', numReac
    write (*,*)
  end subroutine readNumberOfSpeciesAndReactions


  subroutine readSpecies( neq, speciesName, speciesNumber )
    use storage, only : maxSpecLength
    implicit none

    integer(kind=NPI), intent(in) :: neq
    integer(kind=NPI) :: j
    character(len=maxSpecLength), intent(out) :: speciesName(:)
    integer(kind=NPI), intent(out) :: speciesNumber(:)

    ! Read in species number and name from mC/mechanism.species to speciesName
    ! and speciesNumber. Also set each element of y to 0.
    open (10, file='modelConfiguration/mechanism.species') ! input file
    do j = 1, neq
      read (10,*) speciesNumber(j), speciesName(j)
    end do
    close (10, status='keep')

    return
  end subroutine readSpecies


  subroutine readInitialConcentrations( concSpeciesNames, concentration )
    ! Reads in concentration per species from mC/initialConcentrations.config
    ! Checks that there aren't more inputs than species.
    ! concSpeciesNames is filled with all species names of initial concentrations,
    ! concentration is filled with corresponding concentration values
    use species, only : getNumberOfSpecies
    use directories, only : param_dir
    use storage, only : maxSpecLength, maxFilepathLength
    implicit none

    character(len=maxSpecLength), allocatable, intent(out) :: concSpeciesNames(:)
    character(len=maxSpecLength) :: k
    character(len=maxFilepathLength) :: file
    real(kind=DP), allocatable, intent(out) :: concentration(:)
    real(kind=DP) :: l
    integer(kind=NPI) :: numLines, i, nsp
    integer(kind=IntErr) :: ierr

    write (*,*) 'Reading initial concentrations...'
    file= trim(param_dir) // '/initialConcentrations.config'
    ! Count lines in file, allocate appropriately
    numLines = count_lines_in_file(trim(file), .false.)
    nsp = getNumberOfSpecies()
    if (numLines>nsp) then
      write (51,*) "Error:(number of species initial concentrations are set for) > (number of species) "
      write (51,*) "(number of species initial concentrations are set for) = ", numLines
      write (51,*) "(number of species) = ", nsp
    end if
    allocate (concSpeciesNames(numLines), concentration(numLines))

    open (10, file=trim(file), status='old') ! input file for lhs of equations
    i = 0
    read (10,*, iostat=ierr) k, l
    do while (ierr==0)
      i = i + 1
      concentration(i) = l
      concSpeciesNames(i) = k
      read (10,*, iostat=ierr) k, l
    end do
    close (10, status='keep')

    if (numLines>3) then
      write (*,*) 1, ' ', concSpeciesNames(1), ' ', concentration(1)
      write (*,*) '...'
      write (*,*) numLines, ' ', concSpeciesNames(numLines), ' ', concentration(numLines)
    else
      do i = 1, numLines
        write (*,*) i, ' ', concSpeciesNames(i), ' ', concentration(i)
      end do
    end if
    write (*,*) 'Finished reading initial concentrations.'

    return
  end subroutine readInitialConcentrations


  subroutine readProductsOrReactantsOfInterest( filename, r, i )
    ! Read in contents of modelConfiguration/production/lossRatesOutput.config, which
    ! contains a list of the species we want to have outputted to mC/production/lossRates.output
    ! Output the contents in r, with i as the length of r.
    use storage, only : maxSpecLength
    implicit none

    character(len=*), intent(in) :: filename
    character(len=maxSpecLength), allocatable, intent(out) :: r(:)
    integer(kind=NPI), intent(out) :: i
    integer(kind=NPI) :: j
    integer(kind=NPI) :: length
    logical :: file_exists

    inquire(file=trim(filename), exist=file_exists)
    if (file_exists.eqv..false.) then
      write (*,*) 'No ' // filename // ' file exists.'
    else
      length = count_lines_in_file ( trim(filename), .false.)
      allocate (r(length))
      call read_in_single_column_string_file( trim(filename), r, i, .false. )
    end if
    if (i>3) then
      write (*,*) 1, ' ', r(1)
      write (*,*) '...'
      write (*,*) i, ' ', r(i)
    else
      do j = 1, i
        write (*,*) j, ' ', r(j)
      end do
    end if
    return
  end subroutine readProductsOrReactantsOfInterest


  subroutine readSpeciesConstraints( t, y )
    use species
    use constraints
    use chemicalConstraints
    use directories, only : param_dir, spec_constraints_dir
    use photolysisRates, only : maxNrOfPhotoRates
    use storage, only : maxSpecLength, maxFilepathLength
    use configFunctions_mod, only : matchOneNameToNumber
    use interpolationFunctions_mod, only : getConstrainedQuantAtT2D
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP), intent(inout) :: y(:)
    real(kind=DP) :: concAtT, value
    integer(kind=NPI) :: i, j, id, numberOfSpecies
    integer :: k, dataNumberOfPoints
    integer(kind=IntErr) :: ierr
    integer(kind=NPI) :: countOfVarConSpecNames, countOfFixConSpecNames, countOfConNames
    character(len=maxSpecLength), allocatable :: speciesName(:)
    character(len=maxSpecLength) :: name
    character(len=maxFilepathLength+maxSpecLength) :: fileLocation

    call getSpeciesList( speciesName )

    ! READ IN SPECIES TO BE CONSTRAINED
    write (*,*) 'Counting the species to be constrained (in file constrainedSpecies.config)...'
    countOfVarConSpecNames = count_lines_in_file( trim(param_dir) // '/constrainedSpecies.config' )

    write (*,*) 'Finished counting the names of the species to be constrained.'
    write (*,*) 'Number of names for variable constrained species:', countOfVarConSpecNames

    ! read in numberOfFixedConstrainedSpecies

    write (*,*) 'Counting the fixed-concentration species to be constrained (in file constrainedFixedSpecies.config)...'
    countOfFixConSpecNames = count_lines_in_file( trim(param_dir) // '/constrainedFixedSpecies.config' )
    write (*,*) 'Finished counting the names of fixed-concentration species'

    write (*,*) 'Number names of fixed constrained species:', countOfFixConSpecNames

    countOfConNames = countOfVarConSpecNames + countOfFixConSpecNames
    allocate (constrainedSpecies(countOfConNames), constrainedName(countOfConNames))



    write (*,*) 'Reading in the names of variable constrained species...'
    open (12, file=trim(param_dir) // '/constrainedSpecies.config', status='old') ! input file
    j = 0
    do i = 1, maxNrOfPhotoRates
      read (12,*, iostat=ierr) name
      if (ierr/=0) then
        exit
      end if
      id = matchOneNameToNumber (speciesName, name)
      if (id/=0) then
        j = j + 1
        constrainedName(j) = name
        constrainedSpecies(j) = id
      end if
    end do
    close (12, status='keep')
    numberOfVariableConstrainedSpecies = j
    write (*,*) 'Finished reading the names of variable constrained species'
    write (*,*) 'Number of constrained variable species:', numberOfVariableConstrainedSpecies

    write (*,*) 'maxNumberOfDataPoints:', maxNumberOfDataPoints
    write (*,*) 'Allocating storage for variable constrained species...'
    allocate (dataX (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
    allocate (dataY (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
    allocate (dataY2 (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
    write (*,*) 'Finished allocating storage for variable constrained species.'

    if (numberOfVariableConstrainedSpecies>3) then
      write (*,*) 1, constrainedName(1)
      write (*,*) '...'
      write (*,*) numberOfVariableConstrainedSpecies, constrainedName(numberOfVariableConstrainedSpecies)
    else
      do i = 1, numberOfVariableConstrainedSpecies
        write (*,*) i, constrainedName(i)
      end do
    end if

    ! READ CONCENTRATION DATA FOR VARIABLE CONSTRAINED SPECIES
    write (*,*) 'Reading concentration data for constrained species...'
    allocate (speciesNumberOfPoints(numberOfVariableConstrainedSpecies+countOfFixConSpecNames))
    do i = 1, numberOfVariableConstrainedSpecies
      if (i<3 .or. i==numberOfVariableConstrainedSpecies) then
        write (*,*) constrainedName(i), '...'
      else
        if (i==2) write (*,*) '...'
      end if

      fileLocation = trim(spec_constraints_dir) // '/' // trim(constrainedName(i))
      open (13, file=fileLocation, status='old')

      read (13,*) dataNumberOfPoints
      if (dataNumberOfPoints>maxNumberOfDataPoints) then
        dataNumberOfPoints = maxNumberOfDataPoints
        write (*,*) 'Warning! Truncated constraint data to', dataNumberOfPoints, '.'!
      end if

      speciesNumberOfPoints(i) = dataNumberOfPoints
      do k = 1, dataNumberOfPoints
        read (13,*) dataX (i, k), dataY (i, k) !, dataY2 (i, k)
      end do
      close (13, status='keep')

    end do


    ! READ IN NAMES AND CONCENTRATION DATA FOR FIXED CONSTRAINED SPECIES
    allocate (dataFixedY (countOfFixConSpecNames))
    write (*,*) 'Reading in the names and concentration of the fixed constrained species ' // &
                '(in file constrainedFixedSpecies.config)...'
    open (14, file=trim(param_dir) // '/constrainedFixedSpecies.config', status='old') ! input file
    id = 0
    j = 0
    do i = 1, countOfFixConSpecNames
      read (14,*) name, value
      id = matchOneNameToNumber (speciesName, name)
      if (id/=0) then
        j = j+1
        constrainedName(j+numberOfVariableConstrainedSpecies) = name
        dataFixedY (j) = value
        constrainedSpecies(j+numberOfVariableConstrainedSpecies) = id
      end if
    end do
    close (14, status='keep')
    numberOfFixedConstrainedSpecies = j
    write (51,*) 'Number of fixed constrained species:', numberOfFixedConstrainedSpecies

    if (numberOfFixedConstrainedSpecies>3) then
      write (*,*) 1, constrainedName(1+numberOfVariableConstrainedSpecies), dataFixedY (1)
      write (*,*) '...'
      write (*,*) numberOfFixedConstrainedSpecies, &
                  constrainedName(numberOfFixedConstrainedSpecies+numberOfVariableConstrainedSpecies), &
                  dataFixedY (numberOfFixedConstrainedSpecies )
    else
      do i = 1, numberOfFixedConstrainedSpecies
        write (*,*) i, constrainedName(numberOfFixedConstrainedSpecies+numberOfVariableConstrainedSpecies), &
                    dataFixedY (numberOfFixedConstrainedSpecies )
      end do
    end if
    write (*,*) 'Finished reading in the names and concentration of fixed-concentration species.'

    numberOfConstrainedSpecies = numberOfVariableConstrainedSpecies + numberOfFixedConstrainedSpecies
    write (51,*) "Number of constrained species:", numberOfConstrainedSpecies

    ! ERROR HANDLING
    numberOfSpecies = getNumberOfSpecies()
    if (numberOfConstrainedSpecies>=numberOfSpecies) then
      write (51,*) "Error: Number of (number of constrained species) >= (number of species) "
      write (51,*) "(number of constrained species) = ", numberOfConstrainedSpecies
      write (51,*) "(number of species) = ", numberOfSpecies
      stop 2
    end if

    allocate (constrainedConcs(numberOfConstrainedSpecies))

    call setNumberOfConstrainedSpecies( numberOfConstrainedSpecies )

    write (*,*) 'Finished reading constrained species.'

    ! initialise concentrations of constrained species
    write (*,*) 'Initialising concentrations of constrained species...'
    do i = 1, numberOfConstrainedSpecies
      if (i<=numberOfVariableConstrainedSpecies) then
        call getConstrainedQuantAtT2D( t, datax, datay, datay2, speciesNumberOfPoints(i), concAtT, 1, i, &
                                       maxNumberOfDataPoints, numberOfVariableConstrainedSpecies )
      else
        concAtT = dataFixedY (i-numberOfVariableConstrainedSpecies)
      end if
      constrainedConcs(i) = concAtT
      call setConstrainedConc( i, concAtT )
      y(constrainedSpecies(i)) = concAtT
    end do
    write (*,*) 'Finished initialising concentrations of constrained species.'

    return
  end subroutine readSpeciesConstraints


  subroutine readEnvVar()
    ! This function reads in data from environmentVariables.config, and sets
    ! envVarTypesNum for each one. In the case of a constrained variable, this
    ! also reads in the constraint data from environmentConstraints directory, the
    ! file named after the environmental variable.
    use envVars
    use directories, only : param_dir, env_constraints_dir
    use constraints, only : maxNumberOfDataPoints
    use storage, only : maxFilepathLength, maxEnvVarNameLength
    implicit none

    integer(kind=NPI) :: i, counter, k
    character(len=10) :: dummy
    character(len=maxFilepathLength) :: fileLocationPrefix
    character(len=maxFilepathLength+maxEnvVarNameLength) :: fileLocation

    write (*,*) 'Reading environment variables...'

    ! Count number of environment variables by reading in lines from file
    counter = count_lines_in_file( trim(param_dir) // '/environmentVariables.config' )

    numEnvVars = counter

    ! Allocate storage for current values of env vars used for output
    allocate (currentEnvVarValues(numEnvVars))

    write (*,*) 'Number of environment variables: ', numEnvVars
    allocate (envVarTypesNum(numEnvVars), envVarNames(numEnvVars), envVarTypes(numEnvVars))
    allocate (envVarFixedValues(numEnvVars))

    open (10, file=trim(param_dir) // '/environmentVariables.config', status='old') ! input file
    ! Read in environment variables - if
    do i = 1, numEnvVars
      read (10,*) dummy, envVarNames(i), envVarTypes(i)
      write (*, '(A, A4, A12, A30) ') ' ', dummy, envVarNames(i), adjustr(envVarTypes(i))

      select case (trim(envVarTypes(i)))
      case ('CALC')
      envVarTypesNum(i) = 1
      case ('CONSTRAINED')
      envVarTypesNum(i) = 2
      case ('NOTUSED')
      envVarTypesNum(i) = 4
      case default
      envVarTypesNum(i) = 3
      ! Copy 3rd column value to envVarFixedValues(i)
      read (envVarTypes(i),*) envVarFixedValues(i)
    end select
  end do
  close (10, status='keep')

  write (*,*) 'Finished reading environment variables.'
  write (*,*)

  ! Allocate variables for next section
  allocate (envVarX (numEnvVars, maxNumberOfDataPoints))
  allocate (envVarY (numEnvVars, maxNumberOfDataPoints))
  allocate (envVarY2 (numEnvVars, maxNumberOfDataPoints))
  allocate (envVarNumberOfPoints(numEnvVars))

  ! TODO: convert this to a command line input argument
  fileLocationPrefix = trim(env_constraints_dir) // "/"
  ! If environment variable is constrained, read in constraint data
  write (*,*) 'Checking for constrained environment variables...'
  do i = 1, numEnvVars
    if (envVarTypes(i)=='CONSTRAINED') then

      write (*,*) 'Reading constraint data for', envVarNames(i)

      fileLocation = trim(fileLocationPrefix) // trim(envVarNames(i))

      open (11, file=fileLocation, status='old')

      read (11,*) envVarNumberOfPoints(i)
      do k = 1, envVarNumberOfPoints(i)
        read (11,*) envVarX (i, k), envVarY (i, k) ! envVarY2 (i, k)
      end do
      close (11, status='keep')
      write (*,*) 'Finished reading constraint data.'
    end if
  end do
  write (*,*) 'Finished checking for constrained environment variables.'

  return
end subroutine readEnvVar


function count_lines_in_file( filename, skip_first_line_in ) result ( counter )
  character(len=*), intent(in) :: filename
  logical, intent(in), optional:: skip_first_line_in
  integer(kind=NPI) :: counter
  logical :: skip_first_line
  character(len=10) :: dummy
  integer(kind=IntErr) :: ierr
  ! Set default to not skip first line
  if (.not. present(skip_first_line_in)) then
    skip_first_line = .false.
  else
    skip_first_line = skip_first_line_in
  end if
  counter = 0
  ierr = 0
  open (11, file=filename, status='old')
  if (skip_first_line) read (11,*, iostat=ierr) dummy
  do while (ierr==0)
    counter = counter + 1
    read (11,*, iostat=ierr) dummy
  end do
  close (11, status='keep')
  ! Remove 1 from counter, as the last wasn't used
  counter = counter - 1
  ! Handle the case where skip_first_line==.true. and there was no contents: return 0.
  if (counter==-1) counter=0
end function count_lines_in_file

subroutine read_in_single_column_string_file( filename, output_vector, i, skip_first_line_in )
  use storage, only : maxSpecLength

  character(len=*), intent(in) :: filename
  character(len=*), intent(out) :: output_vector(:)
  integer(kind=NPI), intent(out) :: i
  logical, intent(in), optional:: skip_first_line_in
  logical :: skip_first_line
  character(len=maxSpecLength) :: c
  integer(kind=IntErr) :: ierr
  ! Set default to not skip first line
  if (.not. present(skip_first_line_in)) then
    skip_first_line = .false.
  else
    skip_first_line = skip_first_line_in
  end if
  open (10, file=filename, status='old')
  ! Skip first line if necessary.
  if (skip_first_line) read (11,*, iostat=ierr) c
  ! Loop over all lines of the file, and add each entry to r(i)
  ! Then check we don't have more species of interest than total species
  i = 0
  read (10,*, iostat=ierr) c
  do while (ierr==0)
    i = i + 1
    output_vector(i) = c
    read (10,*, iostat=ierr) c
  end do
  close (10, status='keep')
end subroutine read_in_single_column_string_file
end module inputFunctions_mod
