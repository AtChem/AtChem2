! ******************************************************************** !
! ATCHEM -- MODULE outputFunctions
!
! This module contains functions that control output to file.
! ******************************************************************** !
module outputFunctions_mod
contains

  ! ----------------------------------------------------------------- !
  ! Returns the sum of all ro2 species' concentrations
  pure function ro2Sum( y ) result ( ro2 )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: y(*)
    real(kind=DP) :: ro2

    include 'ro2-rates.f90'

    return
  end function ro2Sum

  ! ----------------------------------------------------------------- !
  ! Write each environment variable to file
  subroutine outputEnvVar( t )
    use types_mod
    use env_vars_mod
    implicit none

    real(kind=DP), intent(in) :: t
    integer(kind=NPI) :: i
    logical :: first_time = .true.

    if ( first_time .eqv. .true. ) then
      write (52, '(100A15) ') 'time', (trim( envVarNames(i) ), i = 1, numEnvVars), 'RO2'
      first_time = .false.
    end if

    if ( ro2 < 0 ) ro2 = 0.0

    write (52, '(100 (1P e15.7)) ') t, (currentEnvVarValues(i), i = 1, numEnvVars), ro2

    return
  end subroutine outputEnvVar

  ! ----------------------------------------------------------------- !
  ! Write parameters output by CVODE solver to file
  subroutine outputSolverParameters( t, prev, this, array, solver_type )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: t, prev, this
    integer(kind=NPI), intent(in) :: array(:)
    integer(kind=SI), intent(in) :: solver_type
    integer(kind=SI) :: i
    logical :: first_time = .true.

    if ( ( solver_type == 1 ) .or. ( solver_type == 2 ) ) then
      ! CVSPILS type solver
      if ( first_time .eqv. .true. ) then
        write (57, '(A9, 2A17, 20A9) ') 't', 'currentStepSize', 'previousStepSize', 'LENRW', 'LENIW', 'NST', 'NFE', &
                                        'NETF', 'NCFN', 'NNI', 'NSETUPS', 'QU', 'QCUR', 'NOR', 'LENRWLS', 'LENIWLS', &
                                        'LS_FLAG', 'NFELS', 'NJTV', 'NPE', 'NPS', 'NLI', 'NCFL'
        first_time = .false.
      end if
      write (57, '(1P e9.2, 2 (1P e17.9), 20I9) ') t, prev, this, (array(i), i = 1, 11), (array(i), i = 13, 21)

    else if ( solver_type == 3 ) then
      ! CVDLS type solver
      if ( first_time .eqv. .true. ) then
        write (57, '(A9, 2A17, 16A9) ') 't', 'currentStepSize', 'previousStepSize', 'LENRW', 'LENIW', 'NST', 'NFE', &
                                        'NETF', 'NCFN', 'NNI', 'NSETUPS', 'QU', 'QCUR', 'NOR', 'LENRWLS', 'LENIWLS', &
                                        'LS_FLAG', 'NFELS', 'NJE'
        first_time = .false.
      end if
      write (57, '(1P e9.2, 2 (1P e17.9), 16I9) ') t, prev, this, (array(i), i = 1, 11), (array(i), i = 13, 17)

    else
      write (stderr,*) 'outputSolverParameters(): Error with solver_type = ', solver_type
      write (stderr,*) 'Available options are 1, 2, 3.'
      stop
    end if

    return
  end subroutine outputSolverParameters

  ! ----------------------------------------------------------------- !
  ! Write parameters used in calculation of photolysis rates to file.
  subroutine outputPhotoRateCalcParameters( t )
    use types_mod
    use zenith_data_mod
    implicit none

    real(kind=DP), intent(in) :: t
    logical :: first_time = .true.

    if ( first_time .eqv. .true. ) then
      write (59, '(100A15) ') 't', 'latitude', 'longitude', 'secx', 'cosx', 'lha', 'sinld', 'cosld', 'theta', 'eqtime'
      first_time = .false.
    end if

    write (59, '(100(1P e15.7)) ') t, latitude, longitude, secx, cosx, lha, sinld, cosld, theta, eqtime

    return
  end subroutine outputPhotoRateCalcParameters

  ! ----------------------------------------------------------------- !
  ! Write photolysis rates to file.
  subroutine outputPhotolysisRates( t )
    use types_mod
    use photolysis_rates_mod, only : nrOfPhotoRates, ck, j, photoRateNames
    implicit none

    real(kind=DP), intent(in) :: t
    integer(kind=NPI) :: i
    logical :: firstTime = .true.

    if ( firstTime .eqv. .true. ) then
      write (58, '(100A15) ') 't', (trim( photoRateNames(i) ), i = 1, nrOfPhotoRates)
      firstTime = .false.
    end if
    write (58, '(100 (1P e15.7)) ') t, (j(ck(i)), i = 1, nrOfPhotoRates)

    return
  end subroutine outputPhotolysisRates

  ! -----------------------------------------------------------------
  ! Given a list speciesNames, and an integer reactionNumber, return
  ! reaction, a string containing the string representing that
  ! reaction.
  pure function getReaction( speciesNames, reactionNumber ) result ( reaction )
    use types_mod
    use reaction_structure_mod
    use storage_mod, only : maxSpecLength, maxReactionStringLength
    implicit none

    character(len=maxSpecLength) :: reactants(10), products(10)
    character(len=maxSpecLength), intent(in) :: speciesNames(*)
    integer(kind=NPI) :: i, numReactants, numProducts
    integer(kind=NPI), intent(in) :: reactionNumber
    character(len=maxReactionStringLength) :: reactantStr, productStr, reaction

    ! Loop over reactants, and copy the reactant name for any reactant
    ! used in reaction reactionNumber. use numReactants as a counter
    ! of the number of reactants.  String these together with '+', and
    ! append a '='
    numReactants = 0
    do i = 1, size( clhs, 2 )
      if ( clhs(1, i) == reactionNumber ) then
        numReactants = numReactants + 1
        reactants(numReactants) = speciesNames(clhs(2, i))
      end if
    end do

    reactantStr = ' '
    do i = 1, numReactants
      reactantStr = trim( adjustl( trim( reactantStr ) // trim( reactants(i) ) ) )
      if ( i < numReactants ) then
        reactantStr = trim( reactantStr ) // '+'
      end if
    end do
    reactantStr = trim( reactantStr ) // '='

    ! Loop over products, and copy the product name for any product
    ! created in reaction reactionNumber. use numProducts as a counter
    ! of the number of products.  String these together with '+', and
    ! append this to reactantStr. Save the result in reaction, which
    ! is returned
    numProducts = 0
    do i = 1, size( crhs, 2 )
      if ( crhs(1, i) == reactionNumber ) then
        numProducts = numProducts + 1
        products(numProducts) = speciesNames(crhs(2, i))
      end if
    end do

    productStr = ' '
    do i = 1, numProducts
      productStr = trim( adjustl( trim( productStr ) // trim( products(i) ) ) )
      if ( i < numProducts ) then
        productStr = trim( productStr ) // '+'
      end if
    end do

    reaction = trim( reactantStr ) // trim( productStr )

    return
  end function getReaction

  ! ----------------------------------------------------------------- !
  ! Write production and loss rates to file.
  subroutine outputRates( r, arrayLen, t, p, flag )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use reaction_structure_mod
    use species_mod, only : getSpeciesList
    use storage_mod, only : maxSpecLength, maxReactionStringLength
    implicit none

    integer(kind=NPI), intent(in) :: r(:,:), arrayLen(:)
    real(kind=DP), intent(in) :: t, p(:)
    integer(kind=SI), intent(in) :: flag
    character(len=maxSpecLength), allocatable :: speciesNames(:)
    integer(kind=NPI) :: i, j, output_file_number
    character(len=maxReactionStringLength) :: reaction
    logical :: first_time = .true.

    if ( size( r, 1 ) /= size( arrayLen ) ) then
      stop "size( r, 1 ) /= size( arrayLen ) in outputRates()."
    end if
    ! Add headers at the first call
    if ( first_time .eqv. .true. ) then
      write (56,*) '          time speciesNumber speciesName reactionNumber           rate'
      write (60,*) '          time speciesNumber speciesName reactionNumber           rate'
      first_time = .false.
    end if

    speciesNames = getSpeciesList()

    ! Flag = 0 for loss, 1 for production
    select case ( flag )
      case ( 0_SI )
        output_file_number = 56
      case ( 1_SI )
        output_file_number = 60
      case default
        write (stderr,*) "Unexpected flag value to outputRates(). flag = ", flag
        stop
    end select

    do i = 1, size( arrayLen )
      if ( arrayLen(i) > size( r, 2 ) ) then
        write (stderr,*) "arrayLen(i) > size( r, 2 ) in outputRates(). i = ", i
        stop
      end if
      do j = 2, arrayLen(i)
        if ( r(i, j) /= -1 ) then
          reaction = getReaction( speciesNames, r(i, j) )
          write (output_file_number, '(1P e15.7, I14, A12, I15, 1P e15.7, A, A)') t, r(i, 1), trim( speciesNames(r(i, 1)) ), &
                                                                                  r(i, j), p(r(i, j)), '  ', trim( reaction )
        end if
      end do
    end do

    return
  end subroutine outputRates

  ! ----------------------------------------------------------------- !
  ! Write instantaneous rates to file.
  subroutine outputInstantaneousRates( time )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use reaction_structure_mod
    use directories_mod, only : instantaneousRates_dir
    use reaction_rates_mod, only : instantaneousRates
    use storage_mod, only : maxFilepathLength
    implicit none

    integer(kind=QI), intent(in) :: time
    integer(kind=NPI) :: i
    character(len=maxFilepathLength+30) :: irfileLocation
    character(len=30) :: strTime

    write (strTime,*) time

    irfileLocation = trim( instantaneousRates_dir ) // '/' // adjustl( strTime )

    open (10, file=irfileLocation)
    do i = 1, size( instantaneousRates )
      write (10,*) instantaneousRates(i)
    end do
    close (10, status='keep')

    return
  end subroutine outputInstantaneousRates

  ! -----------------------------------------------------------------
  ! Print each element of arrayOfConcs, with size arrayOfConcsSize.
  ! If any concentration is negative, then set it to zero before
  ! printing.
  subroutine outputSpeciesOfInterest( t, specOutReqNames, arrayOfConcs )
    use types_mod
    use storage_mod, only : maxSpecLength
    implicit none

    real(kind=DP), intent(in) :: t
    character(len=maxSpecLength), intent(in) :: specOutReqNames(:)
    real(kind=DP), intent(inout) :: arrayOfConcs(:)
    integer(kind=NPI) :: i
    logical :: first_time = .true.

    if ( size( specOutReqNames ) /= size( arrayOfConcs ) ) then
      stop "size( specOutReqNames ) /= size( arrayOfConcs ) in outputSpeciesOfInterest()."
    end if

    if ( first_time .eqv. .true. ) then
      write (50, '(100A15) ') 't', (trim( specOutReqNames(i) ), i = 1, size( specOutReqNames ))
      first_time = .false.
    end if

    do i = 1, size( arrayOfConcs )
      if ( arrayOfConcs(i) < 0.0_DP ) then
        arrayOfConcs(i) = 0.0_DP
      end if
    end do
    write (50, '(100 (1P e15.7)) ') t, (arrayOfConcs(i), i = 1, size( arrayOfConcs ))

    return
  end subroutine outputSpeciesOfInterest

  ! -----------------------------------------------------------------
  ! This routine outputs speciesNames and speciesConcs to
  ! modelOutput/finalModelState.output
  subroutine outputFinalModelState( names, concentrations )
    use types_mod
    implicit none

    character(len=*), intent(in) :: names(:)
    real(kind=DP), intent(in) :: concentrations(:)
    integer(kind=NPI) :: species_counter

    if ( size( names ) /= size( concentrations ) ) then
      stop 'size( speciesName ) /= size( concentrations ) in outputFinalModelState().'
    end if
    do species_counter = 1, size( names )
      write (53,*) names(species_counter), concentrations(species_counter)
    end do

    return
  end subroutine outputFinalModelState

end module outputFunctions_mod
