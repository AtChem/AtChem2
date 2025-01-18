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
! ATCHEM2 -- MODULE outputFunctions
!
! This module contains functions that control output to file.
! ******************************************************************** !
module output_functions_mod
  implicit none

contains

  ! -----------------------------------------------------------------
  ! Returns the sum of all ro2 concentrations.
  pure function ro2Sum( y ) result ( ro2 )
    use types_mod
    use env_vars_mod, only : ro2Numbers

    real(kind=DP), intent(in) :: y(*)
    real(kind=DP) :: ro2
    integer(kind=NPI) :: i

    ro2 = 0.0_DP
    if ( size( ro2Numbers ) > 0 ) then
      do i = 1, size( ro2Numbers )
        ro2 = ro2 + y(ro2Numbers(i))
      end do
    end if
    return
  end function ro2Sum

  ! -----------------------------------------------------------------
  ! Write each environment variable to file.
  subroutine outputEnvVar( t )
    use types_mod
    use env_vars_mod, only : envVarNames, numEnvVars, currentEnvVarValues, ro2

    real(kind=DP), intent(in) :: t
    integer(kind=NPI) :: i
    logical :: first_time = .true.

    if ( first_time .eqv. .true. ) then
      write (52, '(100A15) ') 't', (trim( envVarNames(i) ), i = 1, numEnvVars), 'RO2'
      first_time = .false.
    end if

    if ( ro2 < 0 ) ro2 = 0.0

    write (52, '(100 (ES15.6E3)) ') t, (currentEnvVarValues(i), i = 1, numEnvVars), ro2

    return
  end subroutine outputEnvVar

  ! -----------------------------------------------------------------
  ! Write parameters output by CVODE solver to file.
  subroutine outputSolverParameters( t, prev, this, array, solver_type )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod

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
      write (57, '(1P e9.2, 2 (ES17.8E3), 20I9) ') t, prev, this, (array(i), i = 1, 11), (array(i), i = 13, 21)

    else if ( solver_type == 3 ) then
      ! CVDLS type solver
      if ( first_time .eqv. .true. ) then
        write (57, '(A9, 2A17, 16A9) ') 't', 'currentStepSize', 'previousStepSize', 'LENRW', 'LENIW', 'NST', 'NFE', &
                                        'NETF', 'NCFN', 'NNI', 'NSETUPS', 'QU', 'QCUR', 'NOR', 'LENRWLS', 'LENIWLS', &
                                        'LS_FLAG', 'NFELS', 'NJE'
        first_time = .false.
      end if
      write (57, '(1P e9.2, 2 (ES17.8E3), 16I9) ') t, prev, this, (array(i), i = 1, 11), (array(i), i = 13, 17)

    else
      write (stderr,*) 'outputSolverParameters(): Error with solver_type = ', solver_type
      write (stderr,*) 'Available options are 1, 2, 3.'
      stop
    end if

    return
  end subroutine outputSolverParameters

  ! -----------------------------------------------------------------
  ! Write parameters used in calculation of photolysis rates to file.
  subroutine outputPhotoRateCalcParameters( t )
    use types_mod
    use zenith_data_mod, only : latitude, longitude, secx, cosx, lha, sinld, &
                                cosld, eqtime

    real(kind=DP), intent(in) :: t
    logical :: first_time = .true.

    if ( first_time .eqv. .true. ) then
      write (59, '(100A15) ') 't', 'latitude', 'longitude', 'secx', 'cosx', 'lha', 'sinld', 'cosld', 'eqtime'
      first_time = .false.
    end if

    write (59, '(100 (ES15.6E3)) ') t, latitude, longitude, secx, cosx, lha, sinld, cosld, eqtime

    return
  end subroutine outputPhotoRateCalcParameters

  ! -----------------------------------------------------------------
  ! Write photolysis rates to file.
  subroutine outputPhotolysisRates( t )
    use types_mod
    use photolysis_rates_mod, only : ck, constrainedPhotoNumbers, constantPhotoNumbers, &
                                     j, PR_type, constantPhotoNames, numConstantPhotoRates, &
                                     constrainedPhotoNames, numConstrainedPhotoRates, &
                                     unconstrainedPhotoNames, numUnconstrainedPhotoRates

    real(kind=DP), intent(in) :: t
    integer(kind=NPI) :: i
    logical :: firstTime = .true.

    ! Output constant photolysis rates if any.
    ! Otherwise, output constrained (if any), then unconstrained (if any).
    select case ( PR_type )
      case ( 1_SI )
        if ( firstTime .eqv. .true. ) then
          write (58, '(100A15) ') 't', (trim( constantPhotoNames(i) ), i = 1_NPI, numConstantPhotoRates)
          firstTime = .false.
        end if
        write (58, '(100 (ES15.6E3)) ') t, (j(constantPhotoNumbers(i)), i = 1_NPI, numConstantPhotoRates)

      case ( 2_SI )
        if ( firstTime .eqv. .true. ) then
          write (58, '(100A15) ') 't', (trim( constrainedPhotoNames(i) ), i = 1_NPI, numConstrainedPhotoRates)
          firstTime = .false.
        end if
        write (58, '(100 (ES15.6E3)) ') t, (j(constrainedPhotoNumbers(i)), i = 1_NPI, numConstrainedPhotoRates)

      case ( 3_SI )
        if ( firstTime .eqv. .true. ) then
          write (58, '(100A15) ') 't', (trim( unconstrainedPhotoNames(i) ), i = 1_NPI, numUnconstrainedPhotoRates), &
                                  (trim( constrainedPhotoNames(i) ), i = 1_NPI, numConstrainedPhotoRates)
          firstTime = .false.
        end if
        write (58, '(100 (ES15.6E3)) ') t, (j(ck(i)), i = 1_NPI, numUnconstrainedPhotoRates), &
                                        (j(constrainedPhotoNumbers(i)), i = 1_NPI, numConstrainedPhotoRates)

      case ( 4_SI )
        if ( firstTime .eqv. .true. ) then
          write (58, '(100A15) ') 't', (trim( unconstrainedPhotoNames(i) ), i = 1_NPI, numUnconstrainedPhotoRates)
          firstTime = .false.
        end if
        write (58, '(100 (ES15.6E3)) ') t, (j(ck(i)), i = 1_NPI, numUnconstrainedPhotoRates)

      case default
        stop 'outputPhotolysisRates(): invalid case of PR_type.'

    end select

    return
  end subroutine outputPhotolysisRates

  ! -----------------------------------------------------------------
  ! Given a list speciesNames, and an integer reactionNumber, return
  ! reaction, a string representing that reaction.
  pure function getReaction( speciesNames, reactionNumber ) result ( reaction )
    use types_mod
    use reaction_structure_mod, only : clhs, crhs, clcoeff, crcoeff
    use storage_mod, only : maxSpecLength, maxReactionStringLength

    character(len=maxSpecLength) :: reactants(10), products(10)
    character(len=4) :: reactCoeffs(10), prodCoeffs(10)
    character(len=4) :: tmpCoeffStr
    character(len=maxSpecLength), intent(in) :: speciesNames(*)
    integer(kind=NPI) :: i, numReactants, numProducts
    integer(kind=NPI), intent(in) :: reactionNumber
    character(len=maxReactionStringLength) :: reactantStr, productStr, reaction

    ! Loop over reactants, and copy the reactant name for any reactant used
    ! in reaction reactionNumber. Use numReactants as a counter of the number
    ! of reactants. String these together with '+', and append a '='.
    numReactants = 0
    do i = 1, size( clhs, 2 )
      if ( clhs(1, i) == reactionNumber ) then
        numReactants = numReactants + 1
        reactants(numReactants) = speciesNames(clhs(2, i))

        !Store the reaction coefficient (stoichiometry) string for this reactant. If
        !the coefficient is 1, then just use an empty string
        if ( clcoeff(i) == 1.0 ) then
          reactCoeffs(numReactants) = ''
        else
          write(tmpCoeffStr, '(F4.2)') clcoeff(i)
          reactCoeffs(numReactants) = tmpCoeffStr
        end if
      end if
    end do

    reactantStr = ' '
    do i = 1, numReactants
      reactantStr = trim( adjustl( trim( reactantStr ) // trim( reactCoeffs(i) ) // trim( reactants(i) ) ) )
      if ( i < numReactants ) then
        reactantStr = trim( reactantStr ) // '+'
      end if
    end do
    reactantStr = trim( reactantStr ) // '='

    ! Loop over products, and copy the product name for any product
    ! created in reaction reactionNumber. use numProducts as a counter
    ! of the number of products. String these together with '+', and
    ! append this to reactantStr. Save the result in reaction, which
    ! is returned.
    numProducts = 0
    do i = 1, size( crhs, 2 )
      if ( crhs(1, i) == reactionNumber ) then
        numProducts = numProducts + 1
        products(numProducts) = speciesNames(crhs(2, i))

        !Store the reaction coefficient (stoichiometry) string for this product. If
        !the coefficient is 1, then just use an empty string
        if ( crcoeff(i) == 1.0 ) then
          prodCoeffs(numProducts) = ''
        else
          write(tmpCoeffStr, '(F4.2)') crcoeff(i)
          prodCoeffs(numProducts) = tmpCoeffStr
        end if
      end if
    end do

    productStr = ' '
    do i = 1, numProducts
      productStr = trim( adjustl( trim( productStr ) // trim( prodCoeffs(i) ) //  trim( products(i) ) ) )
      if ( i < numProducts ) then
        productStr = trim( productStr ) // '+'
      end if
    end do

    reaction = trim( reactantStr ) // trim( productStr )

    return
  end function getReaction

  ! -----------------------------------------------------------------
  ! Write production and loss rates to file.
  subroutine outputRates( rSpecies, r, arrayLen, t, p, flag )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use species_mod, only : getSpeciesList
    use storage_mod, only : maxSpecLength, maxReactionStringLength

    type(reaction_frequency_pair), intent(in) :: r(:,:)
    integer(kind=NPI), intent(in) :: arrayLen(:), rSpecies(:)
    real(kind=DP), intent(in) :: t, p(:)
    integer(kind=SI), intent(in) :: flag
    character(len=maxSpecLength), allocatable :: speciesNames(:)
    integer(kind=NPI) :: i, j, output_file_number
    character(len=maxReactionStringLength) :: reaction
    character(len=maxReactionStringLength) :: header
    logical :: first_time = .true.

    if ( size( r, 1 ) /= size( arrayLen ) ) then
      stop "size( r, 1 ) /= size( arrayLen ) in outputRates()."
    end if
    ! Add headers at the first call
    if ( first_time .eqv. .true. ) then
      header = "          time speciesNumber speciesName reactionNumber           rate  reaction"
      write (56,*) header
      write (60,*) header
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

      do j = 1, arrayLen(i)
        if ( ( r(i, j)%reaction /= -1_NPI ) .and. ( r(i, j)%frequency /= 0_NPI ) ) then
          reaction = getReaction( speciesNames, r(i, j)%reaction )
          ! r contains the occurences of each of the detailed species in reactions.
          ! r should have row lengths as in arrayLen, so all accesss to r(i,j) should
          ! be valid.
          write (output_file_number, '(ES15.6E3, I14, A52, I15, ES15.6E3, A, A)') t, rSpecies(i), &
                                                                                  trim( speciesNames(rSpecies(i)) ), &
                                                                                  r(i, j)%reaction, &
                                                                                  r(i, j)%stoich * p(r(i, j)%reaction), &
                                                                                  '  ', trim( reaction )
        end if
      end do
    end do

    return
  end subroutine outputRates

  ! -----------------------------------------------------------------
  ! Write reaction rates to file.
  subroutine outputreactionRates( time )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use directories_mod, only : reactionRates_dir
    use reaction_rates_mod, only : reactionRates
    use storage_mod, only : maxFilepathLength

    integer(kind=QI), intent(in) :: time
    integer(kind=NPI) :: i
    character(len=maxFilepathLength+30) :: irfileLocation
    character(len=30) :: strTime

    write (strTime,*) time

    irfileLocation = trim( reactionRates_dir ) // '/' // adjustl( strTime )

    open (10, file=irfileLocation)
    write (10,*) 'reactionNumber reactionRate'
    do i = 1, size( reactionRates )
      write (10, '(I0, ES15.6E3)') i, reactionRates(i)
    end do
    close (10, status='keep')

    return
  end subroutine outputreactionRates

  ! -----------------------------------------------------------------
  ! Print each element of arrayOfConcs, with size arrayOfConcsSize.
  ! If a concentration is negative, then set it to zero before printing.
  subroutine outputSpeciesOfInterest( t, specOutReqNames, allSpeciesConcs )
    use types_mod
    use storage_mod, only : maxSpecLength
    use config_functions_mod, only : getSubsetOfConcs

    real(kind=DP), intent(in) :: t
    character(len=maxSpecLength), intent(in) :: specOutReqNames(:)
    real(kind=DP), intent(in) :: allSpeciesConcs(:)
    real(kind=DP), allocatable :: arrayOfConcs(:)
    integer(kind=NPI) :: i
    logical :: first_time = .true.

    arrayOfConcs = getSubsetOfConcs( allSpeciesConcs, specOutReqNames )
    if ( size( specOutReqNames ) /= size( arrayOfConcs ) ) then
      stop "size( specOutReqNames ) /= size( arrayOfConcs ) in outputSpeciesOfInterest()."
    end if

    if ( first_time .eqv. .true. ) then
      write (50, '(1000A50) ') 't', (trim( specOutReqNames(i) ), i = 1, size( specOutReqNames ))
      first_time = .false.
    end if

    do i = 1, size( arrayOfConcs )
      if ( arrayOfConcs(i) < 0.0_DP ) then
        arrayOfConcs(i) = 0.0_DP
      end if
    end do
    write (50, '(1000 (ES50.6E3)) ') t, (arrayOfConcs(i), i = 1, size( arrayOfConcs ))

    return
  end subroutine outputSpeciesOfInterest

  ! -----------------------------------------------------------------
  ! This routine outputs speciesNames and speciesConcs to
  ! modelOutput/finalModelState.output
  subroutine outputFinalModelState( names, concentrations )
    use types_mod

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

end module output_functions_mod
