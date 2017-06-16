! ******************************************************************** !
! ATCHEM -- MODULE constraintFunctions
!
! ??? Text describing the module ???
! ******************************************************************** !
module constraintFunctions_mod
contains

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine calcJFac( t, jfac )
    use types_mod
    use zenith_data_mod
    use photolysis_rates_mod
    use constraints_mod
    use interpolationFunctions_mod, only : getConstrainedQuantAtT
    use interpolation_method_mod, only : getConditionsInterpMethod
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP), intent(out) :: jfac
    real(kind=DP) :: JSpeciesAtT
    integer(kind=NPI) :: basePhotoRateNum, i
    logical :: firstTime = .true.

    if ( firstTime .eqv. .true. ) then
      write (*, '(2A)') ' basePhotoRate: ', trim( jFacSpecies )
      firstTime = .false.
    end if

    ! GET INDEX OF basePhotoRate SPECIES IN PHOTO CONSTRAINT ARRAY
    basePhotoRateNum = 0
    do i = 1, numConPhotoRates
      if ( trim( constrainedPhotoRates(i) ) == trim( jFacSpecies ) ) then
        basePhotoRateNum = i
      end if
    end do

    if ( basePhotoRateNum == 0 ) then
      write (*, '(2A)') ' Error! Missing constrained photo rate data for the JFAC species provided: ', trim( jFacSpecies )!
      stop 2
    end if

    ! GET CURRENT VALUE OF basePhotoRate

    call getConstrainedQuantAtT( t, photoX, photoY, photoNumberOfPoints (basePhotoRateNum), &
                                 getConditionsInterpMethod(), basePhotoRateNum, JSpeciesAtT )

    if ( JSpeciesAtT == 0 ) then
      jfac = 0
    else
      if ( usePhotolysisConstants .eqv. .false. ) then
        if ( infty_secx .eqv. .false. ) then
          jfac = JspeciesAtT / ( transmissionFactor(jfacSpeciesLine) * cl(jfacSpeciesLine) * &
                 ( cosx ** cmm(jfacSpeciesLine) ) * exp( -cnn(jfacSpeciesLine) * secx ) )
        else
          jfac = 0
        end if
      else
        write (*, '(A)') ' Error! JFAC should not be used, as constant photolysis rates have been provided.'
        stop 2
      end if
    end if
    return
  end subroutine calcJFac

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine addConstrainedSpeciesToProbSpec( z, constrainedConcentrations, constrainedSpecs, x )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: z(*), constrainedConcentrations(:)
    integer(kind=NPI), intent(in) :: constrainedSpecs(:)
    real(kind=DP), intent(out) :: x(:)
    integer(kind=NPI) :: zCounter, i, j, speciesConstrained

    ! This fills x with the contents of z, plus the contents of
    ! constrainedConcentrations, using constrainedSpecs as the key to
    ! which species are constrained.
    if ( size( constrainedConcentrations ) /= size( constrainedSpecs ) ) then
      stop 'size( constrainedConcentrations ) /= size( constrainedSpecs ) in addConstrainedSpeciesToProbSpec().'
    end if
    zCounter = 1
    do i = 1, size( x )
      speciesConstrained = 0
      do j = 1, size( constrainedSpecs )
        if ( i == constrainedSpecs(j) ) then
          speciesConstrained = j
          exit
        end if
      end do
      if ( speciesConstrained > 0 ) then
        x(i) = constrainedConcentrations(speciesConstrained)
      else if ( speciesConstrained == 0 ) then
        x(i) = z(zCounter)
        zCounter = zCounter + 1
      else
        stop 'Error adding constrained values to measured values'
      end if
    end do
    return
  end subroutine addConstrainedSpeciesToProbSpec

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine removeConstrainedSpeciesFromProbSpec( y, constrainedSpecs, z )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: y(:)
    integer(kind=NPI), intent(in) :: constrainedSpecs(:)
    real(kind=DP), intent(inout) :: z(*)
    integer(kind=NPI) :: zCounter, i, k
    logical :: speciesConstrained

    zCounter = 1
    ! loop through y(), check if its items are in constrainedSpecs
    do i = 1, size( y )
      speciesConstrained = .false.
      do k = 1, size( constrainedSpecs )
        if ( i == constrainedSpecs(k) ) then
          ! exit loop if in constrainedSpecs
          speciesConstrained = .true.
          exit
        end if
      end do
      ! if item is not in constrainedSpecs, then add species to z
      if ( speciesConstrained .eqv. .false. ) then
        z(zCounter) = y(i)
        zCounter = zCounter + 1
      end if
    end do
    return
  end subroutine removeConstrainedSpeciesFromProbSpec

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine getEnvVarsAtT( t )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use storage_mod, only : maxEnvVarNameLength
    use env_vars_mod
    use constraints_mod
    use zenith_data_mod
    use interpolationFunctions_mod, only : getConstrainedQuantAtT
    use interpolation_method_mod, only : getConditionsInterpMethod
    use atmosphereFunctions_mod
    use solarFunctions_mod
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP) :: this_env_val
    integer(kind=NPI) :: envVarNum, orderedEnvVarNum
    character(len=maxEnvVarNameLength) :: this_env_var_name, orderedEnvVarNames(size( envVarNames ))
    logical :: pressure_set, rh_set, temp_set

    ! loop over each environment variable, in a defined order, rather
    ! than just the order of envVarNames, so we can ensure the right
    ! ones are calculated before each other.
    !
    ! Specifically, handle that some variables need to be set before
    ! others (e.g., pressure, temperature and RH before H2O, and DEC
    ! before JFAC). Currently, this relies on
    ! environmentVariables.config having exactly the lines relating to
    ! these 10 variables.
    !
    ! To add another environment variable, the user would need to add
    ! that line to environmentVariables.config, and then add this as
    ! element 11 in the orderedEnvVarNames initialisation below.  Its
    ! treatment needs defining in each of cases 1-3 and default below.

    ! calculate the Day Angle
    call calcTheta( t )

    if ( size( envVarNames ) /= 10 ) then
      write(stderr,*) 'size( envVarNames ) /= 10 in getEnvVarsAtT().'
    end if
    orderedEnvVarNames(1) = 'PRESS'
    orderedEnvVarNames(2) = 'TEMP'
    orderedEnvVarNames(3) = 'M'
    orderedEnvVarNames(4) = 'RH'
    orderedEnvVarNames(5) = 'H2O'
    orderedEnvVarNames(6) = 'BLHEIGHT'
    orderedEnvVarNames(7) = 'DEC'
    orderedEnvVarNames(8) = 'JFAC'
    orderedEnvVarNames(9) = 'DILUTE'
    orderedEnvVarNames(10) = 'ROOFOPEN'

    pressure_set = .false.
    rh_set = .false.
    temp_set = .false.

    do orderedEnvVarNum = 1, size( envVarNames )
      ! loop over in a defined order, then find which number that is
      ! in the unordered list that comes from the input file
      this_env_var_name = orderedEnvVarNames(orderedEnvVarNum)
      envVarNum = getEnvVarNum( trim( this_env_var_name ) )

      ! Find which type it is (calc, constrained, fixed, other)
      select case ( envVarTypesNum(envVarNum) )
        case ( 1 ) ! CALC
          select case ( this_env_var_name )
            case ( 'PRESS', 'TEMP', 'RH', 'BLHEIGHT', 'DILUTE', 'ROOFOPEN' )
              write (stderr,*) 'getEnvVarsAtT(): No calculation available for ' // trim( this_env_var_name )
              stop
            case ( 'M' )
              if ( ( temp_set .eqv. .true. ) .and. ( pressure_set .eqv. .true. ) ) then
                this_env_val = calcAirDensity( currentEnvVarValues( getEnvVarNum( 'PRESS' ) ), &
                                               currentEnvVarValues( getEnvVarNum( 'TEMP' ) ) )
              else
                write (stderr,*) 'getEnvVarsAtT(): calcAirDensity() called, but no value is yet given to either TEMP, or PRESS.'
                stop
              end if
            case ( 'H2O' )
              if ( ( rh_set .eqv. .true. ) .and. ( temp_set .eqv. .true. ) .and. ( pressure_set .eqv. .true. ) ) then
                this_env_val = convertRHtoH2O( currentEnvVarValues( getEnvVarNum( 'RH' ) ), &
                                               currentEnvVarValues( getEnvVarNum( 'TEMP' ) ), &
                                               currentEnvVarValues( getEnvVarNum( 'PRESS' ) ) )
              else
                write (stderr,*) 'getEnvVarsAtT(): convertRHtoH2O() called, but no value is yet given to either RH, TEMP, or PRESS.'
                stop
              end if
            case ( 'DEC' )
              this_env_val = calcDec()
            case ( 'JFAC' )
              call calcJFac( t, this_env_val )
            case default
              write (stderr,*) 'getEnvVarsAtT(): invalid environment name ' // trim( this_env_var_name )
              stop
          end select

        case ( 2 ) ! CONSTRAINED
          call getConstrainedQuantAtT( t, envVarX, envVarY, envVarNumberOfPoints(envVarNum), &
                                       getConditionsInterpMethod(), envVarNum, this_env_val )

          if (this_env_var_name == 'PRESS') pressure_set = .true.
          if (this_env_var_name == 'TEMP') temp_set = .true.
          if (this_env_var_name == 'RH') rh_set = .true.

        case ( 3 ) ! FIXED VALUE
          this_env_val = envVarFixedValues(envVarNum)

          if (this_env_var_name == 'PRESS') pressure_set = .true.
          if (this_env_var_name == 'TEMP') temp_set = .true.
          if (this_env_var_name == 'RH') rh_set = .true.

        case default ! DEFAULT VALUES
          select case ( this_env_var_name )
            case ( 'TEMP' )
              this_env_val = 298.15_DP
              temp_set = .true.
            case ( 'H2O' )
              this_env_val = 3.91d+17
            case ( 'PRESS' )
              this_env_val = 1013.25_DP
            case ( 'BLHEIGHT', 'RH', 'DILUTE', 'M' )
              this_env_val = -1.0_DP
            case ( 'DEC' )
              this_env_val = 0.41_DP
            case ( 'JFAC', 'ROOFOPEN' )
              this_env_val = 1.0_DP
            case default
              write (stderr,*) 'getEnvVarsAtT(): invalid environment name ' // trim( this_env_var_name )
              stop
          end select
      end select
      currentEnvVarValues(envVarNum) = this_env_val

      ! Copy this_env_var_name to the correct output variable
      select case ( this_env_var_name )
        case ( 'TEMP', 'RH', 'H2O', 'PRESS', 'M', 'BLHEIGHT', 'DILUTE', 'JFAC', 'ROOFOPEN' )
        case ( 'DEC' )
          call calcZenith( t, this_env_val )
        case default
          write(stderr,*) 'getEnvVarsAtT(): invalid environment name ' // trim( this_env_var_name )
          stop
      end select
    end do

    return
  end subroutine getEnvVarsAtT

  ! ----------------------------------------------------------------- !
  ! Set envVarNum to the index of name within enVarNames
  function getEnvVarNum( name ) result ( envVarNum )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use env_vars_mod, only : envVarNames, numEnvVars
    implicit none

    character(len=*), intent(in) :: name
    integer(kind=SI) :: envVarNum
    integer(kind=SI) :: i

    envVarNum = 0
    do i = 1, numEnvVars
      if ( name == trim( envVarNames(i) ) ) then
        envVarNum = i
        exit
      end if
    end do
    if ( envVarNum == 0 ) then
      write (stderr,*) 'The name ' // trim( name ) // ' is not found in getEnvVarNum().'
      stop
    end if

    return
  end function getEnvVarNum

  ! ----------------------------------------------------------------- !
  ! check jfac data consistency
  subroutine test_jfac()
    use types_mod
    use photolysis_rates_mod
    use env_vars_mod
    implicit none

    integer(kind=SI) :: envVarNum

    ! If JFAC species is provided (e.g. J4) and constraint file is not provided, then the program should complain.
    envVarNum = getEnvVarNum( 'JFAC' )
    !IF CALC
    ! If JFAC is CALC and there's no JFAC species, the program should complain
    if ( envVarTypesNum(envVarNum) == 1_SI ) then
      if ( '' == trim( jFacSpecies ) ) then
        write (*, '(A)') ' Error! JFAC was set to CALC, but JFac species was not provided!'!
        stop
      end if
      ! If jfacSpeciesLine = 0 (no line in photolysis rates matches the JFac species), program should complain
      if ( jfacSpeciesLine == 0 ) then
        write (*, '(2A)') ' Error! No match found in photolysis rates file for provided JFAC species ', trim( jFacSpecies )
        stop
      end if
      !IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2_SI ) then
      if ( '' /= trim( jFacSpecies ) ) then
        write (*, '(A)') ' Error! JFAC was set to be CONSTRAINED, but at the same time JFac species was provided!'
        write (*, '(2A)') ' JFac species: ', trim( jFacSpecies )
        stop
      end if
      !IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3_SI ) then
      if ( '' /= trim( jFacSpecies ) ) then
        write (*, '(A)') ' Error! JFAC was set to a fixed value, but at the same time JFac species was provided!'
        write (*, '(2A)') ' JFac species: ', trim( jFacSpecies )
        stop
      end if
      !IF NOTUSED
      ! if JFAC is NOTUSED: and JFAC species has anything in, the program should complain.
    else
      if ( '' /= trim( jFacSpecies ) ) then
        write (*, '(A)') ' Error! JFAC was set to NOTUSED, but at the same time JFac species was provided!'
        write (*, '(2A)') ' JFac species: ', trim( jFacSpecies )
        stop
      end if
    end if

  end subroutine test_jfac

end module constraintFunctions_mod
