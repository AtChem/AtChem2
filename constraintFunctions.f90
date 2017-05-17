module constraintFunctions_mod
contains

  ! ----------------------------------------------------------------- !

  subroutine calcJFac( t, jfac )
    use types_mod
    use zenithData1
    use photolysisRates
    use constraints
    use interpolationFunctions_mod, only : getConstrainedQuantAtT
    use interpolationMethod, only : getConditionsInterpMethod
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP), intent(out) :: jfac
    real(kind=DP) :: JSpeciesAtT
    integer(kind=NPI) :: basePhotoRateNum, i
    logical :: firstTime = .true.

    if ( firstTime .eqv. .true. ) then
      write (*,*) "basePhotoRate: ", jFacSpecies
      firstTime = .false.
    end if

    !GET INDEX OF basePhotoRate SPECIES IN PHOTO CONSTRAINT ARRAY
    basePhotoRateNum = 0
    do i = 1, numConPhotoRates
      if ( trim( constrainedPhotoRates(i) ) == trim( jFacSpecies ) ) then
        basePhotoRateNum = i
      end if
    end do

    if ( basePhotoRateNum == 0 ) then
      write (*,*) 'Error! Missing constrained photo rate data for the JFAC species provided: ', trim( jFacSpecies )!
      stop 2
    end if

    !GET CURRENT VALUE OF basePhotoRate

    call getConstrainedQuantAtT( t, photoX, photoY, photoY2, photoNumberOfPoints (basePhotoRateNum), &
                                 getConditionsInterpMethod(), basePhotoRateNum, JSpeciesAtT )

    if ( JSpeciesAtT == 0 ) then
      jfac = 0
    else
      if ( usePhotolysisConstants .eqv. .false. ) then
        jfac = JspeciesAtT / ( transmissionFactor(jfacSpeciesLine) * cl(jfacSpeciesLine) * &
               ( cosx ** cmm(jfacSpeciesLine) ) * exp( -cnn(jfacSpeciesLine) * secx ) )
      else
        write (*,*) 'Error! JFAC should not be used, as constant photolysis rates have been provided.'
        stop 2
      end if
    end if
    return
  end subroutine calcJFac

  ! ----------------------------------------------------------------- !

  subroutine addConstrainedSpeciesToProbSpec( z, constrainedConcs, constrainedSpecies, x )
    ! This fills x with the contents of z, plus the contents of constrainedConcs, using
    ! constrainedSpecies as the key to which species are constrained.
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: z(*), constrainedConcs(:)
    integer(kind=NPI), intent(in) :: constrainedSpecies(:)
    real(kind=DP), intent(out) :: x(:)
    integer(kind=NPI) :: zCounter, i, j, speciesConstrained

    if ( size( constrainedConcs ) /= size( constrainedSpecies ) ) then
      stop 'size( constrainedConcs ) /= size( constrainedSpecies ) in addConstrainedSpeciesToProbSpec().'
    end if
    zCounter = 1
    do i = 1, size( x )
      speciesConstrained = 0
      do j = 1, size( constrainedSpecies )
        if ( i == constrainedSpecies(j) ) then
          speciesConstrained = j
          exit
        end if
      end do
      if ( speciesConstrained > 0 ) then
        x(i) = constrainedConcs(speciesConstrained)
      else if ( speciesConstrained == 0 ) then
        x(i) = z(zCounter)
        zCounter = zCounter + 1
      else
        write (*,*) 'Error adding constrained values to measured values'
      end if
    end do
    return
  end subroutine addConstrainedSpeciesToProbSpec

  ! ----------------------------------------------------------------- !

  subroutine removeConstrainedSpeciesFromProbSpec( y, constrainedSpecies, z )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: y(:)
    integer(kind=NPI), intent(in) :: constrainedSpecies(:)
    real(kind=DP), intent(inout) :: z(*)
    integer(kind=NPI) :: zCounter, i, k
    logical :: speciesConstrained

    zCounter = 1
    ! loop through y(), check if its items are in constrainedSpecies
    do i = 1, size( y )
      speciesConstrained = .false.
      do k = 1, size( constrainedSpecies )
        if ( i == constrainedSpecies(k) ) then
          ! exit loop if in constrainedSpecies
          speciesConstrained = .true.
          exit
        end if
      end do
      ! if item is not in constrainedSpecies, then add species to z.
      if ( speciesConstrained .eqv. .false. ) then
        z(zCounter) = y(i)
        zCounter = zCounter + 1
      end if
    end do
    return
  end subroutine removeConstrainedSpeciesFromProbSpec

  ! ----------------------------------------------------------------- !

  subroutine getEnvVarsAtT( t )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use storage, only : maxEnvVarNameLength
    use envVars
    use constraints
    use zenithData1
    use interpolationFunctions_mod, only : getConstrainedQuantAtT
    use interpolationMethod, only : getConditionsInterpMethod
    use conversionFunctions_mod
    use utilityFunctions_mod, only : zenith
    implicit none

    real(kind=DP) :: t, theta
    real(kind=DP) :: this_env_val
    integer(kind=NPI) :: envVarNum
    character(len=maxEnvVarNameLength) :: this_env_var_name
    logical :: got_temp, got_press, got_h2o, got_dec

    got_temp = .false.
    got_press = .false.
    got_h2o = .false.
    got_dec = .false.
    ! loop over eavh environment variable
    do envVarNum = 1, size( envVarNames )
      this_env_var_name = envVarNames(envVarNum)

      ! Need to keep track of whether RH is called before or after H2O.
      ! Handle the fact pressure and temperature _may_ need calculating before M,
      ! and pressure, temperature and H2O before RH, and DEC before JFAC

      ! Find which type it is (calc, constrained, fixed, other)
      select case ( envVarTypesNum(envVarNum) )
        case ( 1 ) !CALC
          select case ( this_env_var_name )
            case ( 'PRESS', 'TEMP', 'H2O', 'BLHEIGHT', 'RH', 'DILUTE', 'ROOFOPEN' )
              write (stderr,*) 'getEnvVarsAtT(): No calculation available for ' // trim( this_env_var_name )
              stop
            case ( 'M' )
              if ( got_temp .eqv. .false. ) then
                stop 'not got temp before m'
              end if
              if ( got_press .eqv. .false. ) then
                stop 'not got press before m'
              end if
              this_env_val = calcM(currentEnvVarValues(getEnvVarNum( 'PRESS' )), currentEnvVarValues(getEnvVarNum( 'TEMP' )))
            case ( 'DEC' )
              call calcDec( this_env_val, t )
            case ( 'JFAC' )
              if ( got_dec .eqv. .false. ) then
                stop 'not got dec before jfac'
              end if
              call calcJFac( t, this_env_val )
            case default
              write (stderr,*) 'getEnvVarsAtT(): invalid environment name ' // trim( this_env_var_name )
              stop
          end select

        case ( 2 ) ! CONSTRAINED
          call getConstrainedQuantAtT( t, envVarX, envVarY, envVarY2, envVarNumberOfPoints(envVarNum), &
                                       getConditionsInterpMethod(), envVarNum, this_env_val )
          ! if RH, then set H2O based upon RH,
          if ( this_env_var_name == 'RH' ) then
            if ( got_temp .eqv. .false. ) then
              stop 'not got TEMP before RH'
            end if
            if ( got_press .eqv. .false. ) then
              stop 'not got PRESS before RH'
            end if
            if ( got_h2o .eqv. .false. ) then
              stop 'not got H2O before RH, so this will be overwritten when H2O is processed'
            end if
            currentEnvVarValues(getEnvVarNum( 'H2O' )) = convertRHtoH2O(this_env_val, &
                                                                        currentEnvVarValues(getEnvVarNum( 'TEMP' )), &
                                                                        currentEnvVarValues(getEnvVarNum( 'PRESS' )))
          end if

        case ( 3 ) ! FIXED VALUE
          this_env_val = envVarFixedValues(envVarNum)
          ! if RH, then set H2O based upon RH,
          if ( this_env_var_name == 'RH' ) then
            if ( got_temp .eqv. .false. ) then
              stop 'not got TEMP before RH'
            end if
            if ( got_press .eqv. .false. ) then
              stop 'not got PRESS before RH'
            end if
            if ( got_h2o .eqv. .false. ) then
              stop 'not got H2O before RH, so this will be overwritten when H2O is processed'
            end if
            currentEnvVarValues(getEnvVarNum( 'H2O' )) = convertRHtoH2O(this_env_val, &
                                                                        currentEnvVarValues(getEnvVarNum( 'TEMP' )), &
                                                                        currentEnvVarValues(getEnvVarNum( 'PRESS' )))
          end if

        case default
          select case ( this_env_var_name )
            case ( 'PRESS', 'TEMP', 'H2O', 'M', 'BLHEIGHT', 'RH' )
              this_env_val = -1
            case ( 'DEC' )
              stop 'Error! DEC variable must be provided.' // &
                   'Please set it to the declination angle of the sun ' // &
                   '(or to CALC and then set a correct date ).'
            case ( 'DILUTE' )
              this_env_val = 0
            case ( 'JFAC', 'ROOFOPEN' )
              this_env_val = 1
            case default
              write (stderr,*) 'getEnvVarsAtT(): invalid environment name ' // trim( this_env_var_name )
              stop
          end select
      end select
      currentEnvVarValues(envVarNum) = this_env_val
      ! Copy this_env_var_name to the correct output variable
      select case ( this_env_var_name )
        case ( 'TEMP' )
          got_temp = .true.
        case ( 'RH' )
        case ( 'H2O' )
          got_h2o = .true.
        case ( 'DEC' )
          got_dec = .true.
          call zenith( t, this_env_val, theta, secx, cosx )
        case ( 'PRESS' )
          got_press = .true.
        case ( 'M' )
        case ( 'BLHEIGHT' )
        case ( 'DILUTE' )
        case ( 'JFAC' )
        case ( 'ROOFOPEN' )
        case default
          write(stderr,*) 'getEnvVarsAtT(): invalid environment name ' // trim( this_env_var_name )
          stop
      end select
    end do

    return
  end subroutine getEnvVarsAtT

  ! ----------------------------------------------------------------- !

  function getEnvVarNum( name ) result ( envVarNum )
    ! Set envVarNum to the index of name within enVarNames
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use envVars, only : envVarNames, numEnvVars
    implicit none

    character(len=*), intent(in) :: name
    integer(kind=NPI) :: envVarNum
    integer(kind=NPI) :: i

    envVarNum = 0
    do i = 1, numEnvVars
      if ( name == trim( envVarNames(i) ) ) then
        envVarNum = i
        exit
      end if
    end do
    if ( envVarNum == 0 ) then
      write (stderr,*) 'The name ' // trim( name ) // 'is not found in getEnvVarNum().'
      stop
    end if
    return
  end function getEnvVarNum

  ! ----------------------------------------------------------------- !

  subroutine test_jfac()
    ! check jfac data consistency
    use types_mod
    use photolysisRates
    use envVars
    implicit none
    integer(kind=NPI) :: envVarNum
    ! If JFAC species is provided (e.g. JNO2) and constraint file is not provided, then the program should complain.
    envVarNum = getEnvVarNum( 'JFAC' )
    !IF CALC
    ! If JFAC is CALC and there's no JFAC species, the program should complain
    if ( envVarTypesNum(envVarNum) == 1 ) then
      if ( '' == trim( jFacSpecies ) ) then
        write (*,*) 'Error! JFAC was set to CALC, but JFac species was not provided!'!
        stop 2
      end if
      ! If jfacSpeciesLine = 0 (no line in photolysis rates matches the JFac species), program should complain
      if ( jfacSpeciesLine == 0 ) then
        write (*,*) 'Error! No match found in photolysis rates file for provided JFAC species ', jFacSpecies
      end if
      !IF CONSTRAINED
    else if ( envVarTypesNum(envVarNum) == 2 ) then
      !IF FIXED
    else if ( envVarTypesNum(envVarNum) == 3 ) then
      !IF NOTUSED
      ! if JFAC is NOTUSED: and JFAC species has anything in, the program should complain.
    else
      if ( '' /= trim( jFacSpecies ) ) then
        write (*,*) 'Error! JFAC was set to NOTUSED, but at the same time JFac species was provided!'!
        write (*,*) 'JFac species: ', jFacSpecies
        stop 2
      end if
    end if
  end subroutine test_jfac

  ! ----------------------------------------------------------------- !

end module constraintFunctions_mod
