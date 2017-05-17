module mechanismRates_mod

contains
  subroutine mechanism_rates( p, t, y, mnsp )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use storage, only : maxEnvVarNameLength
    use photolysisRates
    use zenithData1, only : cosX, secX
    use constraints
    use envVars, only : ro2, envVarNames, currentEnvVarValues
    use interpolationFunctions_mod, only : getConstrainedQuantAtT
    use interpolationMethod, only : getConditionsInterpMethod
    use outputFunctions_mod, only : ro2sum
    use constraintFunctions_mod, only : getEnvVarsAtT, getEnvVarNum
    use utilityFunctions_mod, only : atmosphere
    implicit none

    ! calculates rate constants from arrhenius information
    real(kind=DP), intent(out) :: p(*)
    real(kind=DP), intent(in) :: t
    integer(kind=NPI), intent(in) :: mnsp
    real(kind=DP), intent(in) :: y(mnsp)
    real(kind=DP) :: temp, pressure, dummy, this_env_val

    integer(kind=NPI) :: i
    real(kind=DP) :: photoRateAtT
    character(len=maxEnvVarNameLength) :: this_env_var_name

    include 'modelConfiguration/mechanism-rate-declarations.f90'

    call ro2sum( ro2, y )
    dummy = y(1)

    dec = -1e16

    call getEnvVarsAtT( t )

    do i = 1, size( envVarNames )
      this_env_var_name = envVarNames(i)
      this_env_val = currentEnvVarValues(getEnvVarNum(this_env_var_name))
      select case ( this_env_var_name )
        case ( 'TEMP' )
          temp = this_env_val
        case ( 'RH' )
          rh = this_env_val
        case ( 'H2O' )
          h2o = this_env_val
        case ( 'DEC' )
          dec = this_env_val
        case ( 'PRESS' )
          pressure = this_env_val
        case ( 'M' )
          m = this_env_val
        case ( 'BLHEIGHT' )
          blh = this_env_val
        case ( 'DILUTE' )
          dilute = this_env_val
        case ( 'JFAC' )
          jfac = this_env_val
        case ( 'ROOFOPEN' )
          roofOpen = this_env_val
        case default
          write(stderr,*) 'getEnvVarsAtT(): invalid environment name ' // trim( this_env_var_name )
          stop
      end select
    end do

    call atmosphere( o2, n2, m )

    !O2 = 0.2095*m
    !N2 = 0.7809*m

    do i = 1, nrOfPhotoRates
      if ( usePhotolysisConstants .eqv. .false. ) then
        if ( cosX < 1.00d-10 ) then
          j(ck(i)) = 1.0d-30
        else
          j(ck(i)) = cl(i) * cosX ** cmm(i) * exp( -cnn(i) * secX ) * transmissionFactor(i) * roofOpen * jfac
        end if
      else
        j(ck(i)) = cl(i)
      end if
    end do

    do i = 1, numConPhotoRates
      call getConstrainedQuantAtT( t, photoX, photoY, photoY2, photoNumberOfPoints(i), &
                                   getConditionsInterpMethod(), i, photoRateAtT )
      j(constrainedPhotoRatesNumbers(i)) = photoRateAtT
    end do

    include 'modelConfiguration/mechanism-rate-coefficients.f90'
    return
  end subroutine mechanism_rates
end module mechanismRates_mod
