module mechanismRates_mod

contains
  subroutine mechanism_rates( p, t, y, mnsp )
    use types_mod
    use photolysisRates
    use zenithData1, only : cosX, secX
    use constraints
    use envVars, only : ro2
    use interpolationFunctions_mod, only : getConstrainedQuantAtT2D
    use outputFunctions_mod, only : ro2sum
    use constraintFunctions_mod, only : getEnvVarsAtT
    use utilityFunctions_mod, only : atmosphere
    implicit none

    ! calculates rate constants from arrhenius information
    real(kind=DP), intent(out) :: p(*)
    real(kind=DP), intent(in) :: t
    integer(kind=NPI), intent(in) :: mnsp
    real(kind=DP), intent(in) :: y(mnsp)
    real(kind=DP) :: temp, pressure, dummy

    integer(kind=NPI) :: i
    real(kind=DP) :: photoRateAtT

    include 'modelConfiguration/mechanism-rate-declarations.f90'

    call ro2sum( ro2, y )
    dummy = y(1)

    dec = -1e16

    call getEnvVarsAtT( t, temp, rh, h2o, dec, pressure, m, blh, dilute, jfac, roofOpen )

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
      call getConstrainedQuantAtT2D( t, photoX, photoY, photoY2, photoNumberOfPoints(i), photoRateAtT, 2, i, &
                                     maxNumberOfDataPoints, numConPhotoRates )
      j(constrainedPhotoRatesNumbers(i)) = photoRateAtT
    end do

    include 'modelConfiguration/mechanism-rate-coefficients.f90'
    return
  end subroutine mechanism_rates
end module mechanismRates_mod
