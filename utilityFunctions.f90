module utilityFunctions_mod
contains
  ! TODO: this is unused?
  subroutine temperature( temp, h2o, ttime )
    use types_mod
    implicit none
    ! subroutine to calculate diurnal variations in temperature
    real(kind=DP) :: temp, ttime, rh, h2o, sin, h2o_factor

    temp = 289.86 + 8.3 * sin( ( 7.2722D-5 * ttime ) - 1.9635 )
    temp = 298.00
    rh = 23.0 * sin( ( 7.2722D-5 * ttime ) + 1.1781 ) + 66.5
    h2o_factor = 10. / ( 1.38D-16 * temp ) * rh
    h2o = 6.1078 * exp( -1.0D+0 * ( 597.3 - 0.57 * ( temp - 273.16 ) ) * 18.0 / 1.986 * ( 1.0 / temp - 1.0 / 273.16 ) ) * h2o_factor

    return
  end subroutine temperature

  subroutine atmosphere( m, o2, n2 )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: m
    real(kind=DP) :: o2, n2

    o2 = 0.2095 * m
    n2 = 0.7809 * m
    return
  end subroutine atmosphere

  subroutine zenith( ttime, dec )
    use zenithData
    use SZACalcVars
    implicit none

    real(kind=DP), intent(in) :: ttime, dec
    real(kind=DP) :: pi, radian, rampValue
    ! solar declination angle from July 1st - Harwell traj model

    lat = latitude
    longt = longitude
    pi = 4.0 * atan( 1.0 )
    ! local hour angle - representing time of day
    lha = ( 1.0 + ( ttime / 4.32D+04 ) ) * pi
    radian = 180.0 / pi
    lat = lat / radian
    ! dec = dec/radian
    sinld = sin( lat ) * sin( dec )
    cosld = cos( lat ) * cos( dec )

    rampValue = ramp( cos( lha ) * cosld + sinld )
    secx = 1.0 / ( rampValue + 1.00d-30 )

    cosx = cos( lha ) * cosld + sinld

    return
  end subroutine zenith

  pure function ramp( arg1 ) result ( rampValue )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: arg1
    real(kind=DP) :: arg2, rampValue

    arg2 = abs( arg1 )
    rampValue = ( arg1 + arg2 ) / 2
    return
  end function ramp
end module utilityFunctions_mod
