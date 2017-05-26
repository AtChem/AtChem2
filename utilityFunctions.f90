module utilityFunctions_mod
contains

  ! ----------------------------------------------------------------- !
  ! TODO: this is unused?
  subroutine temperature( temp, h2o, ttime )
    use types_mod
    implicit none
    ! subroutine to calculate diurnal variations in temperature
    real(kind=DP) :: temp, ttime, rh, h2o, sin, h2o_factor

    temp = 289.86 + 8.3 * sin( ( 7.2722D-5 * ttime ) - 1.9635 )
    temp = 298.00
    rh = 23.0 * sin( ( 7.2722D-5 * ttime ) + 1.1781 ) + 66.5
    h2o_factor = 10.0 / ( 1.38D-16 * temp ) * rh
    h2o = 6.1078 * exp( -1.0D+0 * ( 597.3 - 0.57 * ( temp - 273.16 ) ) * 18.0 / 1.986 * ( 1.0 / temp - 1.0 / 273.16 ) ) * h2o_factor

    return
  end subroutine temperature

  ! ----------------------------------------------------------------- !

  subroutine calcAtmosphere( m, o2, n2 )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: m
    real(kind=DP) :: o2, n2

    o2 = 0.2095 * m
    n2 = 0.7809 * m
    return
  end subroutine calcAtmosphere

  ! ----------------------------------------------------------------- !

  pure function calcDec( t ) result ( dec )
    ! calculate the Sun Declination (radians): the sun declination is
    ! the angle between the Sun and Earth's equatorial plane
    use types_mod
    use date, only : secondsInYear, dayAsFractionOfYear
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP) :: dec, pi, daysInYear, maxDecInRad, currentFYear, temporary

    daysInYear = 365.24
    pi = 4.0 * atan( 1.0 )
    maxDecInRad = 23.44 * pi / 180.0
    currentFYear = dayAsFractionOfYear + ( t / secondsInYear )
    temporary = 2.0 * 0.0167 * sin( 2 * pi * ( currentFYear - ( 2.0 / daysInYear ) ) )
    dec = asin( sin( -maxDecInRad ) * cos( ( 2.0 * pi ) * ( currentFYear + ( 10.0 / daysInYear ) ) + temporary ) )
    return
  end function calcDec

  ! ----------------------------------------------------------------- !

  subroutine calcZenith( t, dec )
    ! calculate the Local Hour Angle (radians) and the Solar Zenith
    ! Angle (radians)
    use types_mod
    use zenithData
    implicit none

    real(kind=DP), intent(in) :: t, dec
    real(kind=DP) :: pi, radian, lat, ramped_cosx

    ! convert latitude to radians
    pi = 4.0 * atan( 1.0 )
    radian = 180.0 / pi
    lat = latitude / radian

    ! calculate local hour angle - representing time of day
    ! LHA is the angle between observer's meridian and Sun's meridian
    lha = ( 1.0 + ( t / 4.32d+04 ) ) * pi

    ! calculate cosine of solar zenith angle (cosx)
    ! SZA is the angle between local vertical and center of the Sun
    sinld = sin( lat ) * sin( dec )
    cosld = cos( lat ) * cos( dec )
    cosx = cos( lha ) * cosld + sinld

    ! set negative cosx to zero and calculate the inverse (secx).
    ! secx is needed for the MCM photolysis parameterisation
    ramped_cosx = max(0.0_DP, cosx )
    secx = 1.0 / ( ramped_cosx + 1.00d-30 )

    return
  end subroutine calcZenith

  ! ----------------------------------------------------------------- !

end module utilityFunctions_mod
