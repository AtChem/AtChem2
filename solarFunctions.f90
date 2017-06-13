! ******************************************************************** !
!
! ******************************************************************** !
module solarFunctions_mod
contains

  ! -----------------------------------------------------------------
  ! calculate number density of oxygen and nitrogen in the atmosphere
  ! from M (air density, molecule cm-3)
  subroutine calcAtmosphere( m, o2, n2 )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: m
    real(kind=DP) :: o2, n2

    o2 = 0.2095_DP * m
    n2 = 0.7809_DP * m

    return
  end subroutine calcAtmosphere

  ! ----------------------------------------------------------------- !
  ! calculate the Sun Declination (radians): the sun declination is
  ! the angle between the Sun and Earth's equatorial plane
  pure function calcDec( t ) result ( dec )
    use types_mod
    use date_mod, only : secondsInYear, dayAsFractionOfYear
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP) :: dec, pi, daysInYear, maxDecInRad, currentFYear, temporary

    daysInYear = 365.24_DP
    pi = 4.0_DP * atan( 1.0_DP )
    maxDecInRad = 23.44_DP * pi / 180.0_DP
    currentFYear = dayAsFractionOfYear + ( t / secondsInYear )
    temporary = 2.0_DP * 0.0167_DP * sin( 2.0_DP * pi * ( currentFYear - ( 2.0_DP / daysInYear ) ) )
    dec = asin( sin( -maxDecInRad ) * cos( ( 2.0_DP * pi ) * ( currentFYear + ( 10.0_DP / daysInYear ) ) + temporary ) )

    return
  end function calcDec

  ! -----------------------------------------------------------------
  ! calculate the Local Hour Angle (radians) and the Solar Zenith
  ! Angle (radians)
  subroutine calcZenith( t, dec )
    use types_mod
    use zenithData
    implicit none

    real(kind=DP), intent(in) :: t, dec
    real(kind=DP) :: pi, radian, lat, ramped_cosx

    ! convert latitude to radians
    pi = 4.0_DP * atan( 1.0_DP )
    !write (*,*) t, pi
    radian = 180.0_DP / pi
    lat = latitude / radian

    ! calculate local hour angle - representing time of day
    ! LHA is the angle between observer's meridian and Sun's meridian
    lha = ( 1.0_DP + ( t / 43200.0_DP ) ) * pi

    ! calculate cosine of solar zenith angle (cosx)
    ! SZA is the angle between local vertical and center of the Sun
    sinld = sin( lat ) * sin( dec )
    cosld = cos( lat ) * cos( dec )
    cosx = cos( lha ) * cosld + sinld

    ! set negative cosx to zero and calculate the inverse (secx).
    ! secx is needed for the MCM photolysis parameterisation
    ramped_cosx = max(0.0_DP, cosx )
    secx = 1.0_DP / ( ramped_cosx + 1.00d-30 )

    return
  end subroutine calcZenith

end module solarFunctions_mod
