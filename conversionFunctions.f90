module conversionFunctions_mod
  use types_mod
contains

  ! ----------------------------------------------------------------- !

  pure function calcDec( t ) result ( dec )
    ! calculate the declination of the Sun as seen from Earth.
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

  pure function calcM( press, temp ) result ( m )
    ! calculate the number density of air (molecule cm-3) from
    ! pressure (mbar) and temperature (K)
    implicit none

    real(kind=DP), intent(in) :: press, temp
    real(kind=DP) :: m, press_pa

    press_pa = press * 1.0d+02
    m = 1.0d-06 * ( 6.02214129d+23 / 8.3144621 ) * ( press_pa / temp )
    return
  end function calcM

  ! ----------------------------------------------------------------- !

  pure function convertRHtoH2O( rh, temp, press ) result ( h2o )
    ! convert relative humidity to water concentration (molecule cm-3)
    ! pressure in mbar (1 mbar = 1 hPa), temperature in K
    implicit none

    real(kind=DP), intent(in) :: rh, temp, press
    real(kind=DP) :: h2o, h2o_ppm, temp_c, wvp

    temp_c = temp - 273.15
    wvp = (rh/100) * 6.116441 * 10**( (7.591386 * temp_c) / (temp_c + 240.7263) )
    h2o_ppm = 1.0d+06 * wvp / (press - wvp)
    h2o = h2o_ppm * calcM(press, temp) * 1.0d-06

    return
  end function convertRHtoH2O

  ! ----------------------------------------------------------------- !

end module conversionFunctions_mod
