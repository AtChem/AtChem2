module conversionFunctions_mod
  use types_mod
contains

  ! ----------------------------------------------------------------- !

  subroutine calcDec( dec, t )
    ! calculate the declination of the Sun as seen from Earth.
    use date, only : secondsInYear, dayAsFractionOfYear
    implicit none

    real(kind=DP) :: dec, t, pi, daysInYear, maxDecInRad, currentFYear, temporary

    daysInYear = 365.24
    pi = 4.0 * atan( 1.0 )
    maxDecInRad = 23.44 * pi / 180.0
    currentFYear = dayAsFractionOfYear + ( t / secondsInYear )
    temporary = 2.0 * 0.0167 * sin( 2 * pi * ( currentFYear - ( 2.0 / daysInYear ) ) )
    dec = asin( sin( -maxDecInRad ) * cos( ( 2.0 * pi ) * ( currentFYear + ( 10.0 / daysInYear ) ) + temporary ) )
    return
  end subroutine calcDec

  ! ----------------------------------------------------------------- !

  function calcM( press, temp ) result ( m )
    ! calculate the number density of air (molecule cm-3) from
    ! pressure (mbar) and temperature (K)
    implicit none

    real(kind=DP) :: press, temp, m
    real(kind=DP) :: press_pa

    press_pa = press * 1.0d+2
    m = ( 6.02214129d+23 / 8.3144621 ) * ( press_pa / temp )
    return
  end function calcM

  ! ----------------------------------------------------------------- !

  function calcPressure( m, temp ) result ( press )
    ! calculate pressure (mbar) from the number density of air
    ! (molecule cm-3) and temperature (K)
    implicit none

    real(kind=DP) :: m, temp, press
    real(kind=DP) :: press_pa

    press_pa = ( m * temp ) / ( 6.02214129d+23 / 8.3144621 )
    press = press_pa/1.0d+2
    return
  end function calcPressure

  ! ----------------------------------------------------------------- !

  function convertRHtoH2O( rh, temp, press ) result ( h2o )
    ! convert relative humidity to water concentration (molecule cm-3)
    ! pressure in mbar, temperature in K
    implicit none

    real(kind=DP) :: rh, temp, press, h2o
    real(kind=DP) :: exponent, e1

    exponent = exp( -1.00d00 * ( 597.30d00 - 0.57d00 * ( temp - 273.16d00 ) ) * &
                   18.00d00 / 1.986d00 * ( 1.00d00 / temp - 1.00d00 / 273.16d00 ) )

    e1 = 10d0 / ( 1.38d-16 * temp ) * rh
    h2o = 6.1078d0 * exponent * e1
    return
  end function convertRHtoH2O

  ! ----------------------------------------------------------------- !

end module conversionFunctions_mod
