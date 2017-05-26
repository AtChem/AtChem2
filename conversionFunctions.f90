module conversionFunctions_mod
contains

  ! ----------------------------------------------------------------- !

  pure function calcAirDensity( press, temp ) result ( m )
    ! calculate the number density of air (molecule cm-3) from
    ! pressure (mbar) and temperature (K)
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: press, temp
    real(kind=DP) :: m, press_pa

    press_pa = press * 1.0d+02
    m = 1.0d-06 * ( 6.02214129d+23 / 8.3144621 ) * ( press_pa / temp )
    return
  end function calcAirDensity

  ! ----------------------------------------------------------------- !

  pure function convertRHtoH2O( rh, temp, press ) result ( h2o )
    ! convert relative humidity to water concentration (molecule cm-3)
    ! pressure in mbar (1 mbar = 1 hPa), temperature in K
    !
    ! from "Humidity Conversion Formulas" published by Vaisala (2013)
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: rh, temp, press
    real(kind=DP) :: h2o, h2o_ppm, temp_c, wvp

    ! convert temperature to celsius; use eq.6 to calculate the water
    ! vapour saturation pressure; use eq.1 to calculate the water
    ! vapour pressure from relative humidity (see Vaisala paper)
    temp_c = temp - 273.15
    wvp = ( rh/100 ) * ( 6.116441 * 10**((7.591386 * temp_c)/(temp_c + 240.7263)) )

    ! calculate volume of water vapour per volume of dry air using
    ! eq.18 (see Vaisala paper)
    h2o_ppm = 1.0d+06 * wvp / (press - wvp)

    ! convert ppm to molecule cm-3
    h2o = h2o_ppm * calcAirDensity(press, temp) * 1.0d-06

    return
  end function convertRHtoH2O

  ! ----------------------------------------------------------------- !

end module conversionFunctions_mod
