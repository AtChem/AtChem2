! ******************************************************************** !
! ATCHEM -- MODULE atmosphereFunctions
!
! ??? Text describing the module ???
! ******************************************************************** !
module atmosphereFunctions_mod
contains

  ! -----------------------------------------------------------------
  ! Calculate the number density of air (molecule cm-3) from pressure
  ! (mbar) and temperature (K).
  pure function calcAirDensity( press, temp ) result ( m )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: press, temp
    real(kind=DP) :: m, press_pa

    press_pa = press * 1.0d+02
    m = 1.0d-06 * ( 6.02214129d+23 / 8.3144621 ) * ( press_pa / temp )

    return
  end function calcAirDensity

  ! -----------------------------------------------------------------
  ! Calculate the number density of oxygen and nitrogen in the atmosphere
  ! from number density of air (molecule cm-3).
  subroutine calcAtmosphere( m, o2, n2 )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: m
    real(kind=DP), intent(out) :: o2, n2

    o2 = 0.2095_DP * m
    n2 = 0.7809_DP * m

    return
  end subroutine calcAtmosphere

  ! -----------------------------------------------------------------
  ! Convert relative humidity to water concentration (molecule cm-3).
  ! Pressure in mbar (1 mbar = 1 hPa), temperature in K. Equations
  ! from "Humidity Conversion Formulas" by Vaisala (2013).
  pure function convertRHtoH2O( rh, temp, press ) result ( h2o )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: rh, temp, press
    real(kind=DP) :: h2o, h2o_ppm, temp_c, wvp

    ! Use Eq.6 to calculate the water vapour saturation pressure and
    ! Eq.1 to calculate the water vapour pressure from relative
    ! humidity (see Vaisala paper).
    temp_c = temp - 273.15
    wvp = ( rh/100 ) * ( 6.116441 * 10**((7.591386 * temp_c)/(temp_c + 240.7263)) )

    ! Calculate volume of water vapour per volume of dry air using
    ! Eq.18 (see Vaisala paper).
    h2o_ppm = 1.0d+06 * wvp / (press - wvp)

    ! convert ppm to molecule cm-3
    h2o = h2o_ppm * calcAirDensity(press, temp) * 1.0d-06

    return
  end function convertRHtoH2O

  ! -----------------------------------------------------------------
  ! subroutine to calculate diurnal variations in temperature
  ! --> CURRENTLY UNUSED, BUT IT MAY BE USEFUL <--
  subroutine temperature( temp, h2o, ttime )
    use types_mod
    implicit none

    real(kind=DP) :: temp, ttime, rh, h2o, sin, h2o_factor

    temp = 289.86 + 8.3 * sin( ( 7.2722D-5 * ttime ) - 1.9635 )
    temp = 298.00
    rh = 23.0 * sin( ( 7.2722D-5 * ttime ) + 1.1781 ) + 66.5
    h2o_factor = 10.0 / ( 1.38D-16 * temp ) * rh
    h2o = 6.1078 * exp( -1.0D+0 * ( 597.3 - 0.57 * ( temp - 273.16 ) ) * 18.0 / 1.986 * ( 1.0 / temp - 1.0 / 273.16 ) ) * h2o_factor

    return
  end subroutine temperature

end module atmosphereFunctions_mod
