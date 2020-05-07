! -----------------------------------------------------------------------------
!
! Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
! Peter Jimack, Mike Pilling
!
! Copyright (c) 2017 Sam Cox, Roberto Sommariva
!
! This file is part of the AtChem2 software package.
!
! This file is covered by the MIT license which can be found in the file
! LICENSE.md at the top level of the AtChem2 distribution.
!
! -----------------------------------------------------------------------------

! ******************************************************************** !
! ATCHEM2 -- MODULE atmosphereFunctions
!
! This module contains functions responsible for calculating quantities
! relevant to the atmosphere: specifically, the air density, O2 and N2
! density, and convert from RH to H2O.
! ******************************************************************** !
module atmosphere_functions_mod
  implicit none

contains

  ! -----------------------------------------------------------------
  ! Calculate the number density of air (molecule cm-3) from pressure
  ! (mbar) and temperature (K).
  pure function calcAirDensity( press, temp ) result ( m )
    use types_mod

    real(kind=DP), intent(in) :: press, temp
    real(kind=DP) :: m, press_pa

    press_pa = press * 1.0e+02_DP
    m = 1.0e-06_DP * ( 6.02214129e+23_DP / 8.3144621_DP ) * ( press_pa / temp )

    return
  end function calcAirDensity

  ! -----------------------------------------------------------------
  ! Calculate the number density of oxygen and nitrogen in the atmosphere
  ! from number density of air (molecule cm-3).
  subroutine calcAtmosphere( m, o2, n2 )
    use types_mod

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

    real(kind=DP), intent(in) :: rh, temp, press
    real(kind=DP) :: h2o, h2o_ppu, temp_c, wvp

    ! Use Eq.6 to calculate the water vapour saturation pressure and
    ! Eq.1 to calculate the water vapour pressure from relative
    ! humidity (see Vaisala paper).
    temp_c = temp - 273.15_DP
    wvp = ( rh/100.0_DP ) * ( 6.116441_DP * 10.0_DP**((7.591386_DP * temp_c)/(temp_c + 240.7263_DP)) )

    ! Calculate volume of water vapour per volume of dry air using
    ! Eq.18 (see Vaisala paper). We don't use ppm here, because doing
    ! so requires a multiplication by 1e6 then division by the same
    ! in the final line. This incurs possible rounding. Instead, we
    ! use 'parts per unit'
    h2o_ppu = wvp / (press - wvp)

    ! convert ppv to molecule cm-3
    h2o = h2o_ppu * calcAirDensity(press, temp)

    return
  end function convertRHtoH2O

  ! -----------------------------------------------------------------
  ! subroutine to calculate diurnal variations in temperature
  ! --> CURRENTLY UNUSED, BUT IT MAY BE USEFUL <--
  subroutine temperature( temp, h2o, ttime )
    use types_mod

    real(kind=DP) :: temp, ttime, rh, h2o, sin, h2o_factor, exponent

    temp = 289.86_DP + 8.3_DP * sin( ( 7.2722e-5_DP * ttime ) - 1.9635_DP )
    temp = 298.00_DP
    rh = 23.0_DP * sin( ( 7.2722e-5_DP * ttime ) + 1.1781_DP ) + 66.5_DP
    h2o_factor = 10.0_DP / ( 1.38e-16_DP * temp ) * rh
    exponent = -1.0_DP * ( 597.3_DP - 0.57_DP * ( temp - 273.16_DP ) ) * 18.0_DP / 1.986_DP * ( 1.0_DP / temp - 1.0_DP / 273.16_DP )
    h2o = 6.1078_DP * exp( exponent ) * h2o_factor

    return
  end subroutine temperature

end module atmosphere_functions_mod
