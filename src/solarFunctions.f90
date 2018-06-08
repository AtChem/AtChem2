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
! ATCHEM2 -- MODULE solarFunctions
!
! This module contains functions that calculate quantities related to
! relative positions of the Earth and Sun.
! ******************************************************************** !
module solar_functions_mod
contains

  ! -----------------------------------------------------------------
  ! Calculate the day angle (radians). Equations taken from "The
  ! Atmosphere and UV-B Radiation at Ground Level" by S. Madronich
  ! (Environmental UV Photobiology, 1993).
  subroutine calcTheta( t )
    use types_mod
    use date_mod, only : currentYear, currentDayOfYear
    use zenith_data_mod, only : theta
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP) :: pi, currentFracDay

    pi = 4.0_DP * atan( 1.0_DP )

    ! Time as day of year and fractional seconds of day
    currentFracDay = currentDayOfYear + ( t / 86400.0_DP )

    ! The day angle accounts for the variation in the Sun-Earth
    ! distance caused by the ellipticity of the Earth's orbit.
    if ( (mod(currentYear, 4_DI)==0 .and. .not. mod(currentYear, 100_DI)==0) .or. (mod(currentYear, 400_DI)==0) ) then
      ! leap year
      theta = 2.0_DP * pi * floor(currentFracDay) / 366.0_DP
    else
      ! not a leap year
      theta = 2.0_DP * pi * floor(currentFracDay) / 365.0_DP
    end if

    return
  end subroutine calcTheta

  ! -----------------------------------------------------------------
  ! Calculate the sun declination (radians). Equations taken from "The
  ! Atmosphere and UV-B Radiation at Ground Level" by S. Madronich
  ! (Environmental UV Photobiology, 1993).
  pure function calcDec() result ( dec )
    use types_mod
    use zenith_data_mod, only : theta
    implicit none

    real(kind=DP) :: dec, b0, b1, b2, b3, b4, b5, b6

    ! The sun declination is the angle between the center of the Sun
    ! and Earth's equatorial plane.
    b0 =  0.006918_DP
    b1 = -0.399912_DP
    b2 =  0.070257_DP
    b3 = -0.006758_DP
    b4 =  0.000907_DP
    b5 = -0.002697_DP
    b6 =  0.001480_DP
    dec = b0 + b1 * cos(theta) + b2 * sin(theta) + b3 * cos(2.0_DP * theta) + &
          b4 * sin(2.0_DP * theta) + b5 * cos(3.0_DP * theta) + b6 * sin(3.0_DP * theta)

    return
  end function calcDec

  ! -----------------------------------------------------------------
  ! Calculate the local hour angle (radians) and the solar zenith
  ! angle (radians). Equations taken from "The Atmosphere and UV-B
  ! Radiation at Ground Level" by S. Madronich (Environmental UV
  ! Photobiology, 1993).
  subroutine calcZenith( t, dec )
    use types_mod
    use date_mod, only : currentDayOfYear
    use zenith_data_mod, only : eqtime, theta, lha, latitude, longitude, sinld, cosld, cosx, secx, cosx_threshold, &
                                cosx_below_threshold
    implicit none

    real(kind=DP), intent(in) :: t, dec
    real(kind=DP) :: pi, c0, c1, c2, c3, c4
    real(kind=DP) :: currentFracDay, currentFracHour
    real(kind=DP) :: lat

    pi = 4.0_DP * atan( 1.0_DP )

    ! The equation of time accounts for the discrepancy between the
    ! apparent and the mean solar time at a given location.
    c0 = 0.000075_DP
    c1 = 0.001868_DP
    c2 = -0.032077_DP
    c3 = -0.014615_DP
    c4 = -0.040849_DP
    eqtime = c0 + c1 * cos(theta) + c2 * sin(theta) + c3 * cos(2.0_DP * theta) + &
             c4 * sin(2.0_DP * theta)

    ! The local hour angle is the angle between the observer's
    ! meridian and the Sun's meridian. Time must be in GMT/UTC and
    ! longitude in degrees.
    currentFracDay = currentDayOfYear + ( t / 86400.0_DP )
    currentFracHour =  (currentFracDay - floor(currentFracDay)) * 24.0_DP
    lha = pi * ((currentFracHour / 12.0_DP) - (1.0_DP + longitude / 180.0_DP)) + eqtime

    ! The solar zenith angle is the angle between the local vertical
    ! and the center of the Sun. Latitude must be in radians.
    lat = latitude * pi / 180.0_DP
    sinld = sin( lat ) * sin( dec )
    cosld = cos( lat ) * cos( dec )
    ! Calculate the cosine of the solar zenith angle.
    cosx = cos( lha ) * cosld + sinld

    ! Set negative cosx to zero and calculate the inverse
    ! (secx=1/cosx). The MCM photolysis parameterisation
    ! (http://mcm.leeds.ac.uk/MCM/parameters/photolysis_param.htt)
    ! requires cosx and secx to calculate the photolysis rates.
    if ( cosx <= cosx_threshold ) then
      cosx = 0.0_DP
      secx = 1.0d+50
      cosx_below_threshold = .true.
    else
      secx = 1.0_DP / cosx
      cosx_below_threshold = .false.
    end if

    return
  end subroutine calcZenith

end module solar_functions_mod
