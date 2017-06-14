! ******************************************************************** !
! ATCHEM -- MODULE solarFunctions
!
! ??? Text describing the module ???
! ******************************************************************** !
module solarFunctions_mod
contains

  ! -----------------------------------------------------------------
  ! Calculate the day angle (radians). Equations taken from "The
  ! Atmosphere and UV-B Radiation at Ground Level" by S. Madronich
  ! (Environmental UV Photobiology, 1993).
  subroutine calcTheta( t )
    use types_mod
    use date_mod, only : year, dayOfYear
    use zenithData
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP) :: pi, currentFracDay

    pi = 4.0_DP * atan( 1.0_DP )

    ! Time as day of year and fractional seconds of day
    currentFracDay = dayOfYear + ( t / 86400.0_DP )

    ! The day angle is 
    if ( (mod(year, 4_DI)==0 .and. .not. mod(year, 100_DI)==0) .or. (mod(year, 400_DI)==0) ) then
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
    use zenithData
    implicit none

    real(kind=DP) :: dec, b0, b1, b2, b3, b4, b5, b6

    ! The sun declination is the angle between the center of the Sun
    ! and Earth's equatorial plane.
    b0 =  0.006918
    b1 = -0.399912
    b2 =  0.070257
    b3 = -0.006758
    b4 =  0.000907
    b5 = -0.002697
    b6 =  0.001480
    dec = b0 + b1 * cos(theta) + b2 * sin(theta) + b3 * cos(2 * theta) + &
          b4 * sin(2 * theta) + b5 * cos(3 * theta) + b6 * sin(3 * theta)

    return
  end function calcDec

  ! -----------------------------------------------------------------
  ! Calculate the local hour angle (radians) and the solar zenith
  ! angle (radians). Equations taken from "The Atmosphere and UV-B
  ! Radiation at Ground Level" by S. Madronich (Environmental UV
  ! Photobiology, 1993).
  subroutine calcZenith( t, dec )
    use types_mod
    use date_mod, only : dayOfYear
    use zenithData
    implicit none

    real(kind=DP), intent(in) :: t, dec
    real(kind=DP) :: pi, c0, c1, c2, c3, c4
    real(kind=DP) :: currentFracDay, currentFracHour
    real(kind=DP) :: lat, ramp_cosx

    pi = 4.0_DP * atan( 1.0_DP )

    ! The equation of time is 
    c0 = 0.000075
    c1 = 0.001868
    c2 = -0.032077
    c3 = -0.014615
    c4 = -0.040849
    eqtime = c0 + c1 * cos(theta) + c2 * sin(theta) + c3 * cos(2 * theta) + &
             c4 * sin(2 * theta)

    ! The local hour angle is the angle between the observer's meridian
    ! and the Sun's meridian. Time must be in GMT/UTC and longitude in degrees.
    currentFracDay = dayOfYear + ( t / 86400.0_DP )
    currentFracHour =  (currentFracDay - floor(currentFracDay)) * 24.0_DP
    lha = pi * ((currentFracHour / 12.0_DP) - (1.0_DP + longitude / 180.0_DP)) + eqtime

    ! The solar zenith angle is the angle between the local vertical and
    ! the center of the Sun. Latitude must be in radians.
    lat = latitude * pi / 180.0_DP
    sinld = sin( lat ) * sin( dec )
    cosld = cos( lat ) * cos( dec )
    ! Calculate the cosine of the solar zenith angle.
    cosx = cos( lha ) * cosld + sinld

    ! Set negative cosx to zero and calculate the inverse
    ! (secx=1/cosx). The MCM photolysis parameterisation
    ! (http://mcm.leeds.ac.uk/MCM/parameters/photolysis_param.htt)
    ! requires cosx and secx to calculate the photolysis rates.
    ramp_cosx = max(0.0_DP, cosx )
    secx = 1.0_DP / ( ramp_cosx + 1.00d-30 )

    return
  end subroutine calcZenith

end module solarFunctions_mod
