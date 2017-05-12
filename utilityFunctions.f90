module utilityFunctions_mod
contains
  ! TODO: this is unused?
  subroutine temperature( temp, h2o, ttime )
    use types_mod
    ! SUBROUTINE TO CALCULATE DIURNAL VARIATIONS IN TEMPERATURE
    real(kind=DP) :: temp, ttime, rh, h2o, sin

    temp = 289.86 + 8.3*sin( (7.2722D-5*ttime)-1.9635 )
    temp = 298.00
    rh = 23.0*sin ((7.2722D-5*ttime)+1.1781)+66.5
    h2o = 6.1078*exp(-1.0D+0*(597.3-0.57*(temp-273.16))*18.0/1.986*(1.0/temp-1.0/273.16))*10./(1.38D-16*temp)*rh

    return
  end subroutine temperature

  subroutine atmosphere( o2, n2, m )
    use types_mod
    real(kind=DP) :: o2, n2, m
    o2 = 0.2095*m
    n2 = 0.7809*m
  end subroutine atmosphere

  subroutine zenith( theta, secx, cosx, ttime, dec )
    use zenithData
    use SZACalcVars
    real(kind=DP) :: pi, radian, dec, ttime, theta, cosx, secx
    real(kind=DP) :: rampValue
    ! real(kind=DP) :: , sinld, cosld, lat, lha, longt
    ! SOLAR decLINATION ANGLE FROM JULY 1ST - HARWELL TRAJ MODEL

    lat = latitude
    longt = longitude
    pi = 4.0*atan (1.0)
    ! LOCAL HOUR ANGLE - REPRESENTING TIME OF DAY
    lha = (1 + ((ttime) / 4.32D+04)) * pi
    radian = 180.0/pi
    lat = lat/radian
    ! dec = dec/RADIAN
    theta = acos( cos( lha ) * cos( dec ) * cos( lat ) + sin( dec ) * sin( lat ) + 1.0D-30 )
    sinld = SIN (lat)*SIN (dec)
    cosld = COS (lat)*COS (dec)

    call ramp( COS (lha)*cosld+sinld, rampValue )
    secx = 1/ (rampValue + 1.00d-30)

    cosx = (COS (lha)*cosld) + sinld

    return
  end subroutine zenith

  subroutine ramp( arg1, rampValue )
    use types_mod
    real(kind=DP) :: arg1, arg2, rampValue

    arg2 = ABS (arg1)
    rampValue = (arg1 + arg2) / 2
    return
  end subroutine ramp
end module utilityFunctions_mod
