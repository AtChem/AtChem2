! TODO: this is unused?
SUBROUTINE temperature (temp, h2o, ttime)
  ! SUBROUTINE TO CALCULATE DIURNAL VARIATIONS IN TEMPERATURE
  DOUBLE PRECISION temp, ttime, rh, h2o, sin

  temp = 289.86 + 8.3*SIN ((7.2722D-5*ttime)-1.9635)
  temp = 298.00
  rh = 23.0*SIN ((7.2722D-5*ttime)+1.1781)+66.5
  h2o = 6.1078*DEXP(-1.0D+0*(597.3-0.57*(temp-273.16))*18.0/1.986*(1.0/temp-1.0/273.16))*10./(1.38D-16*temp)*rh

  RETURN
END SUBROUTINE temperature

SUBROUTINE atmosphere (o2, n2, m)
  DOUBLE PRECISION :: o2, n2, m
  o2 = 0.2095*m
  n2 = 0.7809*m
END SUBROUTINE atmosphere

SUBROUTINE zenith (theta, secx, cosx, ttime, dec)
  USE zenithData
  USE SZACalcVars
  DOUBLE PRECISION pi, radian, dec, ttime, theta, cosx, secx
  DOUBLE PRECISION rampValue
  ! DOUBLE PRECISION :: , sinld, cosld, lat, lha, longt
  ! SOLAR decLINATION ANGLE FROM JULY 1ST - HARWELL TRAJ MODEL

  lat = latitude
  longt = longitude
  pi = 4.0*ATAN (1.0)
  ! LOCAL HOUR ANGLE - REPRESENTING TIME OF DAY
  lha = (1 + ((ttime) / 4.32D+04)) * pi
  radian = 180.0/pi
  lat = lat/radian
  ! dec = dec/RADIAN
  theta = ACOS (COS (LHA)*COS (dec)*COS (lat)+SIN (dec)*SIN (lat)+1.0D-30)
  sinld = SIN (lat)*SIN (dec)
  cosld = COS (lat)*COS (dec)

  CALL ramp (COS (lha)*cosld+sinld, rampValue)
  secx = 1/ (rampValue + 1.00d-30)

  cosx = (COS (lha)*cosld) + sinld

  RETURN
END SUBROUTINE zenith

SUBROUTINE ramp (arg1, rampValue)
  DOUBLE PRECISION arg1, arg2, rampValue

  arg2 = ABS (arg1)
  rampValue = (arg1 + arg2) / 2
  RETURN
END SUBROUTINE ramp
