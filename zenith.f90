
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
