SUBROUTINE ZENITH (THETA, SECX, COSX, TTIME, DEC)
  USE zenithData
  USE SZACalcVars
  DOUBLE PRECISION PI, RADIAN, DEC, TTIME, THETA, COSX, SECX
  DOUBLE PRECISION rampValue
  ! DOUBLE PRECISION :: , sinld, cosld, LAT, LHA, longt
  ! SOLAR DECLINATION ANGLE FROM JULY 1ST - HARWELL TRAJ MODEL

  lat = latitude
  longt= longitude
  PI = 4.0*ATAN (1.0)
  ! LOCAL HOUR ANGLE - REPRESENTING TIME OF DAY
  lha = (1 + ((TTIME ) / 4.32D+04)) * PI
  RADIAN = 180.0/PI
  LAT = LAT/RADIAN
  ! DEC = DEC/RADIAN
  THETA = ACOS (COS (LHA)*COS (DEC)*COS (LAT)+SIN (DEC)*SIN (LAT)+1.0D-30)
  sinld = SIN (lat)*SIN (dec)
  cosld = COS (lat)*COS (dec)

  CALL ramp (COS (lha)*cosld+sinld, rampValue)
  SECX = 1/ (rampValue + 1.00d-30)

  COSX = (COS (LHA)*COSLD) + SINLD

  RETURN
END SUBROUTINE ZENITH
