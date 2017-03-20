SUBROUTINE temperature (temp, h2o, ttime)
  ! SUBROUTINE TO CALCULATE DIURNAL VARIATIONS IN TEMPERATURE
  DOUBLE PRECISION temp, ttime, rh, h2o, sin

  temp = 289.86 + 8.3*SIN ((7.2722D-5*ttime)-1.9635)
  temp = 298.00
  rh = 23.0*SIN ((7.2722D-5*ttime)+1.1781)+66.5
  h2o = 6.1078*DEXP(-1.0D+0*(597.3-0.57*(temp-273.16))*18.0/1.986*(1.0/temp-1.0/273.16))*10./(1.38D-16*temp)*rh

  RETURN
END SUBROUTINE temperature
