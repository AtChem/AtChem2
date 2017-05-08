MODULE conversionFunctions_mod
CONTAINS
  SUBROUTINE convertRHtoConcH2O (h2o, temp, rh)
  DOUBLE PRECISION h2o, temp, rh, exponent, e1

  exponent = EXP (-1.00d00*(597.30d00-0.57d00*(temp - 273.16d00))* &
       18.00d00/1.986d00*(1.00d00/temp - 1.00d00/273.16d00))

  e1 = 10d0/(1.38D-16*TEMP)*RH
  h2o = 6.1078d0*exponent*e1
  RETURN
END SUBROUTINE convertRHtoConcH2O
END MODULE conversionFunctions_mod
