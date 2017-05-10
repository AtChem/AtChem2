MODULE conversionFunctions_mod
  USE types_mod
CONTAINS

! ----------------------------------------------------------------- !

  FUNCTION convertRHtoH2O (rh, temp, press) result ( h2o )
    ! convert relative humidity to water concentration (molecule cm-3)
    ! pressure in mbar, temperature in K
    IMPLICIT NONE
    real(kind=DP) :: rh, temp, press, h2o
    real(kind=DP) :: exponent, e1

    exponent = EXP(-1.00d00*(597.30d00-0.57d00*(temp - 273.16d00))* &
         18.00d00/1.986d00*(1.00d00/temp - 1.00d00/273.16d00))

    e1 = 10d0/(1.38d-16*temp)*rh
    h2o = 6.1078d0*exponent*e1
    RETURN
  END FUNCTION convertRHtoH2O

! ----------------------------------------------------------------- !

  FUNCTION calcPRESSfromM (m, temp) result ( press )
    ! calculate pressure (mbar) from the number density of air
    ! (molecule cm-3) and temperature (K)
    IMPLICIT NONE
    real(kind=DP) :: m, temp, press
    real(kind=DP) :: press_pa

    press_pa = (m*temp)/(6.02214129d+23/8.3144621)
    press = press_pa/1.0d+2
    RETURN
  END FUNCTION calcPRESSfromM

! ----------------------------------------------------------------- !

END MODULE conversionFunctions_mod
