
SUBROUTINE mechanism_rates (p, t, y, mnsp)
   USE outputFunctions_mod
   USE photolysisRates
   USE zenithData1
   USE constraints
   USE constraintFunctions_mod
   USE interpolationFunctions_mod
   USE envVars, ONLY : ro2

   IMPLICIT NONE

   ! calculates rate constants from arrhenius information
   DOUBLE PRECISION, intent (out) :: p(*)
   DOUBLE PRECISION, intent (in) :: t
   INTEGER, intent (in) :: mnsp
   DOUBLE PRECISION, intent (in) :: y(mnsp)
   DOUBLE PRECISION :: temp, pressure, dummy

   INTEGER(kind=DI) :: i
   DOUBLE PRECISION :: photoRateAtT

   INCLUDE 'modelConfiguration/mechanism-rate-declarations.f90'

   CALL ro2sum (ro2, y)
   dummy = y(1)

   dec = -1e16

   CALL getEnvVarsAtT (t, temp, rh, h2o, dec, pressure, m, blh, dilute, jfac, roofOpen)

   CALL atmosphere (o2, n2, m)

   !O2 = 0.2095*m
   !N2 = 0.7809*m

   DO i = 1, nrOfPhotoRates
      IF (usePhotolysisConstants.EQV..FALSE.) THEN
         IF (cosx<1.00d-10) THEN
            j(ck(i)) = 1.0d-30
         ELSE
            j(ck(i)) = cl(i)*cosx**(cmm(i))*EXP(-cnn(i)*secx)*transmissionFactor(i)*roofOpen*jfac
         ENDIF
      ELSE
         j(ck(i)) = cl(i)
      ENDIF
   ENDDO

   DO i = 1, numConPhotoRates
      CALL getConstrainedQuantAtT2D (t, photoX, photoY, photoY2, photoNumberOfPoints(i), photoRateAtT, 2, i, &
                                     maxNumberOfDataPoints, numConPhotoRates)
      j(constrainedPhotoRatesNumbers(i)) = photoRateAtT
   ENDDO

   INCLUDE 'modelConfiguration/mechanism-rate-coefficients.f90'
   RETURN
END SUBROUTINE mechanism_rates
