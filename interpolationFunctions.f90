SUBROUTINE getConstrainedQuantAtT2D (t, x, y, y2, dataNumberOfPoints, concAtT, constraintType, ind, maxPoints, nConSpec)

  USE interpolationMethod
  USE chemcialConstraints
  INTEGER dataNumberOfPoints, linintsuc, constraintType, maxPoints, nConSpec
  DOUBLE PRECISION :: t, x(nConSpec, maxPoints), y(nConSpec, maxPoints), y2 (nConSpec, maxPoints), concAtT
  DOUBLE PRECISION :: xBefore, xAfter, yBefore, yAfter, m, c
  INTEGER :: indexBefore, indexAfter, intMethod, ind, facintfound, i

  ! GET INTERPOLATION METHOD FOR GIVEN CONSTRAINT TYPE
  IF (constraintType==1) THEN
     CALL getSpeciesIntMethod (intMethod)
  ELSE IF (constraintType==2) THEN
     CALL getConditionIntMethod (intMethod)
  ELSE IF (constraintType==3) THEN
     CALL getDecIntMethod (intMethod)
  ELSE
     WRITE (*,*) 'Error in setting constraintType, error = ', constraintType
  ENDIF

  ! CUBIC SPLINE INTERPOLATION
  IF (intMethod==1) THEN
     CALL splint2D (x, y, y2, dataNumberOfPoints, t, concAtT, ind, maxPoints)
     IF (concAtT<=0) THEN
        concAtT = 0
     ENDIF
     ! CUBIC SPLINE INTERPOLATION (LN)
  ELSE IF (intMethod==2) THEN
     CALL splint2D (x, y, y2, dataNumberOfPoints, t, concAtT, ind, maxPoints)
     concAtT = EXP (concAtT)
     ! PIECEWISE CONSTANT INTERPOLATION
  ELSE IF (intMethod==3) THEN
     facintfound = 0
     DO i = 1, dataNumberOfPoints
        IF ((t>=X (ind, i)).AND.(t<X (ind, i+1))) THEN
           concAtT = Y (ind, i)
           facintfound = 1
        ENDIF
     ENDDO
     IF (facintfound==0) THEN
        WRITE (*,*) 'error in peicewise constant interpolation'
        WRITE (*,*) t, dataNumberOfPoints, concAtT
        concAtT = y(ind, dataNumberOfPoints)
     ENDIF
     ! PIECEWISE LINEAR INTERPOLATION
  ELSE IF (intMethod==4) THEN
     ! FIND THE INDICES OF THE ENCLOSING DATA POINTS
     linintsuc = 0
     DO i = 1, dataNumberOfPoints
        IF ((t>=x(ind, i)).AND.(t<x(ind, i+1))) THEN
           indexBefore = i
           indexAfter = i + 1
           linintsuc = 1
        ENDIF
     ENDDO
     IF (linintsuc==0) THEN
        concAtT = y(ind, dataNumberOfPoints)
        WRITE (*,*) 'Failed to lin int'
     ELSE IF (linintsuc==1) THEN
        ! INDENTIFY COORIDANTES OF ENCLOSING DATA POINTS
        xBefore = x(ind, indexBefore)
        yBefore = y(ind, indexBefore)
        xAfter = x(ind, indexAfter)
        yAfter = y(ind, indexAfter)
        ! DO LINEAR INTERPOLATION (Y = MX + C)
        m = (yAfter - yBefore)/(xAfter - xBefore)
        c = yAfter - (m*xAfter)
        concAtT = m*t + c
     ENDIF
  ELSE
     WRITE (*,*) 'Interpolation method not set, error = ', intMethod
  ENDIF

  RETURN
END SUBROUTINE getConstrainedQuantAtT2D

SUBROUTINE splint2D (xa, ya, y2a, n, x, y, ind, maxPoints)

  INTEGER n, maxPoints, ind
  DOUBLE PRECISION :: x, y, xa(100, maxPoints), y2a(100, maxPoints), ya(100, maxPoints)
  ! Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a function (with the xai�s in order), and given the array y2a(1:n), which is the output from spline above, and given a value of x, this routine returns a cubic-spline interpolated value y.
  INTEGER k, khi, klo
  DOUBLE PRECISION :: a, b, h

  klo = 1 !We will find the right place in the table by means of bisection.
  ! This is optimal if sequential calls to this routine are at random values of x. If sequential calls are in order, and closely spaced, one would do better to store previous values of klo and khi and test if they remain appropriate on the next call.
  khi = n
  DO WHILE (khi-klo>1)
     k = (khi+klo)/2
     IF (xa(ind, k)>x) THEN
        khi = k
     ELSE
        klo = k
     ENDIF
  ENDDO !klo and khi now bracket the input value of x.
  h = xa(ind, khi)-xa(ind, klo)

  IF (h==0.) THEN
     PRINT *, 'Bad input in splint2D! The xa''s must be distinct'
     STOP
  END IF

  a = (xa(ind, khi)-x)/h !Cubic spline polynomial is now evaluated.
  b = (x-xa(ind, klo))/h
  y = a*ya(ind, klo)+b*ya(ind, khi)+((a**3-a)*y2a(ind, klo)+(b**3-b)*y2a(ind, khi))*(h**2)/6.
  RETURN
END SUBROUTINE splint2D

SUBROUTINE splint (xa, ya, y2a, n, x, y)
  INTEGER n
  DOUBLE PRECISION :: x, y, xa(*), y2a(*), ya(*)
  ! Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a function (with the xai�s in order), and given the array y2a(1:n), which is the output from spline above, and given a value of x, this routine returns a cubic-spline interpolated value y.
  INTEGER k, khi, klo
  DOUBLE PRECISION :: a, b, h

  klo = 1 !We will find the right place in the table by means of bisection.
  ! This is optimal if sequential calls to this routine are at random values of x. If sequential calls are in order, and closely spaced, one would do better to store previous values of klo and khi and test if they remain appropriate on the next call.
  khi = n
  DO WHILE (khi-klo>1)
     k = (khi+klo)/2
     IF (xa(k)>x) THEN
        khi = k
     ELSE
        klo = k
     ENDIF
  ENDDO !klo and khi now bracket the input value of x.
  h = xa(khi)-xa(klo)

  IF (h==0.) THEN
     PRINT *, 'bad xa input in splint! The xa''s must be distinct.'
     STOP
  END IF
  a = (xa(khi)-x)/h !Cubic spline polynomial is now evaluated.
  b = (x-xa(klo))/h
  y = a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
  RETURN
END SUBROUTINE splint

SUBROUTINE getConstrainedQuantAtT (t, x, y, y2, dataNumberOfPoints, concAtT, constraintType)

  USE interpolationMethod
  DOUBLE PRECISION :: t, x(*), y(*), y2 (*), concAtT
  INTEGER dataNumberOfPoints, linintsuc, constraintType
  DOUBLE PRECISION :: xBefore, xAfter, yBefore, yAfter, m, c
  INTEGER :: indexBefore, indexAfter, intMethod, facintfound, i

  ! GET INTERPOLATION METHOD FOR GIVEN CONSTRAINT TYPE
  IF (constraintType==1) THEN
     CALL getSpeciesIntMethod (intMethod)
  ELSE IF (constraintType==2) THEN
     CALL getConditionIntMethod (intMethod)
  ELSE IF (constraintType==3) THEN
     CALL getDecIntMethod (intMethod)
  ELSE
     WRITE (*,*) 'Error in setting constraintType, error = ', constraintType
  ENDIF

  ! CUBIC SPLINE INTERPOLATION
  IF (intMethod==1) THEN
     CALL splint (x, y, y2, dataNumberOfPoints, t, concAtT)
     ! CUBIC SPLINE INTERPOLATION (LN)
  ELSE IF (intMethod==2) THEN
     CALL splint (x, y, y2, dataNumberOfPoints, t, concAtT)
     concAtT = EXP (concAtT)
     ! PIECEWISE CONSTANT INTERPOLATION
  ELSE IF (intMethod==3) THEN
     facintfound = 0
     DO i = 1, dataNumberOfPoints
        IF ((t>=x(i)).AND.(t<x(i+1))) THEN
           concAtT = y(i)
           facintfound = 1
        ENDIF
     ENDDO
     IF (facintfound==0) THEN
        WRITE (*,*) 'error in peicewise constant interpolation'
        WRITE (*,*) t, dataNumberOfPoints, concAtT
        concAtT = y(dataNumberOfPoints)
     ENDIF
     ! PIECEWISE LINEAR INTERPOLATION
  ELSE IF (intMethod==4) THEN
     ! FIND THE INDICES OF THE ENCLOSING DATA POINTS
     linintsuc = 0
     DO i = 1, dataNumberOfPoints
        IF ((t>=x(i)).AND.(t<x(i+1))) THEN
           indexBefore = i
           indexAfter = i + 1
           linintsuc = 1
        ENDIF
     ENDDO
     IF (linintsuc==0) THEN
        concAtT = y(dataNumberOfPoints)
        WRITE (*,*) 'Failed to lin int'
     ELSE IF (linintsuc==1) THEN
        ! INDENTIFY COORIDANTES OF ENCLOSING DATA POINTS
        xBefore = x(indexBefore)
        yBefore = y(indexBefore)
        xAfter = x(indexAfter)
        yAfter = y(indexAfter)
        ! DO LINEAR INTERPOLATION (Y = MX + C)
        m = (yAfter - yBefore)/(xAfter - xBefore)
        c = yAfter - (m*xAfter)
        concAtT = m*t + c
     ENDIF
  ELSE
     WRITE (*,*) 'Interpolation method not set, error = ', intMethod
  ENDIF

  RETURN
END SUBROUTINE getConstrainedQuantAtT
