module interpolationFunctions_mod
contains
  subroutine getConstrainedQuantAtT2D( t, x, y, y2, dataNumberOfPoints, constraintType, ind, maxPoints, nConSpec, concAtT )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use interpolationMethod
    use chemicalConstraints
    implicit none

    real(kind=DP), intent(in) :: t
    integer(kind=QI), intent(in) :: maxPoints
    integer(kind=NPI), intent(in) :: nConSpec, dataNumberOfPoints
    real(kind=DP), intent(in) :: x(nConSpec, maxPoints), y(nConSpec, maxPoints), y2(nConSpec, maxPoints)
    integer, intent(in) :: constraintType
    integer(kind=NPI), intent(in) :: ind
    real(kind=DP), intent(out) :: concAtT
    real(kind=DP) :: xBefore, xAfter, yBefore, yAfter, m, c
    integer :: facintfound
    integer(kind=SI) :: interpMethod
    integer(kind=NPI) :: i, indexBefore, indexAfter
    integer :: linintsuc

    ! GET INTERPOLATION METHOD FOR GIVEN CONSTRAINT TYPE
    if ( constraintType == 1 ) then
      call getSpeciesInterpMethod( interpMethod )
    else if ( constraintType == 2 ) then
      call getConditionsInterpMethod( interpMethod )
    else if ( constraintType == 3 ) then
      call getDecInterpMethod( interpMethod )
    else
      write (*,*) 'Error in setting constraintType, error = ', constraintType
    end if

    ! CUBIC SPLINE INTERPOLATION
    if ( interpMethod == 1 ) then
      call splint2D( x, y, y2, dataNumberOfPoints, t, ind, concAtT )
      if ( concAtT <= 0 ) then
        concAtT = 0
      end if
      ! CUBIC SPLINE INTERPOLATION (LN)
    else if ( interpMethod == 2 ) then
      call splint2D( x, y, y2, dataNumberOfPoints, t, ind, concAtT )
      concAtT = exp( concAtT )
      ! PIECEWISE CONSTANT INTERPOLATION
    else if ( interpMethod == 3 ) then
      facintfound = 0
      do i = 1, dataNumberOfPoints
        if ( (t >= X(ind, i) ) .and. ( t < X(ind, i+1) ) ) then
          concAtT = Y(ind, i)
          facintfound = 1
        end if
      end do
      if ( facintfound == 0 ) then
        write (*,*) 'error in piecewise constant interpolation'
        write (*,*) t, dataNumberOfPoints, concAtT
        concAtT = y(ind, dataNumberOfPoints)
      end if
      ! PIECEWISE LINEAR INTERPOLATION
    else if ( interpMethod == 4 ) then
      ! FIND THE INDICES OF THE ENCLOSING DATA POINTS
      linintsuc = 0
      do i = 1, dataNumberOfPoints
        if ( ( t >= x(ind, i) ) .and. ( t < x(ind, i+1) ) ) then
          indexBefore = i
          indexAfter = i + 1
          linintsuc = 1
        end if
      end do
      if ( linintsuc == 0 ) then
        concAtT = y(ind, dataNumberOfPoints)
        write (*,*) 'Failed to lin int'
      else if ( linintsuc == 1 ) then
        ! INDENTIFY COORIDANTES OF ENCLOSING DATA POINTS
        xBefore = x(ind, indexBefore)
        yBefore = y(ind, indexBefore)
        xAfter = x(ind, indexAfter)
        yAfter = y(ind, indexAfter)
        ! DO LINEAR INTERPOLATION (Y = MX + C)
        m = ( yAfter - yBefore ) / ( xAfter - xBefore )
        c = yAfter - ( m * xAfter )
        concAtT = m * t + c
      end if
    else
      write (stderr,*) 'ERROR: Interpolation method not set, interpMethod = ', interpMethod
      stop
    end if

    return
  end subroutine getConstrainedQuantAtT2D

  subroutine splint2D( xa, ya, y2a, n, x, ind, y )
    use types_mod
    implicit none

    integer(kind=NPI), intent(in) :: ind, n
    real(kind=DP), intent(in) :: x, xa(:,:), y2a(:,:), ya(:,:)
    real(kind=DP), intent(out) :: y
    ! Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a function
    ! (with the xai�s in order), and given the array y2a(1:n), which is the output
    ! from spline above, and given a value of x, this routine returns a
    ! cubic-spline interpolated value y.
    integer(kind=NPI) :: k, khi, klo
    real(kind=DP) :: a, b, h

    klo = 1 !We will find the right place in the table by means of bisection.
    ! This is optimal if sequential calls to this routine are at random values of
    ! x. If sequential calls are in order, and closely spaced, one would do better
    ! to store previous values of klo and khi and test if they remain appropriate
    ! on the next call.
    khi = n
    do while ( khi - klo > 1 )
      k = ( khi + klo ) / 2
      if ( xa(ind, k) > x ) then
        khi = k
      else
        klo = k
      end if
    end do !klo and khi now bracket the input value of x.
    h = xa(ind, khi) - xa(ind, klo)

    if ( h == 0. ) then
      print *, 'Bad input in splint2D! The xa''s must be distinct'!
      stop
    end if

    a = ( xa(ind, khi) - x ) / h !Cubic spline polynomial is now evaluated.
    b = ( x - xa(ind, klo) ) / h
    y = a * ya(ind, klo) + b * ya(ind, khi) + ( ( a ** 3 - a ) * y2a(ind, klo) + ( b ** 3 - b ) * y2a(ind, khi) ) * ( h ** 2 ) / 6.
    return
  end subroutine splint2D
end module interpolationFunctions_mod
