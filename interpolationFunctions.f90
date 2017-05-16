module interpolationFunctions_mod
contains
  subroutine getConstrainedQuantAtT2D( t, x, y, y2, dataNumberOfPoints, constraintType, ind, concAtT )
    ! This routine returns in concAtT the value of the requested quantity (referenced
    ! by the ind-th line of x, y, y2) based upon the constraint data given and
    ! interpolation method given
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use interpolationMethod
    use chemicalConstraints
    implicit none

    real(kind=DP), intent(in) :: t, x(:,:), y(:,:), y2(:,:)
    integer(kind=NPI), intent(in) :: dataNumberOfPoints
    integer(kind=SI), intent(in) :: constraintType
    integer(kind=NPI), intent(in) :: ind
    real(kind=DP), intent(out) :: concAtT
    real(kind=DP) :: xBefore, xAfter, yBefore, yAfter, m, c
    integer(kind=SI) :: interpMethod
    integer(kind=NPI) :: i, indexBefore, indexAfter
    logical :: fac_int_found, lin_int_suc

    ! Sanity checks on sizes of x, y and y2.
    if ( size( x, 1 ) /= size( y, 1 ) ) then
      stop 'size( x, 1 ) /= size( y, 1 ) in getConstrainedQuantAtT2D()'
    end if
    if ( size( x, 1 ) /= size( y2, 1 ) ) then
      stop 'size( x, 1 ) /= size( y2, 1 ) in getConstrainedQuantAtT2D()'
    end if
    if ( size( x, 2 ) /= size( y, 2 ) ) then
      stop 'size( x, 2 ) /= size( y, 2 ) in getConstrainedQuantAtT2D()'
    end if
    if ( size( x, 2 ) /= size( y2, 2 ) ) then
      stop 'size( x, 2 ) /= size( y2, 2 ) in getConstrainedQuantAtT2D()'
    end if

    ! Get interpolation method for given constraint type
    select case ( constraintType )
      case (1_SI)
        call getSpeciesInterpMethod( interpMethod )
      case (2_SI)
        call getConditionsInterpMethod( interpMethod )
      case (3_SI)
        call getDecInterpMethod( interpMethod )
      case default
        write (*,*) 'Error in setting constraintType, error = ', constraintType
        stop
    end select

    ! CUBIC SPLINE INTERPOLATION
    if ( interpMethod == 1 ) then
      call cubic_spline( x, y, y2, dataNumberOfPoints, t, ind, concAtT )
      if ( concAtT <= 0 ) then
        concAtT = 0
      end if
      ! CUBIC SPLINE INTERPOLATION (LN)
    else if ( interpMethod == 2 ) then
      call cubic_spline( x, y, y2, dataNumberOfPoints, t, ind, concAtT )
      concAtT = exp( concAtT )
      ! PIECEWISE CONSTANT INTERPOLATION
    else if ( interpMethod == 3 ) then
      fac_int_found = .false.
      do i = 1, dataNumberOfPoints
        if ( (t >= x(ind, i) ) .and. ( t < x(ind, i+1) ) ) then
          concAtT = y(ind, i)
          fac_int_found = .true.
          exit
        end if
      end do
      if ( fac_int_found .eqv. .false. ) then
        write (*,*) 'error in piecewise constant interpolation'
        write (*,*) t, dataNumberOfPoints, concAtT
        concAtT = y(ind, dataNumberOfPoints)
      end if
      ! PIECEWISE LINEAR INTERPOLATION
    else if ( interpMethod == 4 ) then
      ! FIND THE INDICES OF THE ENCLOSING DATA POINTS
      lin_int_suc = .false.
      do i = 1, dataNumberOfPoints
        if ( ( t >= x(ind, i) ) .and. ( t < x(ind, i+1) ) ) then
          indexBefore = i
          indexAfter = i + 1
          lin_int_suc = .true.
        end if
      end do
      if ( lin_int_suc .eqv. .false. ) then
        concAtT = y(ind, dataNumberOfPoints)
        write (*,*) 'Failed to lin int'
      else
        ! IDENTIFY COORDINATES OF ENCLOSING DATA POINTS
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

  subroutine cubic_spline( xa, ya, y2a, n, x, ind, y )
    use types_mod
    implicit none

    real(kind=DP), intent(in) :: xa(:,:), y2a(:,:), ya(:,:), x
    integer(kind=NPI), intent(in) :: n, ind
    real(kind=DP), intent(out) :: y
    ! This routine returns the value y which is the evaluation at x of the cubic spline through
    ! points ya(ind,:) at xa(ind,:), with second derivatives there of y2a(ind,:)

    ! n is the maximum number of values in this line of x1, ya, y2a (which may not be the
    ! length of this line if different species have different constraint times, for example)

    integer(kind=NPI) :: k, khi, klo
    real(kind=DP) :: a, b, h

    if ( n > size( xa, 2 ) ) then
      stop 'n > size( xa, 2 ) in cubic_spline()'
    end if

    klo = 1
    ! We will find the right place in the table by means of bisection.
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
      print *, 'Bad input in cubic_spline()! The xa''s must be distinct'!
      stop
    end if

    a = ( xa(ind, khi) - x ) / h !Cubic spline polynomial is now evaluated.
    b = ( x - xa(ind, klo) ) / h
    y = a * ya(ind, klo) + b * ya(ind, khi) + ( ( a ** 3 - a ) * y2a(ind, klo) + ( b ** 3 - b ) * y2a(ind, khi) ) * ( h ** 2 ) / 6.
    return
  end subroutine cubic_spline
end module interpolationFunctions_mod
