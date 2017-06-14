! ******************************************************************** !
!
! ******************************************************************** !
module interpolationFunctions_mod
contains

  ! -----------------------------------------------------------------
  ! This routine returns in concAtT the value of the requested
  ! quantity (referenced by the ind-th line of x, y, y2) based upon
  ! the constraint data given and interpolation method given
  subroutine getConstrainedQuantAtT( t, x, y, y2, dataNumberOfPoints, interpMethod, ind, concAtT )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use interpolationMethod
    implicit none

    real(kind=DP), intent(in) :: t, x(:,:), y(:,:), y2(:,:)
    integer(kind=NPI), intent(in) :: dataNumberOfPoints
    integer(kind=SI), intent(in) :: interpMethod
    integer(kind=NPI), intent(in) :: ind
    real(kind=DP), intent(out) :: concAtT
    real(kind=DP) :: xBefore, xAfter, yBefore, yAfter, m, c
    integer(kind=NPI) :: i, indexBefore
    logical :: interp_success

    ! Sanity checks on sizes of x, y and y2.
    if ( size( x, 1 ) /= size( y, 1 ) ) then
      stop 'size( x, 1 ) /= size( y, 1 ) in getConstrainedQuantAtT()'
    end if
    if ( size( x, 1 ) /= size( y2, 1 ) ) then
      stop 'size( x, 1 ) /= size( y2, 1 ) in getConstrainedQuantAtT()'
    end if
    if ( size( x, 2 ) /= size( y, 2 ) ) then
      stop 'size( x, 2 ) /= size( y, 2 ) in getConstrainedQuantAtT()'
    end if
    if ( size( x, 2 ) /= size( y2, 2 ) ) then
      stop 'size( x, 2 ) /= size( y2, 2 ) in getConstrainedQuantAtT()'
    end if

    ! Find the interval in which t sits
    interp_success = .false.
    do i = 1, dataNumberOfPoints
      if ( ( t >= x(ind, i) ) .and. ( t < x(ind, i+1) ) ) then
        indexBefore = i
        interp_success = .true.
        exit
      end if
    end do

    select case ( interpMethod )
      ! left-sided piecewise constant interpolation
      case ( 1 )
        if ( interp_success .eqv. .false. ) then
          write (*, '(A)') ' error in piecewise constant interpolation'
          concAtT = y(ind, dataNumberOfPoints)
          write (*, '(1P e15.7, A, I0, 1P e15.7)') t, ' ', dataNumberOfPoints, concAtT
        else
          concAtT = y(ind, indexBefore)
        end if
        ! piecewise linear interpolation
      case ( 2 )
        if ( interp_success .eqv. .false. ) then
          write (*, '(A)') ' error in piecewise linear interpolation'
          concAtT = y(ind, dataNumberOfPoints)
          write (*, '(1P e15.7, A, I0, 1P e15.7)') t, ' ', dataNumberOfPoints, concAtT
        else
          ! Identify coordinates of enclosing data points
          xBefore = x(ind, indexBefore)
          yBefore = y(ind, indexBefore)
          xAfter = x(ind, indexBefore + 1)
          yAfter = y(ind, indexBefore + 1)
          ! Do linear interpolation (Y = MX + C)
          m = ( yAfter - yBefore ) / ( xAfter - xBefore )
          c = yAfter - ( m * xAfter )
          concAtT = m * t + c
        end if
      case default
        write (stderr,*) 'ERROR: Interpolation method not set, interpMethod = ', interpMethod
        stop
    end select

    return
  end subroutine getConstrainedQuantAtT

end module interpolationFunctions_mod
