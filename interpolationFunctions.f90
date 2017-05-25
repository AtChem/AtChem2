module interpolationFunctions_mod
contains
  subroutine getConstrainedQuantAtT( t, x, y, y2, dataNumberOfPoints, interpMethod, ind, concAtT )
    ! This routine returns in concAtT the value of the requested quantity (referenced
    ! by the ind-th line of x, y, y2) based upon the constraint data given and
    ! interpolation method given
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
    integer(kind=NPI) :: i, indexBefore, indexAfter
    logical :: constant_int_success, linear_int_success

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

    select case ( interpMethod )
      ! PIECEWISE CONSTANT INTERPOLATION
      case ( 1 )
        constant_int_success = .false.
        do i = 1, dataNumberOfPoints
          if ( (t >= x(ind, i) ) .and. ( t < x(ind, i+1) ) ) then
            concAtT = y(ind, i)
            constant_int_success = .true.
            exit
          end if
        end do
        if ( constant_int_success .eqv. .false. ) then
          write (*, '(A)') ' error in piecewise constant interpolation'
          write (*,*) t, dataNumberOfPoints, concAtT
          concAtT = y(ind, dataNumberOfPoints)
        end if
        ! PIECEWISE LINEAR INTERPOLATION
      case ( 2 )
        ! FIND THE INDICES OF THE ENCLOSING DATA POINTS
        linear_int_success = .false.
        do i = 1, dataNumberOfPoints
          if ( ( t >= x(ind, i) ) .and. ( t < x(ind, i+1) ) ) then
            indexBefore = i
            indexAfter = i + 1
            linear_int_success = .true.
          end if
        end do
        if ( linear_int_success .eqv. .false. ) then
          concAtT = y(ind, dataNumberOfPoints)
          write (*, '(A)') ' Failed to lin int'
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
      case default
        write (stderr,*) 'ERROR: Interpolation method not set, interpMethod = ', interpMethod
        stop
    end select

    return
  end subroutine getConstrainedQuantAtT
end module interpolationFunctions_mod
