! -----------------------------------------------------------------------------
!
! Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
! Peter Jimack, Mike Pilling
!
! Copyright (c) 2017 Sam Cox, Roberto Sommariva
!
! This file is part of the AtChem2 software package.
!
! This file is covered by the MIT license which can be found in the file
! LICENSE.md at the top level of the AtChem2 distribution.
!
! -----------------------------------------------------------------------------

! ******************************************************************** !
! ATCHEM2 -- MODULE interpolationFunctions
!
! This module contains just the getConstrainedQuantAtT() method.
! ******************************************************************** !
module interpolation_functions_mod
contains

  ! -----------------------------------------------------------------
  ! This routine returns in concAtT the value of the requested
  ! quantity (referenced by the ind-th line of x, y) based upon
  ! the constraint data given and interpolation method given
  subroutine getConstrainedQuantAtT( t, x, y, dataNumberOfPoints, interpMethod, ind, concAtT )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use interpolation_method_mod, only : getSpeciesInterpMethod, getConditionsInterpMethod, &
                                         setSpeciesInterpMethod, setConditionsInterpMethod
    implicit none

    real(kind=DP), intent(in) :: t, x(:,:), y(:,:)
    integer(kind=NPI), intent(in) :: dataNumberOfPoints
    integer(kind=SI), intent(in) :: interpMethod
    integer(kind=NPI), intent(in) :: ind
    real(kind=DP), intent(out) :: concAtT
    real(kind=DP) :: xBefore, xAfter, yBefore, yAfter, m, c
    integer(kind=NPI) :: i, indexBefore
    logical :: interp_success

    ! Sanity checks on sizes of x and y.
    if ( size( x, 1 ) /= size( y, 1 ) ) then
      stop 'size( x, 1 ) /= size( y, 1 ) in getConstrainedQuantAtT()'
    end if
    if ( size( x, 2 ) /= size( y, 2 ) ) then
      stop 'size( x, 2 ) /= size( y, 2 ) in getConstrainedQuantAtT()'
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

end module interpolation_functions_mod
