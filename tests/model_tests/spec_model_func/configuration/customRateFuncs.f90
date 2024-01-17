! -----------------------------------------------------------------------------
!
! Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
! Peter Jimack, Mike Pilling
!
! Copyright (c) 2017 Sam Cox, Roberto Sommariva
!
! Copyright (c) 2023 Alfred Mayhew
!
! This file is part of the AtChem2 software package.
!
! This file is covered by the MIT license which can be found in the file
! LICENSE.md at the top level of the AtChem2 distribution.
!
! -----------------------------------------------------------------------------
! ******************************************************************** !
! ATCHEM2 -- MODULE customRateFunctions
!
! This module contains user-defined functions that can be referenced
! in the mechanism file
! ******************************************************************** !
module custom_functions_mod
  implicit none

contains

  ! -----------------------------------------------------------------
  ! Calculates the rate of KMT15
  pure function calcKMT15( t, m ) result ( KMT15 )
    real*8, intent(in) :: t, m
    real :: K150, K15I, KR15, FC15, NC15, F15
    real :: KMT15

    K150 = 8.6E-29 * m * (t / 300)**(-3.1)
    K15I = 9.0E-12 * (t / 300)**(-0.85)
    KR15 = K150 / K15I
    FC15 = 0.48
    NC15 = 0.75 - 1.27 * (LOG10(FC15))
    F15 = 10**(LOG10(FC15) / (1 + (LOG10(KR15) / NC15)**2))
    KMT15 = (K150*K15I)*F15/(K150+K15I)

    return
  end function calcKMT15

end module custom_functions_mod