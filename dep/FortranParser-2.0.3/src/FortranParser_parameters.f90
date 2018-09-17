!
! Copyright (c) 2000-2008, Roland Schmehl. All rights reserved.
!
! This software is distributable under the BSD license. See the terms of the
! BSD license in the documentation provided with this software.
!
MODULE FortranParser_parameters
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  ! Specify data types
  !--------- -------- --------- --------- --------- --------- --------- --------- -----
  use types_mod
  IMPLICIT NONE
  INTEGER, PARAMETER :: rn = selected_real_kind(DP)          ! Precision of real numbers
  INTEGER, PARAMETER :: is = SELECTED_INT_KIND(NPI) ! Data type of bytecode
END MODULE FortranParser_parameters
