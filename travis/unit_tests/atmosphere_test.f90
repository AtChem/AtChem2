module atmosphere_test
  use fruit
  use types_mod
  implicit none

contains

  subroutine test_calcAtmosphere
    use types_mod
    use atmosphere_functions_mod
    implicit none

    real(kind=DP) :: m, o2, n2

    m = 1.0_DP
    call calcAtmosphere( m, o2, n2 )

    call assert_true( m == 1.0_DP, "m unchanged 1" )
    call assert_true( o2 == 0.2095_DP, "o2 correct 1" )
    call assert_true( n2 == 0.7809_DP, "n2 correct 1" )

    m = 2.0_DP
    call calcAtmosphere( m, o2, n2 )

    call assert_true( m == 2.0_DP, "m unchanged 2" )
    call assert_true( o2 == 0.4190_DP, "o2 correct 2" )
    call assert_true( n2 == 1.5618_DP, "n2 correct 2" )
  end subroutine test_calcAtmosphere

  subroutine test_zero_equal
    use types_mod
    implicit none

    call assert_true( 0.0_DP == 0.0_DP, "Zero equality - sanity check!" )
  end subroutine test_zero_equal

end module atmosphere_test
