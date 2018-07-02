module atmosphere_test
  use fruit
  implicit none

contains
  subroutine test_calc_air_density
    use types_mod
    use atmosphere_functions_mod
    implicit none

    write (*,*) '1'
    call assert_true(calcAirDensity(0.0_DP, 1.0_DP) >= 0.0_DP, "Equals zero")
  end subroutine test_calc_air_density

  subroutine test_zero_equal
    use types_mod
    use atmosphere_functions_mod
    implicit none

    write(*,*) '2'
    call assert_true(0.0_DP == 0.0_DP, "Zero equality")
  end subroutine test_zero_equal
end module atmosphere_test
