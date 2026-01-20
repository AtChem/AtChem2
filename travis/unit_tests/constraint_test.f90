module constraint_test
  use fruit
  use types_mod
  implicit none

contains

  subroutine test_calcPhotolysisRaw
    use types_mod
    use constraint_functions_mod, only : calcPhotolysisRaw
    use zenith_data_mod, only : cosx, secx
    implicit none

    !  photolysis = l * cosx ** m * exp( -n * secx ) * tf
    cosx = 1.0_DP
    secx = 1.0_DP / cosx
    call assert_true(calcPhotolysisRaw( 1.0_DP, 1.0_DP, 1.0_DP, 1.0_DP ) == exp( -1.0_DP ),          "calcPhotolysisRaw all ones")
    call assert_true(calcPhotolysisRaw( 2.0_DP, 1.0_DP, 1.0_DP, 1.0_DP ) == 2.0_DP * exp( -1.0_DP ), "calcPhotolysisRaw 2")

    cosx = 0.5_DP
    secx = 1.0_DP / cosx
    call assert_true(calcPhotolysisRaw( 2.0_DP, 1.0_DP, 1.0_DP, 1.0_DP ) == exp( -2.0_DP ),          "calcPhotolysisRaw 3")
    call assert_true(calcPhotolysisRaw( 2.0_DP, 2.0_DP, 1.0_DP, 1.0_DP ) == 0.5_DP * exp( -2.0_DP ), "calcPhotolysisRaw 4")
    call assert_true(calcPhotolysisRaw( 2.0_DP, 1.0_DP, 2.0_DP, 1.0_DP ) == exp( -4.0_DP ),          "calcPhotolysisRaw 5")
    call assert_true(calcPhotolysisRaw( 2.0_DP, 1.0_DP, 1.0_DP, 2.0_DP ) == 2.0_DP * exp( -2.0_DP ), "calcPhotolysisRaw 6")

  end subroutine test_calcPhotolysisRaw

end module constraint_test
