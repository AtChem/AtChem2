module solar_test
  use fruit
  use types_mod
  implicit none

contains

  subroutine test_calcDec
    use types_mod
    use solar_functions_mod
    use zenith_data_mod, only : theta
    implicit none

    real(kind=DP) :: dec, pi

    pi = 4.0_DP * atan( 1.0_DP )

    theta = 0.0_DP
    dec = calcDec()

    call assert_true( dec == -0.402449_DP, "calcDec(), theta = 0" )
    write(*,*) dec

    theta = pi
    dec = calcDec()

    call assert_true( dec == 0.40276899999999999_DP, "calcDec(), theta = pi" )
    write(*,*) dec

    theta = pi/2.0_DP
    dec = calcDec()

    call assert_true( dec == 8.2452999999999985E-002_DP, "calcDec(), theta = pi/2" )
    write(*,*) dec

    theta = pi*3.0_DP/2.0_DP
    dec = calcDec()

    call assert_true( dec == -5.5100999999999921E-002_DP, "calcDec(), theta = 3pi/2" )
    write(*,*) dec

  end subroutine test_calcDec

end module solar_test
