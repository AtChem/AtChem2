module solar_test
  use fruit
  use types_mod
  use solar_functions_mod
  implicit none

contains

  subroutine test_calcTheta
    use date_mod, only : currentYear, currentDayOfYear
    implicit none
    real(kind=DP) :: theta, pi, threshold

    threshold = 1.0e-8_DP
    pi = 4.0_DP * atan( 1.0_DP )

    currentYear = 2000_DI
    currentDayOfYear = 0_DI
    theta = calcTheta()
    call assert_true( theta == 0.0_DP, "calcTheta(), first day of 2000" )

    currentYear = 2000_DI
    currentDayOfYear = 366_DI
    theta = calcTheta()
    call assert_true( theta == 2.0_DP*pi, "calcTheta(), last day of 2000" )

    currentYear = 2001_DI
    currentDayOfYear = 0_DI
    theta = calcTheta()
    call assert_true( theta == 0.0_DP, "calcTheta(), first day of 2001" )

    currentYear = 2001_DI
    currentDayOfYear = 365_DI
    theta = calcTheta()
    call assert_true( abs(theta - 2.0_DP*pi) <= threshold, "calcTheta(), last day of 2001" )

    currentYear = 2004_DI
    currentDayOfYear = 0_DI
    theta = calcTheta()
    call assert_true( theta == 0.0_DP, "calcTheta(), first day of 2004" )

    currentYear = 2004_DI
    currentDayOfYear = 366_DI
    theta = calcTheta()
    call assert_true( theta == 2.0_DP*pi, "calcTheta(), last day of 2004" )
  end subroutine test_calcTheta

  subroutine test_decFromTheta
    implicit none

    real(kind=DP) :: dec, pi, theta

    pi = 4.0_DP * atan( 1.0_DP )

    theta = 0.0_DP
    dec = decFromTheta( theta )
    call assert_true( dec == -0.402449_DP, "decFromTheta( theta ), theta = 0" )

    theta = pi
    dec = decFromTheta( theta )
    call assert_true( dec == 0.40276899999999999_DP, "decFromTheta( theta ), theta = pi" )

    theta = pi/2.0_DP
    dec = decFromTheta( theta )
    call assert_true( dec == 8.2452999999999985E-002_DP, "decFromTheta( theta ), theta = pi/2" )

    theta = pi*3.0_DP/2.0_DP
    dec = decFromTheta( theta )
    call assert_true( dec == -5.5100999999999921E-002_DP, "decFromTheta( theta ), theta = 3pi/2" )

  end subroutine test_decFromTheta

  subroutine test_calcDec
    use types_mod
    use date_mod, only : currentYear, currentDayOfYear
    implicit none
    currentYear = 2000_DI
    currentDayOfYear = 0_DI
    call assert_true( calcDec() == 0, "calcDec(), first day of 2000" )

    currentYear = 2000_DI
    currentDayOfYear = 366_DI
    call assert_true( calcDec() == 0, "calcDec(), last day of 2000" )

    currentYear = 2001_DI
    currentDayOfYear = 0_DI
    call assert_true( calcDec() == 0, "calcDec(), first day of 2001" )

    currentYear = 2001_DI
    currentDayOfYear = 365_DI
    call assert_true( calcDec() == 0, "calcDec(), last day of 2001" )

    currentYear = 2004_DI
    currentDayOfYear = 0_DI
    call assert_true( calcDec() == 0, "calcDec(), first day of 2004" )

    currentYear = 2004_DI
    currentDayOfYear = 366_DI
    call assert_true( calcDec() == 0, "calcDec(), last day of 2004" )


  end subroutine test_calcDec

  subroutine test_calcZenith
    implicit none

  end subroutine test_calcZenith

end module solar_test
