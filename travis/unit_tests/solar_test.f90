module solar_test
  use fruit
  use types_mod
  use solar_functions_mod
  implicit none

contains

  subroutine test_calcTheta
    use date_mod, only : currentYear, currentDayOfYear
    real(kind=DP) :: theta, pi, threshold

    threshold = 1.0e-8_DP
    pi = 4.0_DP * atan( 1.0_DP )

    currentYear = 2000_DI
    currentDayOfYear = 0_DI
    theta = calcTheta()
    call assert_true( theta == 0.0_DP, "calcTheta(), first day of 2000" )
    currentDayOfYear = 365_DI
    theta = calcTheta()
    call assert_true( theta == 2.0_DP*pi*365_DI/366_DI, "calcTheta(), last day of 2000" )

    currentYear = 2001_DI
    currentDayOfYear = 0_DI
    theta = calcTheta()
    call assert_true( theta == 0.0_DP, "calcTheta(), first day of 2001" )
    currentDayOfYear = 364_DI
    theta = calcTheta()
    call assert_true( abs(theta - 2.0_DP*pi*364_DI/365_DI) <= threshold, "calcTheta(), last day of 2001" )

    currentYear = 2004_DI
    currentDayOfYear = 0_DI
    theta = calcTheta()
    call assert_true( theta == 0.0_DP, "calcTheta(), first day of 2004" )
    currentDayOfYear = 365_DI
    theta = calcTheta()
    call assert_true( theta == 2.0_DP*pi*365_DI/366_DI, "calcTheta(), last day of 2004" )
  end subroutine test_calcTheta

  subroutine test_decFromTheta
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
    real(kind=DP) :: dec0, dec364over365, dec365over366, threshold

    threshold = 1.0e-6_DP
    dec0 = 0.006918_DP - 0.399912_DP - 0.006758_DP - 0.002697_DP
    dec364over365 = -0.40369912461219781
    dec365over366 = -0.40369589165596537

    currentYear = 2000_DI
    currentDayOfYear = 0_DI
    call assert_true( calcDec() == dec0, "calcDec(), first day of 2000" )
    currentDayOfYear = 365_DI
    call assert_true( abs( calcDec() - dec365over366 ) < threshold, "calcDec(), last day of 2000" )

    currentYear = 2001_DI
    currentDayOfYear = 0_DI
    call assert_true( calcDec() == dec0, "calcDec(), first day of 2001" )
    currentDayOfYear = 364_DI
    call assert_true( abs( calcDec() - dec364over365 ) < threshold, "calcDec(), last day of 2001" )

    currentYear = 2004_DI
    currentDayOfYear = 0_DI
    call assert_true( calcDec() == dec0, "calcDec(), first day of 2004" )
    currentDayOfYear = 365_DI
    call assert_true( abs( calcDec() - dec365over366 ) < threshold, "calcDec(), last day of 2004" )
  end subroutine test_calcDec

  subroutine test_calcEQT
    use types_mod
    use date_mod, only : currentYear, currentDayOfYear
    real(kind=DP) :: eqt0, threshold

    threshold = 1.0e-8_DP
    eqt0 = 0.000075_DP + 0.001868_DP - 0.014615_DP

    currentYear = 2000_DI
    currentDayOfYear = 0_DI
    call assert_true( abs( calcEQT() - eqt0 ) < threshold, "calcEQT(), first day of 2000" )
    currentDayOfYear = 365_DI
    call assert_true( abs( calcEQT() + 1.0710769160677822E-002 ) < threshold, "calcEQT(), last day of 2000" )

    currentYear = 2001_DI
    currentDayOfYear = 0_DI
    call assert_true( abs( calcEQT() - eqt0 ) < threshold, "calcEQT(), first day of 2001" )
    currentDayOfYear = 364_DI
    call assert_true( abs( calcEQT() + 1.0705374687579837E-002 ) < threshold, "calcEQT(), last day of 2001" )

    currentYear = 2004_DI
    currentDayOfYear = 0_DI
    call assert_true( abs( calcEQT() - eqt0 ) < threshold, "calcEQT(), first day of 2004" )
    currentDayOfYear = 365_DI
    call assert_true( abs( calcEQT() + 1.0710769160677822E-002 ) < threshold, "calcEQT(), last day of 2004" )
  end subroutine test_calcEQT

  ! TODO:  subroutine test_calcZenith

end module solar_test
