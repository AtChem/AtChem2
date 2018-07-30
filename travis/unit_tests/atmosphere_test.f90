module atmosphere_test
  use fruit
  use types_mod
  implicit none

contains

  subroutine test_calcAirDensity
    use types_mod
    use atmosphere_functions_mod
    implicit none

    real(kind=DP) :: density, press, temp

    press = 0.0_DP
    temp = 1.0_DP
    density = calcAirDensity( press, temp )

    call assert_true( density == 0.0_DP, "calcAirDensity(0,1)" )

    press = 1.0_DP
    temp = 1.0_DP
    density = calcAirDensity( press, temp )

    call assert_true( density == 7.242971604861847e+18_DP, "calcAirDensity(1,1)" )
  end subroutine test_calcAirDensity

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

    m = 0.0_DP
    call calcAtmosphere( m, o2, n2 )

    call assert_true( m == 0.0_DP, "m unchanged 0" )
    call assert_true( o2 == 0.0_DP, "o2 correct 0" )
    call assert_true( n2 == 0.0_DP, "n2 correct 0" )
  end subroutine test_calcAtmosphere

  subroutine test_convertRHtoH2O
    use types_mod
    use atmosphere_functions_mod
    implicit none

    real(kind=DP) :: rh, temp, press, ad, h2o

    rh = 50.0_DP
    temp = 273.15_DP
    press = 6.116441_DP

    ad = calcAirDensity(press, temp)

    h2o = convertRHtoH2O( rh, temp, press )

    call assert_equals( h2o, ad, "convertRHtoH2O: 50% relative humidity, 0C, pressure = A" )

    rh = 100.0_DP
    temp = 273.15_DP
    press = 2.0_DP*6.116441_DP

    ad = calcAirDensity(press, temp)

    h2o = convertRHtoH2O( rh, temp, press )

    call assert_equals( h2o, ad, "convertRHtoH2O: 100% relative humidity, 0C, pressure = 2*A" )

    rh = 0.0_DP
    temp = 300.0_DP
    press = 10.0_DP

    h2o = convertRHtoH2O( rh, temp, press )

    call assert_equals( h2o, 0.0_DP, "convertRHtoH2O: 0% relative humidity" )

  end subroutine test_convertRHtoH2O

  subroutine test_zero_equal
    use types_mod
    implicit none

    call assert_true( 0.0_DP == 0.0_DP, "Zero equality - sanity check!" )
  end subroutine test_zero_equal

end module atmosphere_test
