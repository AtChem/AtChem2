module mechanism_mod
    use, intrinsic :: iso_c_binding
contains

    subroutine update_p(p, q, TEMP, N2, O2, M, RH, H2O, DEC, BLHEIGHT, DILUTE, JFAC, ROOFOPEN, J) bind(c,name='update_p')
        implicit none

	real(c_double), intent(inout) :: p(:), q(:)
        real(c_double), intent(in) :: TEMP, N2, O2, M, RH, H2O, DEC, BLHEIGHT, DILUTE, JFAC, ROOFOPEN, J(:)

        q(1) = 2.7e-12*EXP(360.0/TEMP)  !KRO2NO = 2.7e-12_DP*EXP(360.0_DP/TEMP)
	q(2) = 2.91e-13*EXP(1300.0/TEMP)  !KRO2HO2 = 2.91e-13_DP*EXP(1300.0_DP/TEMP)

        p(1) = 5.6D-34*N2*(TEMP/300)**(-2.6)*O2   !% 5.6D-34*N2*(TEMP/300)@-2.6*O2 : O = O3 ;
	p(2) = 6.0D-34*O2*(TEMP/300)**(-2.6)*O2   !% 6.0D-34*O2*(TEMP/300)@-2.6*O2 : O = O3 ;
	p(3) = 8.0D-12*EXP(-2060/TEMP)   !% 8.0D-12*EXP(-2060/TEMP) : O + O3 = ;
	p(4) = q(1)   !% KMT01 : O + NO = NO2 ;
	p(5) = 5.5D-12*EXP(188/TEMP)   !% 5.5D-12*EXP(188/TEMP) : O + NO2 = NO ;
	p(6) = q(2)   !% KMT02 : O + NO2 = NO3 ;
	p(7) = 3.2D-11*EXP(67/TEMP)*O2   !% 3.2D-11*EXP(67/TEMP)*O2 : O1D = O ;

    end subroutine update_p
end module mechanism_mod
