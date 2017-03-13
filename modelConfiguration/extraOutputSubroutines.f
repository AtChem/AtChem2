subroutine ro2sum(ro2, y)
double precision:: ro2
double precision, intent(in) :: y(*)
	ro2 = 0.00e+00
end subroutine ro2sum

subroutine atmosphere(O2, N2, m)
double precision::  O2, N2, m
	O2 = 0.2095*m
    N2 = 0.7809*m
END subroutine atmosphere