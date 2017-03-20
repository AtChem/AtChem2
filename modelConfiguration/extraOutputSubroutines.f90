SUBROUTINE ro2sum (ro2, y)
  DOUBLE PRECISION :: ro2
  DOUBLE PRECISION, INTENT (in) :: y(*)
  ro2 = 0.00e+00
END SUBROUTINE ro2sum

SUBROUTINE atmosphere (o2, n2, m)
  DOUBLE PRECISION :: o2, n2, m
  o2 = 0.2095*m
  n2 = 0.7809*m
END SUBROUTINE atmosphere
