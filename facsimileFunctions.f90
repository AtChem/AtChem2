SUBROUTINE ramp (arg1, rampValue)
  DOUBLE PRECISION arg1, arg2, rampValue

  arg2 = ABS (arg1)
  rampValue = (arg1 + arg2) / 2
  RETURN
END SUBROUTINE ramp
