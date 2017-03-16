subroutine ramp (arg1,rampValue)
    double precision arg1, arg2, rampValue

    arg2 = abs(arg1)
    rampValue = (arg1 + arg2) / 2
    return
end
