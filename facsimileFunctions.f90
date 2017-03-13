subroutine ramp (arg1,rampValue)
    double precision arg1, arg2, rampValue

    arg2 = abs(arg1)
    rampValue = (arg1 + arg2) / 2
    return
end

function sign(a,b)
    double precision :: a,b,d, sign
    d = a - b
    if (d<0) then
        sign = -1.00d+00
    else if (d==0.00d+00) then
        sign = 0.00d+00
    else if (d>0) then
        sign = 1.00d+00
    endif
    return
end
