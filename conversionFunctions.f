subroutine convertRHtoConcH2O(H2o,temp,RH)
    double precision h2o, temp, rh, exponen,e1

    exponen = exp(-1.00d00*(597.30d00-0.57d00*(temp - 273.16d00))* &
	18.00d00/1.986d00*(1.00d00/temp - 1.00d00/273.16d00))
	
    e1 = 10d0/(1.38D-16*TEMP)*RH
    h2o = 6.1078d0*exponen*e1
    return
end
