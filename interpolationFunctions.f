subroutine getConstrainedQuantAtT2D(t,x,y,y2,dataNumberOfPoints,concAtT, constraintType,ind,maxPoints,nConSpec)

    USE interpolationMethod
    USE chemcialConstraints
    integer dataNumberOfPoints,linintsuc, constraintType, maxPoints,nConSpec
    double precision:: t,x(nConSpec,maxPoints),y(nConSpec,maxPoints),y2(nConSpec,maxPoints),concAtT
    double precision:: xBefore,xAfter,yBefore,yAfter,m,c
    integer:: indexBefore,indexAfter, intMethod, ind, facintfound, i

    ! GET INTERPOLATION METHOD FOR GIVEN CONSTRAINT TYPE
    if (constraintType.eq.1) then
        call getSpeciesIntMethod(intMethod)
    else if (constraintType.eq.2) then
        call getConditionIntMethod(intMethod)
    else if (constraintType.eq.3) then
        call getDecIntMethod(intMethod)
    else
        write(*,*)'Error in setting constraintType, error = ', constraintType
    endif

    ! CUBIC SPLINE INTERPOLATION
    if(intMethod.eq.1) then
        call splint2D(x,y,y2,dataNumberOfPoints,t,concAtT,ind,maxPoints)
        if (concAtT.le.0) then
            concAtT = 0
        endif
    ! CUBIC SPLINE INTERPOLATION (LN)
    else if(intMethod.eq.2) then
        call splint2D(x,y,y2,dataNumberOfPoints,t,concAtT,ind,maxPoints)
        concAtT = exp(concAtT)
    ! PIECEWISE CONSTANT INTERPOLATION
    else if(intMethod.eq.3) then
        facintfound=0
        do i=1,dataNumberOfPoints
            if((t>=X(ind,i)).and.(t<X(ind,i+1))) then
                concAtT = Y(ind,i)
                facintfound =1
            endif
        enddo
        if (facintfound.eq.0) then
            write(*,*)'error in peicewise constant interpolation'
            write(*,*)t,dataNumberOfPoints,concAtT
            concAtT = y(ind,dataNumberOfPoints)
        endif
    ! PIECEWISE LINEAR INTERPOLATION
    else if (intMethod.eq.4) then
    ! FIND THE INDICES OF THE ENCLOSING DATA POINTS
        linintsuc = 0
        do i=1,dataNumberOfPoints
            if((t>=x(ind,i)).and.(t<x(ind,i+1))) then
                indexBefore = i
                indexAfter = i + 1
                linintsuc = 1
            endif
        enddo
        if (linintsuc.eq.0) then
            concAtT = y(ind,dataNumberOfPoints)
            write(*,*)'Failed to lin int'
        else if (linintsuc.eq.1) then
            ! INDENTIFY COORIDANTES OF ENCLOSING DATA POINTS
            xBefore = x(ind, indexBefore)
            yBefore = y(ind,indexBefore)
            xAfter = x(ind,indexAfter)
            yAfter = y(ind,indexAfter)
            ! DO LINEAR INTERPOLATION (Y = MX + C)
            m = (yAfter - yBefore)/(xAfter - xBefore)
            c= yAfter - (m*xAfter)
            concAtT = m*t + c
        endif
    else
        write(*,*)'Interpolation method not set, error = ', intMethod
    endif

    return
end

SUBROUTINE splint2D(xa,ya,y2a,n,x,y,ind,maxPoints)

    INTEGER n,maxPoints, ind
    double precision:: x,y,xa(100,maxPoints),y2a(100,maxPoints),ya(100,maxPoints)
    ! Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a function (with the xai�s in order), and given the array y2a(1:n), which is the output from spline above, and given a value of x, this routine returns a cubic-spline interpolated value y.
    INTEGER k,khi,klo
    double precision:: a,b,h

    klo=1 !We will find the right place in the table by means of bisection.
    ! This is optimal if sequential calls to this routine are at random values of x. If sequential calls are in order, and closely spaced, one would do better to store previous values of klo and khi and test if they remain appropriate on the next call.
    khi=n
    do while (khi-klo.gt.1)
        k=(khi+klo)/2
        if(xa(ind,k).gt.x)then
            khi=k
        else
            klo=k
        endif
    enddo !klo and khi now bracket the input value of x.
    h=xa(ind,khi)-xa(ind,klo)

    if (h.eq.0.) then
        print *, 'Bad input in splint2D! The xa''s must be distinct'
        stop
    end if

    a=(xa(ind, khi)-x)/h !Cubic spline polynomial is now evaluated.
    b=(x-xa(ind,klo))/h
    y=a*ya(ind,klo)+b*ya(ind,khi)+((a**3-a)*y2a(ind,klo)+(b**3-b)*y2a(ind,khi))*(h**2)/6.
    return
END

SUBROUTINE splint(xa,ya,y2a,n,x,y)
    INTEGER n
    double precision:: x,y,xa(*),y2a(*),ya(*)
    ! Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a function (with the xai�s in order), and given the array y2a(1:n), which is the output from spline above, and given a value of x, this routine returns a cubic-spline interpolated value y.
    INTEGER k,khi,klo
    double precision:: a,b,h

    klo=1 !We will find the right place in the table by means of bisection.
    ! This is optimal if sequential calls to this routine are at random values of x. If sequential calls are in order, and closely spaced, one would do better to store previous values of klo and khi and test if they remain appropriate on the next call.
    khi=n
    do while (khi-klo.gt.1) 
        k=(khi+klo)/2
        if(xa(k).gt.x)then
            khi=k
        else
            klo=k
        endif
    enddo !klo and khi now bracket the input value of x.
    h=xa(khi)-xa(klo)

    if (h.eq.0.) then
        print *, 'bad xa input in splint! The xa''s must be distinct.'
        stop
    end if
    a=(xa(khi)-x)/h !Cubic spline polynomial is now evaluated.
    b=(x-xa(klo))/h
    y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
    return
END

subroutine getConstrainedQuantAtT(t,x,y,y2,dataNumberOfPoints,concAtT, constraintType)

    USE interpolationMethod
    double precision:: t,x(*),y(*),y2(*),concAtT
    integer dataNumberOfPoints,linintsuc, constraintType
    double precision:: xBefore,xAfter,yBefore,yAfter,m,c
    integer:: indexBefore,indexAfter, intMethod, facintfound, i

    ! GET INTERPOLATION METHOD FOR GIVEN CONSTRAINT TYPE
    if (constraintType.eq.1) then
        call getSpeciesIntMethod(intMethod)
    else if (constraintType.eq.2) then
        call getConditionIntMethod(intMethod)
    else if (constraintType.eq.3) then
        call getDecIntMethod(intMethod)
    else
        write(*,*)'Error in setting constraintType, error = ', constraintType
    endif

    ! CUBIC SPLINE INTERPOLATION
    if(intMethod.eq.1) then
        call splint(x,y,y2,dataNumberOfPoints,t,concAtT)
    ! CUBIC SPLINE INTERPOLATION (LN)
    else if(intMethod.eq.2) then
        call splint(x,y,y2,dataNumberOfPoints,t,concAtT)
        concAtT = exp(concAtT)
    ! PIECEWISE CONSTANT INTERPOLATION
    else if(intMethod.eq.3) then
        facintfound=0
        do i=1,dataNumberOfPoints
            if((t>=x(i)).and.(t<x(i+1))) then
                concAtT = y(i)
                facintfound =1
            endif
        enddo
        if (facintfound.eq.0) then
            write(*,*)'error in peicewise constant interpolation'
            write(*,*)t,dataNumberOfPoints,concAtT
            concAtT = y(dataNumberOfPoints)
        endif
    ! PIECEWISE LINEAR INTERPOLATION
    else if (intMethod.eq.4) then
        ! FIND THE INDICES OF THE ENCLOSING DATA POINTS
        linintsuc = 0
        do i=1,dataNumberOfPoints
            if((t>=x(i)).and.(t<x(i+1))) then
                indexBefore = i
                indexAfter = i + 1
                linintsuc = 1
            endif
        enddo
        if (linintsuc.eq.0) then
            concAtT = y(dataNumberOfPoints)
            write(*,*)'Failed to lin int'
        else if (linintsuc.eq.1) then
            ! INDENTIFY COORIDANTES OF ENCLOSING DATA POINTS
            xBefore = x(indexBefore)
            yBefore = y(indexBefore)
            xAfter = x(indexAfter)
            yAfter = y(indexAfter)
            ! DO LINEAR INTERPOLATION (Y = MX + C)
            m = (yAfter - yBefore)/(xAfter - xBefore)
            c= yAfter - (m*xAfter)
            concAtT = m*t + c
        endif
    else
        write(*,*)'Interpolation method not set, error = ', intMethod
    endif

    return
end
