!     ---------------------------------------------------------------
subroutine FCVJTIMES(v,fjv,t,y,fy,h,ipar,rpar,work,ier)

    USE species

    integer neq,i,j,ier,ipar(*),np
    double precision t,h,rpar(*),y(*),v(*),fjv(*),fy(*),work(*),delta, deltaV, dummy
    DOUBLE PRECISION, ALLOCATABLE:: yPlusV(:),yPlusVi(:)
    call getNumberOfSpecies(np)
    ALLOCATE (yPlusV(np),yPlusVi(np))

    neq = ipar(1)
    delta = 1.00d-03
    ! fake using variables h and work, to avoid a warning (they are required by CVODE code)
    h = h
    dummy = work(1)

    ! calculate y + delta v
    j = 0
    do i=1,neq
        deltaV = delta * v(i)
        yPlusV(i) = y(i) + deltaV
    enddo

    ! get f(y + delta v)
    call FCVFUN(T,yPlusV,yPlusVi, IPAR, RPAR, IER)

    ! JVminus1 + deltaJV
    do i=1,neq
        fjv(i) =  (yPlusVi(i) - fy(i)) / delta
    enddo
    DEALLOCATE(yPlusV, yPlusVi)

    return
end

!     ---------------------------------------------------------------
SUBROUTINE FCVFUN(T, Y, YDOT, IPAR, RPAR, IER)
    USE species
    USE constraints
    USE reactionStructure
    USE chemcialConstraints

    ! Fortran routine for right-hand side function.
    IMPLICIT NONE
    !
    INTEGER*4 IPAR(*), IER,nConSpec
    DOUBLE PRECISION T, Y(*), YDOT(*), RPAR(*),concAtT, dummy
    DOUBLE PRECISION, ALLOCATABLE::  dy(:), z(:)
    integer np, numReactions, i

    np = ipar(1) + numberOfConstrainedSpecies
    numReactions = ipar(2)
    dummy = rpar(1)

    nConSpec = numberOfConstrainedSpecies
    ALLOCATE(dy(np), z(np))

    do i=1, numberOfConstrainedSpecies
		if (i<=numberOfVariableConstrainedSpecies) THEN
			call    getConstrainedQuantAtT2D(t,datax,datay,datay2,speciesNumberOfPoints(i),concAtT, &
				1,i,maxNumberOfDataPoints,numberOfVariableConstrainedSpecies)
		ELSE
			concAtT = dataFixedY(i-numberOfVariableConstrainedSpecies)
		ENDIF
        constrainedConcs(i) = concAtT
		call setConstrainedConc(i,concAtT)

    enddo

    call  addConstrainedSpeciesToProbSpec(y,z,numberOfConstrainedSpecies,constrainedSpecies,ipar(1),constrainedConcs)

    call resid(np,numReactions,t,z,dy, clhs, crhs, ccoeff, csize1, csize2)

    call removeConstrainedSpeciesFromProbSpec(dy,ydot,numberOfConstrainedSpecies,constrainedSpecies,np)

    DEALLOCATE(dy, z)
    IER = 0

    RETURN
END

!     ----------------------------------------------------------------
!-----------------------------------------------------------------
!     routine for reading in the reaction
subroutine data(lhs, rhs, coeff, size1, size2)
    integer:: k,l, size1, size2
    integer:: lhs(3,size1), rhs(2,size2)
    double precision:: coeff(size2)

    write(*,*)'Reading reactants (lhs) from mechanism.rec...' 
    open (4,file='modelConfiguration/mechanism.reac',status = 'old') ! input file for lhs of equations
    open (14,file = 'modelConfiguration/mechanism.prod',status = 'old') ! input file for rhs of equations

    ! read data for lhs of equations
    size1=0
    read(4,*)
    do
        read(4,*) k,l

        if (k.eq.0) exit
        size1=size1+1
        lhs(1,size1)=k
        lhs(2,size1)=l
        lhs(3,size1)=1

    enddo

    write(*,*) 'Reading products (rhs) from mechanism.prod...'
    ! read data for rhs of equations
    size2=0
    do
        read(14,*) k,l
        if (k.eq.0) exit
        size2=size2+1
        rhs(1,size2)=k
        rhs(2,size2)=l
        coeff(size2)=1

    enddo

    close(4,status = 'keep')
    close(14,status = 'keep')

    write(*,*)'Finished reading lhs and rhs data.'
    return
end


subroutine resid(nsp,nr,clocktime,y,dy, lhs, rhs, coeff, size1, size2)
    ! calculate rhs of rate eqn dy()
    USE productionAndLossRates

    implicit none
    integer:: i
    integer:: nsp ! number of species involved
    integer:: nr ! number of reactions
    integer:: size1, size2 !number of entries in each equation array
    integer:: lhs(3,size1), rhs(2,size2)
    double precision:: coeff(*) ! coeff term of rhs
    double precision:: y(*) ! concentration array
    double precision:: p(nr) ! array to hold rates
    double precision:: r(nr) ! working array
    double precision:: dy(*) ! array to hold value of rate equations
    double precision:: clocktime

    ! set rate eqn to zero
    do i=1,nsp
        dy(i)=0

    enddo

    do i=1,nr
        productionRates(i) = 0
        lossRates(i) = 0
    enddo

    ! get values of reactions rates
    call mechanism_rates(p,clocktime,y,nsp)

    ! calculation of rhs of rate equations
    do i = 1,nr
        r(i) = p(i)
    enddo


    do i = 1,size1
    r(lhs(1,i)) = r(lhs(1,i))*y(lhs(2,i))**lhs(3,i)
    ir(lhs(1,i)) = r(lhs(1,i))
    enddo

    do i = 1,size1
        dy(lhs(2,i)) = dy(lhs(2,i))-lhs(3,i)*r(lhs(1,i))
        lossRates(lhs(1,i)) = abs(dy(lhs(2,i)))
    enddo

    do i = 1,size2
        dy(rhs(2,i))=dy(rhs(2,i))+coeff(i)*r(rhs(1,i))
        productionRates(rhs(1,i)) = productionRates(rhs(1,i)) + coeff(i)*r(rhs(1,i))
    enddo

    return
end

subroutine jfy(ny,nr,y,fy,t)
    ! routine for calculating the Jacobian of the system
    ! ny =  number of species
    ! nr = number of reactions
    ! for each species calculate the rhs of the rate equation
    ! for the reactants array
    ! csize1 is the number of entries
    ! clhs(1,) = reaction number
    ! clhs(2,) = species number
    ! clhs(3,) = stochiometric coefficient

    ! for the products array
    ! csize2 is the number of entries
    ! crhs(1,) = reaction number
    ! crhs(2,) = species number
    ! ccoeff() = stochiometric coefficient (double precision)

    ! y = concentration array   - dimension ny
    ! fy = jacobian array - dimension ny x ny
    ! t = current time (s)
    ! p = reaction rates - dimension nr
    ! r = working array - dimension nr

    use reactionStructure    ! access is, crhs, nclhs, csize2
    implicit none

    integer ny,nr
    double precision  p(nr), fy(ny,*),y(*),r(nr),t
    integer j, is

    ! set jacobian matrix to zero
    fy(1:ny, 1:ny)=0.0

    ! call routine to get reaction rates in array p
    CALL mechanism_rates(p,t,y,ny)

    do j=1,ny
        r(1:nr) = 0.0
        do is=1,csize1
            if(clhs(2,is).eq.j)  then
                r(clhs(1,is)) = p(clhs(1,is))
            endif
        enddo
        do is=1,csize1
            if(clhs(2,is).eq.j) then
                r(clhs(1,is)) = r(clhs(1,is))*clhs(3,is)*y(clhs(2,is))**(clhs(3,is)-1)
            else
                r(clhs(1,is)) = r(clhs(1,is))*y(clhs(2,is))**clhs(3,is)
            endif
        enddo
        do is=1,csize1
            fy(clhs(2,is),j)=fy(clhs(2,is),j)-clhs(3,is)*r(clhs(1,is))
        enddo
        do is=1,csize2
           fy(crhs(2,is),j)=fy(crhs(2,is),j) + ccoeff(is) * r(crhs(1,is))
        enddo
    enddo

    return
end
