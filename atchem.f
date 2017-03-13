!     --------------------------------------------------
!     Revision: $Revision: 101 $
!     Date: $Date: 2010-05-26 17:36:18 +0100 (Wed, 26 May 2010) $
!     ----------------------------------------------------------------
!     MAIN PROGRAM FOR THE ATMOSPHERE CHEMISTRY PROJECT
!     ----------------------------------------------------------------
!  Comment to test svn- CM 20/11/2008
!  2nd Comment to test svn, connecting using TortoiseSVN from windows - CM 20/11/2008
!  3rd Comment to test svn, local connection on chmlin10 - CM 20/11/2008

PROGRAM ATCHEM
 
    USE species
    USE constraints
    USE interpolationMethod
    USE reactionStructure
    USE photolysisRates
    USE chemcialConstraints
    USE zenithData
    USE zenithData1
    USE productionAndLossRates
    USE envVars
    USE SZACalcVars
    USE date
 
    IMPLICIT NONE

!    ********************************************************************************************************
!    DECLARATIONS
!    ********************************************************************************************************

!   DECLARATIONS FOR SOLVER PARAMETERS
    INTEGER IER, I
    INTEGER LNST, LNFE, LNSETUP, LNNI, LNCF, LNETF, LNJE
    INTEGER NFELS,NJTV,NPE,NPS
    INTEGER METH, ITMETH, ITOL, ITASK, JOUT, NOUT
    INTEGER*4 IOUT(21), IPAR(10)
    INTEGER*4 NEQ
    DOUBLE PRECISION RTOL, T, T0, TOUT
    DOUBLE PRECISION  ATOL, ROUT(6)
    DOUBLE PRECISION :: RPAR(1)
    DATA LNST/3/, LNFE/4/, LNETF/5/,  LNCF/6/, LNNI/7/, LNSETUP/8/, &
          LNJE/17/, NFELS/16/, NJTV/17/ ,NPE/18/,NPS/19/
    DOUBLE PRECISION, ALLOCATABLE:: y(:)

!   DECLARATIONS FOR CONFIGURABLE SOLVER PARAMETERS
    double precision ::     deltaJv, deltaMain,max_step
    integer ::  JvApprox, lookBack
    integer:: SpeciesIntMethod, conditionsIntMethod, decIntMethod
    integer:: preconBandUpper,preconBandLower, solverType
    real*8 :: d

!   DECLARATIONS FOR TIME PARAMETERS
    integer runStart, runEnd, runTime, rate, previousSeconds
    integer np, numReactions, outputStepSize, numSteps
    double precision tminus1

!   DECLARATIONS FOR SPECIES PARAMETERS
    integer  concCounter
    double precision, allocatable:: concentration(:)
    character*10, allocatable:: speciesName(:), concSpeciesName(:)
    integer, allocatable::  speciesNumber(:)

!   DECLARATIONS FOR RATES OF PRODUCTION AND LOSS
    integer, allocatable:: prodIntSpecies(:,:),returnArray(:),SORNumber(:),reacIntSpecies(:,:)
    integer speciesOutputRequiredSize,SORNumberSize,prodIntNameSize,returnArraySize,reacIntNameSize
    double precision, allocatable::  yInt(:)
    character*10, allocatable::  prodIntName(:),reacIntName(:),speciesOutputRequired(:)
    integer rateOfProdNS, prodLossArrayLen, rateOfLossNS, ratesOutputStepSize,time, elapsed
    integer, allocatable:: prodArrayLen(:), lossArrayLen(:)

!    DECLARATIONS FOR CULUMATIVE RATES OF PRODUCTION AND LOSS
    integer, allocatable:: culmSpeciesProduced(:), culmReactionNumber(:)
    double precision, allocatable::  culmRates(:)
    integer, allocatable::  culmSpeciesLoss(:), culmReactionNumberLoss(:)
    double precision, allocatable::  culmRatesLoss(:)

!   DECLARATIONS FOR CHEMICAL SPECIES CONSTRAINTS
    double precision, allocatable:: z(:)
    double precision solverParameters(100), modelParameters(100)

    double precision:: modelStartTime

!   DECLARATIONS FOR JACOBIAN PRODUCTION
    double precision, allocatable:: fy(:,:)
    integer:: jacobianOutputStepSize

    character (len = 30):: photoRateNamesForHeader(200)
    character (len = 400) :: fmt

!   DECLARATIONS FOR IR OUTPUT
    CHARACTER(LEN=21)::  irfileLocationPrefix 
    CHARACTER(LEN=57):: irfileLocation
    integer::irOutStepSize
    character (len = 30)::strTime
    character(10) svn_revision
		    
!    MISC
    character(len=40) :: solverTypeName(3)
    character(len=40) :: InterpolationMethodName(4)
    solverTypeName(1) = 'SPGMR'
    solverTypeName(2) = 'SPGMR + Banded Preconditioner'
    solverTypeName(3) = 'Dense'
    InterpolationMethodName(1) = 'cubic spline'
    InterpolationMethodName(2) = 'cubic spline ln'
    InterpolationMethodName(3) = 'piecewise constant'
    InterpolationMethodName(4) = 'piecewise linear'

!    ********************************************************************************************************
!    MODEL SETUP AND CONFIGURATION
!    ********************************************************************************************************

!   PRINT THE SVN REVISION NUMBER
    write(*,"(a)") "Atchem code -- Revision " // trim(svn_revision()) // "."

    call system_clock(runStart)
    previousSeconds=0

!   OPEN FILES FOR OUTPUT
    open(unit=91, file="modelOutput/concentration.output")
    open(unit=86, file="modelOutput/photolysisRates.output")
    open(unit=49, file="modelOutput/photoRateCalcParameters.output")
    open(unit=23, file="modelOutput/mainSolverParameters.output")
    open(unit=21, file="modelOutput/sparseSolverParameters.output")
    open(unit=22, file="modelOutput/stepSize.output")
    open(unit=99, file="modelOutput/initialConditionsSetting.output")
    open(unit=89, file="modelOutput/productionRates.output")
    open(unit=90, file="modelOutput/lossRates.output")
    open(unit=92, file="modelOutput/finalModelState.output")
    open(unit=93, file="modelOutput/jacobian.output")
    open(unit=94, file="modelOutput/errors.output")
    open(unit=95, file="modelOutput/envVar.output")
	flush(6)

!    OPEN FILES FOR INPUT
    open(5, file='modelConfiguration/mechanism.reac',status = 'old') ! input file

!   READ IN MECHANSIM PARAMETERS
    read(5,*) np,numReactions
    write(*,*)
    write(*,*)'Number of Species',np
    write(*,*)'Number of Reactions = ',numReactions
    write(*,*)
    close(5,status = 'keep')

    prodLossArrayLen = numReactions*2

!    SET  ARRAY SIZES = NO. OF SPECIES
    ALLOCATE (y(np), speciesName(np), concSpeciesName(np*2),speciesNumber(np),z(np),concentration(np*2))
    ALLOCATE (prodIntSpecies(np,prodLossArrayLen),returnArray(np),reacIntSpecies(np,prodLossArrayLen))
    ALLOCATE (SORNumber(np), yInt(np), prodIntName(np),reacIntName(np),speciesOutputRequired(np))
    ALLOCATE (fy(np,np))
!    SET  ARRAY SIZES = NO. OF REACTIONS
    ALLOCATE (culmSpeciesProduced(numReactions), culmReactionNumber(numReactions),culmRates(numReactions))
    ALLOCATE (culmSpeciesLoss(numReactions), culmReactionNumberLoss(numReactions),  culmRatesLoss(numReactions))
    ALLOCATE(lossRates(numReactions),productionRates(numReactions), ir(numReactions))

!   GET SIZES OF REACTANTS AND PRODUCT INPUT FILES
    call getReactionListSizes(csize1,csize2)

    ALLOCATE (clhs(3,csize1), crhs(2,csize2), ccoeff(csize2))

!   READ IN CHEMICAL REACTIONS
    call data(clhs, crhs, ccoeff, csize1, csize2)
    NEQ = np

    write (*,*) 'Size of lhs =',csize1,'size of rhs2 = ',csize2, '.'

!   READ  SPECIES NAMES AND NUMBERS
    write(*,*)
    write(*,*)'Reading species names from mechanism.species...'
    call readSpecies(y,neq,speciesName,speciesNumber)
    write(*,*)'Finished reading species names.'
    write(*,*)

!   SET PARAMETERS FOR SPECIES OBJECT
    call setNumberOfSpecies(neq)
    call setSpeciesList(speciesName)

!   SET INITIAL SPECIES CONCENTRATIONS
    call readConcentrations(concSpeciesName, concentration, concCounter,np)
    call setConcentrations(y,speciesName,concSpeciesName,concentration,concCounter,neq)
    write(*,*)
		    
!   READ IN PHOTOLYSIS RATE INFORMATION
    call readPhotoloysisConstants(ck,cl,cmm,cnn,photoRateNames,transmissionFactor)
    write(*,*)
!   Set default value for photonames array
    do i=1,200
    photoRateNamesForHeader(i) = 'na'
    enddo

    do i=1,nrOfPhotoRates
    photoRateNamesForHeader(ck(i)) = photoRateNames(i)
    enddo

!   READ IN JFAC SPECIES
    call readJFacSpecies()
    write(*,*)

!   CONFIGURE FOR OUTPUT OF PRODUCTION RATES
    call readProductsOfInterest(prodIntName,prodIntNameSize)

    call matchNameToNumber(speciesName,prodIntName,prodIntNameSize,neq,returnArray,returnArraySize)

    rateOfProdNS =     returnArraySize - 1
    ALLOCATE(prodArrayLen(rateOfProdNS))

    do i=1,rateOfProdNS
        prodIntSpecies(i,1) = returnArray(i)
    enddo

    call findReactionsWithProductX(prodIntSpecies,crhs,csize2,rateOfProdNS,prodArrayLen,prodLossArrayLen,np)
    write(*,*)'rateOfProdNS (number of species found):',rateOfProdNS
    write(*,*)

!   CONFIGURE FOR OUTPUT OF LOSS RATES
    call readReactantsOfInterest(reacIntName,reacIntNameSize)
    call matchNameToNumber(speciesName,reacIntName,reacIntNameSize,neq,returnArray,returnArraySize)

    rateOfLossNS =     returnArraySize - 1
    write(*,*)'lossOfProdNS (number of species found):',rateOfLossNS
    write(*,*)

    ALLOCATE(lossArrayLen(rateOfLossNS))

    do i=1,rateOfLossNS
        reacIntSpecies(i,1) = returnArray(i)
    enddo

    call findReactionsWithReactant(reacIntSpecies,clhs,csize1,rateOfLossNS,lossArrayLen,prodLossArrayLen,np)

    do i = 1, numReactions
        culmReactionNumberLoss(i) = reacIntSpecies(1,i)
        culmRates(i) = 0
    enddo


!    READ IN SOLVER PARAMETERS
    write(*,*) 'Reading solver parameters from file...'
    call getParametersFromFile("modelConfiguration/solver.parameters",solverParameters)
    write(*,*) 'Finished reading solver parameters from file.'

!   READ IN MODEL PARAMETERS
    write(*,*) 'Reading model parameters from file...'
    call getParametersFromFile("modelConfiguration/model.parameters",modelParameters)
    write(*,*) 'Finished eading model parameters from file.'
    write(*,*)

!   SET SOLVER PARAMETERS
    atol = solverParameters(1)
    rtol = solverParameters(2)
    JvApprox = solverParameters(3)
    deltaJv = solverParameters(4)
    deltaMain = solverParameters(5)
    lookBack = solverParameters(6)
    max_step = solverParameters(7)
    solverType = solverParameters(8)
    preconBandUpper = solverParameters(9)
    preconBandLower = solverParameters(10)

    write(*,*) 'Solver parameters:'
    write(*,*) 'atol: ', atol, 'rtol:', rtol, 'JacVApprox:', JvApprox, 'deltaJv:', deltaJv
    write(*,*) 'deltaMain:', deltaMain, 'lookBack:', lookBack, 'max_step:', max_step, &
        'precondBandUpper:', preconBandUpper, 'preconBandLower', preconBandLower 
    write(*,*) 'solverType:', solverTypeName(solverType)
    write(*,*)

!   SET MODEL PARAMETERS
    nout = modelParameters(1)
    tout = modelParameters(2)
    SpeciesIntMethod = modelParameters(3)
    call setSpeciesIntMethod(SpeciesIntMethod)
    conditionsIntMethod = modelParameters(4)
    call setConditionIntMethod(conditionsIntMethod)
    decIntMethod = modelParameters(5)
    call setDecIntMethod(decIntMethod)
    maxNumberOfDataPoints = modelParameters(6)
    numberOfConstrainedSpecies    = modelParameters(7)
    ratesOutputStepSize = modelParameters(8)
    modelStartTime = modelParameters(9)
    jacobianOutputStepSize = modelParameters(10)
    latitude = modelParameters(11)
    longitude =modelParameters(12)
    day =modelParameters(13)
    month =modelParameters(14)
    year =modelParameters(15)
    irOutStepSize = modelParameters(16)

    write(*,*) 'Model parameters:'
    write(*,*) 'number of steps: ', nout, 'step size (seconds):', tout
    write(*,*) 'species interpolation method: ', InterpolationMethodName(SpeciesIntMethod)
    write(*,*) 'conditions interpolation method: ', InterpolationMethodName(conditionsIntMethod)		    
    write(*,*) 'dec interpolation method: ', InterpolationMethodName(decIntMethod)
    write(*,*) 'maximum number of data points in constraint file:', maxNumberOfDataPoints
    write(*,*) 'maximum number of constrained species:', numberOfConstrainedSpecies
    write(*,*) 'ratesOutputStepSize', ratesOutputStepSize, &
        'instantaneous rates output step size:', irOutStepSize, &
        'modelStartTime:', modelStartTime, 'jacobianOutputStepSize:', jacobianOutputStepSize
    write(*,*) 'latitude:', latitude, 'longitude:', longitude, &
        'day/month/year:', day, month, year
    write(*,*)

    call calcDateParameters()

!   HARD CODED SOLVER PARAMETERS
    T0 = modelStartTime
    tout = tout + t0
    outputStepSize = tout - t0
    t = t0
    METH = 2
    ITMETH = 2
    ITOL = 1
    ITASK = 1
    JOUT = 0

!   READ IN ENVIRONEMENTAL VARIABLES (FIXED VALUES, CONSTRAIEND, CALCUALTED OR NOT USED, SEE ENVVAR.CONFIG)
    call readEnvVar(maxNumberOfDataPoints)
    write(*,*)

!   WRITE FILE OUTPUT HEADERS AND OUTPUT AT t=0
    call writeFileHeaders(photoRateNamesForHeader)

!   CONCENTRATION OUTPUT
    call readSpeciesOutputRequired(speciesOutputRequired,speciesOutputRequiredSize,np)
    call matchNameToNumber(speciesName,speciesOutputRequired,speciesOutputRequiredSize,neq,SORNumber,SORNumberSize)
    call getConcForSpecInt(y,yInt,SORNumber,SORNumberSize,neq)
    call outputInterestingNames(speciesOutputRequired,speciesOutputRequiredSize)
    SORNumberSize = SORNumberSize -1

    write(*,*) 'Output required for concentration of', speciesOutputRequiredSize, 'species:'
    if(speciesOutputRequiredSize.gt.2) then
        write(*,*) 1, speciesOutputRequired(1)
        write(*,*) '...'
        write(*,*) speciesOutputRequiredSize, speciesOutputRequired(speciesOutputRequiredSize)
    else
        do i=1,speciesOutputRequiredSize
            write(*,*) i, speciesOutputRequired(i)
        enddo
    endif

flush(6)
!    ********************************************************************************************************
!    CONSTRAINTS
!    ********************************************************************************************************

    write(*,*)
    call readPhotoRates(maxNumberOfDataPoints)
    write(*,*)

    call readSpeciesConstraints (speciesName,neq,y, t)

    write(*,*)
!test
	call getConcForSpecInt(y,yInt,SORNumber,SORNumberSize,neq+numberOfConstrainedSpecies)
	call outputInteresting(t,yInt,SORNumberSize)

    call removeConstrainedSpeciesFromProbSpec(y,z,numberOfConstrainedSpecies,constrainedSpecies,neq)

!   ADJUST PROBLEM SPECIFICATION TO GIVE NUMBER OF SPECIES TO BE SOLVED FOR (N - C = M)
    neq = neq - numberOfConstrainedSpecies
    write(*,*)'neq = ', neq, ' numberOfConstrainedSpecies = ',numberOfConstrainedSpecies

flush(6)
!    ********************************************************************************************************
!    CONFIGURE SOLVER
!    ********************************************************************************************************

    WRITE(6,*) 'Dense example problem:'
    WRITE(6,*) ' Robertson kinetics, NEQ = ', NEQ

    ipar(1) = neq
    ipar(2) = numReactions

    CALL FNVINITS(1, NEQ, IER)
    IF (IER .NE. 0) THEN
        WRITE(6,20) IER
 20     FORMAT(///' SUNDIALS_ERROR: FNVINITS returned IER = ', I5)
        STOP
    ENDIF

    write(*,*)'t0 = ', t0
    CALL FCVMALLOC(T0, Z, METH, ITMETH, ITOL, RTOL, ATOL, &
                    IOUT, ROUT, IPAR, RPAR, IER)
    IF (IER .NE. 0) THEN
        WRITE(6,30) IER
 30     FORMAT(///' SUNDIALS_ERROR: FCVMALLOC returned IER = ', I5)
        STOP
    ENDIF

    numsteps = 100000
    call fcvsetiin('MAX_NSTEPS',numsteps,ier)
    write(*,*)'setting maxsteps IER = ',ier

    call FCVSETRIN('MAX_STEP',max_step,IER)

!   SELECT SOLVER TYPE ACCORDING TO FILE INPUT
!   SPGMR SOLVER
    if(solverType.eq.1) then
        call FCVSPGMR(0,1,lookBack,deltaMain,IER)
    ! SPGMR SOLVER WITH BANDED PRECONDITIONER
    else if(solverType.eq.2) then
        call FCVSPGMR(1,1,lookBack,deltaMain,IER)
        call FCVBPINIT(neq,preconBandUpper,preconBandLower,IER)
        if(IER .ne. 0 ) then
            write(6,*) 'SUNDIALS_ERROR: preconditioner returned IER = ', IER ;
            call FCVFREE
            stop
        end if
    ! DENSE SOLVER
    else if(solverType.eq.3) then
        ! make sure no Jacobian approximation is required
        if (JVapprox.eq.1) then
            write(6,*) 'Solver parameter conflict! Jv approximation cannot be used for dense solver.'
            write(6,*) 'Fix parameters in "modelConfiguration/solver.parameters" file.'
            stop
        end if
        CALL FCVDENSE(NEQ, IER)
    ! UNEXPECTED SOLVER TYPE
    else
        write(*,*) 'error with solverType input, error = ',solverType
    endif
    ! ERROR HANDLING
    if(IER .ne. 0 ) then
        write(6,*) ' SUNDIALS_ERROR: SOLVER returned IER = ', IER
        call FCVFREE
        stop
    endif

!    USE JACOBIAN APPROXIMATION IF REQUIRED
    if (JVapprox.eq.1) then
        call FCVSPILSSETJAC(1,IER)
    endif

    IF (IER .NE. 0) THEN
        WRITE(6,40) IER
 40     FORMAT(///' SUNDIALS_ERROR: FCVDENSE returned IER = ', I5)
        CALL FCVFREE
        STOP
    ENDIF

    ! check JFac data consistency:
    call test_jfac()
			
!    ********************************************************************************************************
!    RUN MODEL
!    ********************************************************************************************************

    DO WHILE(JOUT .LT. NOUT)
        ! GET CONCENTRATIONS FOR SOLVED SPECIES
        write(49,*)t,secx,cosx,lat,longt,lha,sinld,cosld
        CALL FCVODE(TOUT, T, Z, ITASK, IER)
		if (IER.ne.0) then
			WRITE (*,*)'IER POST FCVODE = ', IER
		endif 
flush(6)
        ! GET CONCENTRATIONS FOR CONSTRAINED SPECIES AND ADD TO ARRAY FOR OUTPUT
        do i=1,numberOfConstrainedSpecies
            call getConstrainedConc(i,d)
            constrainedConcs(i) = d
        enddo

        call  addConstrainedSpeciesToProbSpec(z,y,numberOfConstrainedSpecies,constrainedSpecies,neq,constrainedConcs)

        ! OUTPUT ON SCREEN
        fmt = "('At t = ', E12.4, '   y = ', 3E14.6)"
		! printing concentration of two first species - seems unnecessary at the moment
        ! WRITE(6,fmt) T, Y(1), Y(2)

        ! OUTPUT RATES OF PRODIUCTION ON LOSS (OUTPUT FREQUENCY SET IN MODEL.PARAMETERS)
        time = int(t)
		elapsed = int(t-modelStartTime)
        if (mod(elapsed,ratesOutputStepSize).EQ.0) then
            call outputRates(prodIntSpecies,t,productionRates,1,np,rateOfProdNS,prodLossArrayLen,rateOfLossNS, prodArrayLen, &
                lossArrayLen ,speciesName)
            call outputRates(reacIntSpecies,t,lossRates,0,np,rateOfProdNS,prodLossArrayLen,rateOfProdNS,prodArrayLen,lossArrayLen &
            ,speciesName)
        endif

        ! OUTPUT JACOBIAN MATRIX (OUTPUT FREQUENCY SET IN MODEL PARAMETERS)
        write(*,*)'time = ', time
        if (mod(elapsed,jacobianOutputStepSize).EQ.0) then
            call jfy(np,numReactions,y,fy,t)
            call outputjfy(fy,np,t)
        endif

        call getConcForSpecInt(y,yInt,SORNumber,SORNumberSize,neq+numberOfConstrainedSpecies)
		call outputInteresting(t,yInt,SORNumberSize)
        call outputPhotolysisRates(j,t)


        !OUTPUT INSTANTANEOUS RATES
        if (mod(elapsed,irOutStepSize).EQ.0) then
            write(strTime,*),time

            irfileLocationPrefix = './instantaneousRates/'
            irfileLocation = irfileLocationPrefix // ADJUSTL(strTime)

            open(27,file=irfileLocation)
            do i=1,numReactions
                write(27,*)ir(i)
            enddo
            close(27,status = 'keep')
        endif

        ! OUTPUT FOR CVODE MAIN SOLVER
        write(23,*),t,' ',IOUT(LNST),' ', IOUT(LNFE),' ',IOUT(LNETF)
        ! OUTPUT FOR SPARSE SOLVER
        write(21,*),t,' ',IOUT(NFELS),' ',IOUT(NJTV),' ',IOUT(NPE),' ',IOUT(NPS)
        ! OUTPUT STEP SIZE
        write(22,*)t,' ',ROUT(3),' ',ROUT(2)

        !OUTPUT ENVVAR VALUES
		call ro2sum(ro2, y)
        call outputEnvVar(t)

        ! CALCULATE AND OUTPUT RUNTIME
		! not using timing at the moment
        ! call system_clock(current, rate)
        ! currentSeconds = (current - runStart) / rate
        ! stepTime = currentSeconds - previousSeconds
        ! write (*,*)'Current time = ', currentSeconds,'        step time = ',stepTime
        ! previousSeconds = currentSeconds

        ! ERROR HANDLING
        IF (IER .LT. 0) THEN
            fmt = "(///' SUNDIALS_ERROR: FCVODE returned IER = ', I5, /,'                 Linear Solver returned IER = ', I5)"
            WRITE(6,fmt) IER, IOUT(15)
            ! free memory
           CALL FCVFREE
           STOP
        ENDIF

        IF (IER .EQ. 0) THEN
            tminus1 = t
            TOUT = TOUT + outputStepSize
            JOUT = JOUT + 1
        ENDIF

    ENDDO

    CALL FCVDKY(T, 1, Z, IER)
    IF (IER .NE. 0) THEN
        fmt = "(///' SUNDIALS_ERROR: FCVDKY returned IER = ', I4)"
        WRITE(6,fmt) IER
        CALL FCVFREE
        STOP
    ENDIF

!   OUPUT FINAL MODEL CONCENTRATIONS FOR MODEL RESTART
    do i=1,np
        write(92,*)speciesName(i), y(i)
    enddo

!   printing of final statistics desactivated - nobody finds it useful
!   FINAL ON SCREEN OUTPUT
    fmt = "(//'Final statistics:'//" // &
        "' No. steps = ', I4, '   No. f-s = ', I4," // &
        "'   No. J-s = ', I4, '   No. LU-s = ', I4/" // &
        "' No. nonlinear iterations = ', I4/" // &
        "' No. nonlinear convergence failures = ', I4/" // &
        "' No. error test failures = ', I4/)"

    WRITE(6,fmt) IOUT(LNST), IOUT(LNFE), IOUT(LNJE), IOUT(LNSETUP), &
        IOUT(LNNI), IOUT(LNCF), IOUT(LNETF)

    call system_clock(runEnd, rate)
    runTime = (runEnd - runStart) / rate
    write (*,*)'Runtime = ', runTime

!   deallocate all
    write(*,*) 'Deallocating memory.'
!   deallocate CVODE internal data
	
	CALL FCVFREE
    DEALLOCATE (y, speciesName, concSpeciesName,speciesNumber,z,concentration)
    DEALLOCATE (prodIntSpecies,returnArray,reacIntSpecies)
    DEALLOCATE (SORNumber, yInt, prodIntName,reacIntName,speciesOutputRequired)
    DEALLOCATE (fy)
    DEALLOCATE (culmSpeciesProduced, culmReactionNumber,culmRates,ir)
    DEALLOCATE (culmSpeciesLoss, culmReactionNumberLoss,  culmRatesLoss)
    DEALLOCATE(lossRates,productionRates)
    DEALLOCATE (clhs, crhs, ccoeff)
    DEALLOCATE(prodArrayLen)
    DEALLOCATE(lossArrayLen)
!   deallocate data allocated before in input functions (inputFunctions.f)
!   deallocate arrays from module constraints
    CALL deallocateConstrainedSpecies()
!   deallocate arrays from module species
    CALL deallocateSpeciesList
!   deallocate arrays from module chemcialConstraints
    DEALLOCATE(dataX,dataY, dataY2, dataFixedY, constrainedConcs, constrainedName)
    DEALLOCATE(speciesNumberOfPoints, constrainedSpecies)
!   deallocate arrays from module envVars
    DEALLOCATE(envVarTypesNum, envVarNames, envVarTypes, envVarFixedValues)
    DEALLOCATE(envVarX, envVarY, envVarY2, envVarNumberOfPoints)
!   deallocate arrays from module photolysisRates
    DEALLOCATE(photoX, photoY, photoY2, photoNumberOfPoints)

    STOP
END PROGRAM ATCHEM
