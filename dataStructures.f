!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
MODULE date
    IMPLICIT NONE

    SAVE
    integer:: day, month, year, monthList(12),totalDays
    double precision:: fractionYear, secYear, currentFYear

END MODULE date
!    ********************************************************************************************************
!    ENVIRONMENTAL VARIABLES MODULE
!    ********************************************************************************************************
MODULE envVars
    IMPLICIT NONE

    SAVE
    CHARACTER(LEN=30), ALLOCATABLE:: envVarNames(:),envVarTypes(:)
    INTEGER, ALLOCATABLE:: envVarTypesNum(:)
    DOUBLE PRECISION,ALLOCATABLE:: envVarFixedValues(:), currentEnvVarValues(:)
    INTEGER:: numEnvVars, tempNum


    DOUBLE PRECISION, ALLOCATABLE::envVarX(:,:),envVarY(:,:),envVarY2(:,:)
    integer, ALLOCATABLE:: envVarNumberOfPoints(:)
	DOUBLE PRECISION :: ro2

END MODULE envVars

!    ********************************************************************************************************
MODULE constraints
    IMPLICIT NONE
    SAVE
    INTEGER :: numberOfConstrainedSpecies,  maxNumberOfDataPoints
	INTEGER :: numberOfFixedConstrainedSpecies, numberOfVariableConstrainedSpecies
    INTEGER, ALLOCATABLE ::      constrainedSpecies(:)
    REAL(8), ALLOCATABLE::    constrainedConcs(:)

    INTEGER :: i

    PRIVATE :: numberOfConstrainedSpecies, constrainedSpecies,constrainedConcs, i
    PUBLIC ::  getNumberOfConstrainedSpecies, setNumberOfConstrainedSpecies, deallocateConstrainedSpecies

    CONTAINS

    ! METHODS FOR numberOfConstrainedSpecies

    SUBROUTINE getNumberOfConstrainedSpecies(n)
        INTEGER :: n
        n = numberOfConstrainedSpecies
    END SUBROUTINE

    SUBROUTINE setNumberOfConstrainedSpecies(n)
        INTEGER :: n
        numberOfConstrainedSpecies =n
        ALLOCATE (constrainedSpecies(n), constrainedConcs(n))
        write(*,*)'Setting size of constraint arrays, n = ',n
    END SUBROUTINE

    SUBROUTINE deallocateConstrainedSpecies()
        DEALLOCATE(constrainedSpecies, constrainedConcs)
    END SUBROUTINE

    ! METHODS FOR constrainedConcs

    SUBROUTINE getConstrainedConc(n,r)
        INTEGER :: n
        REAL(8):: r
        r = constrainedConcs(n)
    END SUBROUTINE

    SUBROUTINE setConstrainedConc(n,r)
        INTEGER :: n
        REAL(8):: r
        constrainedConcs(n) = r
    END SUBROUTINE

    ! METHODS FOR constrainedSpecies

    SUBROUTINE getConstrainedSpecies(n,j)
        INTEGER :: n,j
        j = constrainedSpecies(n)
    END SUBROUTINE

    SUBROUTINE setConstrainedSpecies(n,j)
        INTEGER :: n,j
        constrainedSpecies(n) = j
    END SUBROUTINE

END MODULE constraints

MODULE species
    IMPLICIT NONE
    SAVE
    INTEGER ::     neq
    CHARACTER (len=10), ALLOCATABLE :: speciesList(:)

    INTEGER :: i

    PRIVATE :: neq, speciesList,i
    PUBLIC :: getNumberOfSpecies, setNumberOfSpecies, deallocateSpeciesList

    CONTAINS

    SUBROUTINE getNumberOfSpecies(n)
        INTEGER :: n
        n = neq
    END SUBROUTINE

    SUBROUTINE setNumberOfSpecies(n)
        INTEGER :: n
        neq = n
        ALLOCATE(speciesList(n))
    END SUBROUTINE

    SUBROUTINE deallocateSpeciesList
        DEALLOCATE(speciesList)
    END SUBROUTINE

    SUBROUTINE getSpeciesList (sl)
        CHARACTER(len=10) :: sl(*)
        do i=1, neq
            sl(i) = speciesList(i)
        enddo
    END SUBROUTINE

    SUBROUTINE setSpeciesList (sl)
        CHARACTER(len=10) :: sl(*)
        do i=1, neq
            speciesList(i) = sl(i)
        enddo
    END SUBROUTINE

END MODULE species

!    ********************************************************************************************************
!    INTERPOLATION METHOD MODULE
!    ********************************************************************************************************
MODULE interpolationMethod
    IMPLICIT NONE
    SAVE
    INTEGER :: speciesIntMethod, conditionsIntMethod, decIntMethod
    double precision, allocatable:: testArray(:)

    PUBLIC:: testArray

    PRIVATE :: speciesIntMethod, conditionsIntMethod, decIntMethod
    PUBLIC :: getSpeciesIntMethod, setSpeciesIntMethod
    PUBLIC :: getConditionIntMethod, setConditionIntMethod
    PUBLIC :: getDecIntMethod, setDecIntMethod

    CONTAINS

    SUBROUTINE getSpeciesIntMethod(n)
        INTEGER :: n
        n = speciesIntMethod
    END SUBROUTINE

    SUBROUTINE setSpeciesIntMethod(n)
        INTEGER :: n
        speciesIntMethod = n
    END SUBROUTINE

    SUBROUTINE getConditionIntMethod(n)
        INTEGER :: n
        n = conditionsIntMethod
    END SUBROUTINE

    SUBROUTINE setConditionIntMethod(n)
        INTEGER :: n
        conditionsIntMethod = n
    END SUBROUTINE

    SUBROUTINE getDecIntMethod(n)
        INTEGER :: n
        n = decIntMethod
    END SUBROUTINE

    SUBROUTINE setDecIntMethod(n)
        INTEGER :: n
        decIntMethod = n
    END SUBROUTINE

END MODULE interpolationMethod

!    ********************************************************************************************************
!    INTERPOLATION METHOD MODULE
!    ********************************************************************************************************
MODULE reactionStructure
    IMPLICIT NONE

    SAVE
    INTEGER, ALLOCATABLE:: clhs(:,:), crhs(:,:)
    INTEGER:: csize1,csize2
    DOUBLE PRECISION, ALLOCATABLE:: ccoeff(:)

END MODULE reactionStructure

!    ********************************************************************************************************
!    PHOTOLYSIS RATES METHOD MODULE
!    ********************************************************************************************************
MODULE photolysisRates
    IMPLICIT NONE

    SAVE
     integer, parameter :: maxNrOfPhotoRates = 200, maxNrOfConPhotoRates = 100
     integer:: ck(maxNrOfPhotoRates), numConPhotoRates, constrainedPhotoRatesNumbers(maxNrOfConPhotoRates)
     integer :: jfacSpeciesLine ! number of line in photolysis rates file corresponding to Jfac species
     integer :: useConstantValues, nrOfPhotoRates
     double precision:: cl(maxNrOfPhotoRates),cmm(maxNrOfPhotoRates),cnn(maxNrOfPhotoRates)
     double precision:: j(maxNrOfPhotoRates), transmissionFactor(maxNrOfPhotoRates)
     character (len = 30):: photoRateNames(maxNrOfPhotoRates), constrainedPhotoRates(maxNrOfConPhotoRates),jfacBase
    DOUBLE PRECISION, ALLOCATABLE::photoX(:,:),photoY(:,:),photoY2(:,:)
    integer, ALLOCATABLE:: photoNumberOfPoints(:)

END MODULE photolysisRates

!    ********************************************************************************************************
!    CHEMICAL CONSTRAINTS  MODULE
!    ********************************************************************************************************
MODULE chemcialConstraints
    IMPLICIT NONE

    SAVE
    DOUBLE PRECISION, ALLOCATABLE::dataX(:,:),dataY(:,:),dataY2(:,:),dataFixedY(:)
    DOUBLE PRECISION, ALLOCATABLE::constrainedConcs(:)
    integer:: numberOfConstrainedSpecies, maxNumberOfChemDataPoints
    character*10, ALLOCATABLE:: constrainedName(:)
    integer, ALLOCATABLE:: speciesNumberOfPoints(:), constrainedSpecies(:)

END MODULE chemcialConstraints

!    ********************************************************************************************************
!    PHOTOLYSIS RATES PARAMETERS  MODULE
!    ********************************************************************************************************
MODULE zenithData
    IMPLICIT NONE

    SAVE
    DOUBLE PRECISION:: lat,longt,lha,sinld,cosld

END MODULE zenithData

MODULE zenithData1
    IMPLICIT NONE

    SAVE
    DOUBLE PRECISION:: cosX,secX

END MODULE zenithData1

!    ********************************************************************************************************
!    RATES OF PRODUCTION AND LOSS  MODULE
!    ********************************************************************************************************
MODULE productionAndLossRates
    IMPLICIT NONE

    SAVE
    double precision, ALLOCATABLE:: lossRates(:),productionRates(:), ir(:)

END MODULE productionAndLossRates

!    ----------------------------------------------------------------
!     *******************************************************************************************************
!    ********************************************************************************************************
!    SOLAR ZENITH ANGLE CALCULATION VARIABLES MODULE
!    ********************************************************************************************************
MODULE SZACalcVars
    IMPLICIT NONE

    SAVE
    double precision:: latitude, longitude

END MODULE SZACalcVars
