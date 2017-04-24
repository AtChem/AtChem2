MODULE storage
  IMPLICIT NONE
  INTEGER, PARAMETER :: maxSpecLength=10
  INTEGER, PARAMETER :: maxPhotoRateNameLength=6
  INTEGER, PARAMETER :: maxEnvVarNameLength=9
  INTEGER, PARAMETER :: maxEnvVarLength=15
END MODULE

!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
MODULE directories
  IMPLICIT NONE

  SAVE
  CHARACTER (LEN=80) :: output_dir, instantaneousRates_dir, param_dir

END MODULE directories
!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
MODULE date
  IMPLICIT NONE

  SAVE
  INTEGER :: day, month, year, monthList(12), totalDays
  DOUBLE PRECISION :: fractionYear, secYear, currentFYear

END MODULE date
!    ********************************************************************************************************
!    ENVIRONMENT VARIABLES MODULE
!    ********************************************************************************************************
MODULE envVars
  USE storage, ONLY : maxEnvVarNameLength, maxEnvVarLength
  IMPLICIT NONE

  SAVE
  CHARACTER (LEN=maxEnvVarNameLength), ALLOCATABLE :: envVarNames(:)
  CHARACTER (LEN=maxEnvVarLength), ALLOCATABLE :: envVarTypes(:)
  INTEGER, ALLOCATABLE :: envVarTypesNum(:)
  DOUBLE PRECISION, ALLOCATABLE :: envVarFixedValues(:), currentEnvVarValues(:)
  INTEGER :: numEnvVars, tempNum


  DOUBLE PRECISION, ALLOCATABLE :: envVarX (:,:), envVarY (:,:), envVarY2 (:,:)
  INTEGER, ALLOCATABLE :: envVarNumberOfPoints(:)
  DOUBLE PRECISION :: ro2

END MODULE envVars

!    ********************************************************************************************************
MODULE constraints
  IMPLICIT NONE
  SAVE
  INTEGER :: numberOfConstrainedSpecies, maxNumberOfDataPoints
  INTEGER :: numberOfFixedConstrainedSpecies, numberOfVariableConstrainedSpecies
  INTEGER, ALLOCATABLE :: constrainedSpecies(:)
  REAL (8), ALLOCATABLE :: constrainedConcs(:)

  PRIVATE :: numberOfConstrainedSpecies, constrainedSpecies, constrainedConcs
  PUBLIC :: getNumberOfConstrainedSpecies, setNumberOfConstrainedSpecies, deallocateConstrainedSpecies

CONTAINS

  ! METHODS FOR numberOfConstrainedSpecies

  SUBROUTINE getNumberOfConstrainedSpecies (n)
    INTEGER :: n
    n = numberOfConstrainedSpecies
  END SUBROUTINE getNumberOfConstrainedSpecies

  SUBROUTINE setNumberOfConstrainedSpecies (n)
    INTEGER :: n
    numberOfConstrainedSpecies = n
    ALLOCATE (constrainedSpecies(n), constrainedConcs(n))
    WRITE (*,*) 'Setting size of constraint arrays, n = ', n
  END SUBROUTINE setNumberOfConstrainedSpecies

  SUBROUTINE deallocateConstrainedSpecies ()
    DEALLOCATE (constrainedSpecies, constrainedConcs)
  END SUBROUTINE deallocateConstrainedSpecies

  ! METHODS FOR constrainedConcs

  SUBROUTINE getConstrainedConc (n, r)
    INTEGER :: n
    REAL (8) :: r
    r = constrainedConcs(n)
  END SUBROUTINE getConstrainedConc

  SUBROUTINE setConstrainedConc (n, r)
    INTEGER :: n
    REAL (8) :: r
    constrainedConcs(n) = r
  END SUBROUTINE setConstrainedConc

  ! METHODS FOR constrainedSpecies

  SUBROUTINE getConstrainedSpecies (n, j)
    INTEGER :: n, j
    j = constrainedSpecies(n)
  END SUBROUTINE getConstrainedSpecies

  SUBROUTINE setConstrainedSpecies (n, j)
    INTEGER :: n, j
    constrainedSpecies(n) = j
  END SUBROUTINE setConstrainedSpecies

END MODULE constraints

MODULE species
  USE storage, ONLY : maxSpecLength

  IMPLICIT NONE
  SAVE
  INTEGER :: neq
  CHARACTER (LEN=maxSpecLength), ALLOCATABLE :: speciesList(:)

  INTEGER :: i

  PRIVATE :: neq, speciesList, i
  PUBLIC :: getNumberOfSpecies, setNumberOfSpecies, deallocateSpeciesList

CONTAINS

  SUBROUTINE getNumberOfSpecies (n)
    INTEGER :: n
    n = neq
  END SUBROUTINE getNumberOfSpecies

  SUBROUTINE setNumberOfSpecies (n)
    INTEGER :: n
    neq = n
    ALLOCATE (speciesList(n))
  END SUBROUTINE setNumberOfSpecies

  SUBROUTINE deallocateSpeciesList
    DEALLOCATE (speciesList)
  END SUBROUTINE deallocateSpeciesList

  SUBROUTINE getSpeciesList (sl)
    CHARACTER (LEN=maxSpecLength) :: sl(*)
    DO i = 1, neq
       sl(i) = speciesList(i)
    ENDDO
  END SUBROUTINE getSpeciesList

  SUBROUTINE setSpeciesList (sl)
    CHARACTER (LEN=maxSpecLength) :: sl(*)
    DO i = 1, neq
       speciesList(i) = sl(i)
    ENDDO
  END SUBROUTINE setSpeciesList

END MODULE species

!    ********************************************************************************************************
!    INTERPOLATION METHOD MODULE
!    ********************************************************************************************************
MODULE interpolationMethod
  IMPLICIT NONE
  SAVE
  INTEGER :: speciesInterpMethod, conditionsInterpMethod, decInterpMethod
  DOUBLE PRECISION, ALLOCATABLE :: testArray(:)

  PUBLIC :: testArray

  PRIVATE :: speciesInterpMethod, conditionsInterpMethod, decInterpMethod
  PUBLIC :: getSpeciesInterpMethod, setSpeciesInterpMethod
  PUBLIC :: getConditionsInterpMethod, setConditionsInterpMethod
  PUBLIC :: getDecInterpMethod, setDecInterpMethod

CONTAINS

  SUBROUTINE getSpeciesInterpMethod (n)
    INTEGER :: n
    n = speciesInterpMethod
  END SUBROUTINE getSpeciesInterpMethod

  SUBROUTINE setSpeciesInterpMethod (n)
    INTEGER :: n
    speciesInterpMethod = n
  END SUBROUTINE setSpeciesInterpMethod

  SUBROUTINE getConditionsInterpMethod (n)
    INTEGER :: n
    n = conditionsInterpMethod
  END SUBROUTINE getConditionsInterpMethod

  SUBROUTINE setConditionsInterpMethod (n)
    INTEGER :: n
    conditionsInterpMethod = n
  END SUBROUTINE setConditionsInterpMethod

  SUBROUTINE getDecInterpMethod (n)
    INTEGER :: n
    n = decInterpMethod
  END SUBROUTINE getDecInterpMethod

  SUBROUTINE setDecInterpMethod (n)
    INTEGER :: n
    decInterpMethod = n
  END SUBROUTINE setDecInterpMethod

END MODULE interpolationMethod

!    ********************************************************************************************************
!    INTERPOLATION METHOD MODULE
!    ********************************************************************************************************
MODULE reactionStructure
  IMPLICIT NONE

  SAVE
  INTEGER, ALLOCATABLE :: clhs(:,:), crhs(:,:)
  INTEGER :: csize1, csize2
  DOUBLE PRECISION, ALLOCATABLE :: ccoeff(:)

END MODULE reactionStructure

!    ********************************************************************************************************
!    PHOTOLYSIS RATES METHOD MODULE
!    ********************************************************************************************************
MODULE photolysisRates
  USE storage, ONLY : maxPhotoRateNameLength
  IMPLICIT NONE

  SAVE
  INTEGER, PARAMETER :: maxNrOfPhotoRates = 200, maxNrOfConPhotoRates = 100
  INTEGER :: ck(maxNrOfPhotoRates), numConPhotoRates, constrainedPhotoRatesNumbers(maxNrOfConPhotoRates)
  INTEGER :: jfacSpeciesLine ! number of line in photolysis rates file corresponding to Jfac species
  INTEGER :: nrOfPhotoRates
  LOGICAL :: usePhotolysisConstants
  DOUBLE PRECISION :: cl(maxNrOfPhotoRates), cmm(maxNrOfPhotoRates), cnn(maxNrOfPhotoRates)
  DOUBLE PRECISION :: j(maxNrOfPhotoRates), transmissionFactor(maxNrOfPhotoRates)
  CHARACTER (LEN=maxPhotoRateNameLength) :: photoRateNames(maxNrOfPhotoRates)
  CHARACTER (LEN=maxPhotoRateNameLength) :: constrainedPhotoRates(maxNrOfConPhotoRates), jFacSpecies
  DOUBLE PRECISION, ALLOCATABLE :: photoX (:,:), photoY (:,:), photoY2 (:,:)
  INTEGER, ALLOCATABLE :: photoNumberOfPoints(:)

END MODULE photolysisRates

!    ********************************************************************************************************
!    CHEMICAL CONSTRAINTS MODULE
!    ********************************************************************************************************
MODULE chemicalConstraints
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  SAVE
  DOUBLE PRECISION, ALLOCATABLE :: dataX (:,:), dataY (:,:), dataY2 (:,:), dataFixedY (:)
  DOUBLE PRECISION, ALLOCATABLE :: constrainedConcs(:)
  INTEGER :: numberOfConstrainedSpecies
  CHARACTER(LEN=maxSpecLength), ALLOCATABLE :: constrainedName(:)
  INTEGER, ALLOCATABLE :: speciesNumberOfPoints(:), constrainedSpecies(:)

END MODULE chemicalConstraints

!    ********************************************************************************************************
!    PHOTOLYSIS RATES PARAMETERS MODULE
!    ********************************************************************************************************
MODULE zenithData
  IMPLICIT NONE

  SAVE
  DOUBLE PRECISION :: lat, longt, lha, sinld, cosld

END MODULE zenithData

MODULE zenithData1
  IMPLICIT NONE

  SAVE
  DOUBLE PRECISION :: cosX, secX

END MODULE zenithData1

!    ********************************************************************************************************
!    RATES OF PRODUCTION AND LOSS MODULE
!    ********************************************************************************************************
MODULE productionAndLossRates
  IMPLICIT NONE

  SAVE
  DOUBLE PRECISION, ALLOCATABLE :: lossRates(:), productionRates(:), ir(:)

END MODULE productionAndLossRates

!    ----------------------------------------------------------------
!     *******************************************************************************************************
!    ********************************************************************************************************
!    SOLAR ZENITH ANGLE CALCULATION VARIABLES MODULE
!    ********************************************************************************************************
MODULE SZACalcVars
  IMPLICIT NONE

  SAVE
  DOUBLE PRECISION :: latitude, longitude

END MODULE SZACalcVars
