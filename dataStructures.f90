module types_mod
   use, intrinsic :: iso_fortran_env
   implicit none

   public ::  SI, DI, QI, SP, DP, QP

   integer, parameter :: SI = INT8
   integer, parameter :: DI = INT16
   integer, parameter :: QI = INT32
   integer, parameter :: LONG = INT64
   real, parameter :: SP = selected_real_kind( p = 6, r = 37 )
   real, parameter :: DP = selected_real_kind( p = 15, r = 307 )
   real, parameter :: QP = selected_real_kind( p = 33, r = 4931 )
contains
end module types_mod

MODULE storage
  USE types_mod
  IMPLICIT NONE
  INTEGER(kind=SI), PARAMETER :: maxSpecLength=10
  INTEGER(kind=SI), PARAMETER :: maxPhotoRateNameLength=6
  INTEGER(kind=SI), PARAMETER :: maxEnvVarNameLength=9
  INTEGER(kind=SI), PARAMETER :: maxEnvVarLength=15
  INTEGER(kind=SI), PARAMETER :: maxFilepathLength=100
  INTEGER(kind=DI), PARAMETER :: maxReactionStringLength=1000
END MODULE

!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
MODULE directories
  USE storage, ONLY : maxFilepathLength
  IMPLICIT NONE

  SAVE
  CHARACTER (LEN=maxFilepathLength) :: output_dir, instantaneousRates_dir, param_dir, spec_constraints_dir, env_constraints_dir

END MODULE directories
!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
MODULE date
  USE types_mod
  IMPLICIT NONE

  SAVE
  INTEGER(kind=DI) :: day, month, year, dayOfYear
  DOUBLE PRECISION :: dayAsFractionOfYear, secondsInYear

END MODULE date
!    ********************************************************************************************************
!    ENVIRONMENT VARIABLES MODULE
!    ********************************************************************************************************
MODULE envVars
  USE types_mod
  USE storage, ONLY : maxEnvVarNameLength, maxEnvVarLength
  IMPLICIT NONE

  SAVE
  CHARACTER (LEN=maxEnvVarNameLength), ALLOCATABLE :: envVarNames(:)
  CHARACTER (LEN=maxEnvVarLength), ALLOCATABLE :: envVarTypes(:)
  INTEGER(kind=SI), ALLOCATABLE :: envVarTypesNum(:)
  DOUBLE PRECISION, ALLOCATABLE :: envVarFixedValues(:), currentEnvVarValues(:)
  INTEGER(kind=SI) :: numEnvVars


  DOUBLE PRECISION, ALLOCATABLE :: envVarX (:,:), envVarY (:,:), envVarY2 (:,:)
  INTEGER(kind=DI), ALLOCATABLE :: envVarNumberOfPoints(:)
  DOUBLE PRECISION :: ro2

END MODULE envVars

!    ********************************************************************************************************
MODULE constraints
  USE types_mod
  IMPLICIT NONE
  SAVE
  INTEGER(kind=DI) :: numberOfConstrainedSpecies
  INTEGER(kind=DI) :: maxNumberOfDataPoints
  INTEGER(kind=DI) :: numberOfFixedConstrainedSpecies, numberOfVariableConstrainedSpecies
  INTEGER(kind=DI), ALLOCATABLE :: constrainedSpecies(:)
  REAL (8), ALLOCATABLE :: constrainedConcs(:)

  PRIVATE :: numberOfConstrainedSpecies, constrainedSpecies, constrainedConcs
  PUBLIC :: getNumberOfConstrainedSpecies, setNumberOfConstrainedSpecies, deallocateConstrainedSpecies

CONTAINS

  ! METHODS FOR numberOfConstrainedSpecies

  SUBROUTINE getNumberOfConstrainedSpecies (n)
    INTEGER(kind=DI) :: n
    n = numberOfConstrainedSpecies
  END SUBROUTINE getNumberOfConstrainedSpecies

  SUBROUTINE setNumberOfConstrainedSpecies (n)
    INTEGER(kind=DI) :: n
    numberOfConstrainedSpecies = n
    ALLOCATE (constrainedSpecies(n), constrainedConcs(n))
    WRITE (*,*) 'Setting size of constraint arrays, n = ', n
  END SUBROUTINE setNumberOfConstrainedSpecies

  SUBROUTINE deallocateConstrainedSpecies ()
    DEALLOCATE (constrainedSpecies, constrainedConcs)
  END SUBROUTINE deallocateConstrainedSpecies

  ! METHODS FOR constrainedConcs

  SUBROUTINE getConstrainedConc (n, r)
    INTEGER(kind=DI) :: n
    REAL (8) :: r
    r = constrainedConcs(n)
  END SUBROUTINE getConstrainedConc

  SUBROUTINE setConstrainedConc (n, r)
    INTEGER(kind=DI) :: n
    REAL (8) :: r
    constrainedConcs(n) = r
  END SUBROUTINE setConstrainedConc

  ! METHODS FOR constrainedSpecies

  SUBROUTINE getConstrainedSpecies (n, j)
    INTEGER(kind=DI) :: n, j
    j = constrainedSpecies(n)
  END SUBROUTINE getConstrainedSpecies

  SUBROUTINE setConstrainedSpecies (n, j)
    INTEGER(kind=DI) :: n, j
    constrainedSpecies(n) = j
  END SUBROUTINE setConstrainedSpecies

END MODULE constraints

MODULE species
  USE types_mod
  USE storage, ONLY : maxSpecLength

  IMPLICIT NONE
  SAVE
  INTEGER(kind=DI) :: neq
  CHARACTER (LEN=maxSpecLength), ALLOCATABLE :: speciesList(:)

  INTEGER(kind=DI) :: i

  PRIVATE :: neq, speciesList, i
  PUBLIC :: getNumberOfSpecies, setNumberOfSpecies, deallocateSpeciesList

CONTAINS

  SUBROUTINE getNumberOfSpecies (n)
    INTEGER(kind=DI) :: n
    n = neq
  END SUBROUTINE getNumberOfSpecies

  SUBROUTINE setNumberOfSpecies (n)
    INTEGER(kind=DI) :: n
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
  USE types_mod
  IMPLICIT NONE
  SAVE
  INTEGER(kind=SI), PRIVATE :: speciesInterpMethod, conditionsInterpMethod, decInterpMethod
  PUBLIC :: getSpeciesInterpMethod, setSpeciesInterpMethod
  PUBLIC :: getConditionsInterpMethod, setConditionsInterpMethod
  PUBLIC :: getDecInterpMethod, setDecInterpMethod

CONTAINS

  SUBROUTINE getSpeciesInterpMethod (n)
    INTEGER(kind=SI) :: n
    n = speciesInterpMethod
  END SUBROUTINE getSpeciesInterpMethod

  SUBROUTINE setSpeciesInterpMethod (n)
    INTEGER(kind=SI) :: n
    speciesInterpMethod = n
  END SUBROUTINE setSpeciesInterpMethod

  SUBROUTINE getConditionsInterpMethod (n)
    INTEGER(kind=SI) :: n
    n = conditionsInterpMethod
  END SUBROUTINE getConditionsInterpMethod

  SUBROUTINE setConditionsInterpMethod (n)
    INTEGER(kind=SI) :: n
    conditionsInterpMethod = n
  END SUBROUTINE setConditionsInterpMethod

  SUBROUTINE getDecInterpMethod (n)
    INTEGER(kind=SI) :: n
    n = decInterpMethod
  END SUBROUTINE getDecInterpMethod

  SUBROUTINE setDecInterpMethod (n)
    INTEGER(kind=SI) :: n
    decInterpMethod = n
  END SUBROUTINE setDecInterpMethod

END MODULE interpolationMethod

!    ********************************************************************************************************
!    INTERPOLATION METHOD MODULE
!    ********************************************************************************************************
MODULE reactionStructure
  USE types_mod
  IMPLICIT NONE

  SAVE
  INTEGER(kind=DI), ALLOCATABLE :: clhs(:,:), crhs(:,:)
  INTEGER(kind=DI) :: csize1, csize2
  DOUBLE PRECISION, ALLOCATABLE :: ccoeff(:)

END MODULE reactionStructure

!    ********************************************************************************************************
!    PHOTOLYSIS RATES METHOD MODULE
!    ********************************************************************************************************
MODULE photolysisRates
  USE types_mod
  USE storage, ONLY : maxPhotoRateNameLength
  IMPLICIT NONE

  SAVE
  INTEGER(kind=DI), PARAMETER :: maxNrOfPhotoRates = 200, maxNrOfConPhotoRates = 100
  INTEGER(kind=DI) :: ck(maxNrOfPhotoRates), numConPhotoRates, constrainedPhotoRatesNumbers(maxNrOfConPhotoRates)
  INTEGER(kind=DI) :: jfacSpeciesLine ! number of line in photolysis rates file corresponding to Jfac species
  INTEGER(kind=DI) :: nrOfPhotoRates
  LOGICAL :: usePhotolysisConstants
  DOUBLE PRECISION :: cl(maxNrOfPhotoRates), cmm(maxNrOfPhotoRates), cnn(maxNrOfPhotoRates)
  DOUBLE PRECISION :: j(maxNrOfPhotoRates), transmissionFactor(maxNrOfPhotoRates)
  CHARACTER (LEN=maxPhotoRateNameLength) :: photoRateNames(maxNrOfPhotoRates)
  CHARACTER (LEN=maxPhotoRateNameLength) :: constrainedPhotoRates(maxNrOfConPhotoRates), jFacSpecies
  DOUBLE PRECISION, ALLOCATABLE :: photoX (:,:), photoY (:,:), photoY2 (:,:)
  INTEGER(kind=DI), ALLOCATABLE :: photoNumberOfPoints(:)

END MODULE photolysisRates

!    ********************************************************************************************************
!    CHEMICAL CONSTRAINTS MODULE
!    ********************************************************************************************************
MODULE chemicalConstraints
  USE types_mod
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  SAVE
  DOUBLE PRECISION, ALLOCATABLE :: dataX (:,:), dataY (:,:), dataY2 (:,:), dataFixedY (:)
  DOUBLE PRECISION, ALLOCATABLE :: constrainedConcs(:)
  INTEGER(kind=DI) :: numberOfConstrainedSpecies
  CHARACTER(LEN=maxSpecLength), ALLOCATABLE :: constrainedName(:)
  INTEGER(kind=DI), ALLOCATABLE :: speciesNumberOfPoints(:), constrainedSpecies(:)

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
