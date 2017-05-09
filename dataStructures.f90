module types_mod
   use, intrinsic :: iso_fortran_env
   implicit none

   public ::  SI, DI, QI, SP, DP, QP

   integer, parameter :: SI = INT8
   integer, parameter :: DI = INT16
   integer, parameter :: QI = INT32
   integer, parameter :: LONG = INT64
   integer, parameter :: NPI = INT64 ! Must be INT32 or INT64, as that's what the CVODE functions take
   integer, parameter :: IntErr = INT32
   integer, parameter :: SP = selected_real_kind( p = 6, r = 37 )
   integer, parameter :: DP = selected_real_kind( p = 15, r = 307 )
   integer, parameter :: QP = selected_real_kind( p = 33, r = 4931 )
contains
end module types_mod

MODULE storage
  IMPLICIT NONE
  INTEGER, PARAMETER :: maxSpecLength=10
  INTEGER, PARAMETER :: maxPhotoRateNameLength=6
  INTEGER, PARAMETER :: maxEnvVarNameLength=9
  INTEGER, PARAMETER :: maxEnvVarLength=15
  INTEGER, PARAMETER :: maxFilepathLength=100
  INTEGER, PARAMETER :: maxReactionStringLength=1000
END MODULE

!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
MODULE directories
  USE storage, ONLY : maxFilepathLength
  IMPLICIT NONE

  SAVE
  CHARACTER(LEN=maxFilepathLength) :: output_dir, instantaneousRates_dir, param_dir, spec_constraints_dir, env_constraints_dir

END MODULE directories
!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
MODULE date
  USE types_mod
  IMPLICIT NONE

  SAVE
  INTEGER :: day, month, year, dayOfYear
  real(kind=DP) :: dayAsFractionOfYear, secondsInYear

END MODULE date
!    ********************************************************************************************************
!    ENVIRONMENT VARIABLES MODULE
!    ********************************************************************************************************
MODULE envVars
  USE types_mod
  USE storage, ONLY : maxEnvVarNameLength, maxEnvVarLength
  IMPLICIT NONE

  SAVE
  CHARACTER(LEN=maxEnvVarNameLength), ALLOCATABLE :: envVarNames(:)
  CHARACTER(LEN=maxEnvVarLength), ALLOCATABLE :: envVarTypes(:)
  INTEGER, ALLOCATABLE :: envVarTypesNum(:)
  real(kind=DP), ALLOCATABLE :: envVarFixedValues(:), currentEnvVarValues(:)
  INTEGER(kind=NPI) :: numEnvVars
  INTEGER :: tempNum


  real(kind=DP), ALLOCATABLE :: envVarX (:,:), envVarY (:,:), envVarY2 (:,:)
  INTEGER(kind=NPI), ALLOCATABLE :: envVarNumberOfPoints(:)
  real(kind=DP) :: ro2

END MODULE envVars

!    ********************************************************************************************************
MODULE constraints
  USE types_mod
  IMPLICIT NONE
  SAVE
  INTEGER(kind=NPI) :: numberOfConstrainedSpecies
  INTEGER :: maxNumberOfDataPoints
  INTEGER(kind=NPI) :: numberOfFixedConstrainedSpecies, numberOfVariableConstrainedSpecies
  INTEGER(kind=NPI), ALLOCATABLE :: constrainedSpecies(:)
  real(kind=DP), ALLOCATABLE :: constrainedConcs(:)

  PRIVATE :: numberOfConstrainedSpecies, constrainedSpecies, constrainedConcs
  PUBLIC :: getNumberOfConstrainedSpecies, setNumberOfConstrainedSpecies, deallocateConstrainedSpecies

CONTAINS

  ! METHODS FOR numberOfConstrainedSpecies

  SUBROUTINE getNumberOfConstrainedSpecies (n)
    INTEGER(kind=NPI) :: n
    n = numberOfConstrainedSpecies
  END SUBROUTINE getNumberOfConstrainedSpecies

  SUBROUTINE setNumberOfConstrainedSpecies (n)
    INTEGER(kind=NPI) :: n
    numberOfConstrainedSpecies = n
    ALLOCATE (constrainedSpecies(n), constrainedConcs(n))
    WRITE (*,*) 'Setting size of constraint arrays, n = ', n
  END SUBROUTINE setNumberOfConstrainedSpecies

  SUBROUTINE deallocateConstrainedSpecies ()
    DEALLOCATE (constrainedSpecies, constrainedConcs)
  END SUBROUTINE deallocateConstrainedSpecies

  ! METHODS FOR constrainedConcs

  SUBROUTINE getConstrainedConc (n, r)
    INTEGER(kind=NPI) :: n
    real(kind=DP) :: r
    r = constrainedConcs(n)
  END SUBROUTINE getConstrainedConc

  SUBROUTINE setConstrainedConc (n, r)
    INTEGER(kind=NPI) :: n
    real(kind=DP) :: r
    constrainedConcs(n) = r
  END SUBROUTINE setConstrainedConc

  ! METHODS FOR constrainedSpecies

  SUBROUTINE getConstrainedSpecies (n, j)
    INTEGER(kind=NPI) :: n, j
    j = constrainedSpecies(n)
  END SUBROUTINE getConstrainedSpecies

  SUBROUTINE setConstrainedSpecies (n, j)
    INTEGER(kind=NPI) :: n, j
    constrainedSpecies(n) = j
  END SUBROUTINE setConstrainedSpecies

END MODULE constraints

MODULE species
  USE types_mod
  USE storage, ONLY : maxSpecLength

  IMPLICIT NONE
  SAVE
  INTEGER(kind=NPI) :: numSpecies, numReactions
  CHARACTER(LEN=maxSpecLength), ALLOCATABLE :: speciesList(:)

  INTEGER(kind=NPI) :: i

  PRIVATE :: numSpecies, speciesList, i
  PUBLIC :: getNumberOfSpecies, setNumberOfSpecies, getNumberOfReactions, setNumberOfReactions
  PUBLIC :: deallocateSpeciesList, getSpeciesList, setSpeciesList

CONTAINS

  FUNCTION getNumberOfSpecies( ) result (n)
    INTEGER(kind=NPI) :: n
    n = numSpecies
  END FUNCTION getNumberOfSpecies

  SUBROUTINE setNumberOfSpecies (n)
    INTEGER(kind=NPI) :: n
    numSpecies = n
    ALLOCATE (speciesList(n))
  END SUBROUTINE setNumberOfSpecies

  FUNCTION getNumberOfReactions( ) result (n)
    INTEGER(kind=NPI) :: n
    n = numReactions
  END FUNCTION getNumberOfReactions

  SUBROUTINE setNumberOfReactions (n)
    INTEGER(kind=NPI) :: n
    numReactions = n
  END SUBROUTINE setNumberOfReactions

  SUBROUTINE deallocateSpeciesList
    DEALLOCATE (speciesList)
  END SUBROUTINE deallocateSpeciesList

  SUBROUTINE getSpeciesList (sl)
    CHARACTER(LEN=maxSpecLength), ALLOCATABLE :: sl(:)
    ALLOCATE( sl(numSpecies) )
    DO i = 1, numSpecies
       sl(i) = speciesList(i)
    ENDDO
  END SUBROUTINE getSpeciesList

  SUBROUTINE setSpeciesList (sl)
    CHARACTER(LEN=maxSpecLength) :: sl(:)
    DO i = 1, numSpecies
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
  INTEGER(kind=NPI), ALLOCATABLE :: clhs(:,:), crhs(:,:)
  INTEGER(kind=NPI) :: lhs_size, rhs_size
  real(kind=DP), ALLOCATABLE :: ccoeff(:)

END MODULE reactionStructure

!    ********************************************************************************************************
!    PHOTOLYSIS RATES METHOD MODULE
!    ********************************************************************************************************
MODULE photolysisRates
  USE types_mod
  USE storage, ONLY : maxPhotoRateNameLength
  IMPLICIT NONE

  SAVE
  INTEGER, PARAMETER :: maxNrOfPhotoRates = 200, maxNrOfConPhotoRates = 100
  INTEGER(kind=NPI) :: ck(maxNrOfPhotoRates), numConPhotoRates, constrainedPhotoRatesNumbers(maxNrOfConPhotoRates)
  INTEGER(kind=NPI) :: jfacSpeciesLine ! number of line in photolysis rates file corresponding to Jfac species
  INTEGER(kind=NPI) :: nrOfPhotoRates
  LOGICAL :: usePhotolysisConstants
  real(kind=DP) :: cl(maxNrOfPhotoRates), cmm(maxNrOfPhotoRates), cnn(maxNrOfPhotoRates)
  real(kind=DP) :: j(maxNrOfPhotoRates), transmissionFactor(maxNrOfPhotoRates)
  CHARACTER(LEN=maxPhotoRateNameLength) :: photoRateNames(maxNrOfPhotoRates)
  CHARACTER(LEN=maxPhotoRateNameLength) :: constrainedPhotoRates(maxNrOfConPhotoRates), jFacSpecies
  real(kind=DP), ALLOCATABLE :: photoX (:,:), photoY (:,:), photoY2 (:,:)
  INTEGER(kind=NPI), ALLOCATABLE :: photoNumberOfPoints(:)

END MODULE photolysisRates

!    ********************************************************************************************************
!    CHEMICAL CONSTRAINTS MODULE
!    ********************************************************************************************************
MODULE chemicalConstraints
  USE types_mod
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  SAVE
  real(kind=DP), ALLOCATABLE :: dataX (:,:), dataY (:,:), dataY2 (:,:), dataFixedY (:)
  real(kind=DP), ALLOCATABLE :: constrainedConcs(:)
  INTEGER(kind=NPI) :: numberOfConstrainedSpecies
  CHARACTER(LEN=maxSpecLength), ALLOCATABLE :: constrainedName(:)
  INTEGER(kind=NPI), ALLOCATABLE :: speciesNumberOfPoints(:), constrainedSpecies(:)

END MODULE chemicalConstraints

!    ********************************************************************************************************
!    PHOTOLYSIS RATES PARAMETERS MODULE
!    ********************************************************************************************************
MODULE zenithData
  USE types_mod
  IMPLICIT NONE

  SAVE
  real(kind=DP) :: lat, longt, lha, sinld, cosld

END MODULE zenithData

MODULE zenithData1
  USE types_mod
  IMPLICIT NONE

  SAVE
  real(kind=DP) :: cosX, secX

END MODULE zenithData1

!    ********************************************************************************************************
!    RATES OF PRODUCTION AND LOSS MODULE
!    ********************************************************************************************************
MODULE productionAndLossRates
  USE types_mod
  IMPLICIT NONE

  SAVE
  real(kind=DP), ALLOCATABLE :: lossRates(:), productionRates(:), ir(:)

END MODULE productionAndLossRates

!    ----------------------------------------------------------------
!     *******************************************************************************************************
!    ********************************************************************************************************
!    SOLAR ZENITH ANGLE CALCULATION VARIABLES MODULE
!    ********************************************************************************************************
MODULE SZACalcVars
  USE types_mod
  IMPLICIT NONE

  SAVE
  real(kind=DP) :: latitude, longitude

END MODULE SZACalcVars
