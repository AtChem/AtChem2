module types_mod
  use, intrinsic :: iso_fortran_env
  implicit none

  public :: SI, DI, QI, SP, DP, QP

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

module storage
  implicit none
  integer, parameter :: maxSpecLength=10
  integer, parameter :: maxPhotoRateNameLength=6
  integer, parameter :: maxEnvVarNameLength=9
  integer, parameter :: maxEnvVarLength=15
  integer, parameter :: maxFilepathLength=100
  integer, parameter :: maxReactionStringLength=1000
end module

!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
module directories
  use storage, only : maxFilepathLength
  implicit none

  SAVE
  character(len=maxFilepathLength) :: output_dir, instantaneousRates_dir, param_dir, spec_constraints_dir, env_constraints_dir

end module directories
!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
module date
  use types_mod
  implicit none

  SAVE
  integer :: day, month, year, dayOfYear
  real(kind=DP) :: dayAsFractionOfYear, secondsInYear

end module date
!    ********************************************************************************************************
!    ENVIRONMENT VARIABLES MODULE
!    ********************************************************************************************************
module envVars
  use types_mod
  use storage, only : maxEnvVarNameLength, maxEnvVarLength
  implicit none

  SAVE
  character(len=maxEnvVarNameLength), allocatable :: envVarNames(:)
  character(len=maxEnvVarLength), allocatable :: envVarTypes(:)
  integer, allocatable :: envVarTypesNum(:)
  real(kind=DP), allocatable :: envVarFixedValues(:), currentEnvVarValues(:)
  integer(kind=NPI) :: numEnvVars
  integer :: tempNum


  real(kind=DP), allocatable :: envVarX (:,:), envVarY (:,:), envVarY2 (:,:)
  integer(kind=NPI), allocatable :: envVarNumberOfPoints(:)
  real(kind=DP) :: ro2

end module envVars

!    ********************************************************************************************************
module constraints
  use types_mod
  implicit none
  SAVE
  integer(kind=NPI) :: numberOfConstrainedSpecies
  integer :: maxNumberOfDataPoints
  integer(kind=NPI) :: numberOfFixedConstrainedSpecies, numberOfVariableConstrainedSpecies
  integer(kind=NPI), allocatable :: constrainedSpecies(:)
  real(kind=DP), allocatable :: constrainedConcs(:)

  private :: numberOfConstrainedSpecies, constrainedSpecies, constrainedConcs
  public :: getNumberOfConstrainedSpecies, setNumberOfConstrainedSpecies, deallocateConstrainedSpecies

contains

  ! METHODS FOR numberOfConstrainedSpecies

  subroutine getNumberOfConstrainedSpecies( n )
    integer(kind=NPI) :: n
    n = numberOfConstrainedSpecies
  end subroutine getNumberOfConstrainedSpecies

  subroutine setNumberOfConstrainedSpecies( n )
    integer(kind=NPI) :: n
    numberOfConstrainedSpecies = n
    allocate (constrainedSpecies(n), constrainedConcs(n))
    write (*,*) 'Setting size of constraint arrays, n = ', n
  end subroutine setNumberOfConstrainedSpecies

  subroutine deallocateConstrainedSpecies()
    deallocate (constrainedSpecies, constrainedConcs)
  end subroutine deallocateConstrainedSpecies

  ! METHODS FOR constrainedConcs

  subroutine getConstrainedConc( n, r )
    integer(kind=NPI) :: n
    real(kind=DP) :: r
    r = constrainedConcs(n)
  end subroutine getConstrainedConc

  subroutine setConstrainedConc( n, r )
    integer(kind=NPI) :: n
    real(kind=DP) :: r
    constrainedConcs(n) = r
  end subroutine setConstrainedConc

  ! METHODS FOR constrainedSpecies

  subroutine getConstrainedSpecies( n, j )
    integer(kind=NPI) :: n, j
    j = constrainedSpecies(n)
  end subroutine getConstrainedSpecies

  subroutine setConstrainedSpecies( n, j )
    integer(kind=NPI) :: n, j
    constrainedSpecies(n) = j
  end subroutine setConstrainedSpecies

end module constraints

module species
  use types_mod
  use storage, only : maxSpecLength

  implicit none
  SAVE
  integer(kind=NPI) :: numSpecies, numReactions
  character(len=maxSpecLength), allocatable :: speciesList(:)

  integer(kind=NPI) :: i

  private :: numSpecies, speciesList, i
  public :: getNumberOfSpecies, setNumberOfSpecies, getNumberOfReactions, setNumberOfReactions
  public :: deallocateSpeciesList, getSpeciesList, setSpeciesList

contains

  function getNumberOfSpecies() result ( n )
    integer(kind=NPI) :: n
    n = numSpecies
  end function getNumberOfSpecies

  subroutine setNumberOfSpecies( n )
    integer(kind=NPI) :: n
    numSpecies = n
    allocate (speciesList(n))
  end subroutine setNumberOfSpecies

  function getNumberOfReactions() result ( n )
    integer(kind=NPI) :: n
    n = numReactions
  end function getNumberOfReactions

  subroutine setNumberOfReactions( n )
    integer(kind=NPI) :: n
    numReactions = n
  end subroutine setNumberOfReactions

  subroutine deallocateSpeciesList
    deallocate (speciesList)
  end subroutine deallocateSpeciesList

  subroutine getSpeciesList( sl )
    character(len=maxSpecLength), allocatable :: sl(:)
    allocate (sl(numSpecies) )
    do i = 1, numSpecies
      sl(i) = speciesList(i)
    end do
  end subroutine getSpeciesList

  subroutine setSpeciesList( sl )
    character(len=maxSpecLength) :: sl(:)
    do i = 1, numSpecies
      speciesList(i) = sl(i)
    end do
  end subroutine setSpeciesList

end module species

!    ********************************************************************************************************
!    INTERPOLATION METHOD MODULE
!    ********************************************************************************************************
module interpolationMethod
  use types_mod
  implicit none
  SAVE
  integer(kind=SI), private :: speciesInterpMethod, conditionsInterpMethod, decInterpMethod
  public :: getSpeciesInterpMethod, setSpeciesInterpMethod
  public :: getConditionsInterpMethod, setConditionsInterpMethod
  public :: getDecInterpMethod, setDecInterpMethod

contains

  subroutine getSpeciesInterpMethod( n )
    integer(kind=SI) :: n
    n = speciesInterpMethod
  end subroutine getSpeciesInterpMethod

  subroutine setSpeciesInterpMethod( n )
    integer(kind=SI) :: n
    speciesInterpMethod = n
  end subroutine setSpeciesInterpMethod

  subroutine getConditionsInterpMethod( n )
    integer(kind=SI) :: n
    n = conditionsInterpMethod
  end subroutine getConditionsInterpMethod

  subroutine setConditionsInterpMethod( n )
    integer(kind=SI) :: n
    conditionsInterpMethod = n
  end subroutine setConditionsInterpMethod

  subroutine getDecInterpMethod( n )
    integer(kind=SI) :: n
    n = decInterpMethod
  end subroutine getDecInterpMethod

  subroutine setDecInterpMethod( n )
    integer(kind=SI) :: n
    decInterpMethod = n
  end subroutine setDecInterpMethod

end module interpolationMethod

!    ********************************************************************************************************
!    INTERPOLATION METHOD MODULE
!    ********************************************************************************************************
module reactionStructure
  use types_mod
  implicit none

  SAVE
  integer(kind=NPI), allocatable :: clhs(:,:), crhs(:,:)
  integer(kind=NPI) :: lhs_size, rhs_size
  real(kind=DP), allocatable :: ccoeff(:)

end module reactionStructure

!    ********************************************************************************************************
!    PHOTOLYSIS RATES METHOD MODULE
!    ********************************************************************************************************
module photolysisRates
  use types_mod
  use storage, only : maxPhotoRateNameLength
  implicit none

  SAVE
  integer, parameter :: maxNrOfPhotoRates = 200, maxNrOfConPhotoRates = 100
  integer(kind=NPI) :: ck(maxNrOfPhotoRates), numConPhotoRates, constrainedPhotoRatesNumbers(maxNrOfConPhotoRates)
  integer(kind=NPI) :: jfacSpeciesLine ! number of line in photolysis rates file corresponding to Jfac species
  integer(kind=NPI) :: nrOfPhotoRates
  logical :: usePhotolysisConstants
  real(kind=DP) :: cl(maxNrOfPhotoRates), cmm(maxNrOfPhotoRates), cnn(maxNrOfPhotoRates)
  real(kind=DP) :: j(maxNrOfPhotoRates), transmissionFactor(maxNrOfPhotoRates)
  character(len=maxPhotoRateNameLength) :: photoRateNames(maxNrOfPhotoRates)
  character(len=maxPhotoRateNameLength) :: constrainedPhotoRates(maxNrOfConPhotoRates), jFacSpecies
  real(kind=DP), allocatable :: photoX (:,:), photoY (:,:), photoY2 (:,:)
  integer(kind=NPI), allocatable :: photoNumberOfPoints(:)

end module photolysisRates

!    ********************************************************************************************************
!    CHEMICAL CONSTRAINTS MODULE
!    ********************************************************************************************************
module chemicalConstraints
  use types_mod
  use storage, only : maxSpecLength
  implicit none

  SAVE
  real(kind=DP), allocatable :: dataX (:,:), dataY (:,:), dataY2 (:,:), dataFixedY (:)
  real(kind=DP), allocatable :: constrainedConcs(:)
  integer(kind=NPI) :: numberOfConstrainedSpecies
  character(len=maxSpecLength), allocatable :: constrainedName(:)
  integer(kind=NPI), allocatable :: speciesNumberOfPoints(:), constrainedSpecies(:)

end module chemicalConstraints

!    ********************************************************************************************************
!    PHOTOLYSIS RATES PARAMETERS MODULE
!    ********************************************************************************************************
module zenithData
  use types_mod
  implicit none

  SAVE
  real(kind=DP) :: lat, longt, lha, sinld, cosld

end module zenithData

module zenithData1
  use types_mod
  implicit none

  SAVE
  real(kind=DP) :: cosX, secX

end module zenithData1

!    ********************************************************************************************************
!    RATES OF PRODUCTION AND LOSS MODULE
!    ********************************************************************************************************
module productionAndLossRates
  use types_mod
  implicit none

  SAVE
  real(kind=DP), allocatable :: lossRates(:), productionRates(:), ir(:)

end module productionAndLossRates

!    ----------------------------------------------------------------
!     *******************************************************************************************************
!    ********************************************************************************************************
!    SOLAR ZENITH ANGLE CALCULATION VARIABLES MODULE
!    ********************************************************************************************************
module SZACalcVars
  use types_mod
  implicit none

  SAVE
  real(kind=DP) :: latitude, longitude

end module SZACalcVars
