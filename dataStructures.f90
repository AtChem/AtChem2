module types_mod
  use, intrinsic :: iso_fortran_env
  implicit none
  save

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
end module types_mod

module storage
  implicit none
  save

  integer, parameter :: maxSpecLength=10
  integer, parameter :: maxPhotoRateNameLength=6
  integer, parameter :: maxEnvVarNameLength=9
  integer, parameter :: maxEnvVarLength=15
  integer, parameter :: maxFilepathLength=100
  integer, parameter :: maxReactionStringLength=1000

end module storage

!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
module directories
  use storage, only : maxFilepathLength
  implicit none
  save

  character(len=maxFilepathLength) :: output_dir, instantaneousRates_dir, param_dir, spec_constraints_dir, env_constraints_dir

end module directories

!    ********************************************************************************************************
!    DATE VARIABLES MODULE - DATE USED FOR CALCULATION OF DEC
!    ********************************************************************************************************
module date
  use types_mod
  implicit none
  save

  integer(kind=SI) :: day, month
  integer(kind=DI) :: year, dayOfYear
  real(kind=DP) :: dayAsFractionOfYear, secondsInYear

contains
  subroutine calcDateParameters()
    implicit none

    integer(kind=SI) :: monthList(12)

    monthList = [31_SI, 28_SI, 31_SI, 30_SI, 31_SI, 30_SI, 31_SI, 31_SI, 30_SI, 31_SI, 30_SI, 31_SI]
    ! calculate which day of the year day/month refers to
    dayOfYear = sum( monthList(1:month - 1_SI) )
    dayOfYear = dayOfYear + day - 1_SI
    ! This day refers to the following fraction through the year
    dayAsFractionOfYear = dayOfYear / 365_DI
    ! Set number of seconds per year
    secondsInYear = 3.6525d+02 * 2.40d+01 * 3.60d+03
    return
  end subroutine calcDateParameters
end module date


module envVars
  use types_mod
  use storage, only : maxEnvVarNameLength, maxEnvVarLength
  implicit none
  save

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


module constraints
  use types_mod
  implicit none
  save

  integer(kind=NPI) :: numberOfConstrainedSpecies
  integer(kind=NPI) :: numberOfFixedConstrainedSpecies, numberOfVariableConstrainedSpecies
  real(kind=DP), allocatable :: constrainedConcs(:)
  real(kind=DP), allocatable :: dataX(:,:), dataY(:,:), dataY2(:,:), dataFixedY(:)
  integer(kind=NPI), allocatable :: constrainedSpecies(:)
  integer(kind=NPI) :: maxNumberOfDataPoints
  integer(kind=NPI), allocatable :: speciesNumberOfPoints(:)

  private :: numberOfConstrainedSpecies, constrainedConcs, constrainedSpecies
  public :: getNumberOfConstrainedSpecies, setNumberOfConstrainedSpecies
  public :: getConstrainedConcs, setConstrainedConcs, deallocateConstrainedConcs
  public :: getConstrainedSpecies, setConstrainedSpecies, deallocateConstrainedSpecies, getOneConstrainedSpecies
contains

  ! Methods for numberOfConstrainedSpecies

  pure function getNumberOfConstrainedSpecies() result ( n )
    implicit none
    integer(kind=NPI) :: n
    n = numberOfConstrainedSpecies
  end function getNumberOfConstrainedSpecies

  subroutine setNumberOfConstrainedSpecies( n )
    implicit none
    integer(kind=NPI) :: n
    numberOfConstrainedSpecies = n
    allocate (constrainedConcs(n), constrainedSpecies(n))
    write (*, '(A, I0)') ' Setting size of constraint arrays, n = ', n
  end subroutine setNumberOfConstrainedSpecies

  ! Methods for constrainedConcs

  pure function getConstrainedConcs() result ( r )
    implicit none
    real(kind=DP) :: r(numberOfConstrainedSpecies)
    r = constrainedConcs(:)
  end function getConstrainedConcs

  subroutine setConstrainedConcs( r )
    implicit none
    real(kind=DP) :: r(:)
    constrainedConcs(:) = r(:)
  end subroutine setConstrainedConcs

  subroutine deallocateConstrainedConcs()
    implicit none
    deallocate (constrainedConcs)
  end subroutine deallocateConstrainedConcs

  ! Methods for constrainedSpecies

  pure function getConstrainedSpecies() result ( r )
    implicit none
    integer(kind=NPI) :: r(numberOfConstrainedSpecies)
    r = constrainedSpecies(:)
  end function getConstrainedSpecies

  pure function getOneConstrainedSpecies( i ) result ( r )
    implicit none
    integer(kind=NPI), intent(in) :: i
    integer(kind=NPI) :: r
    r = constrainedSpecies(i)
  end function getOneConstrainedSpecies

  subroutine setConstrainedSpecies( n, r )
    implicit none
    integer(kind=NPI) :: n, r
    constrainedSpecies(n) = r
  end subroutine setConstrainedSpecies

  subroutine deallocateConstrainedSpecies()
    implicit none
    deallocate (constrainedSpecies)
  end subroutine deallocateConstrainedSpecies
end module constraints


module species
  use types_mod
  use storage, only : maxSpecLength
  implicit none
  save

  integer(kind=NPI) :: numSpecies, numReactions, i
  character(len=maxSpecLength), allocatable :: speciesList(:)

  private :: numSpecies, numReactions, speciesList, i
  public :: getNumberOfSpecies, setNumberOfSpecies, getNumberOfReactions, setNumberOfReactions
  public :: deallocateSpeciesList, getSpeciesList, setSpeciesList

contains

  pure function getNumberOfSpecies() result ( n )
    implicit none
    integer(kind=NPI) :: n
    n = numSpecies
  end function getNumberOfSpecies

  subroutine setNumberOfSpecies( n )
    implicit none
    integer(kind=NPI) :: n
    numSpecies = n
    allocate (speciesList(n))
  end subroutine setNumberOfSpecies

  pure function getNumberOfReactions() result ( n )
    implicit none
    integer(kind=NPI) :: n
    n = numReactions
  end function getNumberOfReactions

  subroutine setNumberOfReactions( n )
    implicit none
    integer(kind=NPI) :: n
    numReactions = n
  end subroutine setNumberOfReactions

  subroutine deallocateSpeciesList
    implicit none
    deallocate (speciesList)
  end subroutine deallocateSpeciesList

  function getSpeciesList() result ( sl )
    implicit none
    character(len=maxSpecLength), allocatable :: sl(:)
    allocate (sl(numSpecies) )
    do i = 1, numSpecies
      sl(i) = speciesList(i)
    end do
  end function getSpeciesList

  subroutine setSpeciesList( sl )
    implicit none
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
  save

  integer(kind=SI), private :: speciesInterpMethod, conditionsInterpMethod, decInterpMethod
  public :: getSpeciesInterpMethod, getConditionsInterpMethod, getDecInterpMethod
  public :: setSpeciesInterpMethod, setConditionsInterpMethod, setDecInterpMethod

contains

  pure function getSpeciesInterpMethod() result ( n )
    implicit none
    integer(kind=SI) :: n
    n = speciesInterpMethod
  end function getSpeciesInterpMethod

  subroutine setSpeciesInterpMethod( n )
    implicit none
    integer(kind=SI) :: n
    speciesInterpMethod = n
  end subroutine setSpeciesInterpMethod

  pure function getConditionsInterpMethod() result ( n )
    implicit none
    integer(kind=SI) :: n
    n = conditionsInterpMethod
  end function getConditionsInterpMethod

  subroutine setConditionsInterpMethod( n )
    implicit none
    integer(kind=SI) :: n
    conditionsInterpMethod = n
  end subroutine setConditionsInterpMethod

  pure function getDecInterpMethod() result ( n )
    implicit none
    integer(kind=SI) :: n
    n = decInterpMethod
  end function getDecInterpMethod

  subroutine setDecInterpMethod( n )
    implicit none
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
  save

  integer(kind=NPI), allocatable :: clhs(:,:), crhs(:,:)
  real(kind=DP), allocatable :: clcoeff(:), crcoeff(:)

end module reactionStructure

!    ********************************************************************************************************
!    PHOTOLYSIS RATES METHOD MODULE
!    ********************************************************************************************************
module photolysisRates_mod
  use types_mod
  use storage, only : maxPhotoRateNameLength
  implicit none
  save

  integer, parameter :: maxNrOfConPhotoRates = 100
  integer(kind=NPI) :: numConPhotoRates, constrainedPhotoRatesNumbers(maxNrOfConPhotoRates)
  integer(kind=NPI) :: jfacSpeciesLine ! number of line in photolysis rates file corresponding to Jfac species
  integer(kind=NPI) :: nrOfPhotoRates
  integer(kind=NPI), allocatable :: ck(:)
  logical :: usePhotolysisConstants
  real(kind=DP), allocatable :: cl(:), cmm(:), cnn(:), transmissionFactor(:)
  real(kind=DP), allocatable :: j(:)
  character(len=maxPhotoRateNameLength), allocatable :: photoRateNames(:)
  character(len=maxPhotoRateNameLength) :: constrainedPhotoRates(maxNrOfConPhotoRates), jFacSpecies
  real(kind=DP), allocatable :: photoX(:,:), photoY(:,:), photoY2(:,:)
  integer(kind=NPI), allocatable :: photoNumberOfPoints(:)
  integer(kind=NPI) :: size_of_j

contains
  subroutine allocate_photolysis_constants_variables()
    implicit none

    allocate (ck(nrOfPhotoRates), cl(nrOfPhotoRates), photoRateNames(nrOfPhotoRates))
  end subroutine allocate_photolysis_constants_variables

  subroutine allocate_photolysis_rates_variables()
    implicit none

    allocate (ck(nrOfPhotoRates), cl(nrOfPhotoRates), cmm(nrOfPhotoRates))
    allocate (cnn(nrOfPhotoRates), photoRateNames(nrOfPhotoRates), transmissionFactor(nrOfPhotoRates))
  end subroutine allocate_photolysis_rates_variables

  subroutine allocate_photolysis_j()
    implicit none

    allocate (j(size_of_j))
  end subroutine allocate_photolysis_j
end module photolysisRates_mod

!    ********************************************************************************************************
!    PHOTOLYSIS RATES PARAMETERS MODULE
!    ********************************************************************************************************
module zenithData
  use types_mod
  implicit none
  save

  real(kind=DP) :: lat, longt, lha, sinld, cosld, cosX, secX

end module zenithData

!    ********************************************************************************************************
!    RATES OF PRODUCTION AND LOSS MODULE
!    ********************************************************************************************************
module productionAndLossRates
  use types_mod
  implicit none
  save

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
  save

  real(kind=DP) :: latitude, longitude

end module SZACalcVars
