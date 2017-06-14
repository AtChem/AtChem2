! ******************************************************************** !
! ATCHEM -- MODULE dataStructures file
!
! ??? Text describing the content of the module ???
! ******************************************************************** !


! ******************************************************************** !
! MODULE types_mod
! ???
! ******************************************************************** !
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

! ******************************************************************** !
! MODULE storage
! ???
! ******************************************************************** !
module storage
  use types_mod
  implicit none
  save

  integer(kind=DI), parameter :: maxSpecLength=10
  integer(kind=DI), parameter :: maxPhotoRateNameLength=6
  integer(kind=DI), parameter :: maxEnvVarNameLength=9
  integer(kind=DI), parameter :: maxEnvVarLength=15
  integer(kind=DI), parameter :: maxFilepathLength=100
  integer(kind=DI), parameter :: maxReactionStringLength=1000

end module storage

! ******************************************************************** !
! MODULE directories
! ???
! ******************************************************************** !
module directories
  use storage, only : maxFilepathLength
  implicit none
  save

  character(len=maxFilepathLength) :: output_dir, instantaneousRates_dir, param_dir
  character(len=maxFilepathLength) :: spec_constraints_dir, env_constraints_dir

end module directories

! ******************************************************************** !
! MODULE date_mod
! date variables - date used for calculation of DEC
! ******************************************************************** !
module date_mod
  use types_mod
  implicit none
  save

  integer(kind=SI) :: day, month
  integer(kind=DI) :: year, dayOfYear

contains

  ! -----------------------------------------------------------------
  ! ???
  subroutine calcDateParameters()
    implicit none

    integer(kind=SI) :: monthList(12), i

    ! Number of days in each month; year is set in model.parameters.
    if ( (mod(year, 4_DI)==0 .and. .not. mod(year, 100_DI)==0) .or. (mod(year, 400_DI)==0) ) then
      ! leap year
      monthList = [31_SI, 28_SI, 31_SI, 30_SI, 31_SI, 30_SI, 31_SI, 31_SI, 30_SI, 31_SI, 30_SI, 31_SI]
    else
      ! not a leap year
      monthList = [31_SI, 29_SI, 31_SI, 30_SI, 31_SI, 30_SI, 31_SI, 31_SI, 30_SI, 31_SI, 30_SI, 31_SI]
    end if

    ! Day of year; day and month are set in model.parameters.
    ! January 1 = 0, January 2 = 1, etc...
    dayOfYear = 0
    do i=1, (month - 1_SI)
      dayOfYear = dayOfYear + monthList(i)
    end do
    !dayOfYear = sum( monthList(1:month - 1_SI) )  ! it does not work if implemented like this !
    !write(*,*) "=======================>", dayOfYear
    dayOfYear = dayOfYear + day - 1_SI

    return
  end subroutine calcDateParameters

end module date_mod

! ******************************************************************** !
! MODULE envVars
! ???
! ******************************************************************** !
module envVars
  use types_mod
  use storage, only : maxEnvVarNameLength, maxEnvVarLength
  implicit none
  save

  character(len=maxEnvVarNameLength), allocatable :: envVarNames(:)
  character(len=maxEnvVarLength), allocatable :: envVarTypes(:)
  integer(kind=SI), allocatable :: envVarTypesNum(:)
  real(kind=DP), allocatable :: envVarFixedValues(:), currentEnvVarValues(:)
  integer(kind=SI) :: numEnvVars
  real(kind=DP), allocatable :: envVarX (:,:), envVarY (:,:), envVarY2 (:,:)
  integer(kind=NPI), allocatable :: envVarNumberOfPoints(:)
  real(kind=DP) :: ro2

end module envVars

! ******************************************************************** !
! MODULE constraints
! ???
! ******************************************************************** !
module constraints
  use types_mod
  implicit none
  save

  private :: numberOfConstrainedSpecies, constrainedConcs, constrainedSpecies
  public :: getNumberOfConstrainedSpecies, setNumberOfConstrainedSpecies
  public :: getConstrainedConcs, setConstrainedConcs, deallocateConstrainedConcs
  public :: getConstrainedSpecies, setConstrainedSpecies
  public :: deallocateConstrainedSpecies, getOneConstrainedSpecies

  integer(kind=NPI) :: numberOfConstrainedSpecies
  integer(kind=NPI) :: numberOfFixedConstrainedSpecies, numberOfVariableConstrainedSpecies
  real(kind=DP), allocatable :: constrainedConcs(:)
  real(kind=DP), allocatable :: dataX(:,:), dataY(:,:), dataY2(:,:), dataFixedY(:)
  integer(kind=NPI), allocatable :: constrainedSpecies(:)
  integer(kind=NPI) :: maxNumberOfDataPoints
  integer(kind=NPI), allocatable :: speciesNumberOfPoints(:)

contains

  ! Methods for numberOfConstrainedSpecies

  ! -----------------------------------------------------------------
  ! ???
  pure function getNumberOfConstrainedSpecies() result ( n )
    implicit none
    integer(kind=NPI) :: n
    n = numberOfConstrainedSpecies
  end function getNumberOfConstrainedSpecies

  ! -----------------------------------------------------------------
  ! ???
  subroutine setNumberOfConstrainedSpecies( n )
    implicit none
    integer(kind=NPI) :: n
    numberOfConstrainedSpecies = n
    allocate (constrainedConcs(n), constrainedSpecies(n))
    write (*, '(A, I0)') ' Setting size of constraint arrays, n = ', n
  end subroutine setNumberOfConstrainedSpecies

  ! Methods for constrainedConcs

  ! -----------------------------------------------------------------
  ! ???
  pure function getConstrainedConcs() result ( r )
    implicit none
    real(kind=DP) :: r(numberOfConstrainedSpecies)
    r = constrainedConcs(:)
  end function getConstrainedConcs

  ! -----------------------------------------------------------------
  ! ???
  subroutine setConstrainedConcs( r )
    implicit none
    real(kind=DP) :: r(:)
    constrainedConcs(:) = r(:)
  end subroutine setConstrainedConcs

  ! -----------------------------------------------------------------
  ! ???
  subroutine deallocateConstrainedConcs()
    implicit none
    deallocate (constrainedConcs)
  end subroutine deallocateConstrainedConcs

  ! Methods for constrainedSpecies

  ! -----------------------------------------------------------------
  ! ???
  pure function getConstrainedSpecies() result ( r )
    implicit none
    integer(kind=NPI) :: r(numberOfConstrainedSpecies)
    r = constrainedSpecies(:)
  end function getConstrainedSpecies

  ! -----------------------------------------------------------------
  ! ???
  pure function getOneConstrainedSpecies( i ) result ( r )
    implicit none
    integer(kind=NPI), intent(in) :: i
    integer(kind=NPI) :: r
    r = constrainedSpecies(i)
  end function getOneConstrainedSpecies

  ! -----------------------------------------------------------------
  ! ???
  subroutine setConstrainedSpecies( n, r )
    implicit none
    integer(kind=NPI) :: n, r
    constrainedSpecies(n) = r
  end subroutine setConstrainedSpecies

  ! -----------------------------------------------------------------
  ! ???
  subroutine deallocateConstrainedSpecies()
    implicit none
    deallocate (constrainedSpecies)
  end subroutine deallocateConstrainedSpecies

end module constraints

! ******************************************************************** !
! MODULE species
! ???
! ******************************************************************** !
module species
  use types_mod
  use storage, only : maxSpecLength
  implicit none
  save

  private :: numSpecies, numReactions, speciesList
  public :: getNumberOfSpecies, setNumberOfSpecies,
  public :: getNumberOfReactions, setNumberOfReactions
  public :: deallocateSpeciesList, getSpeciesList, setSpeciesList

  integer(kind=NPI) :: numSpecies, numReactions
  character(len=maxSpecLength), allocatable :: speciesList(:)

contains

  ! -----------------------------------------------------------------
  ! ???
  pure function getNumberOfSpecies() result ( n )
    implicit none
    integer(kind=NPI) :: n
    n = numSpecies
  end function getNumberOfSpecies

  ! -----------------------------------------------------------------
  ! ???
  subroutine setNumberOfSpecies( n )
    implicit none
    integer(kind=NPI) :: n
    numSpecies = n
    allocate (speciesList(n))
  end subroutine setNumberOfSpecies

  ! -----------------------------------------------------------------
  ! ???
  pure function getNumberOfReactions() result ( n )
    implicit none
    integer(kind=NPI) :: n
    n = numReactions
  end function getNumberOfReactions

  ! -----------------------------------------------------------------
  ! ???
  subroutine setNumberOfReactions( n )
    implicit none
    integer(kind=NPI) :: n
    numReactions = n
  end subroutine setNumberOfReactions

  ! -----------------------------------------------------------------
  ! ???
  subroutine deallocateSpeciesList
    implicit none
    deallocate (speciesList)
  end subroutine deallocateSpeciesList

  ! -----------------------------------------------------------------
  ! ???
  pure function getSpeciesList() result ( sl )
    implicit none
    character(len=maxSpecLength), allocatable :: sl(:)
    integer(kind=NPI) :: i
    allocate (sl(numSpecies) )
    do i = 1, numSpecies
      sl(i) = speciesList(i)
    end do
  end function getSpeciesList

  ! -----------------------------------------------------------------
  ! ???
  subroutine setSpeciesList( sl )
    implicit none
    character(len=maxSpecLength) :: sl(:)
    integer(kind=NPI) :: i
    do i = 1, numSpecies
      speciesList(i) = sl(i)
    end do
  end subroutine setSpeciesList

end module species

! ******************************************************************** !
! MODULE interpolationMethod
! interpolation methods
! ******************************************************************** !
module interpolationMethod
  use types_mod
  implicit none
  save

  public :: getSpeciesInterpMethod, getConditionsInterpMethod, getDecInterpMethod
  public :: setSpeciesInterpMethod, setConditionsInterpMethod, setDecInterpMethod

  integer(kind=SI), private :: speciesInterpMethod, conditionsInterpMethod, decInterpMethod

contains

  ! -----------------------------------------------------------------
  ! ???
  pure function getSpeciesInterpMethod() result ( n )
    implicit none
    integer(kind=SI) :: n
    n = speciesInterpMethod
  end function getSpeciesInterpMethod

  ! -----------------------------------------------------------------
  ! ???
  subroutine setSpeciesInterpMethod( n )
    implicit none
    integer(kind=SI) :: n
    speciesInterpMethod = n
  end subroutine setSpeciesInterpMethod

  ! -----------------------------------------------------------------
  ! ???
  pure function getConditionsInterpMethod() result ( n )
    implicit none
    integer(kind=SI) :: n
    n = conditionsInterpMethod
  end function getConditionsInterpMethod

  ! -----------------------------------------------------------------
  ! ???
  subroutine setConditionsInterpMethod( n )
    implicit none
    integer(kind=SI) :: n
    conditionsInterpMethod = n
  end subroutine setConditionsInterpMethod

  ! -----------------------------------------------------------------
  ! ???
  pure function getDecInterpMethod() result ( n )
    implicit none
    integer(kind=SI) :: n
    n = decInterpMethod
  end function getDecInterpMethod

  ! -----------------------------------------------------------------
  ! ???
  subroutine setDecInterpMethod( n )
    implicit none
    integer(kind=SI) :: n
    decInterpMethod = n
  end subroutine setDecInterpMethod

end module interpolationMethod

! ******************************************************************** !
! MODULE reactionStructure
! ???
! ******************************************************************** !
module reactionStructure
  use types_mod
  implicit none
  save

  integer(kind=NPI), allocatable :: clhs(:,:), crhs(:,:)
  real(kind=DP), allocatable :: clcoeff(:), crcoeff(:)

end module reactionStructure

! ******************************************************************** !
! MODULE photolysisRates
! photolysis rates method
! ******************************************************************** !
module photolysisRates_mod
  use types_mod
  use storage, only : maxPhotoRateNameLength
  implicit none
  save

  integer(kind=NPI), parameter :: maxNrOfConPhotoRates = 100
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

  ! -----------------------------------------------------------------
  ! ???
  subroutine allocate_photolysis_constants_variables()
    implicit none

    allocate (ck(nrOfPhotoRates), cl(nrOfPhotoRates), photoRateNames(nrOfPhotoRates))
  end subroutine allocate_photolysis_constants_variables

  ! -----------------------------------------------------------------
  ! ???
  subroutine allocate_photolysis_rates_variables()
    implicit none

    allocate (ck(nrOfPhotoRates), cl(nrOfPhotoRates), cmm(nrOfPhotoRates))
    allocate (cnn(nrOfPhotoRates), photoRateNames(nrOfPhotoRates), transmissionFactor(nrOfPhotoRates))
  end subroutine allocate_photolysis_rates_variables

  ! -----------------------------------------------------------------
  ! ???
  subroutine allocate_photolysis_j()
    implicit none

    allocate (j(size_of_j))
    j(:) = 0.0_DP
  end subroutine allocate_photolysis_j

end module photolysisRates_mod

! ******************************************************************** !
! MODULE zenithData
! solar zenith angle and photolysis rates parameters
! ******************************************************************** !
module zenithData
  use types_mod
  implicit none
  save

  real(kind=DP) :: latitude, longitude
  real(kind=DP) :: lha, sinld, cosld, cosx, secx
  real(kind=DP) :: theta, eqtime

end module zenithData

! ******************************************************************** !
! MODULE productionAndLossRates
! rates of production and loss
! ******************************************************************** !
module productionAndLossRates
  use types_mod
  implicit none
  save

  real(kind=DP), allocatable :: lossRates(:), productionRates(:), instantaneousRates(:)

end module productionAndLossRates
