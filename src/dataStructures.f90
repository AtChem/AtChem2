! -----------------------------------------------------------------------------
!
! Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
! Peter Jimack, Mike Pilling
!
! Copyright (c) 2017 Sam Cox, Roberto Sommariva
!
! This file is part of the AtChem2 software package.
!
! This file is covered by the MIT license which can be found in the file
! LICENSE.md at the top level of the AtChem2 distribution.
!
! -----------------------------------------------------------------------------

! ******************************************************************** !
! ATCHEM2 -- dataStructures file
!
! This file contains numerous modules that hold logically separate
! variables and functions
! ******************************************************************** !


! ******************************************************************** !
! MODULE types_mod
! Defines the integer and real data types available to the progrem.
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
! MODULE storage_mod
! Defines the maximum length of different string types
! ******************************************************************** !
module storage_mod
  use types_mod
  implicit none
  save

  integer(kind=DI), parameter :: maxSpecLength=10
  integer(kind=DI), parameter :: maxPhotoRateNameLength=6
  integer(kind=DI), parameter :: maxEnvVarNameLength=9
  integer(kind=DI), parameter :: maxEnvVarLength=15
  integer(kind=DI), parameter :: maxFilepathLength=100
  integer(kind=DI), parameter :: maxReactionStringLength=1000

end module storage_mod

! ******************************************************************** !
! MODULE directories_mod
! Holds the locations of relevant input and output directories
! ******************************************************************** !
module directories_mod
  use storage_mod, only : maxFilepathLength
  implicit none
  save

  character(len=maxFilepathLength) :: output_dir, instantaneousRates_dir, param_dir, &
                                      spec_constraints_dir, env_constraints_dir

end module directories_mod

! ******************************************************************** !
! MODULE date_mod
! Holds variables defining the start time and date of the simulation
! and the current time and date in the simulation -  used for
! calculation of solar functions and photolysis rates.
! Also contains functions to set the initial time and date parameters
! and convert the current time into a date/time format.
! ******************************************************************** !
module date_mod
  use types_mod
  implicit none
  save

  integer(kind=DI) :: startDay, startMonth
  integer(kind=DI) :: startYear, startDayOfYear
  integer(kind=QI) :: secondsToEndOfStartDay

  integer(kind=DI) :: currentMonth, currentDayOfMonth
  integer(kind=DI) :: currentYear, currentDayOfYear

contains

  ! -----------------------------------------------------------------
  ! Set startDayOfYear from startDay, startMonth, and startYear
  subroutine calcInitialDateParameters()
    implicit none

    integer(kind=DI) :: monthList(12)

    ! Number of days in each month; year is set in model.parameters.
    monthList = [31_DI, 29_DI, 31_DI, 30_DI, 31_DI, 30_DI, 31_DI, 31_DI, 30_DI, 31_DI, 30_DI, 31_DI]
    ! Alter February length if a leap year
    if ( (mod(startYear, 4_DI)==0 .and. .not. mod(startYear, 100_DI)==0) .or. (mod(startYear, 400_DI)==0) ) then
      monthList(2) = 28_DI
    end if

    ! Day of year; day and month are set in model.parameters.
    ! January 1 = 0, January 2 = 1, etc...
    startDayOfYear = sum( monthList(1:startMonth - 1_DI) ) + startDay - 1_DI

    return
  end subroutine calcInitialDateParameters

  ! -----------------------------------------------------------------
  ! Set currentMonth, currentDayOfMonth, currentYear and
  ! currentDayOfYear from the start date/time and the current time
  subroutine calcCurrentDateParameters( t )
    implicit none

    real(kind=DP), intent(in) :: t
    integer(kind=DI) :: monthList(12)
    integer(kind=QI) :: completedDays, countingDays

    ! Calculate how many days have elapsed since the beginning of startDay
    completedDays = int( t / ( 24 * 60 * 60 ) )
    currentMonth = startMonth
    currentDayOfMonth = startDay
    currentYear = startYear
    monthList = [31_DI, 29_DI, 31_DI, 30_DI, 31_DI, 30_DI, 31_DI, 31_DI, 30_DI, 31_DI, 30_DI, 31_DI]
    ! Alter February length if a leap year
    if ( (mod(currentYear, 4_DI)==0 .and. .not. mod(currentYear, 100_DI)==0) .or. (mod(currentYear, 400_DI)==0) ) then
      monthList(2) = 28_DI
    end if
    ! Count through the days - tick over into next month and year as appropriate
    if ( completedDays > 0 ) then
      countingDays = completedDays

      do while ( countingDays > 0 )
        ! Check whether this year is a leap year, and reset current month list as appropriate
        monthList = [31_DI, 29_DI, 31_DI, 30_DI, 31_DI, 30_DI, 31_DI, 31_DI, 30_DI, 31_DI, 30_DI, 31_DI]
        ! Alter February length if a leap year
        if ( (mod(currentYear, 4_DI)==0 .and. .not. mod(currentYear, 100_DI)==0) .or. (mod(currentYear, 400_DI)==0) ) then
          monthList(2) = 28_DI
        end if
        ! Increment day of the month
        currentDayOfMonth = currentDayOfMonth + 1_DI
        ! If incrementing causes us to tick over into the next month, then update
        ! the month and reset the currentDayOfMonth counter
        if ( currentDayOfMonth > monthList(currentMonth) ) then
          currentMonth = currentMonth + 1_DI
          currentDayOfMonth = 1_DI
          ! If incrementing the month causes us to tick into the next year, then
          ! update the year and reset the currentMonth counter
          if ( currentMonth > 12_DI ) then
            currentYear = currentYear + 1_DI
            currentMonth = 1_DI
          end if
        end if

        ! decrease countingDays by one
        countingDays = countingDays - 1_QI
      end do
    end if
    currentDayOfYear = sum( monthList(1:currentMonth - 1_DI) ) + currentDayOfMonth - 1_DI
    return
  end subroutine calcCurrentDateParameters

end module date_mod

! ******************************************************************** !
! MODULE env_vars_mod
! Holds variable controlling the environment variables and related data
! ******************************************************************** !
module env_vars_mod
  use types_mod
  use storage_mod, only : maxEnvVarNameLength, maxEnvVarLength
  implicit none
  save

  character(len=maxEnvVarNameLength), allocatable :: envVarNames(:)
  character(len=maxEnvVarLength), allocatable :: envVarTypes(:)
  integer(kind=SI), allocatable :: envVarTypesNum(:)
  real(kind=DP), allocatable :: envVarFixedValues(:), currentEnvVarValues(:)
  integer(kind=SI) :: numEnvVars, numConEnvVarRates
  real(kind=DP), allocatable :: envVarX (:,:), envVarY (:,:)
  integer(kind=NPI), allocatable :: envVarNumberOfPoints(:)
  real(kind=DP) :: ro2

end module env_vars_mod

! ******************************************************************** !
! MODULE constraints_mod
! Holds variable controlling the species constraints and related data
! ******************************************************************** !
module constraints_mod
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
  real(kind=DP), allocatable :: dataX(:,:), dataY(:,:), dataFixedY(:)
  integer(kind=NPI), allocatable :: constrainedSpecies(:)
  integer(kind=NPI) :: maxNumberOfConstraintDataPoints, maxNumberOfEnvVarDataPoints, maxNumberOfPhotoDataPoints
  integer(kind=NPI), allocatable :: speciesNumberOfPoints(:)

contains

  ! *****************************************************************
  ! Methods for numberOfConstrainedSpecies

  pure function getNumberOfConstrainedSpecies() result ( n )
    implicit none
    integer(kind=NPI) :: n
    n = numberOfConstrainedSpecies
  end function getNumberOfConstrainedSpecies

  ! -----------------------------------------------------------------
  ! Also allocate constrainedConcs and constrainedSpecies.
  subroutine setNumberOfConstrainedSpecies( n )
    implicit none
    integer(kind=NPI) :: n
    numberOfConstrainedSpecies = n
    allocate (constrainedConcs(n), constrainedSpecies(n))
    write (*, '(A, I0)') ' Setting size of constraint arrays, n = ', n
  end subroutine setNumberOfConstrainedSpecies

  ! *****************************************************************
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

  ! *****************************************************************
  ! Methods for constrainedSpecies

  pure function getConstrainedSpecies() result ( r )
    implicit none
    integer(kind=NPI) :: r(numberOfConstrainedSpecies)
    r = constrainedSpecies(:)
  end function getConstrainedSpecies

  ! -----------------------------------------------------------------
  ! Query the contrained species list for the given index
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

end module constraints_mod

! ******************************************************************** !
! MODULE species_mod
! Holds variables and functions to control the the problem in terms of
! the names/numbers of species and reactions
! ******************************************************************** !
module species_mod
  use types_mod
  use storage_mod, only : maxSpecLength
  implicit none
  save

  private :: numSpecies, numReactions, speciesList
  public :: getNumberOfSpecies, setNumberOfSpecies
  public :: getNumberOfReactions, setNumberOfReactions
  public :: deallocateSpeciesList, getSpeciesList, setSpeciesList

  integer(kind=NPI) :: numSpecies, numReactions
  character(len=maxSpecLength), allocatable :: speciesList(:)

contains

  pure function getNumberOfSpecies() result ( n )
    implicit none
    integer(kind=NPI) :: n
    n = numSpecies
  end function getNumberOfSpecies

  ! -----------------------------------------------------------------
  ! Also allocate speciesList
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

  pure function getSpeciesList() result ( sl )
    implicit none
    character(len=maxSpecLength), allocatable :: sl(:)
    integer(kind=NPI) :: i
    allocate (sl(numSpecies) )
    do i = 1, numSpecies
      sl(i) = speciesList(i)
    end do
  end function getSpeciesList

  subroutine setSpeciesList( sl )
    implicit none
    character(len=maxSpecLength) :: sl(:)
    integer(kind=NPI) :: i
    do i = 1, numSpecies
      speciesList(i) = sl(i)
    end do
  end subroutine setSpeciesList

end module species_mod

! ******************************************************************** !
! MODULE interpolation_method_mod
! get and set interpolation methods
! ******************************************************************** !
module interpolation_method_mod
  use types_mod
  implicit none
  save

  public :: getSpeciesInterpMethod, getConditionsInterpMethod
  public :: setSpeciesInterpMethod, setConditionsInterpMethod

  integer(kind=SI), private :: speciesInterpMethod, conditionsInterpMethod

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

end module interpolation_method_mod

! ******************************************************************** !
! MODULE reaction_structure_mod
! Arrays containing the encoded reactions
! ******************************************************************** !
module reaction_structure_mod
  use types_mod
  implicit none
  save

  integer(kind=NPI), allocatable :: clhs(:,:), crhs(:,:)
  real(kind=DP), allocatable :: clcoeff(:), crcoeff(:)

end module reaction_structure_mod

! ******************************************************************** !
! MODULE photolysis_rates_mod
! Controls many aspects of the photolysis rates.
! ******************************************************************** !
module photolysis_rates_mod
  use types_mod
  use storage_mod, only : maxPhotoRateNameLength
  implicit none
  save

  integer(kind=NPI) :: numConPhotoRates
  integer(kind=NPI), allocatable :: constrainedPhotoRatesNumbers(:)
  integer(kind=NPI) :: jFacSpeciesLine = 0_NPI ! number of line in photolysis rates file corresponding to Jfac species
  integer(kind=NPI) :: nrOfPhotoRates
  integer(kind=NPI), allocatable :: ck(:)
  logical :: usePhotolysisConstants
  real(kind=DP), allocatable :: cl(:), cmm(:), cnn(:), transmissionFactor(:)
  real(kind=DP), allocatable :: j(:)
  character(len=maxPhotoRateNameLength), allocatable :: photoRateNames(:), constrainedPhotoRates(:)
  character(len=maxPhotoRateNameLength) :: jFacSpecies
  real(kind=DP), allocatable :: photoX(:,:), photoY(:,:)
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
    j(:) = 0.0_DP
  end subroutine allocate_photolysis_j

end module photolysis_rates_mod

! ******************************************************************** !
! MODULE zenith_data_mod
! solar zenith angle and photolysis rates parameters
! ******************************************************************** !
module zenith_data_mod
  use types_mod
  implicit none
  save

  real(kind=DP) :: latitude, longitude
  real(kind=DP) :: lha, sinld, cosld, cosx, secx
  real(kind=DP) :: theta, eqtime
  real(kind=DP), parameter :: cosx_threshold = 1.0d-30
  ! cosx_below_threshold contains whether or not cosx is currently
  ! below cosx_threshold
  logical :: cosx_below_threshold = .false.

end module zenith_data_mod

! ******************************************************************** !
! MODULE reaction_rates_mod
! rates of production and loss
! ******************************************************************** !
module reaction_rates_mod
  use types_mod
  implicit none
  save

  real(kind=DP), allocatable :: lossRates(:), productionRates(:)
  real(kind=DP), allocatable :: instantaneousRates(:)

end module reaction_rates_mod
