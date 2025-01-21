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
! ATCHEM2 -- FILE dataStructures
!
! This file contains modules that hold logically separate data types,
! variables, arrays and procedures.
! ******************************************************************** !

! ******************************************************************** !
! MODULE types_mod
! Define integer and real data types.
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
  integer, parameter :: NPI = INT64 ! Must be INT32 or INT64, as required by CVODE functions
  integer, parameter :: IntErr = INT32
  integer, parameter :: SP = selected_real_kind( p = 6, r = 37 )
  integer, parameter :: DP = selected_real_kind( p = 15, r = 307 )
  integer, parameter :: QP = selected_real_kind( p = 33, r = 4931 )

  type reaction_frequency_pair
    integer(kind=NPI) :: reaction
    integer(kind=NPI) :: frequency
    ! stoich was added in Jan 2025 by Alfred Mayhew to allow proper output of rates for reactions
    ! involving non-integer stoichiometry (instead of using frequency)
    real(kind=DP) :: stoich
  end type reaction_frequency_pair

  interface operator (==)
    module procedure reaction_frequency_pair_equals
  end interface

contains

  ! -----------------------------------------------------------------
  ! Equality operator for reaction_frequency_pair type
  function reaction_frequency_pair_equals( a, b ) result ( res )
    type(reaction_frequency_pair), intent(in) :: a, b
    logical :: res

    res = ( ( a%reaction == b%reaction ) .and. ( a%frequency == b%frequency ) )
  end function reaction_frequency_pair_equals

end module types_mod

! ******************************************************************** !
! MODULE storage_mod
! Define the maximum length of different string types.
! ******************************************************************** !
module storage_mod
  use types_mod
  implicit none
  save

  integer(kind=DI), parameter :: maxSpecLength=50
  integer(kind=DI), parameter :: maxPhotoRateNameLength=6
  integer(kind=DI), parameter :: maxEnvVarNameLength=9
  integer(kind=DI), parameter :: maxEnvVarLength=15
  integer(kind=DI), parameter :: maxFilepathLength=100
  integer(kind=DI), parameter :: maxReactionStringLength=1000

end module storage_mod

! ******************************************************************** !
! MODULE directories_mod
! Define the model input and output directories.
! ******************************************************************** !
module directories_mod
  use storage_mod, only : maxFilepathLength
  implicit none
  save

  character(len=maxFilepathLength) :: model_dir, output_dir, reactionRates_dir, &
                                      configuration_dir, mcm_dir, shared_library, &
                                      constraints_dir, spec_constraints_dir, &
                                      env_constraints_dir, photo_constraints_dir

end module directories_mod

! ******************************************************************** !
! MODULE date_mod
! Define variables and procedures to handle date and time
! calculations, including start and current date/time of the
! simulation, day of the year, leap years, and conversion of the
! current time into a date/time format.
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

  integer(kind=DI), parameter :: refMonthList(12) = [31_DI, 28_DI, 31_DI, 30_DI, 31_DI, 30_DI, &
                                                     31_DI, 31_DI, 30_DI, 31_DI, 30_DI, 31_DI]

contains

  ! -----------------------------------------------------------------
  ! Determine whether year is a leap year
  pure function isLeapYear( year ) result ( result )
    integer(kind=DI), intent(in) :: year
    logical :: result

    result = ( (mod(year, 4_DI)==0 .and. .not. mod(year, 100_DI)==0) .or. (mod(year, 400_DI)==0) )

    return
  end function isLeapYear

  ! -----------------------------------------------------------------
  ! Return a copy of inMonthList which has the number of days in
  ! February set to 29 if year is a leap year
  subroutine applyLeapDay( monthList, year )
    integer(kind=DI), intent(inout) :: monthList(12)
    integer(kind=DI), intent(in) :: year

    if ( isLeapYear( year ) .eqv. .true. ) then
      monthList(2) = 29_DI
    end if
    return
  end subroutine applyLeapDay

  ! -----------------------------------------------------------------
  ! Return the number of days of the year that have been completed,
  ! based on which day of the day it is, and which month it is. So
  ! Jan 1 = 0, Jan 2 = 1 etc...
  pure function calcDayOfYear( monthList, month, day ) result ( result )
    integer(kind=DI), intent(in) :: monthList(12), month, day
    integer(kind=DI) :: result

    result = sum( monthList(1:month - 1_DI) ) + day - 1_DI

    return
  end function calcDayOfYear

  ! -----------------------------------------------------------------
  ! Set startDayOfYear from startDay, startMonth, and startYear
  subroutine calcInitialDateParameters()
    integer(kind=DI) :: monthList(12)

    ! Number of days in each month; year is set in model.parameters
    monthList = refMonthList
    ! Alter February length if a leap year
    call applyLeapDay( monthList, startYear )

    ! Day of year; day and month are set in model.parameters
    ! January 1 = 0, January 2 = 1, etc...
    startDayOfYear = calcDayOfYear( monthList, startMonth, startDay )

    return
  end subroutine calcInitialDateParameters

  ! -----------------------------------------------------------------
  ! Set currentMonth, currentDayOfMonth, currentYear and
  ! currentDayOfYear from the start date/time and the current time
  subroutine calcCurrentDateParameters( t )
    real(kind=DP), intent(in) :: t
    integer(kind=DI) :: monthList(12)
    integer(kind=QI) :: completedDays, countingDays

    ! Calculate how many days have elapsed since the beginning of startDay
    completedDays = int( t / ( 24 * 60 * 60 ) )
    currentMonth = startMonth
    currentDayOfMonth = startDay
    currentYear = startYear
    monthList = refMonthList

    ! Count through the days - tick over into next month and year as appropriate
    if ( completedDays > 0 ) then
      countingDays = completedDays

      do while ( countingDays > 0 )
        ! Check whether this year is a leap year, and reset current
        ! month list as appropriate
        monthList = refMonthList
        ! Alter February length if a leap year
        call applyLeapDay( monthList, currentYear )

        ! Increment day of the month
        currentDayOfMonth = currentDayOfMonth + 1_DI
        ! If incrementing causes us to tick over into the next month,
        ! then update the month and reset the currentDayOfMonth counter
        if ( currentDayOfMonth > monthList(currentMonth) ) then
          currentMonth = currentMonth + 1_DI
          currentDayOfMonth = 1_DI
          ! If incrementing the month causes us to tick into the next
          ! year, then update the year and reset the currentMonth counter
          if ( currentMonth > 12_DI ) then
            currentYear = currentYear + 1_DI
            currentMonth = 1_DI
          end if
        end if

        ! decrease countingDays by one
        countingDays = countingDays - 1_QI
      end do
    end if
    ! Alter February length if a leap year
    call applyLeapDay( monthList, currentYear )

    currentDayOfYear = calcDayOfYear( monthList, currentMonth, currentDayOfMonth )
    return
  end subroutine calcCurrentDateParameters

end module date_mod

! ******************************************************************** !
! MODULE env_vars_mod
! Define variables and arrays for the environment variables.
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
  integer(kind=NPI), allocatable :: ro2Numbers(:)

end module env_vars_mod

! ******************************************************************** !
! MODULE constraints_mod
! Define variables and procedures to handle constrained species and
! concentrations, and related data.
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
  integer(kind=NPI) :: maxNumberOfConstraintDataPoints, maxNumberOfEnvVarDataPoints
  integer(kind=NPI), allocatable :: speciesNumberOfPoints(:)

contains

  ! -----------------------------------------------------------------
  ! Methods for number of constrained species
  pure function getNumberOfConstrainedSpecies() result ( n )
    integer(kind=NPI) :: n
    n = numberOfConstrainedSpecies
  end function getNumberOfConstrainedSpecies

  subroutine setNumberOfConstrainedSpecies( n )
    integer(kind=NPI) :: n
    numberOfConstrainedSpecies = n
    allocate (constrainedConcs(n), constrainedSpecies(n))
    write (*, '(A, I0)') ' Setting size of constraint arrays, n = ', n
  end subroutine setNumberOfConstrainedSpecies

  ! -----------------------------------------------------------------
  ! Methods for constrained concentrations
  pure function getConstrainedConcs() result ( r )
    real(kind=DP) :: r(numberOfConstrainedSpecies)
    r = constrainedConcs(:)
  end function getConstrainedConcs

  subroutine setConstrainedConcs( r )
    real(kind=DP) :: r(:)
    constrainedConcs(:) = r(:)
  end subroutine setConstrainedConcs

  subroutine deallocateConstrainedConcs()
    deallocate (constrainedConcs)
  end subroutine deallocateConstrainedConcs

  ! -----------------------------------------------------------------
  ! Methods for constrained species
  pure function getConstrainedSpecies() result ( r )
    integer(kind=NPI) :: r(numberOfConstrainedSpecies)
    r = constrainedSpecies(:)
  end function getConstrainedSpecies

  pure function getOneConstrainedSpecies( i ) result ( r )
    integer(kind=NPI), intent(in) :: i
    integer(kind=NPI) :: r
    r = constrainedSpecies(i)
  end function getOneConstrainedSpecies

  subroutine setConstrainedSpecies( n, r )
    integer(kind=NPI) :: n, r
    constrainedSpecies(n) = r
  end subroutine setConstrainedSpecies

  subroutine deallocateConstrainedSpecies()
    deallocate (constrainedSpecies)
  end subroutine deallocateConstrainedSpecies

end module constraints_mod

! ******************************************************************** !
! MODULE species_mod
! Define variables and procedures for the names and numbers of
! species, reactions and rate coefficients.
! ******************************************************************** !
module species_mod
  use types_mod
  use storage_mod, only : maxSpecLength
  implicit none
  save

  private :: numSpecies, numReactions, numGenericComplex, speciesList
  public :: getNumberOfSpecies, setNumberOfSpecies
  public :: getNumberOfReactions, setNumberOfReactions
  public :: getNumberOfGenericComplex, setNumberOfGenericComplex
  public :: deallocateSpeciesList, getSpeciesList, setSpeciesList

  integer(kind=NPI) :: numSpecies, numReactions, numGenericComplex
  character(len=maxSpecLength), allocatable :: speciesList(:)

contains

  ! -----------------------------------------------------------------
  ! Methods for number of species
  pure function getNumberOfSpecies() result ( n )
    integer(kind=NPI) :: n
    n = numSpecies
  end function getNumberOfSpecies

  subroutine setNumberOfSpecies( n )
    integer(kind=NPI) :: n
    numSpecies = n
    allocate (speciesList(n))
  end subroutine setNumberOfSpecies

  ! -----------------------------------------------------------------
  ! Methods for number of reactions
  pure function getNumberOfReactions() result ( n )
    integer(kind=NPI) :: n
    n = numReactions
  end function getNumberOfReactions

  subroutine setNumberOfReactions( n )
    integer(kind=NPI) :: n
    numReactions = n
  end subroutine setNumberOfReactions

  ! -----------------------------------------------------------------
  ! Methods for number of generic and complex rate coefficients
  pure function getNumberOfGenericComplex() result ( n )
    integer(kind=NPI) :: n
    n = numGenericComplex
  end function getNumberOfGenericComplex

  subroutine setNumberOfGenericComplex( n )
    integer(kind=NPI) :: n
    numGenericComplex = n
  end subroutine setNumberOfGenericComplex

  ! -----------------------------------------------------------------
  ! Methods for list of species
  subroutine deallocateSpeciesList
    deallocate (speciesList)
  end subroutine deallocateSpeciesList

  pure function getSpeciesList() result ( sl )
    character(len=maxSpecLength), allocatable :: sl(:)
    integer(kind=NPI) :: i
    allocate (sl(numSpecies))
    do i = 1, numSpecies
      sl(i) = speciesList(i)
    end do
  end function getSpeciesList

  subroutine setSpeciesList( sl )
    character(len=maxSpecLength) :: sl(:)
    integer(kind=NPI) :: i
    do i = 1, numSpecies
      speciesList(i) = sl(i)
    end do
  end subroutine setSpeciesList

end module species_mod

! ******************************************************************** !
! MODULE interpolation_method_mod
! Define procedures to handle the interpolation methods for species
! and conditions.
! ******************************************************************** !
module interpolation_method_mod
  use types_mod
  implicit none
  save

  public :: getSpeciesInterpMethod, getConditionsInterpMethod
  public :: setSpeciesInterpMethod, setConditionsInterpMethod

  integer(kind=SI), private :: speciesInterpMethod, conditionsInterpMethod

contains

  ! -----------------------------------------------------------------
  ! Methods for interpolation of species
  pure function getSpeciesInterpMethod() result ( n )
    integer(kind=SI) :: n
    n = speciesInterpMethod
  end function getSpeciesInterpMethod

  subroutine setSpeciesInterpMethod( n )
    integer(kind=SI) :: n
    speciesInterpMethod = n
  end subroutine setSpeciesInterpMethod

  ! -----------------------------------------------------------------
  ! Methods for interpolation of conditions
  pure function getConditionsInterpMethod() result ( n )
    integer(kind=SI) :: n
    n = conditionsInterpMethod
  end function getConditionsInterpMethod

  subroutine setConditionsInterpMethod( n )
    integer(kind=SI) :: n
    conditionsInterpMethod = n
  end subroutine setConditionsInterpMethod

end module interpolation_method_mod

! ******************************************************************** !
! MODULE reaction_structure_mod
! Define arrays for the encoded reactions.
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
! Define variables and procedures for the photolysis rates and
! related parameters.
! ******************************************************************** !
module photolysis_rates_mod
  use types_mod
  use storage_mod, only : maxPhotoRateNameLength
  implicit none
  save

  integer(kind=NPI) :: totalNumPhotos, numConstantPhotoRates, numConstrainedPhotoRates, &
                       numUnconstrainedPhotoRates
  integer(kind=NPI), allocatable :: photoNumbers(:), constantPhotoNumbers(:), &
                                    constrainedPhotoNumbers(:), unconstrainedPhotoNumbers(:), ck(:)
  real(kind=DP), allocatable :: cl(:), cmm(:), cnn(:), transmissionFactor(:)
  real(kind=DP), allocatable :: j(:), constantPhotoValues(:)
  character(len=maxPhotoRateNameLength), allocatable :: photoRateNames(:), constantPhotoNames(:), &
                                                        constrainedPhotoNames(:), unconstrainedPhotoNames(:)
  character(len=maxPhotoRateNameLength) :: jFacSpecies
  logical :: usePhotolysisConstants, existUnconstrainedPhotos, jFacSpeciesFound
  integer(kind=NPI) :: maxNumberOfPhotoDataPoints
  real(kind=DP), allocatable :: photoX(:,:), photoY(:,:)
  integer(kind=NPI), allocatable :: photoNumberOfPoints(:)
  integer(kind=SI) :: PR_type
  real(kind=DP) :: jFacL, jFacM, jFacN, jFacTransmissionFactor

contains

  ! -----------------------------------------------------------------
  ! Method for constant photolysis rates
  subroutine allocate_photolysis_constants_variables()
    allocate (constantPhotoNumbers(numConstantPhotoRates), &
              constantPhotoValues(numConstantPhotoRates), &
              constantPhotoNames(numConstantPhotoRates))
  end subroutine allocate_photolysis_constants_variables

  ! -----------------------------------------------------------------
  ! Method for number of photolysis rates
  subroutine allocate_photolysis_numbers_variables()
    allocate (photoNumbers(totalNumPhotos))
  end subroutine allocate_photolysis_numbers_variables

  ! -----------------------------------------------------------------
  ! Methods for constrained photolysis rates
  subroutine allocate_constrained_photolysis_rates_variables()
    allocate (constrainedPhotoNames(numConstrainedPhotoRates), &
              constrainedPhotoNumbers(numConstrainedPhotoRates))
  end subroutine allocate_constrained_photolysis_rates_variables

  subroutine allocate_constrained_photolysis_data()
    allocate (photoX(numConstrainedPhotoRates, maxNumberOfPhotoDataPoints), &
              photoY(numConstrainedPhotoRates, maxNumberOfPhotoDataPoints), &
              photoNumberOfPoints(numConstrainedPhotoRates))
  end subroutine allocate_constrained_photolysis_data

  ! -----------------------------------------------------------------
  ! Method for unconstrained photolysis rates
  subroutine allocate_unconstrained_photolysis_rates_variables()
    allocate (ck(numUnconstrainedPhotoRates), cl(numUnconstrainedPhotoRates), &
              cmm(numUnconstrainedPhotoRates), cnn(numUnconstrainedPhotoRates), &
              unconstrainedPhotoNames(numUnconstrainedPhotoRates), &
              transmissionFactor(numUnconstrainedPhotoRates))
  end subroutine allocate_unconstrained_photolysis_rates_variables

  ! -----------------------------------------------------------------
  ! Initialize photolysis rates array
  subroutine allocate_photolysis_j( size_of_j )
    integer(kind=NPI), intent(in) :: size_of_j
    allocate (j(size_of_j))
    j(:) = 0.0_DP
  end subroutine allocate_photolysis_j

end module photolysis_rates_mod

! ******************************************************************** !
! MODULE zenith_data_mod
! Define variables for latitude, longitude, solar zenith angle and
! other parameters related to photolysis.
! ******************************************************************** !
module zenith_data_mod
  use types_mod
  implicit none
  save

  real(kind=DP) :: latitude, longitude
  real(kind=DP) :: lha, sinld, cosld, cosx, secx, eqtime
  real(kind=DP), parameter :: cosx_threshold = 1.0d-2
  ! cosx_below_threshold indicates whether cosx is currently below or
  ! above cosx_threshold
  logical :: cosx_below_threshold = .false.

end module zenith_data_mod

! ******************************************************************** !
! MODULE reaction_rates_mod
! Define array for the reaction rates.
! ******************************************************************** !
module reaction_rates_mod
  use types_mod
  implicit none
  save

  real(kind=DP), allocatable :: reactionRates(:)

end module reaction_rates_mod
