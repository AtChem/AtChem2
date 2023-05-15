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
! ATCHEM2 -- MODULE configFunctions
!
! This module contains helper functions that query lists in various ways
! in order to extract the required species etc based on some criterion.
! ******************************************************************** !
module config_functions_mod
  implicit none

contains

  ! -----------------------------------------------------------------
  ! This takes in masterSpeciesList, and checks whether each member of
  ! testspeciesList is in masterSpeciesList. When it finds a match, it
  ! adds the number of the line in masterSpeciesList to returnArray in
  ! the next available space, thus each element of returnArray
  ! corresponds to the same element in testSpeciesList.
  ! If not found, then abort.
  subroutine matchNameToNumber( masterSpeciesList, testSpeciesList, returnArray )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use storage_mod, only : maxSpecLength

    character(len=maxSpecLength), contiguous, intent(in) :: masterSpeciesList(:), testSpeciesList(:)
    integer(kind=NPI), intent(out) :: returnArray(:)
    integer(kind=NPI) :: counter, i

    ! loop over each element of testSpeciesList. If a match is made,
    ! then append to returnArray the number of the species from
    ! testSpeciesList within the masterSpeciesList. Otherwise, abort.
    counter = 0
    do i = 1, size( testSpeciesList )
      counter = counter + 1
      if ( counter > size( returnArray ) ) then
        write (stderr,*) 'matchNameToNumber(): counter > size( returnArray ).'
        stop
      end if
      returnArray(counter) = getIndexWithinList(masterSpeciesList, testSpeciesList(i))
      if ( returnArray(counter) == 0 ) then
        write (stderr,*) 'matchNameToNumber(): ' // testSpeciesList(i) // ' not found in list.'
        stop
      end if
    end do

    return
  end subroutine matchNameToNumber

  ! -----------------------------------------------------------------
  ! Search masterList for target, and return the index id. If not
  ! found, return 0.
  pure function getIndexWithinList( masterList, target ) result ( id )
    use types_mod

    character(len=*), contiguous, intent(in) :: masterList(:)
    character(len=*), intent(in) :: target
    integer(kind=NPI) :: j, id

    id = 0
    do j = 1, size( masterList )
      if ( masterList(j) == target ) then
        id = j
        return
      end if
    end do

  end function getIndexWithinList

  ! -----------------------------------------------------------------
  ! At the end of this function, each row of r is associated to one of
  ! the species in rSpecies. Each element of the row corresponds to a
  ! reaction in which this species appears as a product or reactant.
  ! Each element consists of %reaction holding the reaction number, and
  ! %frequency holding the number of occurences of that species in that
  ! reaction. chs contains the product or reactant information, and
  ! arrayLen is used to keep track of how many are present in each row.
  subroutine findReactionsWithProductOrReactant( rSpecies, chs, r, arrayLen )
    use types_mod

    integer(kind=NPI), intent(in) :: rSpecies(:), chs(:,:)
    type(reaction_frequency_pair), intent(inout) :: r(:,:)
    integer(kind=NPI), intent(out) :: arrayLen(:)
    integer(kind=NPI) :: rCounter, i, j

    ! For each interesting species, held in rSpecies, find all reactions
    ! in chs which match (i.e. second column of chs matches first element of
    ! given row from r), and append the number of that reaction (first column
    ! of chs) to this row of r. arrayLen keeps track of how long each row in r is.
    if ( size( arrayLen ) /= size( r, 1 ) ) then
      stop "size(arrayLen) /= size(r, 1) in findReactionsWithProductOrReactant()."
    end if
    ! initialise counter for r array
    rCounter = 0_NPI
    ! loop over interesting species (i.e. over 1st index of r)
    do i = 1, size( arrayLen )
      ! loop over elements of 2nd index of chs
      do j = 1, size( chs, 2 )
        ! Is the second element of this row in chs (a species number)
        ! equal to the same element in rSpecies?  If so, then append the first
        ! element of this row in chs (the equation number) to this row
        ! in r, and update the length counter arrayLen for this row.
        if ( chs(2, j) == rSpecies(i) ) then
          ! Match found
          ! If the reaction number is not the same as previously, then move on in the array
          ! Regardless of that, fill r with the reaction number
          ! Handle the corner case

          if ( rCounter == 0_NPI ) then
            rCounter = 1_NPI
          elseif ( chs(1, j) /= r(i, rCounter)%reaction ) then
            rCounter = rCounter + 1_NPI
          end if
          r(i, rCounter)%reaction = chs(1, j)
          r(i, rCounter)%frequency = r(i, rCounter)%frequency + 1_NPI
        end if
      end do
      arrayLen(i) = rCounter
      rCounter = 0_NPI
    end do

    return
  end subroutine findReactionsWithProductOrReactant

  ! -----------------------------------------------------------------
  ! This function outputs interestSpeciesConcList, the concentration
  ! of each species of interest, in the same order as the species are
  ! in subsetOfSpecies
  pure function getSubsetOfConcs( masterConcList, subsetOfSpecies ) result ( interestSpeciesConcList )
    use types_mod
    use species_mod, only : getSpeciesList
    use storage_mod, only : maxSpecLength

    real(kind=DP), intent(in) :: masterConcList(:)
    character(len=maxSpecLength), intent(in) :: subsetOfSpecies(:)
    character(len=maxSpecLength), allocatable :: allSpecies(:)
    real(kind=DP) :: interestSpeciesConcList(size( subsetOfSpecies ))
    integer(kind=NPI) :: i, j

    ! Set interestSpeciesConcList(j) to the value of the concentration
    ! pulled from masterConcList, using the elements of specInt as key
    allSpecies = getSpeciesList()
    do i = 1, size( allSpecies )
      do j = 1, size( subsetOfSpecies )
        if ( trim( subsetOfSpecies(j) ) == trim( allSpecies(i) ) ) then
          interestSpeciesConcList(j) = masterConcList(i)
          exit
        end if
      end do
    end do

    return
  end function getSubsetOfConcs

end module config_functions_mod
