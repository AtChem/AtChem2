! ******************************************************************** !
! ATCHEM -- MODULE configFunctions
!
! ??? Text describing the module ???
! ******************************************************************** !
module configFunctions_mod
contains

  ! -----------------------------------------------------------------
  ! This takes in masterSpeciesList, and checks whether each member of
  ! testspeciesList is in masterSpeciesList. When it finds a match, it
  ! adds the number of the line in masterSpeciesList to returnArray in
  ! the next available space, thus each element of returnArray
  ! corresponds to the same element in testSpeciesList. If not found,
  ! then abort.
  subroutine matchNameToNumber( masterSpeciesList, testSpeciesList, returnArray )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use types_mod
    use storage, only : maxSpecLength
    implicit none

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
    implicit none

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

  ! ----------------------------------------------------------------- !
  ! ???
  subroutine findReactionsWithProductOrReactant( r, chs, arrayLen )
    use types_mod
    implicit none

    integer(kind=NPI), intent(in) :: chs(:,:)
    integer(kind=NPI), intent(out) :: arrayLen(:)
    integer(kind=NPI), intent(inout) :: r(:,:)
    integer(kind=NPI) :: rCounter, i, j

    ! For each interesting species, held in the first element of each
    ! row of r, find all reactions in chs which match (i.e. second
    ! column of chs matches first element of given row from r), and
    ! append the number of that reaction (first column of chs) to this
    ! row of r. arrayLen keeps track of how long each row in r is.
    if ( size( arrayLen ) /= size( r, 1 ) ) then
      stop "size(arrayLen) /= size(r, 1) in findReactionsWithProductOrReactant()."
    end if
    ! initialise counter for r array
    rCounter = 2
    ! loop over interesting species (i.e. over 1st index of r)
    do i = 1, size( arrayLen )
      ! loop over elements of 2nd index of chs
      do j = 1, size( chs, 2 )
        ! Is the second element of this row in chs (a species number)
        ! equal to the first element of this column in r (the
        ! interesting species number)?  If so, then append the first
        ! element of this row in chs (the equation number) to this row
        ! in r, and update the length counter arrayLen for this row.
        if ( chs(2, j) == r(i, 1) ) then
          ! Match found
          r(i, rCounter) = chs(1, j)
          rCounter = rCounter + 1
        end if
      end do
      arrayLen(i) = rCounter - 1
      rCounter = 2
    end do

    return
  end subroutine findReactionsWithProductOrReactant

  ! -----------------------------------------------------------------
  ! This function outputs interestSpeciesConcList, the concentration
  ! of each species of interest, in the same order as the species are
  ! in specInt
  pure function getConcForSpeciesOfInterest( masterConcList, speciesOfInterest ) result ( interestSpeciesConcList )
    use types_mod
    use species, only : getSpeciesList
    use storage, only : maxSpecLength
    implicit none

    real(kind=DP), intent(in) :: masterConcList(:)
    character(len=maxSpecLength), intent(in) :: speciesOfInterest(:)
    character(len=maxSpecLength), allocatable :: allSpecies(:)
    real(kind=DP) :: interestSpeciesConcList(size( speciesOfInterest ))
    integer(kind=NPI) :: i, j

    ! Set interestSpeciesConcList(j) to the value of the concentration
    ! pulled from masterConcList, using the elements of specInt as key
    allSpecies = getSpeciesList()
    do i = 1, size( allSpecies )
      do j = 1, size( speciesOfInterest )
        if ( trim( speciesOfInterest(j) ) == trim( allSpecies(i) ) ) then
          interestSpeciesConcList(j) = masterConcList(i)
          exit
        end if
      end do
    end do

    return
  end function getConcForSpeciesOfInterest

end module configFunctions_mod
