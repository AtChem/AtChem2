module configFunctions_mod
  use types_mod
contains
  subroutine matchNameToNumber( masterSpeciesList, testSpeciesList, returnArray )
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use storage, only : maxSpecLength
    implicit none
    ! This takes in masterSpeciesList, and checks whether each member of
    ! testspeciesList is in masterSpeciesList.
    ! When it finds a match, it adds the number of the line in masterSpeciesList to
    ! returnArray in the next available space.
    ! returnArraySize contains the size of returnArray, which is the
    ! number of times a match was made
    character(len=maxSpecLength), contiguous, intent(in) :: masterSpeciesList(:)
    character(len=maxSpecLength), contiguous, intent(inout) :: testSpeciesList(:)
    integer(kind=NPI), intent(out) :: returnArray(:)
    integer(kind=NPI) :: counter, i

    counter = 0
    ! loop over each element of testSpeciesList. If a match is made, then append to
    ! returnArray the number of the species from testSpeciesList within the masterSpeciesList. Otherwise, abort.
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

  pure function getIndexWithinList( masterList, target ) result ( id )
    ! Search masterList for target, and return the index id. If not found, return 0.
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


  subroutine findReactionsWithProductOrReactant( r, chs, arrayLen )
    use types_mod
    ! For each interesting species, held in the first element of each row of r,
    ! find all reactions in chs which match (i.e. second column of chs matches first element
    ! of given row from r), and append the number of that reaction (first column of chs)
    ! to this row of r.
    ! arrayLen keeps track of how long each row in r is.
    integer(kind=NPI), intent(in) :: chs(:,:)
    integer(kind=NPI), intent(out) :: arrayLen(:)
    integer(kind=NPI), intent(inout) :: r(:,:)
    integer(kind=NPI) :: rCounter, i, j

    if ( size( arrayLen ) /= size( r, 1 ) ) then
      stop "size(arrayLen) /= size(r, 1) in findReactionsWithProductOrReactant()."
    end if
    ! initialise counter for r array
    rCounter = 2
    ! loop over interesting species (i.e. over 1st index of r)
    do i = 1, size( arrayLen )
      ! loop over elements of 2nd index of chs
      do j = 1, size( chs, 2 )
        ! Is the second element of this row in chs (a species number) equal to the first element of this column in r (the interesting species number)?
        ! If so, then append the first element of this row in chs (the equation number) to this row in r,
        ! and update the length counter arrayLen for this row.
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


  subroutine getConcForSpecInt( masterConcList, speciesOfInterest, interestSpeciesConcList )
    ! This subroutine outputs interestSpeciesConcList, the concentration of each species of interest,
    ! in the same order as the species are in specInt
    real(kind=DP), intent(in) :: masterConcList(:)
    integer(kind=NPI), intent(in) :: speciesOfInterest(:)
    real(kind=DP), intent(out) :: interestSpeciesConcList(:)
    integer(kind=NPI) :: i, j
    ! Set interestSpeciesConcList(j) to the value of the concentration pulled from masterConcList,
    ! using the elements of specInt as a key
    if ( size( interestSpeciesConcList ) /= size( speciesOfInterest ) ) then
      stop 'size(interestSpeciesConcList) /= size(speciesOfInterest) in getConcForSpecInt'
    end if
    do i = 1, size( masterConcList )
      do j = 1, size( speciesOfInterest )
        if ( speciesOfInterest(j) == i ) then
          interestSpeciesConcList(j) = masterConcList(i)
        end if
      end do
    end do
    return
  end subroutine getConcForSpecInt


  subroutine setConcentrations( refSpeciesNames, concSpeciesNames, &
                                inputConcentrations, outputConcentrations )
    ! For each input species in concSpeciesNames (size concCounter), and matching value in inputConcentrations (size inputConcentrationsSize),
    ! look through refSpeciesNames (size numSpecies) for the number of this species in that list,
    ! then transer the value from inputConcentrations to outputConcentrations. If no match is found,
    ! output this to errors.output, but don't stop, just ignore the input value.
    ! Print outcome of each search into initialConditionsSetting.output.
    use storage, only : maxSpecLength
    implicit none

    character(len=maxSpecLength), intent(in) :: concSpeciesNames(:), refSpeciesNames(:)
    character(len=maxSpecLength) :: k, m
    real(kind=DP), intent(in) :: inputConcentrations(:)
    real(kind=DP), intent(out) :: outputConcentrations(:)
    integer(kind=NPI) :: j, i
    logical :: match

    if ( size( concSpeciesNames ) /= size( inputConcentrations ) ) then
      stop "size(concSpeciesNames) /= size(inputConcentrations) in setConcentrations()."
    end if
    if ( size( refSpeciesNames ) /= size( outputConcentrations ) ) then
      stop "size(refSpeciesNames) /= size(outputConcentrations) in setConcentrations()."
    end if
    do i = 1, size( concSpeciesNames )
      match = .false.
      k = concSpeciesNames(i)
      do j = 1, size( refSpeciesNames )
        m = refSpeciesNames(j)
        if ( m == k ) then
          match = .true.
          ! Set concentration in outputConcentrations
          outputConcentrations(j) = inputConcentrations(i)
          write (54, '(A, A, A, 1P e15.3)') 'match, m = k = ', m, ' concentration = ', inputConcentrations(i)
          exit
        else
          write (54, '(A, A, A, A, A, 1P e15.3)') 'no match, m = ', m, ' != k = ', k, ' concentration = ', inputConcentrations(i)!
        end if
      end do
      if ( match .eqv. .false. ) then
        ! If we reach this point, we've failed to find this species
        write (51,*) "Error in setConcentrations"
        write (51, '(A, A, A)') "Can't find species: ", k, " in species list"
      end if
    end do
    return
  end subroutine setConcentrations
end module configFunctions_mod
