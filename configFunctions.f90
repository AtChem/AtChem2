module configFunctions_mod
  use types_mod
contains
  subroutine calcDateParameters()
    use date
    implicit none

    integer :: i, monthList(12)

    monthList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    ! calculate which day of the year day/month refers to
    dayOfYear = 0
    do i = 1, month - 1
      dayOfYear = dayOfYear + monthList(i)
    end do
    dayOfYear = dayOfYear + day - 1
    ! This day refers to the following fraction through the year
    dayAsFractionOfYear = dayOfYear / 365
    ! Set number of seconds per year
    secondsInYear = 3.6525d+02 * 2.40d+01 * 3.60d+03
    return
  end subroutine calcDateParameters

  subroutine writeFileHeaders( photoRateNamesForHeader, specOutReqNames )
    use envVars
    use photolysisRates, only : nrOfPhotoRates, ck
    use storage, only : maxPhotoRateNameLength, maxSpecLength
    implicit none

    character(len=maxPhotoRateNameLength), intent(in) :: photoRateNamesForHeader(:)
    character(len=maxSpecLength), intent(in) :: specOutReqNames(:)
    integer(kind=NPI) :: i

    ! WRITE FILE OUTPUT HEADERS AND OUTPUT AT t = 0
    ! OUTPUT FOR CVODE MAIN SOLVER
    write (57,*) 't LNST LNFE LNETF LNGE'

    ! OUTPUT FOR SPARSE SOLVER
    write (56,*) 'time speciesNumber speciesName reactionNumber rate'
    write (60,*) 'time speciesNumber speciesName reactionNumber rate'
    write (61,*) 't NFELS NJTV NPE NPS'

    ! OTHER OUPUT
    write (50, '(100 (1x, a)) ') 't         ', (specOutReqNames(i), i = 1, size( specOutReqNames ))
    ! 51, 53, 54, 55 don't need a header.
    write (52,*) 'time ', (envVarNames(i), i = 1, numEnvVars), 'RO2'
    write (58,*) 't ', (trim( photoRateNamesForHeader(ck(i)) )// '    ', i = 1, nrOfPhotoRates)
    write (59,*) 't secx cosx lat longt lha sinld cosld'
    write (62,*) 't currentStepSize previousStepSize'
    return
  end subroutine writeFileHeaders


  subroutine matchNameToNumber( masterSpeciesList, testSpeciesList, &
                                returnArray, returnArraySize )
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
    integer(kind=NPI), intent(out) :: returnArray(:), returnArraySize
    integer :: i, j
    logical :: match

    returnArraySize = 0
    ! loop over testSpeciesList, and masterSpeciesList. If a match is made, then append
    ! returnArray with the number of the species from testSpeciesList within the masterSpeciesList
    do i = 1, size( testSpeciesList )
      match = .false.
      do j = 1, size( masterSpeciesList )
        if ( masterSpeciesList(j) == testSpeciesList(i) ) then
          match = .true.
          returnArraySize = returnArraySize + 1
          returnArray(returnArraySize) = j
        end if
      end do
      ! substitute empty strings for invalid species
      if ( match .eqv. .false. ) then
        testSpeciesList(i) = ''
      end if
    end do
    return
  end subroutine matchNameToNumber

  pure function matchOneNameToNumber( masterList, target ) result ( id )
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
  end function matchOneNameToNumber


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
          write (54,*) 'match, m = k = ', m, ' concentration = ', inputConcentrations(i)
          exit
        else
          write (54,*) 'no match, m = ', m, ' != k = ', k, ' concentration = ', inputConcentrations(i)!
        end if
      end do
      if ( match .eqv. .false. ) then
        ! If we reach this point, we've failed to find this species
        write (51,*) "Error in setConcentrations"
        write (51,*) "Can't find species: ", k, " in species list"
      end if
    end do
    return
  end subroutine setConcentrations
end module configFunctions_mod
