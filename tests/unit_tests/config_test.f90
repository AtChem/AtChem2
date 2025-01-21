! -----------------------------------------------------------------------------
!
! Copyright (c) 2017 Sam Cox, Roberto Sommariva
!
! This file is part of the AtChem2 software package.
!
! This file is covered by the MIT license which can be found in the file
! LICENSE.md at the top level of the AtChem2 distribution.
!
! -----------------------------------------------------------------------------

module config_test
  use fruit
  use types_mod
  implicit none

contains

  subroutine test_matchNameToNumber
    use types_mod
    use config_functions_mod, only : matchNameToNumber
    use storage_mod, only : maxSpecLength

    character(len=maxSpecLength) :: masterSpeciesList(10), testSpeciesList(6)
    character(len=35) :: testNumber
    integer(kind=NPI) :: returnArray(6), answerArray(6), i

    masterSpeciesList = (/ ' CO', 'NO2', ' O3', ' O2', 'H2O', ' OH', 'CO2', ' C6', '  K', ' Pb' /)
    testSpeciesList = (/ ' O3', 'NO2', ' CO', ' C6', 'H2O', ' Pb' /)

    answerArray = (/ 3, 2, 1, 8, 5, 10/)

    call matchNameToNumber( masterSpeciesList, testSpeciesList, returnArray )
    ! loop over each element, checking that equality with answerArray holds
    do i = 1, 6
        write (testNumber,'(A31, I1)') "test_matchNameToNumber element ", i
        call assert_true( returnArray(i) == answerArray(i), testNumber )
    enddo
  end subroutine test_matchNameToNumber

  subroutine test_getIndexWithinList
    use types_mod
    use config_functions_mod, only : getIndexWithinList

    character(len=2) :: list(5)

    list = (/ 'a', 'b', 'c', 'd', 'e' /)

    call assert_true(getIndexWithinList( list, 'a' ) == 1_NPI, "getIndexWithinList, a = 1")
    call assert_true(getIndexWithinList( list, 'e' ) == 5_NPI, "getIndexWithinList, e = 5")
    ! Any items not in the list should return 0
    call assert_true(getIndexWithinList( list, 'f' ) == 0_NPI, "getIndexWithinList, f = 0")
    call assert_true(getIndexWithinList( list, 'z' ) == 0_NPI, "getIndexWithinList, z = 0")
    call assert_true(getIndexWithinList( list, ' 0' ) == 0_NPI, "getIndexWithinList,  0 = 0")
  end subroutine test_getIndexWithinList

  subroutine test_findReactionsWithProductOrReactant
    use types_mod
    use config_functions_mod, only : findReactionsWithProductOrReactant

    integer(kind=NPI) :: speciesList(3), chs(2, 7), lens(3), i, j, lenAnswer(3)
    real(kind=DP) :: ccoeff(7)
    type(reaction_frequency_pair) :: r(3, 10), rAnswer(3, 10)

    do i = 1, size( r, 1 )
      do j = 1, size( r, 2 )
        r(i,j)%reaction = 0_NPI
        rAnswer(i, j)%reaction = 0_NPI
        r(i,j)%frequency = 0_NPI
        rAnswer(i, j)%frequency = 0_NPI
      enddo
    enddo
    speciesList = (/ 3_NPI, 2_NPI, 1_NPI /)
    !      reaction number, species number
    chs = reshape((/ 1_NPI, 3_NPI, &
                     1_NPI, 4_NPI, &
                     2_NPI, 1_NPI, &
                     2_NPI, 2_NPI, &
                     2_NPI, 2_NPI, &
                     2_NPI, 3_NPI, &
                     3_NPI, 2_NPI /), (/ size( chs, 1 ), size( chs, 2 )/))
    lenAnswer = (/ 2_NPI, 2_NPI, 1_NPI /)
    ! species 3 occurs in reaction 1 once
    rAnswer(1,1)%reaction = 1_NPI
    rAnswer(1,1)%frequency = 1_NPI
    ! species 3 occurs in reaction 2 once
    rAnswer(1,2)%reaction = 2_NPI
    rAnswer(1,2)%frequency = 1_NPI
    ! species 2 occurs in reaction 2 twice
    rAnswer(2,1)%reaction = 2_NPI
    rAnswer(2,1)%frequency = 2_NPI
    ! species 2 occurs in reaction 3 once
    rAnswer(2,2)%reaction = 3_NPI
    rAnswer(2,2)%frequency = 1_NPI
    ! species 1 occurs in reaction 2 once
    rAnswer(3,1)%reaction = 2_NPI
    rAnswer(3,1)%frequency = 1_NPI

    call findReactionsWithProductOrReactant(speciesList,chs,ccoeff,r,lens)
    do i = 1_NPI, size( lenAnswer )
      call assert_true( lenAnswer(i) == lens(i), "test_findReactionsWithProductOrReactant length" )
    enddo

    do i = 1_NPI, size( r, 1 )
      do j = 1_NPI, size( r, 2 )
        call assert_true( rAnswer(i,j) == r(i,j), "test_findReactionsWithProductOrReactant r" )
      enddo
    enddo
  end subroutine test_findReactionsWithProductOrReactant

  subroutine test_getSubsetOfConcs
    use types_mod
    use config_functions_mod, only : getSubsetOfConcs
    use species_mod, only : setSpeciesList, setNumberOfSpecies
    use storage_mod, only : maxSpecLength

    character(len=maxSpecLength) :: fullListOfSpecies(10), speciesSubset(6)
    real(kind=DP) :: fullListOfConcs(10), concsSubset(6), concsAnswer(6)
    integer(kind=NPI) :: i

    ! Set up the environment of species
    fullListOfSpecies = (/ ' CO', 'NO2', ' O3', ' O2', 'H2O', ' OH', 'CO2', ' C6', '  K', ' Pb' /)
    call setNumberOfSpecies( 10_NPI )
    call setSpeciesList( fullListOfSpecies )

    ! Pass in the full list of cncentrations, and those we're interest in
    fullListOfConcs = (/ 0.1_DP, 0.2_DP, 0.5_DP, 0.7_DP, 1.0_DP, 1.3_DP, 1.5_DP, 1.7_DP, 1.9_DP, 2.0_DP /)
    speciesSubset = (/ ' O3', 'NO2', ' CO', ' C6', 'H2O', ' Pb' /)
    concsSubset = getSubsetOfConcs( fullListOfConcs, speciesSubset )

    ! The function should return the subset of concentrations for the given species
    concsAnswer = (/ 0.5_DP, 0.2_DP, 0.1_DP, 1.7_DP, 1.0_DP, 2.0_DP /)

    do i = 1_NPI, size( speciesSubset )
      call assert_true( concsSubset(i) == concsAnswer(i), "test_getSubsetOfConcs" )
    enddo
  end subroutine test_getSubsetOfConcs

end module config_test
