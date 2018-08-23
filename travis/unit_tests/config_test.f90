module config_test
  use fruit
  use types_mod
  implicit none

contains

  subroutine test_matchNameToNumber
    use types_mod
    use config_functions_mod, only : matchNameToNumber
    use storage_mod, only : maxSpecLength
    implicit none

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
    implicit none

    character(len=2) :: list(5)

    list = (/ 'a', 'b', 'c', 'd', 'e' /)

    call assert_true(getIndexWithinList( list, 'a' ) == 1_NPI, "getIndexWithinList, a = 1")
    call assert_true(getIndexWithinList( list, 'e' ) == 5_NPI, "getIndexWithinList, e = 5")
    call assert_true(getIndexWithinList( list, 'f' ) == 0_NPI, "getIndexWithinList, f = 0")
    call assert_true(getIndexWithinList( list, 'z' ) == 0_NPI, "getIndexWithinList, z = 0")
    call assert_true(getIndexWithinList( list, ' 0' ) == 0_NPI, "getIndexWithinList,  0 = 0")

  end subroutine test_getIndexWithinList

  subroutine test_findReactionsWithProductOrReactant
    use types_mod
    use config_functions_mod, only : findReactionsWithProductOrReactant
    implicit none

    integer(kind=NPI) :: speciesList(3), chs(2, 6), r(3, 10), lens(3), i, j, lenAnswer(3), rAnswer(3, 10)

    do i = 1, size( r, 1 )
      do j = 1, size( r, 2 )
        r(i,j) = 0_NPI
        rAnswer(i, j) = 0_NPI
      enddo
    enddo
    speciesList = (/ 3_NPI, 2_NPI, 1_NPI /)
    !      reaction number, species number
    chs = reshape((/ 1_NPI, 3_NPI, &
                     1_NPI, 4_NPI, &
                     2_NPI, 1_NPI, &
                     2_NPI, 2_NPI, &
                     2_NPI, 3_NPI, &
                     3_NPI, 2_NPI /), (/ size( chs, 1 ), size( chs, 2 )/))
    lenAnswer = (/ 2_NPI, 2_NPI, 1_NPI /)
    rAnswer(1,1) = 1_NPI
    rAnswer(1,2) = 2_NPI
    rAnswer(2,1) = 2_NPI
    rAnswer(2,2) = 3_NPI
    rAnswer(3,1) = 2_NPI

    call findReactionsWithProductOrReactant(speciesList,r,chs,lens)
    do i = 1_NPI, size( lenAnswer )
      call assert_true( lenAnswer(i) == lens(i), "test_findReactionsWithProductOrReactant length" )
    enddo

    do i = 1_NPI, size( r, 1 )
      do j = 1_NPI, size( r, 2 )
        call assert_true( rAnswer(i,j) == r(i,j), "test_findReactionsWithProductOrReactant r" )
      enddo
    enddo

  end subroutine test_findReactionsWithProductOrReactant
end module config_test
