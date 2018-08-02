module date_test
  use fruit
  use types_mod
  use date_mod
  implicit none

contains

  subroutine test_isLeapYear
    use types_mod
    implicit none

    call assert_true( isLeapYear(2004_DI) ,  "2004 is a leap year" )
    call assert_true( isLeapYear(2000_DI) ,  "2000 is a leap year" )
    call assert_true( isLeapYear(1996_DI) ,  "1996 is a leap year" )
    call assert_false( isLeapYear(2001_DI) , "2001 is not a leap year" )
    call assert_true( isLeapYear(1600_DI) ,  "1600 is a leap year" )
    call assert_false( isLeapYear(1900_DI) , "1900 is not a leap year" )

  end subroutine test_isLeapYear

  subroutine test_applyLeapDay
    use types_mod
    implicit none
    integer(kind=DI) :: monthList(12)

    monthList = refMonthList
    call applyLeapDay( monthList, 2004_DI)
    call assert_true( sum(monthList) == sum(refMonthList) + 1 , "apply leap day to 2004" )
    monthList = refMonthList
    call applyLeapDay( monthList, 2000_DI)
    call assert_true( sum(monthList) == sum(refMonthList) + 1 , "apply leap day to 2000" )
    monthList = refMonthList
    call applyLeapDay( monthList, 1996_DI)
    call assert_true( sum(monthList) == sum(refMonthList) + 1 , "apply leap day to 1996" )
    monthList = refMonthList
    call applyLeapDay( monthList, 2001_DI)
    call assert_true( sum(monthList) == sum(refMonthList) , "do not apply leap day to 2001" )
    monthList = refMonthList
    call applyLeapDay( monthList, 1600_DI)
    call assert_true( sum(monthList) == sum(refMonthList) + 1 , "apply leap day to 1600" )
    monthList = refMonthList
    call applyLeapDay( monthList, 1900_DI)
    call assert_true( sum(monthList) == sum(refMonthList) , "do not apply leap day to 1900" )

  end subroutine test_applyLeapDay

  subroutine test_calcDayOfYear
    use types_mod
    implicit none
    integer(kind=DI) :: monthList(12)

    monthList = refMonthList
    call applyLeapDay( monthList, 2004_DI)
    call assert_true( calcDayOfYear(monthList, 1_DI, 1_DI) == 0_DI , "Jan 1 2004 is day 0" )
    monthList = refMonthList
    call applyLeapDay( monthList, 2004_DI)
    call assert_true( calcDayOfYear(monthList, 12_DI, 31_DI) == 365_DI , "Dec 31 1 2004 is day 365" )
    monthList = refMonthList
    call applyLeapDay( monthList, 2004_DI)
    call assert_true( calcDayOfYear(monthList, 6_DI, 12_DI) == 163_DI , "June 12 2004 is day 163" )
    monthList = refMonthList
    call applyLeapDay( monthList, 2001_DI)
    call assert_true( calcDayOfYear(monthList, 1_DI, 1_DI) == 0_DI , "Jan 1 2001 is day 0" )
    monthList = refMonthList
    call applyLeapDay( monthList, 2001_DI)
    call assert_true( calcDayOfYear(monthList, 12_DI, 31_DI) == 364_DI , "Dec 31 1 2001 is day 364" )
    monthList = refMonthList
    call applyLeapDay( monthList, 2001_DI)
    call assert_true( calcDayOfYear(monthList, 6_DI, 12_DI) == 162_DI , "June 12 2001 is day 162" )

  end subroutine test_calcDayOfYear

  subroutine test_calcInitialDateParameters
    use types_mod
    implicit none

    startDay = 1_DI
    startMonth = 1_DI
    startYear = 2004_DI
    call calcInitialDateParameters()
    call assert_true( startDayOfYear == 0_DI , "Initial Jan 1 2004 is day 0")

    startDay = 31_DI
    startMonth = 12_DI
    startYear = 2004_DI
    call calcInitialDateParameters()
    call assert_true( startDayOfYear == 365_DI , "Initial Dec 31 2004 is day 365")

    startDay = 12_DI
    startMonth = 6_DI
    startYear = 2004_DI
    call calcInitialDateParameters()
    call assert_true( startDayOfYear == 163 , "Initial June 12 2004 is day 163")

    startDay = 1_DI
    startMonth = 1_DI
    startYear = 2001_DI
    call calcInitialDateParameters()
    call assert_true( startDayOfYear == 0_DI , "Initial Jan 1 2001 is day 0")

    startDay = 31_DI
    startMonth = 12_DI
    startYear = 2001_DI
    call calcInitialDateParameters()
    call assert_true( startDayOfYear == 364_DI , "Initial Dec 31 2001 is day 364")

    startDay = 12_DI
    startMonth = 6_DI
    startYear = 2001_DI
    call calcInitialDateParameters()
    call assert_true( startDayOfYear == 162 , "Initial June 12 2001 is day 162")

  end subroutine test_calcInitialDateParameters

  subroutine test_calcCurrentDateParameters
    use types_mod
    implicit none

    ! early in the day
    startDay = 1_DI
    startMonth = 1_DI
    startYear = 2004_DI
    call calcCurrentDateParameters( 10.0_DP )
    call assert_true( currentDayOfYear == 0_DI , "Current day of year, starting at Jan 1 2004, with 10 seconds, is day 0")
    call assert_true( currentMonth == 1_DI , "Current month, starting at Jan 1 2004, with 10 seconds, is month 1")
    call assert_true( currentDayOfMonth == 1_DI , "Current day of month, starting at Jan 1 2004, with 10 seconds, is day 1")

    ! last second in the same day
    call calcCurrentDateParameters( 86399.0_DP )
    call assert_true( currentDayOfYear == 0_DI , "Current day of year, starting at Jan 1 2004, with 86399 seconds, is day 0")
    call assert_true( currentMonth == 1_DI , "Current month, starting at Jan 1 2004, with 86399 seconds, is month 1")
    call assert_true( currentDayOfMonth == 1_DI , "Current day of month, starting at Jan 1 2004, with 86399 seconds, is day 1")

    ! first second in the next day
    call calcCurrentDateParameters( 86400.0_DP )
    call assert_true( currentDayOfYear == 1_DI , "Current day of year, starting at Jan 1 2004, with 86400 seconds, is day 1")
    call assert_true( currentMonth == 1_DI , "Current month, starting at Jan 1 2004, with 86400 seconds, is month 1")
    call assert_true( currentDayOfMonth == 2_DI , "Current day of month, starting at Jan 1 2004, with 86400 seconds, is day 2")

    ! last second of a day after leap day
    startDay = 1_DI
    startMonth = 1_DI
    startYear = 2004_DI
    call calcCurrentDateParameters( 163.0_DP * 86400.0_DP-1 )
    call assert_true( currentDayOfYear == 162_DI , "Current day of year, starting at Jan 1 2004, &
    with 163*86400-1 seconds, is day 162")
    call assert_true( currentMonth == 6_DI , "Current month, starting at Jan 1 2004, with 163*86400-1 seconds, is month 6")
    call assert_true( currentDayOfMonth == 11_DI , "Current day of month, starting at Jan 1 2004, &
    with 163*86400-1 seconds, is day 11")

    ! first second of a day after leap day
    call calcCurrentDateParameters( 163.0_DP * 86400.0_DP )
    call assert_true( currentDayOfYear == 163_DI , "Current day of year, starting at Jan 1 2004, &
    with 163*86400 seconds, is day 163")
    call assert_true( currentMonth == 6_DI , "Current month, starting at Jan 1 2004, with 163*86400 seconds, is month 6")
    call assert_true( currentDayOfMonth == 12_DI , "Current day of month, starting at Jan 1 2004, &
    with 163*86400 seconds, is day 12")

    ! last second of a day after non-existent leap day
    startDay = 1_DI
    startMonth = 1_DI
    startYear = 2001_DI
    call calcCurrentDateParameters( 163.0_DP * 86400.0_DP-1 )
    call assert_true( currentDayOfYear == 162_DI , "Current day of year, starting at Jan 1 2001, &
    with 163*86400-1 seconds, is day 162")
    call assert_true( currentMonth == 6_DI , "Current month, starting at Jan 1 2001, with 163*86400-1 seconds, is month 6")
    call assert_true( currentDayOfMonth == 12_DI , "Current day of month, starting at Jan 1 2001, &
    with 163*86400-1 seconds, is day 12")

    ! first second of a day after non-existent leap day
    call calcCurrentDateParameters( 163.0_DP * 86400.0_DP )
    call assert_true( currentDayOfYear == 163_DI , "Current day of year, starting at Jan 1 2001, &
    with 163*86400 seconds, is day 163")
    call assert_true( currentMonth == 6_DI , "Current month, starting at Jan 1 2001, with 163*86400 seconds, is month 6")
    call assert_true( currentDayOfMonth == 13_DI , "Current day of month, starting at Jan 1 2001, &
    with 163*86400 seconds, is day 13")

    ! last second of a day after leap day with different start day
    startDay = 12_DI
    startMonth = 2_DI
    startYear = 2004_DI
    call calcCurrentDateParameters( 163.0_DP * 86400.0_DP-1 )
    call assert_true( currentDayOfYear == 204_DI , "Current day of year, starting at June 12 2004, &
    with 163*86400-1 seconds, is day 162")
    call assert_true( currentMonth == 7_DI , "Current month, starting at June 12 2004, with 163*86400-1 seconds, is month 6")
    call assert_true( currentDayOfMonth == 23_DI , "Current day of month, starting at June 12 2004, &
    with 163*86400-1 seconds, is day 11")

    ! first second of a day after leap day with different start day
    call calcCurrentDateParameters( 163.0_DP * 86400.0_DP )
    call assert_true( currentDayOfYear == 205_DI , "Current day of year, starting at Feb 12 2004, &
    with 163*86400 seconds, is day 163")
    call assert_true( currentMonth == 7_DI , "Current month, starting at Feb 12 2004, with 163*86400 seconds, is month 6")
    call assert_true( currentDayOfMonth == 24_DI , "Current day of month, starting at Feb 12 2004, &
    with 163*86400 seconds, is day 12")

    ! last second of a day after non-existent leap day with different start day
    startDay = 12_DI
    startMonth = 2_DI
    startYear = 2001_DI
    call calcCurrentDateParameters( 163.0_DP * 86400.0_DP-1 )
    call assert_true( currentDayOfYear == 204_DI , "Current day of year, starting at Feb 12 2001, &
    with 163*86400-1 seconds, is day 162")
    call assert_true( currentMonth == 7_DI , "Current month, starting at Feb 12 2001, with 163*86400-1 seconds, is month 6")
    call assert_true( currentDayOfMonth == 24_DI , "Current day of month, starting at Feb 12 2001, &
    with 163*86400-1 seconds, is day 12")

    ! first second of a day after non-existent leap day with different start day
    call calcCurrentDateParameters( 163.0_DP * 86400.0_DP )
    call assert_true( currentDayOfYear == 205_DI , "Current day of year, starting at Feb 12 2001, &
    with 163*86400 seconds, is day 163")
    call assert_true( currentMonth == 7_DI , "Current month, starting at Feb 12 2001, with 163*86400 seconds, is month 6")
    call assert_true( currentDayOfMonth == 25_DI , "Current day of month, starting at Feb 12 2001, &
    with 163*86400 seconds, is day 13")

    startDay = 31_DI
    startMonth = 12_DI
    startYear = 2001_DI
    call calcCurrentDateParameters( 1.0_DP * 86400.0_DP )
    call assert_true( currentYear == 2002_DI , "Current year, starting at Dec 31 2001, &
    with 1*86400 seconds, is 2002")
    call calcCurrentDateParameters( 1.0_DP * 86400.0_DP-1.0_DP)
    call assert_true( currentYear == 2001_DI , "Current year, starting at Dec 31 2001, &
    with 1*86400-1 seconds, is 2001")
    call calcCurrentDateParameters( (1.0_DP + 365.0_DP) * 86400.0_DP )
    call assert_true( currentYear == 2003_DI , "Current year, starting at Dec 31 2001, &
    with (1+365)*86400 seconds, is 2003")
    call calcCurrentDateParameters( (1.0_DP + 365.0_DP) * 86400.0_DP-1.0_DP)
    call assert_true( currentYear == 2002_DI , "Current year, starting at Dec 31 2001, &
    with (1+365)*86400-1 seconds, is 2002")
    call calcCurrentDateParameters( (1.0_DP + 365.0_DP + 365.0_DP) * 86400.0_DP )
    call assert_true( currentYear == 2004_DI , "Current year, starting at Dec 31 2001, &
    with (1+365+365)*86400 seconds, is 2004")
    call calcCurrentDateParameters( (1.0_DP + 365.0_DP + 365.0_DP) * 86400.0_DP-1.0_DP)
    call assert_true( currentYear == 2003_DI , "Current year, starting at Dec 31 2001, &
    with (1+365+365)*86400-1 seconds, is 2003")
    call calcCurrentDateParameters( (1.0_DP + 365.0_DP + 365.0_DP + 366.0_DP) * 86400.0_DP )
    call assert_true( currentYear == 2005_DI , "Current year, starting at Dec 31 2001, &
    with (1+365+365+366)*86400 seconds, is 2005")
    call calcCurrentDateParameters( (1.0_DP + 365.0_DP + 365.0_DP + 366.0_DP) * 86400.0_DP-1.0_DP)
    call assert_true( currentYear == 2004_DI , "Current year, starting at Dec 31 2001, &
    with (1+365+365+366*86400-1 seconds, is 2004")

  end subroutine test_calcCurrentDateParameters


end module date_test
