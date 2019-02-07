module argparse_test
  use fruit
  use types_mod
  implicit none

contains

  subroutine test_split_string
    use types_mod
    use argparse_mod
    implicit none

    character(100) :: string_in, string_out1, string_out2

    string_in = 'this=that'
    call split_string( string_in, string_out1, string_out2, '=' )

    call assert_true( trim(string_out1) == 'this', "split_string this=that 1" )
    call assert_true( trim(string_out2) == 'that', "split_string this=that 2" )

    string_in = 'this!that'
    call split_string( string_in, string_out1, string_out2, '=' )

    call assert_true( trim(string_out1) == 'this!that', "split_string this!that 1" )
    call assert_true( trim(string_out2) == '', "split_string this!that 2" )

    string_in = 'this!that='
    call split_string( string_in, string_out1, string_out2, '=' )

    call assert_true( trim(string_out1) == 'this!that', "split_string this!that= 1" )
    call assert_true( trim(string_out2) == '', "split_string this!that= 2" )

    string_in = '=this!that'
    call split_string( string_in, string_out1, string_out2, '=' )

    call assert_true( trim(string_out1) == '', "split_string =this!that 1" )
    call assert_true( trim(string_out2) == 'this!that', "split_string =this!that 2" )

  end subroutine test_split_string

  subroutine test_check_name_value_pair_validity
    use types_mod
    use argparse_mod
    implicit none

    logical :: name_valid, value_valid

    call check_name_value_pair_validity( '--test', 'hello', name_valid, value_valid )

    call assert_true( name_valid .eqv. .true., "check_name_value_pair_validity --test=hello name")
    call assert_true( value_valid .eqv. .true., "check_name_value_pair_validity --test=hello value")

    call check_name_value_pair_validity( '-test', 'hello', name_valid, value_valid )

    call assert_true( name_valid .eqv. .false., "check_name_value_pair_validity -test=hello name")
    call assert_true( value_valid .eqv. .true., "check_name_value_pair_validity -test=hello value")

    call check_name_value_pair_validity( '--test', '', name_valid, value_valid )

    call assert_true( name_valid .eqv. .true., "check_name_value_pair_validity --test= name")
    call assert_true( value_valid .eqv. .false., "check_name_value_pair_validity --test= value")

    call check_name_value_pair_validity( '-test', ' ', name_valid, value_valid )

    call assert_true( name_valid .eqv. .false., "check_name_value_pair_validity -test=  name")
    call assert_true( value_valid .eqv. .false., "check_name_value_pair_validity -test=  value")

    call check_name_value_pair_validity( '--help', '', name_valid, value_valid )

    call assert_true( name_valid .eqv. .true., "check_name_value_pair_validity --help name")
    call assert_true( value_valid .eqv. .true., "check_name_value_pair_validity --help value")

    call check_name_value_pair_validity( '--help', 'test', name_valid, value_valid )

    call assert_true( name_valid .eqv. .true., "check_name_value_pair_validity --help=test name")
    call assert_true( value_valid .eqv. .true., "check_name_value_pair_validity --help=test value")

  end subroutine test_check_name_value_pair_validity

end module argparse_test
