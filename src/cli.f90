
module helper_routines

  type flag
    character(100) :: flag_switch
    character(200) :: flag_help
  end type flag

  type(flag), parameter :: valid_flags(9) = &
              [ flag('--help',              'Displays this help message.'), &
                flag('--model',             'The base directory of the model.'), &
                flag('--output',            'The destination directory for output.'), &
                flag('--reactionRates', 'The destination directory for the reactionRates output.'), &
                flag('--configuration', 'The directory of the model configuration.'), &
                flag('--constraints', 'The base directory of constraints. ' // &
                                      'This typically contains 3 subdirectories: environment, photolysis, and species.'), &
                flag('--spec_constraints ', 'The directory containing species constraints data.'), &
                flag('--env_constraints  ', 'The directory containing environment constraints data.'), &
                flag('--photo_constraints', 'The directory containing photolysis constraints data.') ]

contains

  subroutine print_help()
    implicit none

    integer i

    write(*,*) 'This is the help message.'
    write(*,*) 'Possible input flags are: '
    do i=1,size(valid_flags)
      write(*,*) trim(valid_flags(i)%flag_switch)
      write(*,*) '     ', valid_flags(i)%flag_help
      write(*,*)
    end do
    write(*,*) 'End of help message.'
  end subroutine print_help
! split a string into 2 either side of a delimiter token
SUBROUTINE split_string(instring, string1, string2, delim)
  implicit none
  CHARACTER(100) :: instring
  CHARACTER :: delim
  CHARACTER(100),INTENT(OUT):: string1,string2
  INTEGER :: index

  instring = TRIM(instring)

  index = SCAN(instring,delim)
  if ( index /= 0 ) then
    if ( index == 1 ) then
      string1 = ''
      string2 = instring(2:)
else if ( index == len(instring) ) then
      string1 = instring(1:index-1)
      string2 = ''
    else
      string1 = instring(1:index-1)
      string2 = instring(index+1:)
  end if
  else
    if ( instring /= '--help' ) then
      write(*,*) 'delimiter not found'
    end if
    string1 = instring
    string2 = ''
  endif
  ! write(*,*) string1
  ! write(*,*) string2
END SUBROUTINE split_string

SUBROUTINE check_name_value_pair_validity(name, value, name_valid, value_valid)
  implicit none

  CHARACTER(100),INTENT(IN):: name, value
  LOGICAL, intent(out) :: name_valid, value_valid

  name_valid = .true.
  ! Check validity of name
  if ( index(name, '--', back=.true.) == 1 ) then
    write(*,*) 'name "', trim(name), '" is valid ', index(name, '--', back=.true.)
  else if  ( index(name, '--', back=.true.) == 0 ) then
    write(*,*) '-- does not occur in "', trim(name), '"'
    name_valid = .false.
  else
    write(*,*) 'name "', trim(name), '" is not valid - too many or incorrectly placed --s'
    name_valid = .false.
  end if

  value_valid = .true.
  ! Check validity of value - just check it hasn't got any spaces
  ! TODO: note that this can't really fail unless empty
  if ( len(trim(value)) == 0) then
    if ( trim(name) /= '--help' ) then
      write(*,*) 'value is empty'
      value_valid = .false.
    else
      write(*,*) 'value is empty, but that is ok as name == --help'
    end if
  end if
  if ( index(trim(value), ' ') /= 0 ) then
    write(*,*) 'value "', value, '" is invalid as it contains a space'
    value_valid = .false.
  else
    write(*,*) 'value "', trim(value), '" is valid'
  end if
  if ( name_valid .and. value_valid ) then
    write(*,*) 'pair is valid'
  else
    write(*,*) 'pair is not valid'
  end if
END SUBROUTINE check_name_value_pair_validity

function array_contains(array, value) result (index)
  implicit none
  character(100), intent(in) :: array(:), value
  integer :: index
  integer :: i

  ! write(*,*) size(array)
  ! write(*,*) value
  if ( size(array) > 0 ) then
    do i=1,size(array)
      ! write(*,*) 'loop'
      ! write(*,*) i, ' ', trim(array(i)), ' ', trim(value)
      ! write(*,*) i, ' ', array(i), ' ', value
      if ( trim(array(i)) == trim(value) ) then
        index = i
        ! write(*,*) 'found', i, index
        return
      end if
    end do
  end if
  index = 0
  ! write(*,*) 'not found', index
  return
end function array_contains

function flag_array_contains(array, value) result (index)
  implicit none
  type(flag), intent(in) :: array(:)
  character(100), intent(in) :: value
  integer :: index
  integer :: i

  ! write(*,*) size(array)
  ! write(*,*) value
  if ( size(array) > 0 ) then
    do i=1,size(array)
      ! write(*,*) 'loop'
      ! write(*,*) i, ' ', trim(array(i)), ' ', trim(value)
      ! write(*,*) i, ' ', array(i), ' ', value
      if ( trim(array(i)%flag_switch) == trim(value) ) then
        index = i
        ! write(*,*) 'found', i, index
        return
      end if
    end do
  end if
  index = 0
  ! write(*,*) 'not found', index
  return
end function flag_array_contains

function read_value_or_default( output_name, default, names, values ) result ( out )
  implicit none
  character(len=*), intent(in) :: output_name, default, names(:), values(:)
  character(len=100) :: out
  integer :: loc

  loc = array_contains(names, output_name)

  if ( loc /= 0 ) then
    out = trim(values(loc))
  else
    out = trim(default)
  end if

end function  read_value_or_default

end module helper_routines











PROGRAM CLI

  use, intrinsic :: iso_fortran_env, only : stderr => error_unit
  use helper_routines
  implicit none

  integer :: cmd_arg_count, i, loc
  character(len=100), allocatable :: input_strings(:), names(:), values(:)
  ! character(len=100) :: valid_flags(9)
  character(len=100) :: model_dir, output_dir, reactionRates_dir, configuration_dir, constraints_dir, &
                        spec_constraints_dir, env_constraints_dir, photo_constraints_dir
  logical, allocatable :: names_valid(:), values_valid(:)
  logical :: all_valid

  all_valid = .true.
  ! count possible arguments
  cmd_arg_count = command_argument_count()
  ! read in possible arguments
  allocate(input_strings(cmd_arg_count), names(cmd_arg_count), values(cmd_arg_count), &
           names_valid(cmd_arg_count), values_valid(cmd_arg_count))

  if ( cmd_arg_count > 0 ) then
    do i=1,cmd_arg_count
      call get_command_argument( i, input_strings(i) )
    end do

  ! parse arguments and check for naive validity
    do i=1,cmd_arg_count
      ! write(*,*) i, input_strings(i)
      call split_string( input_strings(i), names(i), values(i), '=')
      ! write(*,*) trim(names(i))
      ! write(*,*) trim(values(i))
      call check_name_value_pair_validity(names(i), values(i), names_valid(i), values_valid(i))
      ! write(*,*) names_valid(i), values_valid(i)
      ! write(*,*)
      if ( names_valid(i) .neqv. .true. ) then
        all_valid = .false.
        write(*,*) 'name number ', i, ', "', trim(names(i)), '" is not valid when paired with value "', trim(values(i)), '"'
      end if
      if ( values_valid(i) .neqv. .true. ) then
        all_valid = .false.
        write(*,*) 'value number ', i, ', "', trim(values(i)), '" is not valid when paired with name "', trim(names(i)), '"'
      end if
      if ( flag_array_contains(valid_flags, names(i)) == 0 ) then
        all_valid = .false.
        write(*,*) 'flag "', trim(names(i)) ,'" is not a valid flag.'
        call print_help()
        stop
      end if
    end do
  end if

  ! report back on validity
  if (all_valid) then
    write(*,*) 'ALL VALID - CONTINUE'
  else
    write(*,*) 'NOT ALL VALID'
    call print_help()
    stop
  end if

  ! check for existence of --help flag - if it exists, ignore all others, and print the help text
  loc = array_contains(names, valid_flags(1)%flag_switch)

  if ( loc /= 0 ) then
          write(*,*) 'help called'
          call print_help()
          stop
  else
          write(*,*) 'help NOT called'
          write(*,*) 'continue with checking that the supplied arguments are valid'
  end if

  ! set each of the directory locations from the command line, following the defined logic for defaults if some are not supplied
  model_dir             = read_value_or_default( valid_flags(2)%flag_switch, 'model',                               names, values )
  output_dir            = read_value_or_default( valid_flags(3)%flag_switch, trim(model_dir)//'/output',            names, values )
  reactionRates_dir     = read_value_or_default( valid_flags(4)%flag_switch, trim(output_dir)//'/reactionRates',    names, values )
  configuration_dir     = read_value_or_default( valid_flags(5)%flag_switch, trim(model_dir)//'/configuration',     names, values )
  constraints_dir       = read_value_or_default( valid_flags(6)%flag_switch, trim(model_dir)//'/constraints',       names, values )
  spec_constraints_dir  = read_value_or_default( valid_flags(7)%flag_switch, trim(constraints_dir)//'/species',     names, values )
  env_constraints_dir   = read_value_or_default( valid_flags(8)%flag_switch, trim(constraints_dir)//'/environment', names, values )
  photo_constraints_dir = read_value_or_default( valid_flags(9)%flag_switch, trim(constraints_dir)//'/photolysis',  names, values )

  write(*,*) 'model dir:                   ', model_dir
  write(*,*) 'output dir:                  ', output_dir
  write(*,*) 'reactionRates dir:           ', reactionRates_dir
  write(*,*) 'configuration dir:           ', configuration_dir
  write(*,*) 'constraints dir:             ', constraints_dir
  write(*,*) 'species constraints dir:     ', spec_constraints_dir
  write(*,*) 'environment constraints dir: ', env_constraints_dir
  write(*,*) 'photolysis constraints dir:  ', photo_constraints_dir

END PROGRAM CLI
