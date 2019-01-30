
module helper_routines
contains
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
  do i=1,size(array)
          write(*,*) 'loop'
          write(*,*) i, ' ', trim(array(i)), ' ', trim(value)
          write(*,*) i, ' ', array(i), ' ', value
    if ( trim(array(i)) == trim(value) ) then
      index = i
      write(*,*) 'found', i, index
      return
    end if
  end do
  index = 0
  write(*,*) 'not found', index
  return
end function array_contains

end module helper_routines











PROGRAM CLI

  use, intrinsic :: iso_fortran_env, only : stderr => error_unit
  use helper_routines
  implicit none

  integer :: cmd_arg_count, i, loc
  character(len=100), allocatable :: input_strings(:), names(:), values(:)
  character(len=100) :: model_dir, output_dir, reactionRates_dir, configuration_dir, constraints_dir, &
                        spec_constraints_dir, env_constraints_dir, photo_constraints_dir, &
                        help_name, model_name, output_name, reactionRates_name, configuration_name, &
                        constraints_name, spec_constraints_name, env_constraints_name, photo_constraints_name
  character(len=100) :: param_dir
  logical, allocatable :: names_valid(:), values_valid(:)
  logical :: all_valid

  cmd_arg_count = command_argument_count()
  if ( cmd_arg_count > 0 ) then
    allocate(input_strings(cmd_arg_count), names(cmd_arg_count), values(cmd_arg_count), &
             names_valid(cmd_arg_count), values_valid(cmd_arg_count))
    do i=1,cmd_arg_count
      call get_command_argument( i, input_strings(i) )
    end do
  end if
  if ( cmd_arg_count > 0 ) then
    call get_command_argument( 1, output_dir )
  else
    output_dir = "model/output"
  end if
  if ( cmd_arg_count > 1 ) then
    call get_command_argument( 2, reactionRates_dir )
  else
    reactionRates_dir = "model/output/reactionRates"
  end if
  if ( cmd_arg_count > 2 ) then
    call get_command_argument( 3, param_dir )
  else
    param_dir = "model/configuration"
  end if

  write(*,*) output_dir
  write(*,*) reactionRates_dir
  write(*,*) param_dir
  do i=1,cmd_arg_count
    write(*,*) trim(input_strings(i))
  end do
  write(*,*) size(input_strings)
  write(*,*)
  do i=1,cmd_arg_count
    write(*,*) i, input_strings(i)
    call split_string( input_strings(i), names(i), values(i), '=')
    write(*,*) trim(names(i))
    write(*,*) trim(values(i))
    call check_name_value_pair_validity(names(i), values(i), names_valid(i), values_valid(i))
    write(*,*) names_valid(i), values_valid(i)
    write(*,*)
  end do
  all_valid = .true.
  do i=1,cmd_arg_count
    if ( names_valid(i) .neqv. .true. ) then
      all_valid = .false.
      write(*,*) 'name number ', i, ', "', trim(names(i)), '" is not valid when paired with value "', trim(values(i)), '"'
    end if
    if ( values_valid(i) .neqv. .true. ) then
      all_valid = .false.
      write(*,*) 'value number ', i, ', "', trim(values(i)), '" is not valid when paired with name "', trim(names(i)), '"'
    end if
  end do
  if (all_valid) then
    write(*,*) 'ALL VALID - CONTINUE'
  else
    write(*,*) 'NOT ALL VALID - PRINT HELP TEXT'
    stop
  end if

  help_name = '--help'
  model_name = '--model'
  output_name = '--output'
  reactionRates_name = '--reactionRates'
  configuration_name = '--configuration'
  constraints_name = '--constraints'
  spec_constraints_name = '--spec_constraints'
  env_constraints_name = '--env_constraints'
  photo_constraints_name = '--photo_constraints'

  loc = array_contains(names, help_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          write(*,*) 'help called'
          write(*,*) "HERE IS THE HELP TEXT: NOW I'M GOING TO QUIT"
          stop
  else
          write(*,*) 'help NOT called'
          write(*,*) 'continue with checking that the supplied arguments are valid'
  end if
  loc = array_contains(names, model_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          model_dir = values(loc)
  else
          model_dir = 'model'
  end if
  loc = array_contains(names, output_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          output_dir = values(loc)
  else
          output_dir = trim(model_dir)//'/output'
  end if
  loc = array_contains(names, reactionRates_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          reactionRates_dir = values(loc)
  else
          reactionRates_dir = trim(output_dir)//'/reactionRates'
  end if
  loc = array_contains(names, configuration_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          configuration_dir = values(loc)
  else
          configuration_dir = trim(model_dir)//'/configuration'
  end if
  loc = array_contains(names, constraints_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          constraints_dir = values(loc)
  else
          constraints_dir = trim(model_dir)//'/constraints'
  end if
  loc = array_contains(names, spec_constraints_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          spec_constraints_dir = values(loc)
  else
          spec_constraints_dir = trim(constraints_dir)//'/species'
  end if
  loc = array_contains(names, env_constraints_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          env_constraints_dir = values(loc)
  else
          env_constraints_dir = trim(constraints_dir)//'/environment'
  end if
  loc = array_contains(names, photo_constraints_name)
  write(*,*) loc
  if ( loc /= 0 ) then
          photo_constraints_dir = values(loc)
  else
          photo_constraints_dir = trim(constraints_dir)//'/photolysis'
  end if

  write(*,*) 'model dir:                  ', model_dir
  write(*,*) 'output dir:                 ', output_dir
  write(*,*) 'reactionRates dir:          ', reactionRates_dir
  write(*,*) 'configuration dir:          ', configuration_dir
  write(*,*) 'constraints dir:            ', constraints_dir
  write(*,*) 'species constraints dir:    ', spec_constraints_dir
  write(*,*) 'environment constraints dir:', env_constraints_dir
  write(*,*) 'photolysis constraints dir: ', photo_constraints_dir

END PROGRAM CLI
