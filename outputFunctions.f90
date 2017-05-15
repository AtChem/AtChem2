module outputFunctions_mod
  use types_mod
contains
  subroutine ro2Sum( ro2, y )
    implicit none

    real(kind=DP) :: ro2
    real(kind=DP), intent(in) :: y(*)

    ro2 = 0.00e+00
  end subroutine ro2Sum

  subroutine outputEnvVar( t )
    use envVars
    implicit none

    integer(kind=NPI) :: i
    real(kind=DP), intent(in) :: t

    if ( ro2 < 0 ) ro2 = 0.0
    write (52,*) t, (currentEnvVarValues(i), i = 1, numEnvVars), ro2

    return
  end subroutine outputEnvVar

  !--------------------------------------------------------------------
  subroutine output_jfy( fy, t )
    implicit none

    real(kind=DP), intent(in) :: fy(:,:), t
    integer(kind=NPI) :: i, j

    if ( size( fy, 1 ) /= size( fy, 2 ) ) then
      stop "size( fy, 1 ) /= size( fy, 2 ) in output_jfy()."
    end if
    ! Loop over all elements of fy, and print to jacobian.output, prefixed by t
    do i = 1, size( fy, 1)
      write (55, '(100 (1x, e12.5)) ') t, (fy(i, j), j = 1, size( fy, 1))
    end do
    write (55,*) '---------------'
  end subroutine output_jfy

  !     ---------------------------------------------------------------
  subroutine outputPhotolysisRates( t )
    use photolysisRates, only : nrOfPhotoRates, ck, j
    implicit none

    real(kind=DP), intent(in) :: t
    integer(kind=NPI) :: i

    write (58, '(100 (1x, e12.5)) ') t, (j(ck(i)), i = 1, nrOfPhotoRates)
    return
  end subroutine outputPhotolysisRates

  !     ---------------------------------------------------------------
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

  !     ---------------------------------------------------------------
  subroutine getReaction( speciesNames, reactionNumber, reaction )
    ! Given a list speciesNames, and an integer reactionNumber, return reaction,
    ! a string containing
    use reactionStructure
    use storage, only : maxSpecLength, maxReactionStringLength
    implicit none

    character(len=maxSpecLength) :: reactants(10), products(10)
    character(len=maxSpecLength), intent(in) :: speciesNames(*)
    integer(kind=NPI) :: i, numReactants, numProducts
    integer(kind=NPI), intent(in) :: reactionNumber
    character(len=maxReactionStringLength) :: reactantStr, productStr
    character(len=maxReactionStringLength), intent(out) :: reaction

    ! Loop over reactants, and copy the reactant name for any reactant used in
    ! reaction reactionNumber. use numReactants as a counter of the number of reactants.
    ! String these together with '+', and append a '='
    numReactants = 0
    do i = 1, lhs_size
      if ( clhs(1, i) == reactionNumber ) then
        numReactants = numReactants + 1
        reactants(numReactants) = speciesNames(clhs(2, i))
      end if
    end do

    reactantStr = ' '
    do i = 1, numReactants
      reactantStr = trim( adjustl( trim( reactantStr ) // trim( reactants(i) ) ) )
      if ( i < numReactants ) then
        reactantStr = trim( reactantStr ) // '+'
      end if
    end do
    reactantStr = trim( reactantStr ) // '='

    ! Loop over products, and copy the product name for any product created in
    ! reaction reactionNumber. use numProducts as a counter of the number of products.
    ! String these together with '+', and append this to reactantStr. Save the
    ! result in reaction, which is returned
    numProducts = 0
    do i = 1, rhs_size
      if ( crhs(1, i) == reactionNumber ) then
        numProducts = numProducts + 1
        products(numProducts) = speciesNames(crhs(2, i))
      end if
    end do

    productStr = ' '
    do i = 1, numProducts
      productStr = trim( adjustl( trim( productStr ) // trim( products(i) ) ) )
      if ( i < numProducts ) then
        productStr = trim( productStr ) // '+'
      end if
    end do

    reaction = trim( reactantStr ) // trim( productStr )

    return
  end subroutine getReaction

  subroutine outputRates( r, arrayLen, t, p, flag, speciesNames )
    use reactionStructure
    use storage, only : maxSpecLength, maxReactionStringLength
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    implicit none

    integer(kind=NPI), intent(in) :: r(:,:), arrayLen(:)
    real(kind=DP), intent(in) :: t, p(:)
    integer, intent(in) :: flag
    character(len=maxSpecLength), intent(in) :: speciesNames(:)
    integer(kind=NPI) :: i, j
    character(len=maxReactionStringLength) :: reaction

    if ( size( r, 1 ) /= size( arrayLen ) ) then
      stop "size( r, 1 ) /= size( arrayLen ) in outputRates()."
    end if
    do i = 1, size( arrayLen )
      if ( arrayLen(i) > size( r, 2 ) ) then
        write (stderr,*) "arrayLen(i) > size( r, 2 ) in outputRates(). i = ", i
        stop
      end if
      do j = 2, arrayLen(i)
        if ( r(i, j) /= -1 ) then

          call getReaction( speciesNames, r(i, j), reaction )
          ! Flag = 0 for reaction, 1 for loss
          if ( flag == 0 ) then
            write (56,*) t, ' ', r(i, 1), ' ', speciesNames(r(i, 1)), ' ', r(i, j), ' ', p(r(i, j)), ' ', trim( reaction )
          else
            if ( flag /= 1 ) then
              stop "Unexpected flag value to outputRates()"
            end if
            write (60,*) t, ' ', r(i, 1), ' ', speciesNames(r(i, 1)), ' ', r(i, j), ' ', p(r(i, j)), ' ', trim( reaction )
          end if
        end if
      end do
    end do
    return
  end subroutine outputRates

  subroutine outputInstantaneousRates( time, numReac )
    use reactionStructure
    use directories, only : instantaneousRates_dir
    use productionAndLossRates, only : ir
    use storage, only : maxFilepathLength
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    implicit none

    integer(kind=QI), intent(in) :: time
    integer(kind=NPI), intent(in) :: numReac
    integer(kind=NPI) :: i
    character(len=maxFilepathLength+30) :: irfileLocation
    character(len=30) :: strTime

    write (strTime,*) time

    irfileLocation = trim( instantaneousRates_dir ) // '/' // adjustl( strTime )

    open (10, file=irfileLocation)
    do i = 1, numReac
      write (10,*) ir(i)
    end do
    close (10, status='keep')


    return
  end subroutine outputInstantaneousRates

  subroutine outputSpeciesOutputRequired( t, arrayOfConcs )
    ! Print each element of arrayOfConcs, with size arrayOfConcsSize.
    ! If any concentration is negative, then set it to zero before printing.
    implicit none

    real(kind=DP), intent(in) :: t
    real(kind=DP), intent(inout) :: arrayOfConcs(:)
    integer(kind=NPI) :: i

    do i = 1, size( arrayOfConcs )
      if ( arrayOfConcs(i) < 0.0 ) then
        arrayOfConcs(i) = 0d0
      end if
    end do
    write (50, '(100 (1x, e15.5e3)) ') t, (arrayOfConcs(i), i = 1, size( arrayOfConcs ))
    return
  end subroutine outputSpeciesOutputRequired
end module outputFunctions_mod
