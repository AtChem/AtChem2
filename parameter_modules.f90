module model_params_mod
  use types_mod
  implicit none
  save

  integer :: maxNumTimesteps
  real(kind=DP) :: timestepSize
  integer(kind=SI) :: speciesInterpolationMethod, conditionsInterpolationMethod, decInterpolationMethod
  integer :: ratesOutputStepSize
  real(kind=DP) :: modelStartTime
  integer :: jacobianOutputStepSize
  integer :: irOutStepSize
  character(len=20) :: interpolationMethodName(2)

contains
  subroutine set_model_parameters( input_parameters )
    use types_mod
    use constraints, only : maxNumberOfDataPoints
    use SZACalcVars, only : latitude, longitude
    use date, only : day, month, year
    use interpolationMethod, only : setSpeciesInterpMethod, setConditionsInterpMethod, setDecInterpMethod
    implicit none

    real(kind=DP) :: input_parameters(*)

    interpolationMethodName(1) = 'piecewise constant'
    interpolationMethodName(2) = 'piecewise linear'
    ! maxNumTimesteps sets the maximum number of timesteps to calculate.
    ! Calculation will terminate when currentNumTimestep>=maxNumTimesteps.
    maxNumTimesteps = input_parameters(1)
    ! Size of timestep: tout is incremented by this amount on each iteration of the main while loop.
    timestepSize = input_parameters(2)
    ! Use the local variable speciesInterpolationMethod to set the value speciesInterpMethod,
    ! the private member of MODULE interpolationMethod.
    ! getSpeciesInterpMethod() is called by getConstrainedQuantAtT.
    ! Values:
    ! 1: Piecewise constant
    ! 2: Piecewise linear
    ! otherwise: error
    speciesInterpolationMethod = input_parameters(3)
    call setSpeciesInterpMethod( speciesInterpolationMethod )
    conditionsInterpolationMethod = input_parameters(4)
    call setConditionsInterpMethod( conditionsInterpolationMethod )
    decInterpolationMethod = input_parameters(5)
    call setDecInterpMethod( decInterpolationMethod )
    ! Member variable of MODULE constraints. Used in getConstrainedQuantAtT and readEnvVar
    maxNumberOfDataPoints = input_parameters(6)
    ! Frequency at which outputRates is called below.
    ratesOutputStepSize = input_parameters(7)
    ! Start time of model. Used to set t initially, and to calculate the elapsed time.
    modelStartTime = input_parameters(8)
    ! Frequency at which output_jfy is called below.
    jacobianOutputStepSize = input_parameters(9)
    ! Member variables of module SZACalcVars
    latitude = input_parameters(10)
    longitude = input_parameters(11)
    ! Member variables of module date
    day = input_parameters(12)
    month = input_parameters(13)
    year = input_parameters(14)
    ! Frequency at which to output instantaneous rates
    irOutStepSize = input_parameters(15)

    ! float format
    300 format (A52, E11.3)
    ! integer format
    400 format (A52, I11)
    ! string format
    500 format (A52, A17)
    write (*, '(A)') ' Model parameters:'
    write (*, '(A)') ' -----------------'
    write (*, 400) 'number of steps: ', maxNumTimesteps
    write (*, 300) 'step size (seconds): ', timestepSize
    write (*, 500) 'species interpolation method: ', adjustl( interpolationMethodName(speciesInterpolationMethod) )
    write (*, 500) 'conditions interpolation method: ', adjustl( interpolationMethodName(conditionsInterpolationMethod) )
    write (*, 500) 'dec interpolation method: ', adjustl( interpolationMethodName(decInterpolationMethod) )
    write (*, 400) 'maximum number of data points in constraint file: ', maxNumberOfDataPoints
    write (*, 400) 'ratesOutputStepSize: ', ratesOutputStepSize
    write (*, 400) 'instantaneous rates output step size: ', irOutStepSize
    write (*, 300) 'modelStartTime: ', modelStartTime
    write (*, 400) 'jacobianOutputStepSize: ', jacobianOutputStepSize
    write (*, 300) 'latitude: ', latitude
    write (*, 300) 'longitude: ', longitude
    write (*, '(A52, I3, A, I2, A, I4) ') 'day/month/year: ', day, '/', month, '/', year
    write (*, '(A)') ' -----------------'
    write (*,*)
  end subroutine set_model_parameters
end module model_params_mod
