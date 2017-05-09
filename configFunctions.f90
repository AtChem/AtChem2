MODULE configFunctions_mod
  USE types_mod
CONTAINS
  SUBROUTINE calcDateParameters ()
  USE date
  INTEGER :: i, monthList(12)

  monthList = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
  ! calculate which day of the year day/month refers to
  dayOfYear = 0
  DO i = 1, month-1
     dayOfYear = dayOfYear + monthList(i)
  ENDDO
  dayOfYear = dayOfYear + day -1
  ! This day refers to the following fraction through the year
  dayAsFractionOfYear = dayOfYear / 365
  ! Set number of seconds per year
  secondsInYear = 3.6525d+02*2.40d+01*3.60d+03
  RETURN
END SUBROUTINE calcDateParameters

SUBROUTINE writeFileHeaders (photoRateNamesForHeader, specOutReqNames)
  USE envVars
  USE photolysisRates, ONLY : nrOfPhotoRates, ck
  USE storage, ONLY : maxPhotoRateNameLength, maxSpecLength
  IMPLICIT NONE

  CHARACTER(LEN=maxPhotoRateNameLength), intent(in) :: photoRateNamesForHeader(:)
  CHARACTER(LEN=maxSpecLength), intent(in) :: specOutReqNames(:)
  INTEGER(kind=NPI) :: i

  ! WRITE FILE OUTPUT HEADERS AND OUTPUT AT t = 0
  ! OUTPUT FOR CVODE MAIN SOLVER
  WRITE (57,*) 't LNST LNFE LNETF LNGE'

  ! OUTPUT FOR SPARSE SOLVER
  WRITE (56,*) 'time speciesNumber speciesName reactionNumber rate'
  WRITE (60,*) 'time speciesNumber speciesName reactionNumber rate'
  WRITE (61,*) 't NFELS NJTV NPE NPS'

  ! OTHER OUPUT
  WRITE (50, '(100 (1x, a)) ') 't         ', (specOutReqNames(i), i = 1, size(specOutReqNames))
  ! 51, 53, 54, 55 don't need a header.
  WRITE (52,*) 'time ', (envVarNames(i), i = 1, numEnvVars), 'RO2'
  WRITE (58,*) 't ', (trim(photoRateNamesForHeader(ck(i)) )// '    ', i = 1, nrOfPhotoRates)
  WRITE (59,*) 't secx cosx lat longt lha sinld cosld'
  WRITE (62,*) 't currentStepSize previousStepSize'
  RETURN
END SUBROUTINE writeFileHeaders


SUBROUTINE matchNameToNumber (masterSpeciesList, &
                              testSpeciesList, &
                              returnArray, returnArraySize)
  USE storage, ONLY : maxSpecLength
  ! This takes in masterSpeciesList, and checks whether each member of
  ! testspeciesList is in masterSpeciesList.
  ! When it finds a match, it adds the number of the line in masterSpeciesList to
  ! returnArray in the next available space.
  ! returnArraySize contains the size of returnArray, which is the
  ! number of times a match was made
  CHARACTER(LEN=maxSpecLength), contiguous, intent(in) :: masterSpeciesList(:)
  CHARACTER(LEN=maxSpecLength), contiguous, intent(inout) :: testSpeciesList(:)
  INTEGER(kind=NPI), intent(out) :: returnArray(:), returnArraySize
  INTEGER :: i, j
  LOGICAL match

  returnArraySize = 0
  ! loop over testSpeciesList, and masterSpeciesList. If a match is made, then append
  ! returnArray with the number of the species from testSpeciesList within the masterSpeciesList
  DO i = 1, size(testSpeciesList)
     match = .FALSE.
     DO j = 1, size(masterSpeciesList)
        IF (masterSpeciesList(j)==testSpeciesList(i)) THEN
           match = .TRUE.
           returnArraySize = returnArraySize + 1
           returnArray(returnArraySize) = j
        ENDIF
     ENDDO
     ! substitute empty strings for invalid species
     IF (match.eqv..FALSE.) THEN
        testSpeciesList(i) = ''
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE matchNameToNumber

PURE FUNCTION matchOneNameToNumber (masterList, target) result ( id )
  ! Search masterList for target, and return the index id. If not found, return 0.
  IMPLICIT NONE

  CHARACTER(LEN=*), contiguous, intent(in) :: masterList(:)
  CHARACTER(LEN=*), intent(in) :: target
  INTEGER(kind=NPI) :: j, id

  id = 0
  DO j = 1, size(masterList)
     IF (masterList(j)==target) THEN
        id = j
        RETURN
     ENDIF
  ENDDO
END FUNCTION matchOneNameToNumber


SUBROUTINE findReactionsWithProductOrReactant (r, chs, arrayLen)
USE types_mod
! For each interesting species, held in the first element of each row of r,
! find all reactions in chs which match (i.e. second column of chs matches first element
! of given row from r), and append the number of that reaction (first column of chs)
! to this row of r.
! arrayLen keeps track of how long each row in r is.
INTEGER(kind=NPI), intent(in) :: chs(:, :)
INTEGER(kind=NPI), intent(out) :: arrayLen(:)
INTEGER(kind=NPI), intent(inout) :: r(:, :)
INTEGER(kind=NPI) :: rCounter, i, j

IF (size(arrayLen)/=size(r, 1)) THEN
  STOP "size(arrayLen)/=size(r, 1) in findReactionsWithProductOrReactant()."
END IF
! initialise counter for r array
rCounter = 2
! loop over interesting species (i.e. over 1st index of r)
DO i = 1, size(arrayLen)
  ! loop over elements of 2nd index of chs
   DO j = 1, size(chs, 2)
      ! Is the second element of this row in chs (a species number) equal to the first element of this column in r (the interesting species number)?
      ! If so, then append the first element of this row in chs (the equation number) to this row in r,
      ! and update the length counter arrayLen for this row.
      IF (chs(2, j)==r(i, 1)) THEN
         ! Match found
         r(i, rCounter) = chs(1, j)
         rCounter = rCounter + 1
      ENDIF
   ENDDO
   arrayLen(i) = rCounter -1
   rCounter = 2
ENDDO

RETURN
END SUBROUTINE findReactionsWithProductOrReactant


SUBROUTINE setConcentrations (refSpeciesNames, concSpeciesNames, &
                              inputConcentrations, outputConcentrations)
  ! For each input species in concSpeciesNames (size concCounter), and matching value in inputConcentrations (size inputConcentrationsSize),
  ! look through refSpeciesNames (size numSpecies) for the number of this species in that list,
  ! then transer the value from inputConcentrations to outputConcentrations. If no match is found,
  ! output this to errors.output, but don't stop, just ignore the input value.
  ! Print outcome of each search into initialConditionsSetting.output.
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  CHARACTER(LEN=maxSpecLength), intent(in) :: concSpeciesNames(:), refSpeciesNames(:)
  CHARACTER(LEN=maxSpecLength) :: k, m
  real(kind=DP), intent(in) :: inputConcentrations(:)
  real(kind=DP), intent(out) :: outputConcentrations(:)
  INTEGER(kind=NPI) :: j, i
  LOGICAL :: match

  IF (size(concSpeciesNames)/=size(inputConcentrations)) THEN
    STOP "size(concSpeciesNames)/=size(inputConcentrations) in setConcentrations()."
  END IF
  IF (size(refSpeciesNames)/=size(outputConcentrations)) THEN
    STOP "size(refSpeciesNames)/=size(outputConcentrations) in setConcentrations()."
  END IF
  DO i = 1, size(concSpeciesNames)
     match = .FALSE.
     k = concSpeciesNames(i)
     DO j = 1, size(refSpeciesNames)
        m = refSpeciesNames(j)
        IF (m==k) THEN
           match = .TRUE.
           ! Set concentration in outputConcentrations
           outputConcentrations(j) = inputConcentrations(i)
           WRITE (54,*) 'match, m = k = ', m, ' concentration = ', inputConcentrations(i)
           EXIT
        ELSE
           WRITE (54,*) 'no match, m = ', m, ' != k = ', k, ' concentration = ', inputConcentrations(i)
        ENDIF
     ENDDO
     IF (match.eqv..FALSE.) THEN
        ! If we reach this point, we've failed to find this species
        WRITE (51,*) "Error in setConcentrations"
        WRITE (51,*) "Can't find species: ", k," in species list"
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE setConcentrations
END MODULE configFunctions_mod
