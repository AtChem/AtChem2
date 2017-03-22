# Makefile for AtChem project

.SUFFIXES:
.SUFFIXES: .f90 .o

CVODELIB=
# gfortran flags
F77      =  gfortran
FFLAGS   =  -ffree-form -fimplicit-none -Wall -Wpedantic
LIBDIR   = /usr/lib/:$(CVODELIB)
# Intel Fortran flags
#F77 = ifort
#FFLAGS   = -free -warn
#LIBDIR =

AOUT = atchem

ALL: $(AOUT)

LOCAL = makefile.$$(uname -n | perl -pe 's/\..+//')

makefile.local:
	 touch $(LOCAL)
	 ln  $(LOCAL) makefile.local

include makefile.local

SRCS = dataStructures.f90 atchem.f90 mechanism-rates.f90 modelConfigFunctions.f90 zenith.f90  temperature.f90 solverFunctions.f90  inputFunctions.f90  outputFunctions.f90  interpolationFunctions.f90  constraintFunctions.f90  instantaneousRatesFunctions.f90  facsimileFunctions.f90  conversionFunctions.f90

OBJS = atchem.o mechanism-rates.o dataStructures.o modelConfigFunctions.o zenith.o  temperature.o solverFunctions.o  inputFunctions.o  outputFunctions.o  interpolationFunctions.o  constraintFunctions.o  instantaneousRatesFunctions.o  facsimileFunctions.o  conversionFunctions.o

LDFLAGS  = -L$(LIBDIR) -Wl,-rpath,$(LIBDIR) -lsundials_fcvode -lsundials_cvode -lsundials_fnvecserial -lsundials_nvecserial -lblas -llapack

$(AOUT):
	pwd
	ls -al
	$(F77) -o $(AOUT) $(SRCS) $(FFLAGS) -L$(LIBDIR) $(LDFLAGS)
	@perl -ne 'm/\d+\.\d*[eE][-+]?\d+/ and push @a, "$$ARGV:$$.: $$&:\t$$_";END{@a and print("\nWARNING! Single-precision constants found:\n", @a)}' *.f90

.f90.o:
	$(F77) -c $(FFLAGS) $<

clean:
	rm -rf .libs
	rm -f *.lo
	rm -f *.o
	rm -f fortran_update.sh
	rm -f $(AOUT)
	rm -f fort.*
	rm -f *.mod

webapp:
	bash ./make_webapp.sh


# dependencies:

atchem.o : atchem.f90 dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o
constraintFunctions.o : constraintFunctions.f90 dataStructures.o dataStructures.o
conversionFunctions.o : conversionFunctions.f90
dataStructures.o : dataStructures.f90
errorHandling.o : errorHandling.f90
facsimileFunctions.o : facsimileFunctions.f90
inputFunctions.o : inputFunctions.f90 dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o
instantaneousRatesFunctions.o : instantaneousRatesFunctions.f90
interpolationFunctions.o : interpolationFunctions.f90 dataStructures.o dataStructures.o
mechanism-rates.o : mechanism-rates.f90 modelConfiguration/mechanism-rate-coefficients.f90 dataStructures.o dataStructures.o dataStructures.o
modelConfigFunctions.o : modelConfigFunctions.f90
outputFunctions.o : outputFunctions.f90 dataStructures.o
solverFunctions.o : solverFunctions.f90 dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o
temperature.o : temperature.f90
zenith.o : zenith.f90 dataStructures.o dataStructures.o
