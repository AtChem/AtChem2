# Makefile for AtChem project

.SUFFIXES:
.SUFFIXES: .f .o

# gfortran flags
F77      =  gfortran 
FFLAGS   =  -ffree-form -Wall 
LIBDIR   = 
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

svnversion:
	@echo "function svn_revision ()" > svn_version.f_
	@echo "   character(10) svn_revision" >> svn_version.f_
	@echo "   svn_revision = '$$(svnversion -n)'" >> svn_version.f_
	@echo "end">> svn_version.f_
	@if test -d .svn  ; then mv svn_version.f_ svn_version.f ; fi
	@if test ! -e svn_version.f ; then mv svn_version.f_ svn_version.f ; fi

include makefile.local

SRCS = atchem.f mechanism-rates.f dataStructures.f modelConfigFunctions.f zenith.f  temperature.f solverFunctions.f  inputFunctions.f  outputFunctions.f  interpolationFunctions.f  constraintFunctions.f  instantaneousRatesFunctions.f  facsmileFunctions.f  conversionFunctions.f 

OBJS = atchem.o mechanism-rates.o dataStructures.o modelConfigFunctions.o zenith.o  temperature.o solverFunctions.o  inputFunctions.o  outputFunctions.o  interpolationFunctions.o  constraintFunctions.o  instantaneousRatesFunctions.o  facsmileFunctions.o  conversionFunctions.o 

LDFLAGS  = -lm -lsundials_fcvode -lsundials_cvode -lsundials_fnvecserial -lsundials_nvecserial

$(AOUT): svnversion $(OBJS) 
	$(F77) -c $(FFLAGS) svn_version.f 
	$(F77) -o $(AOUT) $(OBJS) svn_version.o $(FFLAGS) -L$(LIBDIR) $(LDFLAGS)
	@perl -ne 'm/\d+\.\d*[eE][-+]?\d+/ and push @a, "$$ARGV:$$.: $$&:\t$$_";END{@a and print("\nWARNING! Single-precision constants found:\n", @a)}' *.f

.f.o:
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

atchem.o : atchem.f dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o 
constraintFunctions.o : constraintFunctions.f dataStructures.o dataStructures.o 
conversionFunctions.o : conversionFunctions.f 
dataStructures.o : dataStructures.f 
errorHandling.o : errorHandling.f 
facsmileFunctions.o : facsmileFunctions.f 
inputFunctions.o : inputFunctions.f dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o 
instantaneousRatesFunctions.o : instantaneousRatesFunctions.f 
interpolationFunctions.o : interpolationFunctions.f dataStructures.o dataStructures.o 
mechanism-rates.o : mechanism-rates.f modelConfiguration/mechanism-rate-coefficients.f dataStructures.o dataStructures.o dataStructures.o 
modelConfigFunctions.o : modelConfigFunctions.f 
outputFunctions.o : outputFunctions.f dataStructures.o 
solverFunctions.o : solverFunctions.f dataStructures.o dataStructures.o dataStructures.o dataStructures.o dataStructures.o 
temperature.o : temperature.f 
zenith.o : zenith.f dataStructures.o dataStructures.o 
svn_version.o : svn_version.f
