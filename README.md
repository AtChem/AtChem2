AtChem2  [![Build Status](https://travis-ci.org/AtChem/AtChem2.svg?branch=master)](https://travis-ci.org/AtChem/AtChem2)  [![codecov](https://codecov.io/gh/AtChem/AtChem2/branch/master/graph/badge.svg)](https://codecov.io/gh/AtChem/AtChem2)
=======

**AtChem2** is an atmospheric chemistry box-model, primarily designed for use with the Master Chemical Mechanism (MCM, http://mcm.leeds.ac.uk/MCM/). The latest stable release of AtChem can be downloaded here: https://github.com/AtChem/AtChem2/releases. Installation instructions and documentation can be found on the [AtChem2 wiki](https://github.com/AtChem/AtChem2/wiki).

AtChem2 is open source, released under the [MIT license](https://opensource.org/licenses/MIT).

Directory structure
-------------------

- `mcm/` contains data files related to specific versions of the MCM.
- `model/`  contains the chemical mechanism (in FACSIMILE format) and directories for the model configuration, the model constraints and the model output
- `obj/` contains the files generated by the Fortran compiler
- `src/` contains the Fortran source files.
- `tools/` contains Python and shell scripts to build and compile AtChem2, with the chemical mechanism and configuration in the `model/` directory.
- `travis/` contains the _testsuite_ files.

Installation and dependencies
-----------------------------

AtChem2 requires a Fortran compiler (GNU gfortran or Intel ifort), the **CVODE** and **openlibm** libraries, and a **Python 2.x** installation. Compilation of CVODE also requires **cmake**, and the Fortran libraries **BLAS** and **LAPACK**. Optionally, **numdiff**, **FRUIT**, and a **Ruby** installation are required to run the _testsuite_.

Detailed instructions for the installation of Atchem2 and its dependencies can be found in the corresponding [wiki pages](https://github.com/AtChem/AtChem2/wiki/1.-Installation).

Building and configuration
--------------------------

Copy the example `Makefile`  from `tools/` into the main directory and set the variables `CVODELIB`, `OPENLIBMDIR` and `FRUITDIR`to the locations of the CVODE, openlibm and FRUIT libraries.

Type `./tools/build.sh tools/mcm_example.fac` in the main directory to create an executable file called `atchem2`, using an example chemical mechanism and a default configuration. Note that, at present, AtChem2 requires recompilation for each new chemical mechanism file (`*.fac`). This is handled by the script `./tools/build.sh`, which converts the `.fac` file into a format usable by AtChem2 (namely, it generates three Fortran files in the `src/gen/` directory and several data files in the `model/configuration/` directory).

After completing the build step above, set the initial conditions, the required outputs and the other model parameters by editing the files in the `model/configuration/` directory. To run the model, type `./atchem2`. The executable accepts several command line arguments to change the location of input and output files from the default directories.

More information on the configuration and execution of AtChem2 can be found in the corresponding [wiki pages](https://github.com/AtChem/AtChem2/wiki/2.-Model-Configuration-and-Execution).
