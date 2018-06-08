AtChem2  [![Build Status](https://travis-ci.org/AtChem/AtChem2.svg?branch=master)](https://travis-ci.org/AtChem/AtChem2)  [![codecov](https://codecov.io/gh/AtChem/AtChem2/branch/master/graph/badge.svg)](https://codecov.io/gh/AtChem/AtChem2)
=======

AtChem2 is an atmospheric chemistry numerical integrator, primarily designed for use with the Master Chemical Mechanism (MCM, http://mcm.leeds.ac.uk/MCM/). For more information and instructions, see the [AtChem2 wiki](https://github.com/AtChem/AtChem2/wiki).

AtChem2 is open source, released under the [MIT license](https://opensource.org/licenses/MIT).

Directory structure
-------------------

The `src/` directory contains the source files of AtChem2. The `tools/` directory contains Python and shell scripts to build the AtChem2 executable for a given chemical mechanism and configuration. The `travis/` directory contains the _testsuite_ files. The remaining directories are empty, but provide a default location for the input, output and configuration files.

Dependencies
------------

AtChem2 requires a Fortran compiler (GNU gfortran or Intel ifort), the BLAS and LAPACK Fortran libraries, the CVODE and openlibm libraries, and a Python 2 installation. Running the _testsuite_ additionally requires a numdiff installation.

Detailed instructions for the installation of the dependencies can be found on the [wiki page](https://github.com/AtChem/AtChem2/wiki/1.1-Dependencies).

Building AtChem2
----------------

After installation of all the dependencies, refer to the instructions on the [wiki page](https://github.com/AtChem/AtChem2/wiki/1.-Installation) for the installation of AtChem2.

Firstly, create a `makefile.local` file and set the variables `CVODELIB` and `OPENLIBMDIR` to the locations of the CVODE and openlibm libraries. Type `./tools/build.sh tools/mcm_example.fac` in the top-level directory and this should create an executable name `atchem2`, with a default configuration.

Note that, at present, AtChem2 requires recompilation for each new chemical mechanism (`.fac`) file. This is handled by the script `./tools/build.sh`, which converts the `.fac` file into a format usable by AtChem2 (namely, it generates two Fortran files and several data files in the `modelConfiguration/` directory).

Configuring and running AtChem2
-------------------------------

AtChem2 accepts several command line arguments to configure the location of output and input files, with defaults also defined if not provided. After completing the build step above, set the initial conditions, required outputs and other model paramters in the files inside the `modelConfiguration/` directory.

To run the model, type `./atchem2`. More information on model configuration and execution see the [wiki page](https://github.com/AtChem/AtChem2/wiki/2.-Model-Configuration-and-Execution).
