# AtChem2 - CHANGELOG

## v1.1 (August 2018)

- implement a new directory structure
- simplify the installation procedure and remove `makefile.local`
- restructure the handling and calculation of the photolysis rates
- fix the calculation of sun declination and solar zenith angle
- improve handling of dates and numerical precision
- implement code coverage testing with [Codecov](https://codecov.io/)
- implement a unit testing framework (new dependencies: **Ruby**, **FRUIT**)
- add unit tests for some functions
- add plotting tools in R, Python, Matlab, gnuplot

## v1.0 (July 2017)

- create a code repository on [github.com](https://github.com/)
- adopt the [MIT open source license](https://opensource.org/licenses/MIT)
- upgrade to **CVODE** version 2.9.0
- remove the web interface and all the code related to AtChem-online [web service](https://atchem.leeds.ac.uk/)
- standardize the codebase to Fortran95, use a consistent modular structure for the source code
- adopt coding style guidelines and standardize the naming of variables, subroutines, source files
- improve formatting of output files, screen output and error reporting
- improve code comments and create a [wiki](https://github.com/AtChem/AtChem2/wiki) for the documentation
- tidy and speed up the Python and shell scripts
- create a user-friendly command line interface to install, compile and run the model
- verify that the model compiles with both gfortran and intel compilers, and runs on Linux/Unix and macOS machines
- implement continuous integration with [TravisCI](https://travis-ci.org/) and develop a range of test scenarios (new dependency: **numdiff**)
- improve the model stability and numerical accuracy (new dependency: **openlibm**)
- correct the calculations of solar angles, water vapour concentration and of the sum of organic peroxy radicals (`RO2`)
- remove air number density (`M`) as environment variable
- fix a number of minor bugs
