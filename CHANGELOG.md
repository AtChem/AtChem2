# AtChem2 - CHANGELOG

## v1.1 (November 2018)

- implement a new directory structure
- rename some configuration and output files
- add `CHANGELOG.md` and `CITATION.md` files
- simplify the installation procedure and remove `makefile.local`
- restructure the handling and calculation of photolysis rates
- fix the calculations of sun declination and solar zenith angle
- improve handling of dates and numerical precision
- check calculation of reaction rates and improve format of `instantaneousRates` files (now called `reactionRates`)
- merge `outputLossRates.config` and `outputProductionRates.config` into `outputRates.config`
- fix the output of `lossRates.output` and `productionRates.output`
- implement code coverage testing with [Codecov](https://codecov.io/)
- implement a unit testing framework (new requirements: **Ruby**, **FRUIT**)
- add unit tests for atmosphere, configuration, date and solar functions
- add exact solution tests
- improve running and reporting of the testsuite
- rework mechanism conversion procedure and Python scripts
- add plotting tools in R, Python, Matlab, gnuplot
- extend the documentation on the [wiki](https://github.com/AtChem/AtChem2/wiki)
- fix several minor bugs

## v1.0 (July 2017)

- create a code repository on [github.com](https://github.com/)
- adopt the [MIT open source license](https://opensource.org/licenses/MIT)
- upgrade to **CVODE** version 2.9
- remove the web interface and all the code related to [AtChem-online web service](https://atchem.leeds.ac.uk/)
- standardize the codebase to Fortran95, use a consistent modular structure for the source code
- adopt coding style guidelines and standardize the naming of variables, subroutines, source files
- improve formatting of output files, screen output and error reporting
- improve code comments and create a [wiki](https://github.com/AtChem/AtChem2/wiki) for the documentation
- tidy and speed up the Python and shell scripts
- create a user-friendly command line interface to install, compile and run the model
- verify that the model compiles with both **gfortran** and **Intel** compilers
- verify that the model runs on Linux/Unix and macOS machines
- implement continuous integration with [TravisCI](https://travis-ci.org/) and develop a range of test scenarios (new dependency: **numdiff**)
- improve the model stability and numerical accuracy (new dependency: **openlibm**)
- correct the calculations of solar angles, water vapour concentration and of the sum of organic peroxy radicals (`RO2`)
- remove air number density (`M`) as environment variable
- fix several minor bugs
