# AtChem2 - CHANGELOG

## v1.2.1 (January 2021)

- new environment variable `ASA` (aerosol surface area)
- fix bug in the C-binding for `gfortran` version 9 and above
- remove tabs from the configuration files
- update the Travis configuration file
- tidy the comments and printout messages of various source files
- update the manual

## v1.2 (May 2020)

- implement argument parser and named arguments for the  `atchem2` executable
- streamline the build procedure, rename and tidy the build scripts, and move to the `build/` directory
- implement the pre-compilation of the chemical mechanism into a shared library (`mechanism.so`)
- implement the dilution mechanism and redefine the `DILUTE` environment variable
- convert the documentation to LaTeX format
- rewrite the user manual (`doc/AtChem2-Manual.pdf`) and reorganize the [wiki pages](https://github.com/AtChem/AtChem2/wiki)
- add the poster presented at the ACM 2018 conference (`doc/AtChem_poster_ACM2018.pdf`)
- change the reference in `CITATION.md` to the GMD paper (https://www.geosci-model-dev.net/13/169/2020/)
- fix the header of `photolysisConstant.config` and remove the `initialConditionsSetting.output` file
- fix a bug in `mech_converter.py` affecting chemical reactions with no reactants
- change all Python scripts to be independent of the Python version
- rename plotting script `plot-atchem2.py` to `plot-atchem2-numpy.py`
- new Python plotting script `plot-atchem2-pandas.py` (uses `pandas` instead of `numpy`)
- improve the documentation of the behaviour and unit tests
- remove the behaviour test `full`
- new script to set the version of AtChem2 before a release (`tools/version.sh`)
- minor improvements to the install procedure
- move the example chemical mechanism file to the `mcm`/ directory, and add a minimal .fac file (`mechanism_skel.fac`)
- tidy and improve comments in various source files and scripts
- minor fixes and updates

## v1.1.1 (January 2019)

- add `doc/` directory containing the documentation in markdown format
- add `CONTRIBUTING.md` file
- fix the header of `lossRates.output` and `productionRates.output`
- change name of environment variable `ROOFOPEN` to `ROOF`

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
