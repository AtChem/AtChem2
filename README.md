AtChem2 [![license](https://img.shields.io/github/license/AtChem/AtChem2?color=blue)](https://github.com/AtChem/AtChem2/blob/master/LICENSE) [![release](https://img.shields.io/github/v/release/AtChem/AtChem2?color=blue)](https://github.com/AtChem/AtChem2/releases) [![AtChem2 CI](https://github.com/AtChem/AtChem2/actions/workflows/ci.yml/badge.svg)](https://github.com/AtChem/AtChem2/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/AtChem/AtChem2/graph/badge.svg)](https://codecov.io/gh/AtChem/AtChem2)
=======


**AtChem2** is a modelling tool for atmospheric chemistry. It is primarily designed to use the **Master Chemical Mechanism** (MCM, https://mcm.york.ac.uk/MCM), but it can be used with any general set of chemical reactions as long as they are provided in the correct format. The MCM is a near-explicit chemical mechanism which describes the gas-phase oxidation of volatile organic compounds (VOC) in the lower atmosphere.

AtChem2 is _open source_, under the [MIT license](https://opensource.org/licenses/MIT).

**_Please, see the file `CITATION.md` for information on how to cite the model in publications._**

<a href='https://ko-fi.com/I3I1JSFJF' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://storage.ko-fi.com/cdn/kofi2.png?v=3' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>


Directory structure
-------------------

- `build/` contains the Python and shell scripts used to compile AtChem2.
- `doc/` contains the AtChem2 user manual, with the LaTeX source files, and the poster presented at the [ACM 2018 conference](https://acm.aqrc.ucdavis.edu).
- `mcm/` contains data files related to specific versions of the MCM.
- `model/` contains an example directory structure for the chemical mechanism, model configuration, constraints, output, and an example chemical mechanism (in FACSIMILE format). There can be several such directories (with different names).
- `obj/` contains the files generated by the Fortran compiler.
- `src/` contains the Fortran source files.
- `tests/` contains the Testsuite scripts and files.
- `tools/` contains shell scripts to install AtChem2 and its dependencies, plotting scripts in various languages, and other utilities.


Installation, Setup and Execution
---------------------------------

AtChem2 requires a **Fortran** compiler (GNU `gfortran` or Intel `ifort`), the **CVODE** (part of [SUNDIALS](https://computing.llnl.gov/projects/sundials)) and **openlibm** libraries, **make**, and **Python**. Compilation of CVODE also requires **cmake**. Optionally, **numdiff**, **FRUIT**, and **Ruby** are required to run the Testsuite. AtChem2 compiles and runs on Unix/Linux and macOS systems. A working knowledge of the **unix shell** is required to install and use AtChem2.

The latest stable version of AtChem2 can be downloaded from the [Releases page](https://github.com/AtChem/AtChem2/releases). After installing the required dependencies using the scripts in the `tools/install/` directory, copy the file `tools/install/Makefile.skel` to the _Main Directory_ and rename it `Makefile`. Set the variables `CVODELIBDIR`, `OPENLIBMDIR` and `FRUITDIR` in `Makefile` to the paths of CVODE, openlibm and (if installed) FRUIT. To compile the model using the example chemical mechanism, execute the command:

```
./build/build_atchem2.sh ./model/mechanism.fac
```

The build script converts the chemical mechanism from the FACSIMILE format (`mechanism_test.fac`) to a Fortran-compatible format, and generates the shared library `mechanism.so` in the `model/configuration/` directory. After the build process is completed, an executable file called `atchem2` is created in the _Main Directory_.

Set the initial conditions, the required outputs and the other model parameters by editing the files in the `model/configuration/` directory. If required, copy the constraint files to the relevant subdirectory in `model/constraints/`. To run the model with the default configuration, execute the command:

```./atchem2```

The `atchem2` executable accepts several command line arguments to customize the location of the configuration, input and output directories, and of the shared library. More information on AtChem2, and detailed instructions on its installation, configuration and use can be found in the manual (`doc/AtChem2-Manual.pdf`) and in the GMD paper (see `CITATION.md`).

The [AtChem2 wiki](https://github.com/AtChem/AtChem2/wiki) contains a summary of the instructions to install, compile, run and contribute to the development of Atchem2, together with additional information and a list of [known issues](https://github.com/AtChem/AtChem2/wiki/Known-Issues) with the suggested solutions or workarounds.


Docker Container 
---------------------------------

A containerised version of `AtChem2` is available. Currently this is built for release 1.2.2. The container is built on Rocky Linux 8.9 and pre-installs the `cvode` and `openlibm` dependencies via their relevant `tools/install/install_*.sh` scripts.

The image can be downloaded via:

```
docker pull ghcr.io/wacl-york/atchem2:1.2.2
```

When running the container, and changes such and configurations, constraints and mechanisms should be in a folder that matches the AtChem2 directory structure. This folder is then mounted as a volume to the container with the name `/inout/`. The mechanism to use is provided as a positional argument to the image.

#### Example host file structure:
```
my_model_run
├── mcm
│   └── my_mech.fac
└── model
    ├── configuration
    ├── constraints
```

#### Example Docker run command:

```
docker run -it --rm -v /path/to/my_model_run:/inout ghcr.io/wacl-york/atchem2:1.2.2 my_mech.fac
```

Outputs will be copied to `my_model_run/model/output` on completion.

#### Running on Singularity / Apptainer

Some HPC systems use Singularity / Apptainer instead of Docker as their container engine. This image is compatible with those aswell. The image can be converted to a .sif via:

```
apptainer pull path/to/image/atchem2.sif docker://ghcr.io/wacl-york/atchem2:1.2.2
```

> [!Note]
> Currently the github package is set to "internal" so requires the --docker-login flag and a personal access token.

#### Example Apptainer run command:

```
apptainer run --bind /path/to/my_model_run/:/inout/ path/to/image/atchem2.sif my_mech.fac
```
> [!Note]
> The leading "/" is important when mounting the volume for apptainer, as the container opens in the users "~" directory whereas Docker opens at "/"
