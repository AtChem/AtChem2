#!/bin/sh
# -----------------------------------------------------------------------------
#
# Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
# Peter Jimack, Mike Pilling
#
# Copyright (c) 2017 Sam Cox, Roberto Sommariva
#
# Copyright (c) 2024 Will Drysdale, Beth Nelson
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# This is the install script that is run when the container is built. It
# installs the necessary dependacies for installing AtChem2 (gcc-gfortran,
# wget, cmake and python3.11).
#
# It then runs the install scripts provided with the model (install_cvode.sh
# and install_openlibm.sh) to install additional dependancies required to build
# the model.
#
# Next it produces the Makefile from the skeleton, and amends the dependancy
# paths.
#
# Finally it does some housekeeping, updating the build_atchem2.sh to be able
# to find the python installation, and makes the docker entrypoint script
# executable.
# -----------------------------------------------------------------------------

# Install dependancies from package repository
dnf install -y which gcc-gfortran wget cmake python3.11

# Make directories
mkdir /atchem-lib
# mkdir /atchem

# Move to /atchem so the dependacy installation scripts work correctly.
cd atchem

# Install dependancies to /atchem-lib
./tools/install/install_cvode.sh /atchem-lib/
./tools/install/install_openlibm.sh /atchem-lib/

# Change atchem dependancy paths and create Makefile from skeleton
sed 's,cvode/lib,/atchem-lib/cvode/lib,g' tools/install/Makefile.skel > ./Makefile
sed -i 's,openlibm/lib,/atchem-lib/openlibm/,g' ./Makefile

# Fix python command to match installed version
sed -i "s/python/python3/g" ./build/build_atchem2.sh
chmod +x docker/entrypoint.sh
