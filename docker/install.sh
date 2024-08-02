#!/bin/bash
dnf install -y which gcc-gfortran wget cmake python3.11
# could make the version number a variable so this can work with other releases?
curl -L https://github.com/AtChem/AtChem2/archive/refs/tags/v1.2.2.tar.gz > atchem.tar.gz
tar -xzf atchem.tar.gz
rm atchem.tar.gz
mkdir /atchem-lib
cd AtChem2-1.2.2

# Install dependancies
./tools/install/install_cvode.sh /atchem-lib/ # would use version number variable here too...
./tools/install/install_openlibm.sh /atchem-lib/

# Change atchem dependancy paths and create Makefile from skeleton
sed 's,cvode/lib,/atchem-lib/cvode/lib,g' tools/install/Makefile.skel > ./Makefile
sed -i 's,openlibm-0.8.1,/atchem-lib/openlibm-0.8.1,g' ./Makefile

# Fix python command to match installed version
sed -i "s/python/python3/g" ./build/build_atchem2.sh
chmod +x /entrypoint.sh