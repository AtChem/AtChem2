#!/usr/bin/bash
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
# This script is run everytime the container is run. 
#
# First it copies the /atchem/ directory that is created during the build to 
# the home directory of the user running the container. This is for
# compatibility with singularity, where the user running the container is not
# root, and therefore cannot modify the model files in place. 
#
# Next it moves the user configuration from /data_transfer/ and copies it into
# the ~/atchem/ directory. This /data_transfer/ directory is created when the
# the user mounts a volume when running the container (e.g `docker run -v...`)
#
# We then move to the ~/atchem directory and build the model using the mechanism
# specified by the user as a runtime argument to `docker run`. 
#
# Then the model is run by executing the newly built atchem2 file. 
#
# On completion the atchem model output directory is copied to the data_transfer
# directory and as such onto the host filesystem
# -----------------------------------------------------------------------------

# make a copy to home to allow for compatibility with singularity 
\command cp -rf /atchem/ ~/ 

# Transfer in user config
\command cp -rf /data_transfer/* ~/atchem/

# move into atchem dir
cd ~/atchem/

# build model using user specified mechanism
./build/build_atchem2.sh $1
echo $1

# run model
./atchem2

# copy outputs to data_transfer / host filesystem
\command cp -rf ~/atchem/model/output /data_transfer/model/