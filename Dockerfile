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

# get base image
FROM rockylinux:8.9

# copy required bash scripts for installation and runtime.
# eventually this can change to COPY . /atchem/ so it is version agnostic
# but more testing required
COPY docker/install.sh .
COPY docker/entrypoint.sh .

# run install script
RUN /install.sh

# add lable for github container registry
LABEL org.opencontainers.image.source=https://github.com/wacl-york/AtChem2

# set entrypoint as the script that runs on `docker run`
ENTRYPOINT [ "/entrypoint.sh" ]
