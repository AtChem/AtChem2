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

#!/usr/bin/bash
\command cp -rf /AtChem2-1.2.2/ ~/ # make a copy to home to allow for compatibility with singularity 
\command cp -rf /inout/* ~/AtChem2-1.2.2/

cd ~/AtChem2-1.2.2/

./build/build_atchem2.sh $1
echo $1

./atchem2

cp -r ~/AtChem2-1.2.2/model/output /inout/model/output/