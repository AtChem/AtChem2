# -----------------------------------------------------------------------------
#
# Copyright (c) 2017 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

import os
import sys
import mech_converter as mc
import shutil
"""This script builds the program atchem2"""


def setup_files_for_atchem2(in_file, output_dir, mc_dir):
    assert os.path.isfile(in_file), 'Failed find file ' + in_file
    script_directory = os.path.dirname(os.path.abspath(__file__))
    assert os.path.exists(output_dir), 'Failed to find directory ' + output_dir
    # Convert MCM input file to generated files
    mc.convert(in_file, output_dir, mc_dir)


def main():
    assert len(sys.argv) > 1, 'Please enter a filename as argument, pointing to the mcm mechanism file.'
    input_filename = sys.argv[1]
    # output_dir defaults to '.' if not given
    if len(sys.argv) <= 2:
        output_dir = './src/gen/'
    else:
        output_dir = sys.argv[2]
    if len(sys.argv) <= 3:
        param_dir = './model/configuration/'
    else:
        param_dir = sys.argv[3]
    setup_files_for_atchem2(input_filename, output_dir, param_dir)

if __name__ == '__main__':
    main()
