import os
import sys
import mech_converter as mc
import shutil
"""This script builds the program atchem"""


def setup_files_for_atchem(in_file, output_dir):
    assert os.path.isfile(in_file), 'Failed find file ' + in_file
    script_directory = os.path.dirname(os.path.abspath(__file__))
    assert os.path.exists(os.path.join(script_directory, output_dir)), 'Failed to find directory ' + output_dir + ' (looking for ' + os.path.join(script_directory, output_dir) + ' )'
    # Convert MCM input file to generated files
    mc.convert(in_file, output_dir)
    in_directory = os.path.dirname(os.path.abspath(in_file))
    # copy the generated files to modelConfiguration directory
    print os.path.join(in_directory, 'mechanism.species')
    print os.path.abspath(os.path.join(script_directory, '../modelConfiguration/mechanism.species'))
    file_list = ['mechanism.species', 'mechanism.prod', 'mechanism.reac']
    assert os.path.exists(os.path.join(script_directory, '../modelConfiguration/'))
    for filename in file_list:
        assert os.path.isfile(os.path.join(in_directory, filename))
        shutil.copy(os.path.join(in_directory, filename), os.path.join(script_directory, '../modelConfiguration/'+filename))


def main():
    assert len(sys.argv) > 1, 'Please enter a filename as argument, pointing to the mcm subset file.'
    input_filename = sys.argv[1]
    # output_dir defaults to '.' if not given
    if len(sys.argv) < 2:
        output_dir = '.'
    else:
        output_dir = sys.argv[2]
    setup_files_for_atchem(input_filename, output_dir)

if __name__ == '__main__':
    main()
