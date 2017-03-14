# This file was written by Sam Cox, University of Leicester, 2017

#  This file contains functions to fix the contents of a .fac file by removing the incorrect newline characters.
# Call with a single parameter containing the file to fix.
import sys
import re


def fix_fac_contents(filename):
    # Given a filename, return the contents of the file, but with incorrect newline characters removed, and the affected
    # lines concatenated correctly. This will probably fail if a line is REALLY long, stretching over two full lines,
    # but should probably then give an error as output.

    # Using splitlines rather than readlines(), we take out the errant carriage returns, and for any line with such on
    # it, we return to members of the list.
    with open(filename, 'r') as file_open:
        contents = file_open.read().splitlines()
    # print contents
    orig_contents_len = len(contents)
    print str(filename) + ': file read in ' + str(orig_contents_len) + ' items'
    contents_count = 0
    # Firstly ignore comment lines. Then correct the lines which don't start with a % - they should be concatenated
    # onto the previous entry
    for i in range(len(contents)):
        if re.match(r'\*', contents[i]):
            contents_count += 1
        else:
            if not re.match(r'%', contents[i]):
                # print 'fail'
                #print contents[i - 1], 'XX', contents[i], 'XX', contents[i + 1]
                contents[i - 1] += ' ' + contents[i]
                #print contents[i - 1]
                contents_count += 1
    print str(contents_count) + ' corrections made - now removing old'
    # Remove old elements which have now been concatenated onto previous
    contents = [item for item in contents if re.match(r'%', item)]
    assert orig_contents_len == contents_count + len(contents), \
        str(filename) + ': file is probably too messed up with carriage returns for this simple script to fix.'
    return contents


def fix_fac_file(filename):
    # Given a filename, overwrite the contents of the file with the same contents, but with incorrect newline characters
    # removed, and the affected lines concatenated correctly. This will probably fail if a line is REALLY long,
    # stretching over two full lines, but should probably then give an error as output.
    # All the heavy lifting is done by fix_fac_contents - here we just provide a simple wrapper to write back to the
    # same file.
    print 'Running fix_fac_file on ' + str(filename)
    contents = fix_fac_contents(filename)
    contents = [item + '\n' for item in contents]
    with open(filename, 'w') as file_open:
        file_open.writelines(contents)
    return


def compare_files_for_subset(master_filename, compare_filename):
    # Given two FACSIMILE format files, output whether or not the reactions contained in compare_filename are a subset
    # of the reactions defined in master_filename. This includes a step to fix both files in the case of incorrect
    # newlines, but this is not saved back to file.
    m = fix_fac_contents(master_filename)
    c = fix_fac_contents(compare_filename)

    # Check per element whether c is a subset of m. Could use issubset() but I'm not sure of that re repetitions etc.
    print len(m)
    subset = True
    for i in range(len(c)):
        if not c[i] in m:
            print c[i]
            subset = False

    if subset:
        print 'Compared file IS a subset'
    else:
        print '*****************************'
        print 'Compared file is NOT a subset'
    return


def main():
    # Pass argument from command line as path to file.
    if len(sys.argv) > 1:
        fix_fac_file(sys.argv[1])
    else:
        print '******************************'
        print "Please pass a filename as argument. This script will then fix this file to remove incorrect newlines."
        print '******************************'
    return

if __name__ == '__main__':
    main()
