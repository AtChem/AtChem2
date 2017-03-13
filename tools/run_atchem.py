#!/usr/bin/python

"""This script runs the program atchem and displays returned value and the whole output."""

## import necessary modules

import subprocess
import sys
import re
import os
import glob

def clean_up():
	try:
		cleanup_retval = subprocess.call(["rm", '-rf', tmpdir])
	except  OSError, e:
		print >>sys.stderr, "Cleaning of '%s' failed! System error: %s" % (tmpdir, e)
		exit(1)
	return

def failsafe_mkdir(dirname):
	try:
		os.mkdir(dirname)
	except OSError, e:
		print  >>sys.stderr, "Error! Failed to create directory '%s'.\nSystem error: '%s'. Aborting script." % (dirname, e)
		clean_up()
		exit (1)
	return

def failsafe_copy(f1, f2):
	cmd = ['cp', f1, f2 ]
	try: 
		retval = subprocess.call(cmd)
	except OSError, e:
		print  >>sys.stderr, "Error! Failed copying '%s' to '%s'.\nSystem error: '%s'. Aborting script." % (f1, f2, e)
		clean_up()
		exit (1)
	if retval != 0 :
		print "Failed copying '%s' to '%s'. Copy returned value %d. Aborting script." % (f1, f2, retval)
		clean_up()
		exit (1)
	return

## set paths
atchem_exec = '/var/www/html/mcm/atchem/atchem'
atchem_input_dir = '/var/www/html/mcm/atchem/'

## create a temporary directory

pid = os.getpid()
print "PID is %s." % (pid)
tmpdir = '/tmp/atchem_%d' % (pid)
try: 
	os.mkdir(tmpdir )
except OSError, e:
	print  >>sys.stderr, "Error! Failed to create directory '"+tmpdir+"'.\nSystem error: ", e
	exit (1)

## create input directories and copy the appropriate content inside
for dir in ['modelConfiguration', 'environmentalConstraints', 'speciesConstraints']:
	dirname =  tmpdir+'/'+dir
	failsafe_mkdir(dirname)
	for file in glob.glob(atchem_input_dir + '/' +  dir + '/*') :
		if os.path.isfile(file):
			failsafe_copy(file, dirname)

## create output directories
failsafe_mkdir(tmpdir+'/modelOutput')
failsafe_mkdir(tmpdir+'/instantaneousRates')

## execute command defined in cmd variable

cmd = [atchem_exec]

try:
	p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=tmpdir)
except OSError, e:
	print >>sys.stderr, "Execution of command '" , cmd , "' failed! System error:", e
	exit(1)

output = p.communicate()
retval=p.wait()

print "COMMAND ", cmd, " EXECUTED AND RETURNED VALUE ", retval, ".\n"

if  len (output[1]) > 0 :
	print "ERROR OUTPUT\n", output[1], "\n"

print "COMMAND OUTPUT\n", output[0], "\n"

## clean up
clean_up()

exit(0)

