from math import ceil

pylab_ok=False
import os
	
try:
	os.environ['MPLCONFIGDIR'] = '/tmp'
	import matplotlib
	matplotlib.use('Agg')
	from  pylab import *
	import Image
	pylab_ok = True
except Exception, e:
	pylab_ok = False

def table_of_plots(datafile, outputfile, maxrows=10, ncols=2):
	# Plot multiple columns of the datafile in separate plots.
	# Numbers ncols and maxrows determine the layout of the subplots on the figure:
	# there will be <ncols> subplots in a row
	# and up to <maxrows> rows of plots.
	# The result is saved as one large png file in <outputfile>.
	# maxrows<=0 means that all data should be plotted (not tested!)
	if not pylab_ok:	
		return ()
	
	if ncols<1 :
		ncols=1
	try:
		df = open(datafile, "r")
	except Exception, e:
		raise "Problem! Unable to open data file '%s': %s! Aborting multiplot.\n" % (datafile, e)

	lines = df.readlines()
	df.close()

	headers = lines.pop(0).split()
	if len(lines[0].split()) <= 1:
		return()
	data = [ map(float,s.split()) for s in lines ]
	timecol = [d[0] for d in data]

	nplots =  len(data[0])-1
	if maxrows>0:
		maxplots=maxrows*ncols
	else:
		maxplots=-1
	if maxplots>0 and nplots>maxplots:
		nplots=maxplots
	nrows = ceil(float(nplots)/ncols)

	rcParams['xtick.labelsize'] = 6
	rcParams['ytick.labelsize'] = 8
	xsize=ncols*4
	ysize=nrows*1.5
	dpi=80
	figure(figsize=(xsize,ysize), dpi=dpi)
	vmargin=0.3
	relvmargin=float(vmargin)/ysize
	subplots_adjust( bottom=relvmargin,top=1-relvmargin, wspace=0.3, hspace=0.4)

	for i in range(nplots):
		n = i+1
		subplot(nrows, ncols, n)
		legend((headers[n],))
		y = [d[n] for d in data]
		plot(timecol, y)

	savefig(outputfile)
	
	im = Image.open(outputfile)
	return im.size

	
