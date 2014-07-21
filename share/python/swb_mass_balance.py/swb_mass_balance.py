from netCDF4 import Dataset
import numpy as np
import os, glob, subprocess

mydir = '/Users/smwesten/SMWData/Source_Code/SWB_TEST_CASES/Kirstin_Seleen_NY_v3/'

os.chdir(mydir)

print mydir

mycommonvars = ["x", "y", "time"]

filelist = glob.glob("annual_sum_*")
for f in filelist:
    os.remove(f)

filelist = glob.glob("*.nc")
for f in filelist:
    print f
    cmd =  "/usr/local/bin/cdo yearsum " + f + " ann_sum_" + f
    subprocess.call( cmd )
    fh = Dataset(f, mode='r')
    x = fh.variables['x'][:]
    y = fh.variables['y'][:]
    for v in fh.variables:
        if ( v not in mycommonvars ):
            myvar = fh.variables[ v ][:]
            print "My variable = " + str(myvar)

