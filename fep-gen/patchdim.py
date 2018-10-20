#!/usr/bin/env python

# Patch dimensions in a NAMD runfile

import re
import sys,os
from subprocess import call

infile = open('dimensions.out','r')

try:
    (a,b) = infile.readlines()
except e:
    print("dimensions.out seems to have been edited. It should only have two lines")
    print(e)
    sys.exit(1)

# Delete curlybraces
a = a.replace("{","")
a = a.replace("}","")
b = b.replace("{","")
b = b.replace("}","")

a = a.split()
b = b.split()

# Get numbers
xmin = float(a[2])
ymin = float(a[3])
zmin = float(a[4])
xmax = float(a[5])
ymax = float(a[6])
zmax = float(a[7])

xcent = float(b[2])
ycent = float(b[3])
zcent = float(b[4])

xsz = xmax - xmin
ysz = ymax - ymin
zsz = zmax - zmin

cb1string = "cellBasisVector1     %f 0. 0."%xsz
cb2string = "cellBasisVector2     0. %f 0."%ysz
cb3string = "cellBasisVector3     0. 0. %f"%zsz
ccstring  = "cellOrigin           %f %f %f"%(xcent,ycent,zcent)

#print cb1string
#print cb2string
#print cb3string
#print ccstring

for fprefix in ['equilibrate']:
#for fprefix in ['equilibrate','forward','backward']:
    fname = os.path.join('namd-templates', fprefix + ".namd.template")
    outfname = fprefix + ".namd"
    f = open(outfname,'w')
    call(['sed', "s/REPLACE_CELL_BASIS_1/%s/g"%cb1string, fname], stdout=f)
    call(['sed', "-i", "s/REPLACE_CELL_BASIS_2/%s/g"%cb2string,outfname])
    call(['sed', "-i", "s/REPLACE_CELL_BASIS_3/%s/g"%cb3string,outfname])
    call(['sed', "-i", "s/REPLACE_CELL_ORIGIN/%s/g"%ccstring,outfname])
