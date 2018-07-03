import sys
import pickle
from math import sqrt
from os.path import basename, splitext

import numpy as np
from collections import namedtuple

from vpython import *
import matplotlib  # for colormaps

nettest = __import__("network-test")

Config = namedtuple("Config", ["pos","edges","restlens"])

FPS = 60
ms_per_s = 1000

strain_color = matplotlib.cm.get_cmap('bwr')
max_color_strain = 1.0

def loadConfigs(fname):
    """ Load (ref, cut) from picklefile """
    with open(fname,'rb') as infile:
        return pickle.load(infile)

def calcColor(length, restlength):
    """Calculate color of spring on bwr colormap"""

    ostrain = (length - restlength) / restlength
    strain = np.clip(ostrain, -max_color_strain, max_color_strain)

    strain = np.sign(strain) * sqrt(abs(strain))

    strain = ((strain / max_color_strain) + 1.0) / 2.0
    
    return strain_color(strain)[0:3]

def configGenGen(Cref, Ccut):
    """ Create a configuration generator 
    
        Given an initial and final configuration, create a function
        that can return any intermediate configuration by interpolation.

        Configurations are linearly interpolated on t = [0,1]
    """

    def interpolate(t):
        np.clip(t,0,1) # Force t to be in [0,1]

        initPos  = Cref.pos
        endPos   = Ccut.pos
        edges    = Cref.edges
        restlens = Cref.restlens

        newPos = (1.0 - t) * initPos + t * endPos
        return Config(newPos, edges, restlens)

    return interpolate


(Cref, Ccut) = loadConfigs(sys.argv[1])
interpFunc = configGenGen(Cref, Ccut)

print(Cref.edges)

# Parse info out of filename
parts = splitext(basename(sys.argv[1]))[0].split("-")
missingSpring = int(parts[-1])

# Set up initial rod configuration
rods = []
for (ctr, (e1,e2)) in enumerate(Cref.edges):
    start = Cref.pos[e1,:]
    end   = Cref.pos[e2,:]
    axis = end - start
    restlen = Cref.restlens[ctr]

    Ccol = calcColor(np.linalg.norm(axis), restlen)

    rod = cylinder(pos=vector(*start), axis=vector(*axis), radius=0.005)

    if ctr == missingSpring:
        rod.color = vector(0.0,1.0,0.0)
    else:
        rod.color = vector(*Ccol)

    rods.append(rod)


scene.lights = []
scene.ambient = vector(0.8,0.8,0.8)
## CUTOFF
sys.exit(0)

t = 0.0
dt = 1.0 / FPS # Time between frames
while t < 2:
    if t > 1.0:
        t = 0.0

    Ccur = interpFunc(t)

    for (ctr, (e1,e2)) in enumerate(Ccur.edges):
        start = Ccur.pos[e1,:]
        end   = Ccur.pos[e2,:]
        axis = end - start
        restlen = Ccur.restlens[ctr]

        Cstrain = (np.linalg.norm(axis) - restlen) / restlen
        Ccoef = Cstrain if Cstrain < 1.0 else 1.0 / Cstrain
        Ccol = (1.0 - Ccoef) * (compression_color if Ccoef < 1 else tension_color)

        rods[ctr].pos   = vector(*start)
        rods[ctr].axis  = vector(*axis)
        rods[ctr].color = vector(*Ccol)

        if ctr == missingSpring:
            rod.color = vector(0.0,1.0,0.0)
