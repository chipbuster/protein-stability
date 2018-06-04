import sys
import pickle

import numpy as np
from collections import namedtuple

nettest = __import__("network-test")

Config = namedtuple("Config", ["pos","edges","restlens"])

FPS = 60
ms_per_s = 1000

# Time functions taken from https://stackoverflow.com/a/49382421/2914377
def loadConfigs(fname):
    """ Load (ref, cut) from picklefile """
    with open(fname,'rb') as infile:
        return pickle.load(infile)
