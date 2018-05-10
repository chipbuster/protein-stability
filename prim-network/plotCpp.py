#!/usr/bin/env python3

import matplotlib.pyplot as plt
import sys

a = sys.stdin.read()
lines = a.split('\n')[1:-1]
entries = [ x.split(' ') for x in lines]

parseEntries = [ (float(e[1]), float(e[2]), float(e[3])) for e in entries]

sDists = sorted(parseEntries)
nedges = len(parseEntries)

fig = plt.figure()
plt.plot(range(nedges), [x[0] for x in sDists], c='red', marker='o')
plt.plot(range(nedges), [x[1] for x in sDists], c='green', marker='o')
plt.show(block=True)