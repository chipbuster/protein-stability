import sys,csv
import numpy as np
import matplotlib.pyplot as plt

from collections import namedtuple, defaultdict

Measurement = namedtuple("Measurement",["T", "layout", "bb", "ratio"])

values = []

with open(sys.argv[1],'r',newline='') as csvfile:
    rr = csv.reader(csvfile, delimiter='\t')
    next(rr) # skip first row
    for row in rr:
        r = Measurement(float(row[0]), row[1], row[2], float(row[3]))
        values.append(r)

# Group on layout and bb

g_layout = defaultdict(lambda : defaultdict(list))
for entry in values:
    g_layout[entry.layout][entry.bb].append(entry)

for x in g_layout:
    for y in g_layout[x]:
        temptrial = g_layout[x][y]
        Ts = [z.T for z in temptrial]
        Rs = [z.ratio for z in temptrial]
        plt.plot(Ts,Rs)
        plt.title(x + " layout under " + y + " packing")
        plt.show()
