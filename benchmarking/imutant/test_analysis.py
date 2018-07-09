#!/usr/bin/env python3

from test_utils import *
import pickle
import numpy as np

from matplotlib import pyplot as plt

with open('results.pkl', 'rb') as picklefile:
    results = pickle.load(picklefile)

ddG_pred_arr = []
ddG_exp_arr = []
pdbids = []

for entry in results:
    if entry is not None:
        if entry.ddG is not None and entry.params.ddG is not None:
            ddG_pred_arr.append(-entry.ddG)
            ddG_exp_arr.append(entry.params.ddG)
            pdbids.append(entry.params.pdbid)

ddG_exp = np.array(ddG_exp_arr)
ddG_pred = np.array(ddG_pred_arr)

# Compute correlation
ddG_exp_zm = ddG_exp - np.mean(ddG_exp)
ddG_pred_zm = ddG_pred - np.mean(ddG_pred)

corr = np.dot(ddG_exp_zm, ddG_pred_zm) / (
    np.linalg.norm(ddG_exp_zm) * np.linalg.norm(ddG_pred_zm))

# Create color labels for the points
colorIDMap = {}
n = 0
for pdbid in set(pdbids):
    colorIDMap[pdbid] = n
    n += 1

print(set(pdbids))

c = [colorIDMap[x] for x in pdbids]

fig, ax = plt.subplots()
ax.scatter(ddG_exp, ddG_pred, c=c, cmap=plt.cm.prism)

# Create y=x line
lims = [
    np.min([ax.get_xlim(), ax.get_ylim()]),  # min of both axes
    np.max([ax.get_xlim(), ax.get_ylim()]),  # max of both axes
]
# now plot both limits against eachother
ax.plot(lims, lims, 'k-', zorder=2)
ax.plot([0,0], lims, 'r-', zorder=2)

plt.title("ddG Predictions for Pucci 2016 Dataset -- IMutant2.0")
plt.suptitle("Negative ddG is stabilizing")
plt.xlabel("Experimental ddG")
plt.ylabel("Predicted ddG")
plt.text(-4, -5, 'y=x')
plt.text(-6, 8, "Correlation = " + str(corr))
plt.text(2, -4, "Negative ddG is stabilizing")

plt.show()
