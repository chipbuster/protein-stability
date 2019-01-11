import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

import pickle

## Import data
extr = pickle.load(open("results_lzma_extreme.pkl",'rb'))
deft = pickle.load(open("results_lzma_default.pkl",'rb'))

(Ts_d, theor_d, comp_d) = zip(*deft)
(Ts_e, theor_e, comp_e) = zip(*extr)

## Plot things as shown in paper
f, (ax1,ax2) = plt.subplots(nrows=1,ncols=2,sharey=True)
ax1.title.set_text("Results from Default LZMA")
ax2.title.set_text("Results from Extreme LZMA")

ax1.semilogx(Ts_d, comp_d, 'b.')
ax1.semilogx(Ts_d, theor_d, 'r-')
ax2.semilogx(Ts_e, comp_e, 'b.')
ax2.semilogx(Ts_e, theor_e, 'r-')

r_patch = mpatches.Patch(color='red', label='Theoretical')
b_patch = mpatches.Patch(color='blue', label='Empirical')
plt.legend(handles=[r_patch, b_patch])

plt.figure(2)
plt.title("Percent difference of Compressed vs Theoretical")
t_e = np.array(theor_e)
c_e = np.array(comp_e)
plt.plot(Ts_e[500:], ((c_e - t_e)/t_e)[500:])

plt.show()