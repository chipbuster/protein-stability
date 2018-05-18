import sys
import pickle

import numpy as np
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d.axes3d as p3
import matplotlib.animation as animation

from collections import namedtuple

nettest = __import__("network-test")

Config = namedtuple("Config", ["pos","edges","restlens"])

FPS = 60
ms_per_s = 1000

# Time functions taken from https://stackoverflow.com/a/49382421/2914377

def update_time():
    t_min = 0.0
    t = 0.0
    t_max = 1.0

    while t<=t_max and t>=t_min:
        t += 1.0/FPS * anim.direction
        if t < 0:
            t = t_max
        yield t

def on_press(event):
    if event.key.isspace():
        if anim.running:
            anim.event_source.stop()
        else:
            anim.event_source.start()
        anim.running ^= True
    elif event.key == 'left':
        anim.direction = -1
    elif event.key == 'right':
        anim.direction = +1

    # Manually update the plot
    if event.key in ['left','right']:
        anim._step()

def loadConfigs(fname):
    """ Load (ref, cut) from picklefile """
    with open(fname,'rb') as infile:
        return pickle.load(infile)

def genLinesData(pos, edges):
    """ Generate data for the lines of a single config
    
        Data output is an array of numpy arrays. Each sub-array
        is 2x3, representing a single line. The endpoints are in
        the row of each sub-array, e.g. the first endpoint of the
        third line is accessed by data[2,0,:].
    """
    data = np.empty((np.shape(edges)[0], 2, 3))

    for (i, (e1,e2)) in enumerate(edges):
        e1vec = pos[e1,:]
        e2vec = pos[e2,:]

        data[i, 0, :] = e1vec
        data[i, 1, :] = e2vec

    return data

def updateLines(time, lines, defVector1, Cref, Ccut):
    print(time)
    defVector = time * defVector1

    refLineData = genLinesData(Cref.pos, Cref.edges)
    cutLineData = genLinesData(Cref.pos + defVector, Ccut.edges)

    allData = np.concatenate((refLineData, cutLineData), axis=0)

    assert len(allData) == len(lines)

    for (line, newData) in zip(lines, allData):
        line.set_data(newData[:,0], newData[:,1])
        line.set_3d_properties(newData[:,2])

    return lines

def getBB(Cpair):
    """ Get bounding box of configuration pair 
    
        Output: [[minx, miny, minz],
                 [maxx, maxy, maxz]]
    """

    allPos = np.concatenate((Cpair[0].pos, Cpair[1].pos), axis=0)

    mins = np.amin(allPos, axis=0)
    maxs = np.amax(allPos, axis=0)

    return np.array([mins,maxs])

fig = plt.figure()
ax = p3.Axes3D(fig)

(Cref, Ccut) = loadConfigs(sys.argv[1])

refData = genLinesData(Cref.pos, Cref.edges)
cutData = genLinesData(Ccut.pos, Ccut.edges)

allData = np.concatenate((refData,cutData), axis=0)

lines = []
for (i, data) in enumerate(allData):
    color = 'red' if i < np.shape(refData)[0] else 'blue'
    lines.append(ax.plot(data[:,0], data[:,1], data[:,2], c=color)[0])

# Deformation vector from Reference to Cut positions
defVectorRC = Ccut.pos - Cref.pos

# Setting the axes properties
bounds = getBB((Cref, Ccut))
ax.set_xlim3d(bounds[:,0])
ax.set_xlabel('X')

ax.set_ylim3d(bounds[:,1])
ax.set_ylabel('Y')

ax.set_zlim3d(bounds[:,2])
ax.set_zlabel('Z')

ax.set_title('Network Cut Test')

fig.canvas.mpl_connect('key_press_event', on_press)
anim = animation.FuncAnimation(fig, updateLines, frames=update_time, 
                          fargs=(lines, defVectorRC, Cref, Ccut),
                          interval=(ms_per_s // FPS), blit=False)
anim.running = True
anim.direction = +1

plt.show()