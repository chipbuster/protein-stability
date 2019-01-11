import numpy as np
import pdb
import sys

import isingmodelrect

def gen_random_map(shape):
    """Generate a random mapping for randomized linearization, as a dict
        Input should be the shape of the active area of the ising grid
        (not including boundaries)
    """
    (x,y) = shape
    size = x * y
    linearIndices = np.array([ i for i in range (size) ])
    xInd,yInd = np.meshgrid(list(range(x)), list(range(y)))
    rectIndices = list(zip(np.ravel(xInd),np.ravel(yInd)))

    randomIndices = np.random.shuffle(linearIndices)
    assert len(randomIndices) == len(rectIndices)

    randMap = np.empty(shape,dtype=np.int32)
    i = 0
    for (i,j) in rectIndices:
        randMap[i,j] = randomIndices[i]
        i += 1
    return randMap

# A.k.a cache destroyer (?)
def random_linearize(frame, mapping):
    """Apply a random mapping to a frame to get a linear numpy array
    
    Random mapping is an array of the same size as frame with integer entries
    that tell which linear entry the 
    """
    linBuf = np.empty(len(mapping))
    (x,y) = np.shape(frame)
    xInd,yInd = np.meshgrid(list(range(x)), list(range(y)))
    rectIndices = list(zip(np.ravel(xInd),np.ravel(yInd)))

    for (i,j) in rectIndices:
        linBuf[mapping[i,j]] = frame[i,j]
    return linBuf

def colmajor_linearize(frame):
    """Linearize the frame in column-major order"""
    return np.ravel(frame,order="F")

# Code for this nicked off Wikipedia https://en.wikipedia.org/wiki/Hilbert_curve
def rot(n, x, y, rx, ry):
    if ry == 0:
        if rx == 1:
            x = n-1 - x
            y = n-1 - y
        (x,y) = (y,x)
    return (x,y)

def gen_hcurve(size):
    """Generate the mapping for a Hilbert curve of a particular shape.

    Returns the hilbert curve mapping for a square of size 2^size on either 
    side."""

    if np.floor(np.log2(size)) != np.ceil(np.log2(size)):
        raise ValueError("Input must be a power of 2")
    herp = np.zeros((size,size),dtype=np.int32)

    rx = 0
    ry = 0
    s = 0
    for y in range(size):
        for x in range(size):
            xloc = x  # Avoid screwing up the loop counters
            yloc = y
            d = 0
            s = size//2
            while s > 0:
                rx = 1 if xloc & s > 0 else 0
                ry = 1 if yloc & s > 0 else 0
                d += s * s * ((3 * rx) ^ ry)
                (xloc,yloc) = rot(s, xloc, yloc, rx, ry);
                s //= 2
            herp[x,y] = d
    return herp

def spacefill_linearize(frame, curveMap):
    """Linearize the frame with a Hilbert Curve.

    Specifically, if the frame is square with size 2^n on each side, we
    linearize it by the n-th order Hilbert curve.
    
    curveMap should be generated by gen_hcurve, this is done for efficiency
    """

    (x,y) = np.shape(frame)
    if np.shape(frame) != np.shape(curveMap):
        raise ValueError("Frame and mapping should be same size")

    linear = np.empty(np.size(frame))

    for (x,y),linIndex in np.ndenumerate(curveMap):
        print(linIndex,x,y)
        linear[linIndex] = frame[x,y]

    return linear

# adapted from https://stackoverflow.com/a/36889800
def spiral(width, height):
    NORTH, S, W, E = (0, -1), (0, 1), (-1, 0), (1, 0) # directions
    turn_right = {NORTH: E, E: S, S: W, W: NORTH} # old -> new direction

    if width < 1 or height < 1:
        raise ValueError
    x, y = 0,0 # start near the center
    dx, dy = NORTH # initial direction
    matrix = [[None] * width for _ in range(height)]
    count = 0

    def inbounds(x,y):
        return x >= 0 and x < width and y >= 0 and y < height

    while True:
        matrix[y][x] = count # visit
        count += 1
        # try to turn right
        new_dx, new_dy = turn_right[dx,dy]
        turn_x, turn_y = x + new_dx, y + new_dy
        straight_x, straight_y = x + dx, y + dy

        if inbounds(straight_x,straight_y) and matrix[straight_y][straight_x] is None:
            x,y = straight_x, straight_y
        elif inbounds(turn_x,turn_y) and matrix[turn_y][turn_x] is None:
            x,y = turn_x,turn_y
            dx, dy = new_dx, new_dy
        else:
            return matrix

def linearize_frames(frames, layout):
    """Create a linear (numpy) data buffer from a series of frames.

    layout describes the process for turning a 2D sequence into a 1D sequence.

        Possible values:
           - random: Create a random (but consistent per-frame) mapping
           - row: map by row-major order
           - col: map by column-major order
           - spacefill: use a space filling curve (must be a square grid)
           - spiral: spiral from top left corner to right, then down, then cw in
           - temporal: not yet implemented
    """
    (width,height) = isingmodelrect.spingrid_size(frames[0])
    frameSize = width * height
    numEntries = frameSize * len(frames)

    if layout == "random":
        linearized = np.ones(numEntries, dtype=np.int8) * 5 # Dummy value for testing
        mapping = gen_random_map((width,height))
        for (frameNum, rawFrame) in enumerate(frames):
            frame = isingmodelrect.discard_borders(rawFrame)
            startIndex = frameNum * frameSize
            endIndex = (frameNum+1) * frameSize
            linearized[startIndex:endIndex] = random_linearize(frame,mapping)
        return linearized

    elif layout == "spacefill":
        linearized = np.ones(numEntries, dtype=np.int8) * 5 # Dummy value for testing
        if width != height:
            raise ValueError("Grid must square to use spacefilling linearization!")
        mapping = gen_hcurve(width)
        for (frameNum, rawFrame) in enumerate(frames):
            frame = isingmodelrect.discard_borders(rawFrame)
            startIndex = frameNum * frameSize
            endIndex = (frameNum+1) * frameSize
            linearized[startIndex:endIndex] = spacefill_linearize(frame,mapping)

        return linearized

    elif layout == "temporal":
        raise NotImplementedError("Temporal mapping is not yet implemented")

    elif layout == "row":
        return np.ravel(frame,order="C")

    elif layout == "col":
        return np.ravel(frame,order="F")

    elif layout == "spiral":
        linearized = np.ones(numEntries, dtype=np.int8) * 5 # Dummy value for testing
        mapping = np.array(spiral(width,height))
        for (frameNum,frame) in enumerate((isingmodelrect.discard_borders(f)\
                                             for f in frames)):
            it = np.nditer(frame, flags=['multi_index'])
            while not it.finished:
                (i,j) = it.multi_index
                localLinearIndex = mapping[i,j]
                globalLinearIndex = localLinearIndex + frameNum * frameSize
                linearized[globalLinearIndex] = frame[i,j]
                it.iternext()

        return linearized

