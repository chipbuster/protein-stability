from compressstate_ic import *
from MDAnalysis import Universe

try:
    topofile = sys.argv[3]
    trajfile = sys.argv[4:]
    bincount = int(sys.argv[1])
    skipframes = int(sys.argv[2])
except:
    print("Usage:%s <#bins> <#frameskip> <path-to-topo> <path-to-traj> [additional traj]")
    print("Topo file: describes connectivity of molecule, e.g. PSF, PDB")
    print("Traj file: describes positions of atoms, e.g. DCD, TRJ")
    sys.exit(1)

univ = MDAnalysis.Universe(topofile, trajfile)
c = CompressionData(univ, bincount, skipframes)
print("Compression ratio is: " + str(c.get_compression_ratios()))