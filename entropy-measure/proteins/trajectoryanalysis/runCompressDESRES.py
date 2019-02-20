from compressstate_ic import *
from MDAnalysis import Universe

try:
    desresfile = sys.argv[1]
    bincount = int(sys.argv[2])
    skipframes = int(sys.argv[3])
except:
    print("Usage:%s <path-to-desres> <#bins> <#frameskip>")
    sys.exit(1)

univ = MDAnalysis.Universe(desresfile)
c = CompressionData(univ, bincount, skipframes)
print("Compression ratio is: " + str(c.get_compression_ratios()))

