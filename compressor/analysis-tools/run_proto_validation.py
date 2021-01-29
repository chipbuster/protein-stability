import sys, os

from readLZProto import *

proto = read_protofile(sys.argv[1])
stream = translate_deflatestream(proto)
errs = stream.validate()

if errs is None:
    print("No errors for " + os.path.basename(sys.argv[1]))
else:
    print(f"In file {os.path.basename(sys.argv[1])}, found errors:")
    print(errs)
