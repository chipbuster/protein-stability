from analysislib import LZTypes
from analysislib.protospecs import deflateinfo_pb2
import sys, os


def read_protofile(filename):
    """Read the given filename into a protobuf object"""
    with open(filename, "rb") as inf:
        message = deflateinfo_pb2.DEFLATEStream()
        message.ParseFromString(inf.read())
        return message


def translate_deflatestream(stream):
    """Convert a protobuf-typed stream into an LZTypes one."""
    blocks = [translate_deflateblock(block) for block in stream.blocks]
    return LZTypes.DeflateStream(blocks)


if __name__ == "__main__":
    x = read_protofile(sys.argv[1])
    y = translate_deflatestream(x)
