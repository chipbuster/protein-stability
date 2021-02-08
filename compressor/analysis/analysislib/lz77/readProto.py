from analysislib import LZTypes
from analysislib.protospecs import lz77info_pb2
import sys, os


class LZCompressedStream(object):
    def __init__(self, bytes, syms):
        self.bytes = bytes
        self.syms = syms

    def __repr__(self):
        return repr(self.bytes) + "; Syms:" + ",".join((repr(sym) for sym in self.syms))


def read_protofile(filename):
    """Read the given filename into a protobuf object"""
    with open(filename, "rb") as inf:
        message = lz77info_pb2.Compressed()
        message.ParseFromString(inf.read())
        print(message)
        return message


def translate_deflatesym(sym):
    outsym = LZTypes.DeflateSym()
    if sym.WhichOneof("sym") == "lit":
        outsym.set_value(sym.lit.value)
    elif sym.WhichOneof("sym") == "backref":
        #        print("Found backref")
        outsym.set_backref(sym.backref.length, sym.backref.distance)
    elif sym.WhichOneof("sym") == "offset":
        outsym.set_offset(sym.offset.offset, sym.offset.length, sym.offset.distance)
    else:
        print("Error: invalid DeflateSym variant")
    return outsym


def translate_compressed(deflatesym_vec):
    """Convert a protobuf-typed stream into an LZTypes one."""
    syms = [translate_deflatesym(sym) for sym in deflatesym_vec.syms]
    return LZCompressedStream(deflatesym_vec.rawbytes, syms)
