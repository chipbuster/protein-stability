import LZTypes
import deflateinfo_pb2

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


def translate_deflateblock(block):
    bfinal = block.bfinal
    blockdata = block.data
    if blockdata.WhichOneof("data") == "raw":
        tagtype = LZTypes.BlockTag.Raw
        outblockdata = translate_uncompressedblock(blockdata.raw)
    elif blockdata.WhichOneof("data") == "block":
        tagtype = LZTypes.BlockTag.Dyn
        outblockdata = translate_compressedblock(blockdata.block)
    else:
        print("Error: invalid UnderlyingBlock variant")
    newblock = LZTypes.DeflateBlock(bfinal)
    newblock.set_block(tagtype, outblockdata)
    return newblock


def translate_compressedblock(cblock):
    outsyms = [translate_deflatesym(sym) for sym in cblock.data]
    return LZTypes.CompressedBlock(cblock.lenlit_codelen, cblock.dist_codelen, outsyms)


def translate_uncompressedblock(ublock):
    return LZTypes.UncompressedBlock(ublock.data)


def translate_deflatesym(sym):
    outsym = LZTypes.DeflateSym()
    if sym.WhichOneof("sym") == "lit":
        outsym.set_value(sym.lit.value)
    elif sym.WhichOneof("sym") == "backref":
        outsym.set_backref(sym.backref.length, sym.backref.distance)
    elif sym.WhichOneof("sym") == "offset":
        outsym.set_offset(sym.offset.offset, sym.offset.length, sym.offset.distance)
    else:
        print("Error: invalid DeflateSym variant")
    return outsym


if __name__ == "__main__":
    x = read_protofile(sys.argv[1])
    y = translate_deflatestream(x)
