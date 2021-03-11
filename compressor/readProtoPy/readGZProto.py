import deflateinfo_pb2


class SymTag(object):
    Literal = 0
    Backref = 1
    Offset = 2
    Invalid = 3


class BlockTag(object):
    Raw = 0
    Dyn = 1
    Invalid = 2


def try_del_member(obj, name):
    try:
        delattr(obj, name)
    except AttributeError:
        pass


# Since python does not offer great enum support, we encapsulate our last level
# symbols into a single class type.
class DeflateSym(object):
    def __init__(self):
        self.tag = SymTag.Invalid

    def _clear(self):
        try_del_member(self, "tag")
        try_del_member(self, "value")
        try_del_member(self, "length")
        try_del_member(self, "dist")
        try_del_member(self, "offset")

    def set_value(self, val):
        self._clear()
        self.tag = SymTag.Literal
        self.value = val

    def set_backref(self, length, dist):
        self._clear()
        self.tag = SymTag.Backref
        self.length = length
        self.dist = dist

    def set_offset(self, off, length, dist):
        self._clear()
        self.tag = SymTag.Offset
        self.offset = off
        self.length = length
        self.dist = dist

    def is_literal(self):
        return self.tag == SymTag.Literal

    def is_backref(self):
        return self.tag == SymTag.Backref

    def is_offset(self):
        return self.tag == SymTag.Offset

    def issymkind(self, tag):
        return self.tag == tag

    def validate(self):
        if self.tag == SymTag.Literal:
            if not 0 <= self.value <= 256:
                return f"Literal value out of range: {self.value}"
        elif self.tag == SymTag.Backref:
            emsg = ""
            if not 1 <= self.dist <= 32768:
                emsg += f"Backref Distance out of range: {self.dist}"
            if not 3 <= self.length <= 258:
                emsg += f"Backref Length out of range: {self.length}"

            if emsg:
                return emsg
        else:
            return "Invalid DeflateSym variant"

    def __repr__(self):
        if self.tag == SymTag.Literal:
            return f"Literal({self.value})"
        elif self.tag == SymTag.Backref:
            return f"Backref({self.length}, {self.dist})"
        elif self.tag == SymTag.Offset:
            return f"OffsetBref({self.offset}, {self.length}, {self.dist})"
        else:
            raise ValueError("Invalid DeflateSym variant")


class UncompressedBlock(object):
    def __init__(self, data):
        self.bytes = data

    def validate(self):
        return None

    def __repr__(self):
        return f"UncompressedBlock({self.bytes})"


class CompressedBlock(object):
    def __init__(self, lenlit_clen, dist_clen, deflate_symbols):
        self.lenlit_clen = lenlit_clen
        self.dist_clen = dist_clen
        self.deflate_symbols = deflate_symbols

    def validate(self):
        symerrs = [s.validate() for s in self.deflate_symbols]
        symerrs = [s for s in symerrs if s is not None]

        code_errs = ""
        for (k, v) in self.lenlit_clen.items():
            if not 0 <= k <= 286:
                code_errs += f"Invalid lenlit key {k};"
            if not 1 <= v <= 16:
                code_errs += f"Invalid lenlit codelength {v};"

        for (k, v) in self.dist_clen.items():
            if not 0 <= k <= 29:
                code_errs += f"Invalid dist key {k};"
            if not 1 <= v <= 16:
                code_errs += f"Invalid dist codelength {v};"

        if not code_errs and not symerrs:
            return None
        else:
            return code_errs + ";".join(symerrs)

    def __repr__(self):
        reprstring = (
            f"CompessedBlock(lenlit_lens: {self.lenlit_clen}, "
            + f"dist_lens: {self.dist_clen} "
            + f"Symbols: "
        )
        reprstring += ",".join((repr(sym) for sym in self.deflate_symbols))
        reprstring += ")"
        return reprstring


class DeflateBlock(object):
    def __init__(self, final):
        self.tag = BlockTag.Invalid
        self.bfinal = final

    def _clear(self):
        try_del_member(self, "tag")
        try_del_member(self, "block")

    def set_block(self, tag, block):
        self._clear()
        self.tag = tag
        self.block = block

    def set_compressed_block(self, block):
        self._clear()
        self.tag = BlockTag.Raw
        self.block = block

    def set_uncompressed_block(self, block):
        self._clear()
        self.tag = BlockTag.Dyn
        self.block = block

    def validate(self):
        return self.block.validate()

    def repr(self):
        blockstr = repr(self.block)
        if self.tag == BlockTag.Raw:
            retstr = "Block_Raw(" + blockstr + ")"
        elif self.tag == BlockTag.Dyn:
            retstr = "Block_Dyn(" + blockstr + ")"
        else:
            raise ValueError("Invalid Block Tag")

        if self.bfinal:
            retstr = "FINAL " + retstr
        return retstr


class DeflateStream(object):
    def __init__(self, blocks):
        self.blocks = blocks

    def validate(self):
        e = ""
        errmsgs = [b.validate() for b in self.blocks]
        bfinals_nonfinal = [b.bfinal for b in self.blocks[1:-1]]
        if any(bfinals_nonfinal):
            e += f"bfinal set in a nonfinal block;"
        if not self.blocks[-1].bfinal:
            e += f"bfinal not set in final block;"
        es = [e for e in errmsgs if e is not None]
        es = [e for e in es if e]
        if es:
            e += "\n".join(es)
        return e if e else None

    def __repr__(self):
        "\n".join((repr(block) for block in self.blocks))


import sys, os


def read_protofile(filename):
    """Read the given filename into a protobuf object"""
    with open(filename, "rb") as inf:
        message = deflateinfo_pb2.DEFLATEStream()
        message.ParseFromString(inf.read())
        return message


def translate_deflatestream(stream):
    """Convert a protobuf-typed stream into an one."""
    blocks = [translate_deflateblock(block) for block in stream.blocks]
    return DeflateStream(blocks)


def translate_deflateblock(block):
    bfinal = block.bfinal
    blockdata = block.data
    if blockdata.WhichOneof("data") == "raw":
        tagtype = BlockTag.Raw
        outblockdata = translate_uncompressedblock(blockdata.raw)
    elif blockdata.WhichOneof("data") == "block":
        tagtype = BlockTag.Dyn
        outblockdata = translate_compressedblock(blockdata.block)
    else:
        print("Error: invalid UnderlyingBlock variant")
    newblock = DeflateBlock(bfinal)
    newblock.set_block(tagtype, outblockdata)
    return newblock


def translate_compressedblock(cblock):
    outsyms = [translate_deflatesym(sym) for sym in cblock.data]
    return CompressedBlock(cblock.lenlit_codelen, cblock.dist_codelen, outsyms)


def translate_uncompressedblock(ublock):
    return UncompressedBlock(ublock.data)


def translate_deflatesym(sym):
    outsym = DeflateSym()
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


if __name__ == "__main__":
    x = read_protofile(sys.argv[1])
    y = translate_deflatestream(x)
