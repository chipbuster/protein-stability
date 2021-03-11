from analysislib import LZTypes
from analysislib.utils import DEFLATEImpl


def literal_values(block):
    """Given a DeflateBlock, returns a list of literal values found in that block"""
    if block.tag == LZTypes.BlockTag.Raw:
        return [int(x) for x in block.block]
    elif block.tag == LZTypes.BlockTag.Dyn:
        syms = block.block.deflate_symbols
        return [s.value for s in syms if s.issymkind(LZTypes.SymTag.Literal)]
    else:
        raise ValueError("Invalid Block type")


def backref_dists_stream(stream):
    output = []
    for block in stream.blocks:
        output.extend(backref_dists_block(block))
    return output


def backref_lengths_stream(stream):
    output = []
    for block in stream.blocks:
        output.extend(backref_lengths_block(block))
    return output


def backref_dists_block(block):
    """Given a DeflateBlock, returns a list of backreference distances found in that block"""
    if block.tag == LZTypes.BlockTag.Dyn:
        syms = block.block.deflate_symbols
        return [s.length for s in syms if s.issymkind(LZTypes.SymTag.Backref)]
    else:
        raise ValueError("Invalid Block type")


def backref_lengths_block(block):
    """Given a DeflateBlock, returns a list of backreference lengths found in that block"""
    if block.tag == LZTypes.BlockTag.Dyn:
        syms = block.block.deflate_symbols
        return [s.dist for s in syms if s.issymkind(LZTypes.SymTag.Backref)]
    else:
        raise ValueError("Invalid Block type")


def bits_used_sym(sym, lenlit_clens, dist_clens):
    """Given a single DeflateSym and code dictionaries, determines the #bits needed"""
    OFFSET_SIGIL = 286
    if sym.issymkind(LZTypes.SymTag.Literal):
        val = sym.val
        return lenlit_clens[val]
    elif sym.issymkind(LZTypes.SymTag.Backref) or sym.issymkind(LZTypes.SymTag.Offset):
        nbits = 0
        dist = sym.dist
        length = sym.length

        length_cp = DEFLATEImpl.find_lenlit_codepoint(length)
        nbits += lenlit_clens[length_cp.code]
        nbits += length_cp.extrabits

        dist_cp = DEFLATEImpl.find_dist_codepoint(dist)
        nbits += dist_clens[dist_cp.code]
        nbits += dist_cp.extrabits

        if sym.issymkind(LZTypes.SymTag.Offset):
            nbits += lenlit_clens[OFFSET_SIGIL]
            nbits += 8  # Actual offset
    else:
        raise ValueError("Invalid Symbol Type")
