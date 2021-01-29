import LZTypes


def literal_values(block):
    """Given a DeflateBlock, returns a list of literal values found in that block"""
    if block.tag == LZTypes.BlockTag.Raw:
        return [int(x) for x in block.block]
    elif block.tag == LZTypes.BlockTag.Dyn:
        syms = block.block.deflate_symbols
        return [s.value for s in syms if s.issymkind(LZTypes.SymTag.Literal)]
    else:
        raise ValueError("Invalid Block type")


def backref_dists(block):
    """Given a DeflateBlock, returns a list of literal values found in that block"""
    if block.tag == LZTypes.BlockTag.Dyn:
        syms = block.block.deflate_symbols
        return [s.length for s in syms if s.issymkind(LZTypes.SymTag.Literal)]
    else:
        raise ValueError("Invalid Block type")


def backref_lengths(block):
    if block.tag == LZTypes.BlockTag.Dyn:
        syms = block.block.deflate_symbols
        return [s.dist for s in syms if s.issymkind(LZTypes.SymTag.Literal)]
    else:
        raise ValueError("Invalid Block type")
