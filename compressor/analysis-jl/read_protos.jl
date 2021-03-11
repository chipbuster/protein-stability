using PyCall;

fdir = @__DIR__

pushfirst!(PyVector(pyimport("sys")."path"), joinpath(dirname(@__DIR__), "readProtoPy"))

readlz77 = pyimport("readLZProto")
readgzip = pyimport("readGZProto")

"""Generates a DeflateSym from a python DeflateSym"""
function deflatesym_from_pysym(sym)
    if sym.tag == 0   # Literal
        return Literal(sym.value)
    elseif sym.tag == 1  # Backreferenc
        return Backreference(sym.length, sym.dist)
    elseif sym.tag == 2  # OffsetBackreference
        return OffsetBackreference(sym.offset, sym.length, sym.dist)
    else
        error("Invalid symbol tag")
    end
end

"""Generates a DeflateBlock from a python DeflateBlock"""
function deflateblock_from_pyblock(block)
    if block.tag == 0   # Raw block
        return UncompressedBlock(block.block.data)
    elseif block.tag == 1 || block.tag == 2 # Dyn Block
        b = block.block  # Access the underlying CompressedBlock
        return CompressedBlock(b.lenlit_clen, b.dist_clen, map(deflatesym_from_pysym, b.deflate_symbols))
    else
        error("Invalid DeflateBlock tag")
    end
end

function read_gz_proto(filename)
    protof = readgzip.read_protofile(filename)
    pystream = readgzip.translate_deflatestream(protof)

    blocks = [ deflateblock_from_pyblock(pyblock) for pyblock in pystream.blocks ]
    GZIPStream(blocks)
end

function read_lz_proto(filename)
    protof = readlz77.read_protofile(filename)
    pylzcomp = readlz77.translate_compressed(protof)
    syms = [ deflatesym_from_pysym(pysym) for pysym in pylzcomp.syms ]
    LZ77Stream(protof.nbytes_decoded, syms)
end