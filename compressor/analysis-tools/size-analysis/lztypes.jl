## The data types needed in order to be able to work with the JSON'ed data
@enum BlockType Raw Fix Dyn

const HuffCode = Dict{Int,BitArray}

# The individual elements of an LZ-encoded string
abstract type LZElem end;

struct Literal <: LZElem
    value::UInt8
end

struct Backreference <: LZElem
    len::UInt16
    dist::UInt16
end

struct OffsetBackref <: LZElem
    off::UInt8
    len::UInt16
    dist::UInt16
end

struct BlockCode
    lenlit_code::HuffCode
    dist_code::HuffCode
end

## Note: since Julia doesn't have great support for enumerated unions w/ pattern
# matching, we use a plain enum here.
struct Block
    bfinal::Bool
    btype::BlockType
    codetrees::BlockCode
    data::Vector{LZElem}
end


## Custom error type for the parse
struct DecodeLZ77JSONException <: Exception
    what::String
end

Base.showerror(io::IO, e::DecodeLZ77JSONException) = print(io, "Error while parsing JSON for LZ77: ", e.what)

# Replicate the data from tables in 3.2.5, RFC 1951
struct CodePointInfo
    code::Int
    extrabits::Int
    val_range::Tuple{Int,Int}
end

const LENLIT_CODEPOINT_INFO = CodePointInfo[
    CodePointInfo(257, 0, (3,3)),
    CodePointInfo(258, 0, (4,4)),
    CodePointInfo(259, 0, (5,5)),
    CodePointInfo(260, 0, (6,6)),
    CodePointInfo(261, 0, (7,7)),
    CodePointInfo(262, 0, (8,8)),
    CodePointInfo(263, 0, (9,9)),
    CodePointInfo(264, 0, (10,10)),
    CodePointInfo(265, 1, (11,12)),
    CodePointInfo(266, 1, (13,14)),
    CodePointInfo(267, 1, (15,16)),
    CodePointInfo(268, 1, (17,18)),
    CodePointInfo(269, 2, (19,22)),
    CodePointInfo(270, 2, (23,26)),
    CodePointInfo(271, 2, (27,30)),
    CodePointInfo(272, 2, (31,34)),
    CodePointInfo(273, 3, (35,42)),
    CodePointInfo(274, 3, (43,50)),
    CodePointInfo(275, 3, (51,58)),
    CodePointInfo(276, 3, (59,66)),
    CodePointInfo(277, 4, (67,82)),
    CodePointInfo(278, 4, (83,98)),
    CodePointInfo(279, 4, (99,114)),
    CodePointInfo(280, 4, (115,130)),
    CodePointInfo(281, 5, (131,162)),
    CodePointInfo(282, 5, (163,194)),
    CodePointInfo(283, 5, (195,226)),
    CodePointInfo(284, 5, (227,257)),
    CodePointInfo(285, 0, (258,258)),
]

const DIST_CODEPOINT_INFO = CodePointInfo[
    CodePointInfo(0, 0, (1,1)),
    CodePointInfo(1, 0, (2,2)),
    CodePointInfo(2, 0, (3,3)),
    CodePointInfo(3, 0, (4,4)),
    CodePointInfo(4, 1, (5,6)),
    CodePointInfo(5, 1, (7,8)),
    CodePointInfo(6, 2, (9,12)),
    CodePointInfo(7, 2, (13,16)),
    CodePointInfo(8, 3, (17,24)),
    CodePointInfo(9, 3, (25,32)),
    CodePointInfo(10, 4, (33,48)),
    CodePointInfo(11, 4, (49,64)),
    CodePointInfo(12, 5, (65,96)),
    CodePointInfo(13, 5, (97,128)),
    CodePointInfo(14, 6, (129,192)),
    CodePointInfo(15, 6, (193,256)),
    CodePointInfo(16, 7, (257,384)),
    CodePointInfo(17, 7, (385,512)),
    CodePointInfo(18, 8, (513,768)),
    CodePointInfo(19, 8, (769,1024)),
    CodePointInfo(20, 9, (1025,1536)),
    CodePointInfo(21, 9, (1537,2048)),
    CodePointInfo(22, 10, (2049,3072)),
    CodePointInfo(23, 10, (3073,4096)),
    CodePointInfo(24, 11, (4097,6144)),
    CodePointInfo(25, 11, (6145,8192)),
    CodePointInfo(26, 12, (8193,12288)),
    CodePointInfo(27, 12, (12289,16384)),
    CodePointInfo(28, 13, (16385,24576)),
    CodePointInfo(29, 13, (24577,32768)),
]