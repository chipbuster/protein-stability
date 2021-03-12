import Base.isempty

abstract type DeflateSym end
abstract type DeflateBlock end

# For symmetry, store both backref lengths and backref dists under the same type
RefLenTy = UInt32
RefDistTy = UInt32

struct Literal <: DeflateSym
    value::UInt8
end

struct Backreference <: DeflateSym
    length::RefLenTy
    distance::RefDistTy
end

struct OffsetBackreference <: DeflateSym
    offset::UInt8
    length::RefLenTy
    distance::RefDistTy
end

struct UncompressedBlock <: DeflateBlock
    bytes::Vector{UInt8}
end

struct CompressedBlock <: DeflateBlock
    lenlit_clen::Dict{RefLenTy,UInt8}
    dist_clen::Dict{RefDistTy,UInt8}
    syms::Vector{DeflateSym}
end

struct GZIPStream
    blocks::Vector{DeflateBlock}
end

struct LZ77Stream
    decoded_len::UInt
    syms::Vector{DeflateSym}
end

isempty(x::UncompressedBlock) = isempty(x.bytes)
isempty(x::CompressedBlock) = isempty(x.syms)
isempty(x::GZIPStream) = all.(map(isempty, x.blocks))
isempty(x::LZ77Stream) = all.(map(isempty, x.syms))
