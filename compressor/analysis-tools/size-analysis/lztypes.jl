## The data types needed in order to be able to work with the JSON'ed data
@enum BlockType Raw Fix Dyn

const HuffCode = Dict{Int,BitArray}

# The individual elements of an LZ-encoded string
abstract type LZElem end;

struct Literal <: LZElem
    value::Int8
end

struct Backreference <: LZElem
    len::Int16
    dist::Int16
end

struct OffsetBackref <: LZElem
    off::Int8
    len::Int16
    dist::Int16
end

## Note: since Julia doesn't have great support for enumerated unions w/ pattern
# matching, we use a plain enum here.
struct Block
    bfinal::Bool
    btype::BlockType
    lenlit_code::HuffCode  # If btype is Raw, the code fields are allowed to be
    dist_code::HuffCode    # empty since we don't need the dictionaries.
    data::Vector{LZElem}
end


## Custom error type for the parse
struct DecodeLZ77JSONException <: Exception
    what::String
end

Base.showerror(io::IO, e::DecodeLZ77JSONException) = print(io, "Error while parsing JSON for LZ77: ", e.what)