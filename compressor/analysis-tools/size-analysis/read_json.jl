# Contains the code needed to read in the data dumped by dump_gzip_to_json

include("lztypes.jl")
using JSON;

function get_key_from_object(obj, key)
    try
        obj[key]
    catch err
        if isa(err, KeyError)
            throw(DecodeLZ77JSONException("Did not find $(key) where expected"))
        else
            rethrow()
        end
    end
end

function parse_lz_json_symbol(sym)
    if !isa(sym, Dict)
        throw(DecodeLZ77JSONException("LZ Symbol is not a dict"))
    elseif length(sym) != 1
        throw(DecodeLZ77JSONException("Symbol has more than one entry"))
    end

    symtype = collect(keys(sym))[1]
    symdat = sym[symtype]
    if symtype == "Literal"
        value = symdat
        Literal(value)
    elseif symtype == "Backreference"
        len = symdat[1]
        dist = symdat[2]
        Backreference(len, dist)
    elseif symtype == "OffsetBackref"
        off = symdat[1]
        len = symdat[2]
        dist = symdat[3]
        OffsetBackref(off, len, dist)
    else
        throw(DecodeLZ77JSONException("Invalid LZ77 symbol type"))
    end
end

function parse_lz_json_data(object)
    [ parse_lz_json_symbol(sym) for sym in object ]
end

function parse_huff_code(object)
    d = Dict{Int,BitArray}()
    for elem in object
        value = elem[1]
        bits_raw = elem[2]
        bits = convert(BitArray, bits_raw)
        d[value] = bits
    end
    d
end

function parse_compressed_block(object)
    llcode = parse_huff_code(get_key_from_object(object, "lenlit_code"))
    dcode = parse_huff_code(get_key_from_object(object, "dist_code"))
    data = parse_lz_json_data(get_key_from_object(object, "data"))
    (llcode, dcode, data)
end

function parse_raw_block(object)
    println("Got a raw block in the input. Why the hell are we worrying about the compression properties of a raw block??")
    throw(DecodeLZ77JSONException("Found Raw block. What are we doing?"))
end

function parse_block_type(tstr::AbstractString)::BlockType
    if tstr == "Raw"
        Raw
    elseif tstr == "Fix"
        Fix
    elseif tstr == "Dyn"
        Dyn
    else
        error("Invalid block type")
    end
end

function parse_block(object)
    bfinal = get_key_from_object(object, "bfinal")
    data = get_key_from_object(object, "data")

    if length(keys(data)) != 1
        println("WARN: A block object has more than one type!")
    end
    btype_str = collect(keys(data))[1]
    block_json = data[btype_str]

    (llcode, dcode, data) = if btype_str == "Fix" || btype_str == "Dyn"
        parse_compressed_block(block_json)
    elseif btype_str == "Raw"
        parse_raw_block(block_json)  # Doesn't do anything
    else
        throw(DecodeLZ77JSONException("Unknown block type found"))
    end

    btype = parse_block_type(btype_str)

    Block(bfinal, btype, llcode, dcode, data)
end

function parse_gzip_json(filename)
    json_res = open(filename, "r") do f
        JSON.parse(f)
    end
    
    # The output of this should be a dict with a single key "blocks"
    json_blocks = get_key_from_object(json_res, "blocks")
    if !isa(json_blocks, Array)
        throw(DecodeLZ77JSONException("'blocks' was not an array"))
    end

    [ parse_block(b) for b in json_blocks ]
end

