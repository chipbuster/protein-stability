include("protoc_out/DEFLATEProto.jl")

import .DEFLATEProto
Proto = DEFLATEProto

using ProtoBuf;

function construct_codes_from_codelengths(clens)::HuffCode
    out = Dict{Int,BitArray}()

    # Sort keys by keylength and then lexicography
    ks = sort(collect(keys(clens)))
    lns = [ clens[k] for k in ks ]
    ind = sortperm(lns; alg=MergeSort)
    ks = ks[ind]
    lns = lns[ind]

    code = BitArray(zeros(lns[1]))
    for i in 1:length(ks)
        k = ks[i]
        out[k] = copy(code)
        code = inc_bit_array(code)
        if i < length(ks)
            code = sll_bit_array(code, clens[ks[i + 1]] - clens[ks[i]])
        end
    end
    out
end

function validate_stream_proto(protostruct::Proto.Literal)
    val = getproperty(protostruct, :value)
    0 <= val <= 255 ? "" : "Invalid value $(val);"
end

function validate_stream_proto(protostruct::Proto.Backref)
    length = getproperty(protostruct, :length)
    distance = getproperty(protostruct, :distance)
    e1 = 3 <= length <= 258 ? "" : "Invalid length $(length);"
    e2 = 1 <= distance <= 32768 ? "" : "Invalid dist $(distance);"
    e1 * e2
end

function validate_stream_proto(protostruct::Proto.OffsetBackref)
    offset = getproperty(protostruct, :offset)
    length = getproperty(protostruct, :length)
    distance = getproperty(protostruct, :distance)
    e1 = 3 <= length <= 258 ? "" : "Invalid length $(length);"
    e2 = 1 <= distance <= 32768 ? "" : "Invalid dist $(distance);"
    e1 * e2
end

function validate_stream_proto(protostruct::Proto.DeflateSym)
    if hasproperty(protostruct, :lit)
        validate_stream_proto(getproperty(protostruct, :lit))
    elseif hasproperty(protostruct, :backref)

        validate_stream_proto(getproperty(protostruct, :backref))
    elseif hasproperty(protostruct, :offset)
        validate_stream_proto(getproperty(protostruct, :offset))
    end
end

function validate_lenlit_dict(clens)
    out = ""
    for k in keys(clens)
        if ! 0 <= k <= 286
            out *= "Invalid lenlit codepoint key $(k);"
        end
    end
    for v in values(clens)
        if v > 16
            out *= "Invalid lenlit codepoint length $(v)"
        end
    end
    out
end

function validate_dist_dict(clens)
    out = ""
    for k in keys(clens)
        if ! 0 <= k <= 29
            out *= "Invalid dist codepoint key $(k);"
        end
    end
    for v in values(clens)
        if v > 16
            out *= "Invalid dist codepoint length $(v)"
        end
    end
    out
end

function validate_stream_proto(protostruct::Proto.CompressedBlock)
    data = [ validate_stream_proto(d) for d in getproperty(protostruct, :data) ]
    lenlit_codelen = getproperty(protostruct, :lenlit_codelen)
    dist_codelen = getproperty(protostruct, :dist_codelen)
    lenlit_code = construct_codes_from_codelengths(lenlit_codelen)
    dist_code = construct_codes_from_codelengths(dist_codelen)
    
    out = ""
    out *= validate_lenlit_dict(lenlit_codelen)
    out *= validate_dist_dict(dist_codelen)
    out *= join((validate_stream_proto(d) for d in data), ";")
    out
end

function validate_stream_proto(protostruct::Proto.UncompressedBlock)
    ""
end

function validate_stream_proto(protostruct::Proto.UnderlyingBlock)
    underdata = if hasproperty(protostruct, :raw)
        getproperty(protostruct, :raw)
    else
        getproperty(protostruct, :block)
    end
    return validate_stream_proto(underdata)
end

# This is a little funky: in the protobuf, compression trees are associated with
# the CompressedBlock while UncompressedBlocks are empty. To deal with this, we
# get the structure from below the UnderlyingBlock and then deal with it
# appropriately
function validate_stream_proto(protostruct::Proto.DEFLATEBlock)
    bfinal = getproperty(protostruct, :bfinal)
    protodata = getproperty(protostruct, :data)
    validate_stream_proto(protodata)
end

function validate_stream_proto(protostruct::Proto.DEFLATEStream)::DEFLATEStream
    errmsgs = [ validate_stream_proto(b) for b in getproperty(protostruct, :blocks) ]
    join(errmsgs,"")
end
