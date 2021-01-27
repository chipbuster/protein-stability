include("protoc_out/DEFLATEProto.jl")

import .DEFLATEProto
Proto = DEFLATEProto

using ProtoBuf;

function inc_bit_array(z::BitArray)
    if all(z)
        z = BitArray(zeros(length(z) + 1))
        z[1] = 1
    else
        i = length(z)
        while z[i] == 1
            z[i] = 0
            i -= 1
        end
        z[i] = 1
    end
    z
end

function sll_bit_array(z::BitArray, shamt)
    n = length(z)
    newarray = BitArray(zeros(n + shamt))
    newarray[1:n] = z
    newarray
end

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

function decode_stream_proto(protostruct::Proto.Literal)
    val = getproperty(protostruct, :value)
    Literal(val)
end

function decode_stream_proto(protostruct::Proto.Backref)
    length = getproperty(protostruct, :length)
    distance = getproperty(protostruct, :distance)
    Backreference(length, distance)
end

function decode_stream_proto(protostruct::Proto.OffsetBackref)
    offset = getproperty(protostruct, :offset)
    length = getproperty(protostruct, :length)
    distance = getproperty(protostruct, :distance)
    Backreference(offset, length, distance)
end

function decode_stream_proto(protostruct::Proto.DeflateSym)
    if hasproperty(protostruct, :lit)
        decode_stream_proto(getproperty(protostruct, :lit))
    elseif hasproperty(protostruct, :backref)

        decode_stream_proto(getproperty(protostruct, :backref))
    elseif hasproperty(protostruct, :offset)
        decode_stream_proto(getproperty(protostruct, :offset))
    end
end

function decode_stream_proto(protostruct::Proto.CompressedBlock)
    data = [ decode_stream_proto(d) for d in getproperty(protostruct, :data) ]
    lenlit_codelen = getproperty(protostruct, :lenlit_codelen)
    dist_codelen = getproperty(protostruct, :dist_codelen)
    lenlit_code = construct_codes_from_codelengths(lenlit_codelen)
    dist_code = construct_codes_from_codelengths(dist_codelen)
    codetrees = BlockCode(lenlit_code, dist_code)
    Block(false, Dyn, codetrees, data, [])
end

function decode_stream_proto(protostruct::Proto.UncompressedBlock)
    bytes = getproperty(protostruct, :data)
    codes = BlockCode(Dict(), Dict())
    Block(false, Raw, codes, [], bytes)
end

function decode_stream_proto(protostruct::Proto.UnderlyingBlock)
    underdata = if hasproperty(protostruct, :raw)
        getproperty(protostruct, :raw)
    else
        getproperty(protostruct, :block)
    end
    return decode_stream_proto(underdata)
end

# This is a little funky: in the protobuf, compression trees are associated with
# the CompressedBlock while UncompressedBlocks are empty. To deal with this, we
# get the structure from below the UnderlyingBlock and then deal with it
# appropriately
function decode_stream_proto(protostruct::Proto.DEFLATEBlock)
    bfinal = getproperty(protostruct, :bfinal)
    protodata = getproperty(protostruct, :data)
    testblock = decode_stream_proto(protodata)
    if testblock.bfinal == bfinal
        testblock
    else
        Block(bfinal, testblock.btype, testblock.codetrees,
                      testblock.data, testblock.bytedata)
    end
end

function decode_stream_proto(protostruct::Proto.DEFLATEStream)::DEFLATEStream
    blocks = [ decode_stream_proto(b) for b in getproperty(protostruct, :blocks) ]
    DEFLATEStream(blocks)
end