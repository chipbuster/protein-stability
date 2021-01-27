include("lztypes.jl")
include("readprotobuf.jl")

function read_lz_proto(filename)
    open(filename, "r") do f
        proto = readproto(f, Proto.DEFLATEStream())
        decode_stream_proto(proto)        
    end
end