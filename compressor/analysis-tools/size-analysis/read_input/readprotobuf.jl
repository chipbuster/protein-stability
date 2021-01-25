include("protoc_out/deflateinfo_pb.jl")
using ProtoBuf;

open(ARGS[1]) do f
    data = readproto(f, DEFLATEStream())
    display(data)
end