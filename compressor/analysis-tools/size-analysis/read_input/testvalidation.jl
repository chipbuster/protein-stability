include("lztypes.jl")
include("readprotobuf.jl")
include("validateprotobuf.jl")

function test_lz_proto(filename)
    open(filename, "r") do f
        proto = readproto(f, Proto.DEFLATEStream())
        print("$(basename(filename)): ")
        println(validate_stream_proto(proto))
    end
end

test_lz_proto(ARGS[1])
