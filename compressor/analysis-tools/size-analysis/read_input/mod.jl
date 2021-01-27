include("lztypes.jl")
include("readprotobuf.jl")

function read_lz_proto(filename)
#    fn = basename(filename)
#    parts = split(fn, '_')
#    natoms = parse(Int, parts[2])
#    frac = parse(Float64, parts[3])
#    njit = parse(Int, parts[4])
    info = (0, 0, 0)
    open(filename, "r") do f
        proto = readproto(f, Proto.DEFLATEStream())
        stream = decode_stream_proto(proto)        
        return (info, stream)
    end
end