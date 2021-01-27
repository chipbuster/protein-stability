using StatsPlots;
using DataFrames;
using PrettyTables;
using Query;
using Base.Threads;

include("jitfnames.jl")
include("size-analysis/mod.jl")
include("../../protein-tools/core/compressor.jl")

function load_proto_dfs(directory)
    df = DataFrame(natoms=Int[], R=Float64[], njitter=Int[], sorted=String[], aatype=Int[], dataindex=Int[])
    outstreams = Vector{DEFLATEStream}()
    l = SpinLock()
    @time @threads for fn in readdir(directory)
        path = joinpath(directory, fn)
        prms = get_params_from_fn(path)
        stream = read_lz_proto(path)
        lock(l)
        push!(outstreams, stream)
        push!(df, 
            (prms.natoms, prms.ext_frac * (prms.natoms - 1), prms.njitter,
             prms.sorted, prms.aatype, length(outstreams))
        )
        unlock(l)
    end
    (outstreams, df)
end

function load_gz_sizes(directory)
    df = DataFrame(natoms=Int[], R=Float64[], njitter=Int[], sorted=String[], aatype=Int[], fsize=Int[])
    for fn in readdir(directory)
        prms = get_params_from_fn(fn)
        filepath = joinpath(directory, fn)
        fsz = filesize(filepath)
        push!(df, 
            (prms.natoms, prms.ext_frac * (prms.natoms - 1), prms.njitter,
             prms.sorted, prms.aatype, fsz)
        )
    end
    df
end

macro quickload()
    esc(
        quote
        size_df = load_gz_sizes("/home/chipbuster/tmp/outdata/gzips/")
        (streams, stream_df) = load_proto_dfs("/home/chipbuster/tmp/outdata/protos/")
        println("Loaded")
    end
    )
end
