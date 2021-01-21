using Base.Threads
using Serialization
using CSV

include("interactive.jl")

df = DataFrame(N=Int[], R=Float64[], jit=Int[], jittertype=String[], key=Int[])
data = Vector{GZJSON}()

l = SpinLock()
@threads for kind in ("spread", "cluster")
    dir = "/home/chipbuster/tmp/data/configs/jitter_$(kind)/jsons"
    key = 1
    for fn in readdir(dir)
        println("Processing $(fn)")
        j = get_params_from_fn(fn)
        res = parse_gzip_json(joinpath(dir, fn))
        lock(l)
        push!(df, (j.natoms, j.ext_frac, j.njitter, j.jtype, key))
        push!(data, res)
        unlock(l)
        key += 1
    end
end

CSV.write("gzdata.csv", df)
serialize("gzdata.serial", data)