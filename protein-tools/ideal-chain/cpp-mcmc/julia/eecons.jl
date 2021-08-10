# param1 = lo
# param2 = hi

include("mcmc-lib.jl")
using StatsPlots
using Discretizers

# WORKDIR=ENV["SCRATCH"]
WORKDIR="/tmp"

function points_from_angles(angles)
    ϕs = angles .+ π |> cumsum .|> mod2pi
    pts = Matrix{Float64}(undef, 2, length(angles) + 2)
    pts[:,1] = [0.0, 0.0]
    pts[:,2] = [1.0, 0.0]
    for (i, ϕ) in enumerate(ϕs)
        pts[:,i+2] = pts[:,i+1] + [cos(ϕ), sin(ϕ)]
    end
    pts
end

function write_points(infile, outfile, nbins, nskip)
    ang = load_sim_angles(infile)
    binedges = range(0.0,2π, length=nbins+1)
    disc = LinearDiscretizer(binedges, UInt8)

    open(outfile, "w") do f
        for c in eachcol(@view ang[:,1:nskip:end])
            bins = encode(disc, c)
            write(f, bins)
        end
    end
end

function get_compressed_size(infile, nbins, nskip)
    binfile=joinpath(WORKDIR, infile * ".bin")
    xzfile=binfile * ".xz"
    write_points(infile, binfile, nbins, nskip)
    cmd = `xz -9e --keep --format=raw --suffix=.xz $(binfile)`
    run(cmd)
    sz = filesize(binfile * ".xz")
    rm(binfile)
    rm(xzfile)
    return sz
end