# param1 = wall depth


WORKDIR="/tmp"
include("mcmc-lib.jl")
using Discretizers
using StaticArrays

function points_from_angles(angles)
    ϕs = angles
    pts = Matrix{Float64}(undef, 2, length(angles) + 1)
    pts[:,1] = [0.0, 0.0]
    for (i, ϕ) in enumerate(ϕs)
        pts[:,i+1] = pts[:,i] + [cos(ϕ), sin(ϕ)]
    end
    pts
end

function write_points(infile, outfile, nbins, nskip)
    ang = load_sim_angles(infile)
    binedges = range(0.0,2π, length=nbins+1)
    disc = LinearDiscretizer(binedges, UInt8)

    dvec = zeros(SVector{2})

    # Since representation in sim is 2AA, take difference of successive
    open(outfile, "w") do f
        for c in eachcol(ang[:,1:nskip:end])
            bins = encode(disc, diff(c; dims=1))
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