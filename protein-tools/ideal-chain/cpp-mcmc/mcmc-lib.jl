using StaticArrays
using LinearAlgebra
using Logging
using HDF5

struct SimMetadata
    accepted::Int
    rejected::Int
    lo::Float64
    hi::Float64
    numAngles::Int
    numSteps::Int
    skips::Int
    gaussWidth::Float64
end

function gen_points(ϕs)
    pts = Matrix{Float64}(undef, 2, length(ϕs) + 2)
    pts[:,1] = [0.0,0.0]
    pts[:,2] = [1.0,0.0]
    θs = ϕs .+ π |> cumsum .|> mod2pi
    for (i, θ) in enumerate(θs)
        pts[:,i + 2] = pts[:,i + 1] + [ cos(θ), sin(θ) ]
    end
    pts
end

gen_end_point(ϕs) = gen_points(ϕs)[:,end]

e2e_dist(ϕs) = gen_end_point(ϕs) |> norm

function get_sim_angles(filename)
    h5open(filename) do f
        read(f, "angles")
    end
end

function get_sim_metadata(filename)
    h5open(filename) do f
        dset = f["angles"]

        accepted = read_attribute(dset, "accepted")
        rejected = read_attribute(dset, "rejected")
        lo = read_attribute(dset, "lo")
        hi = read_attribute(dset, "hi")
        numAngles = read_attribute(dset, "numAngles")
        numSteps = read_attribute(dset, "numsteps")
        skips = read_attribute(dset, "skips")
        gaussWidth = read_attribute(dset, "gaussWidth")

        SimMetadata(accepted, rejected, lo, hi, numAngles, numSteps, skips, gaussWidth)
    end
end