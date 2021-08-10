using StaticArrays
using LinearAlgebra
using Logging
using HDF5

struct SimMetadata
    accepted::Int
    rejected::Int
    param1::Float64
    param2::Float64
    param3::Float64
    numAngles::Int
    numSteps::Int
    skips::Int
    gaussWidth::Float64
end

accept_frac(simmetadata) = simmetadata.accepted / (simmetadata.accepted + simmetadata.rejected)

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

function load_sim_angles(filename)
    h5open(filename) do f
        read(f, "angles")
    end
end

function load_sim_metadata(filename)
    h5open(filename) do f
        dset = f["angles"]

        accepted = read_attribute(dset, "accepted")
        rejected = read_attribute(dset, "rejected")
        p1 = read_attribute(dset, "p1")
        p2 = read_attribute(dset, "p2")
        p3 = read_attribute(dset, "p3")
        numAngles = read_attribute(dset, "numAngles")
        numSteps = read_attribute(dset, "numsteps")
        skips = read_attribute(dset, "skips")
        gaussWidth = read_attribute(dset, "gaussWidth")

        SimMetadata(accepted, rejected, p1, p2, p3, numAngles, numSteps, skips, gaussWidth)
    end
end